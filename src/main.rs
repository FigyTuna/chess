
// Imports

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::collections::HashMap;
use std::time::Instant;

// Data Structures

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct Pos(usize, usize);

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct Piece {
    piece_type: PieceType,
    team: bool
}

struct Board {
    board: [[Option<Piece>; 8]; 8],
    pieces: HashMap<Piece, Vec<Pos>>,
    to_move: bool,
    running: Option<Pos>,
    can_castle: [bool; 4],
    positions: HashMap<u64, u32>,
    info_eval_count: u64
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
enum MoveNotationData {
    Move,
    KSC,
    QSC,
    PQ,
    PK,
    PB,
    PR
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct Move {
    piece: Piece,
    from: Pos,
    to: Pos,
    promote: bool,
    castle_rook: Option<(Pos, Pos)>,
    captured: Option<(Piece, Pos)>,
    running: Option<Pos>,
    was_running: Option<Pos>,
    can_castle: [bool; 4],
    prev_can_castle: [bool; 4],
    notation_data: MoveNotationData
}

#[derive(Copy, Clone)]
enum Rating {
    Evaluation{score: i64},
    Checkmate{score: bool, turns: u32}
}

struct MoveTree {
    tree: Tree,
    rating: Rating,
    raw_rating: Rating
}

enum Tree {
    Node{branches: Box<HashMap<Move, Box<MoveTree>>>},
    BareNode{moves: Box<Vec<Move>>},
    Leaf
}

// Global Options

const MAX_CPU_MOVES: i32 = 150;

const DEBUG: bool = false;
const ENGINE_INFO: bool = false;

// Constants

const CASTLE_WHITE: usize = 0;
const CASTLE_BLACK: usize = 2;
const KING_SIDE_CASTLE: usize = 0;
const QUEEN_SIDE_CASTLE: usize = 1;

// Initialization

fn initial_board() -> Box<Board> {
    let mut board = Box::new(Board{
        board: [[None; 8]; 8],
        pieces: HashMap::new(),
        to_move: true,
        running: None,
        can_castle: [true; 4],
        positions: HashMap::new(),
        info_eval_count: 0
    });
    fn init_piece_full(board: &mut [[Option<Piece>; 8]; 8], pieces: &mut HashMap<Piece, Vec<Pos>>, piece: Piece, pos: Pos) -> () {
        board[pos.0][pos.1] = Some(piece);
        let e = pieces.entry(piece).or_insert(Vec::new());
        e.push(pos);
    };
    let mut init_piece = |piece, pos| init_piece_full(&mut board.board, &mut board.pieces, piece, pos);
    init_piece(Piece{piece_type: PieceType::Pawn, team: true}, Pos(1, 0));
    init_piece(Piece{piece_type: PieceType::Pawn, team: true}, Pos(1, 1));
    init_piece(Piece{piece_type: PieceType::Pawn, team: true}, Pos(1, 2));
    init_piece(Piece{piece_type: PieceType::Pawn, team: true}, Pos(1, 3));
    init_piece(Piece{piece_type: PieceType::Pawn, team: true}, Pos(1, 4));
    init_piece(Piece{piece_type: PieceType::Pawn, team: true}, Pos(1, 5));
    init_piece(Piece{piece_type: PieceType::Pawn, team: true}, Pos(1, 6));
    init_piece(Piece{piece_type: PieceType::Pawn, team: true}, Pos(1, 7));
    init_piece(Piece{piece_type: PieceType::Rook, team: true}, Pos(0, 0));
    init_piece(Piece{piece_type: PieceType::Knight, team: true}, Pos(0, 1));
    init_piece(Piece{piece_type: PieceType::Bishop, team: true}, Pos(0, 2));
    init_piece(Piece{piece_type: PieceType::Queen, team: true}, Pos(0, 3));
    init_piece(Piece{piece_type: PieceType::King, team: true}, Pos(0, 4));
    init_piece(Piece{piece_type: PieceType::Bishop, team: true}, Pos(0, 5));
    init_piece(Piece{piece_type: PieceType::Knight, team: true}, Pos(0, 6));
    init_piece(Piece{piece_type: PieceType::Rook, team: true}, Pos(0, 7));
    init_piece(Piece{piece_type: PieceType::Pawn, team: false}, Pos(6, 1));
    init_piece(Piece{piece_type: PieceType::Pawn, team: false}, Pos(6, 2));
    init_piece(Piece{piece_type: PieceType::Pawn, team: false}, Pos(6, 0));
    init_piece(Piece{piece_type: PieceType::Pawn, team: false}, Pos(6, 3));
    init_piece(Piece{piece_type: PieceType::Pawn, team: false}, Pos(6, 4));
    init_piece(Piece{piece_type: PieceType::Pawn, team: false}, Pos(6, 5));
    init_piece(Piece{piece_type: PieceType::Pawn, team: false}, Pos(6, 6));
    init_piece(Piece{piece_type: PieceType::Pawn, team: false}, Pos(6, 7));
    init_piece(Piece{piece_type: PieceType::Rook, team: false}, Pos(7, 0));
    init_piece(Piece{piece_type: PieceType::Knight, team: false}, Pos(7, 1));
    init_piece(Piece{piece_type: PieceType::Bishop, team: false}, Pos(7, 2));
    init_piece(Piece{piece_type: PieceType::Queen, team: false}, Pos(7, 3));
    init_piece(Piece{piece_type: PieceType::King, team: false}, Pos(7, 4));
    init_piece(Piece{piece_type: PieceType::Bishop, team: false}, Pos(7, 5));
    init_piece(Piece{piece_type: PieceType::Knight, team: false}, Pos(7, 6));
    init_piece(Piece{piece_type: PieceType::Rook, team: false}, Pos(7, 7));
    board
}

// Board Manipulation

fn remove_item<V>(v: &mut Vec<V>, x: &V) -> () where V: PartialEq {
    let mut idx = None;
    let mut i = 0;
    for y in v.iter() {
        if x == y {
            idx = Some(i);
            break;
        }
        i += 1;
    };
    match idx {
        Some(i) => { v.remove(i); },
        None => ()
    };
}

fn perform_move(board: &mut Board, m: &Move) -> () {
    match m.captured {
        Some((piece, pos)) => { 
            if DEBUG && piece.piece_type == PieceType::King {
                println!("Error: King captured {}", notate_move(m));
            }
            remove_item(&mut board.pieces.entry(piece).or_default(), &pos);
            board.board[pos.0][pos.1] = None
        },
        None => ()
    }

    if m.promote {
        remove_item(&mut board.pieces.entry(Piece{piece_type: PieceType::Pawn, team: m.piece.team}).or_default(), &m.from);
        board.pieces.entry(m.piece).or_default().push(m.to);
    }
    else {
        for pos in board.pieces.entry(m.piece).or_default() {
            if *pos == m.from { *pos = m.to }
        };
    }
    board.board[m.to.0][m.to.1] = Some(m.piece);
    board.board[m.from.0][m.from.1] = None;

    match m.castle_rook {
        Some((from, to)) => {
            for pos in board.pieces.entry(Piece{piece_type: PieceType::Rook, team: m.piece.team}).or_default() {
                if *pos == from { *pos = to }
            };
            board.board[to.0][to.1] = Some(Piece{piece_type: PieceType::Rook, team: m.piece.team});
            board.board[from.0][from.1] = None;
        },
        None => ()
    }

    board.running = m.running;
    board.can_castle = m.can_castle;

    board.to_move = !board.to_move;
}

fn undo_move(board: &mut Board, m: &Move) -> () {
    if m.promote {
        remove_item(board.pieces.entry(m.piece).or_default(), &m.to);
        board.pieces.entry(Piece{piece_type: PieceType::Pawn, team: m.piece.team}).or_default().push(m.from);
        board.board[m.from.0][m.from.1] = Some(Piece{piece_type: PieceType::Pawn, team: m.piece.team});
    }
    else {
        for pos in board.pieces.entry(m.piece).or_default() {
            if *pos == m.to { *pos = m.from }
        };
        board.board[m.from.0][m.from.1] = Some(m.piece);
    }
    board.board[m.to.0][m.to.1] = None;

    match m.captured {
        Some((piece, pos)) => {
            board.board[pos.0][pos.1] = Some(piece);
            board.pieces.entry(piece).or_default().push(pos);
        },
        None => ()
    }

    match m.castle_rook {
        Some((from, to)) => {
            for pos in board.pieces.entry(Piece{piece_type: PieceType::Rook, team: m.piece.team}).or_default() {
                if *pos == to { *pos = from }
            };
            board.board[from.0][from.1] = Some(Piece{piece_type: PieceType::Rook, team: m.piece.team});
            board.board[to.0][to.1] = None;
        },
        None => ()
    }

    board.running = m.was_running;
    board.can_castle = m.prev_can_castle;

    board.to_move = !board.to_move;
}

fn increment_position(board: &mut Board, h: u64) -> () {
    let n = board.positions.entry(h).or_insert(0);
    *n += 1;
}

fn decrement_position(board: &mut Board, h: u64) -> () {
    let n = board.positions.entry(h).or_insert(1);
    *n -= 1;
}

// Move Generation Helpers

fn basic_move(board: &Board, piece: Piece, from: &Pos, to: &Pos, captured: Option<Piece>) -> Move {
    let capturedp = match captured {
        Some(piece) => Some((piece, *to)),
        None => None
    };
    Move{
        piece: piece,
        from: *from,
        to: *to,
        promote: false,
        castle_rook: None,
        captured: capturedp,
        running: None,
        was_running: board.running,
        can_castle: update_can_castle(from, board.can_castle),
        prev_can_castle: board.can_castle,
        notation_data: MoveNotationData::Move
    }
}

fn add_promotion_moves(board: &mut Board, moves: &mut Vec<Move>, from: &Pos, to: &Pos, captured: Option<Piece>) -> () {
    fn promote_move(board: &Board, piece: Piece, from: &Pos, to: &Pos, captured: Option<Piece>, mnd: MoveNotationData) -> Move {
        let capturedp = match captured {
            Some(piece) => Some((piece, *to)),
            None => None
        };
        Move{
            piece: piece,
            from: *from,
            to: *to,
            promote: true,
            castle_rook: None,
            captured: capturedp,
            running: None,
            was_running: board.running,
            can_castle: update_can_castle(from, board.can_castle),
            prev_can_castle: board.can_castle,
            notation_data: mnd
        }
    };
    add_if_no_check(board, moves, promote_move(board, Piece{piece_type: PieceType::Bishop, team: board.to_move}, from, to, captured, MoveNotationData::PB));
    add_if_no_check(board, moves, promote_move(board, Piece{piece_type: PieceType::Knight, team: board.to_move}, from, to, captured, MoveNotationData::PK));
    add_if_no_check(board, moves, promote_move(board, Piece{piece_type: PieceType::Rook, team: board.to_move}, from, to, captured, MoveNotationData::PR));
    add_if_no_check(board, moves, promote_move(board, Piece{piece_type: PieceType::Queen, team: board.to_move}, from, to, captured, MoveNotationData::PQ));
}

fn update_can_castle(pos: &Pos, c: [bool; 4]) -> [bool; 4] {
    match pos {
        Pos(0, 0) => [c[0], false, c[2], c[3]],
        Pos(0, 7) => [false, c[1], c[2], c[3]],
        Pos(7, 0) => [c[0], c[1], c[2], false],
        Pos(7, 7) => [c[0], c[1], false, c[3]],
        Pos(0, 4) => [false, false, c[2], c[3]],
        Pos(7, 4) => [c[0], c[1], false, false],
        _ => [c[0], c[1], c[2], c[3]]
    }
}

fn add_if_no_check(board: &mut Board, moves: &mut Vec<Move>, m: Move) -> () {
    let temp = if DEBUG { Some(board.board.clone()) } else { None };
    perform_move(board, &m);
    if !is_in_check(&board, !board.to_move) {
        moves.push(m);
    }
    undo_move(board, &m);
    if DEBUG && Some(board.board) != temp {
        panic!("OH NO {} at add_if_no_check", notate_move(&m));
    }
}

// Move Generation

fn gen_moves(board: &mut Board) -> Box<Vec<Move>> {
    let mut moves = Box::new(Vec::new());
    for pos in &board.pieces.get(&Piece{piece_type: PieceType::Rook, team: board.to_move}).expect("Error: pieces access").clone() {
        for dir in ([(-1, 0), (1, 0), (0, -1), (0, 1)] as [(i32, i32); 4]).iter() {
            gen_line_moves(board, &mut moves, pos, *dir, PieceType::Rook);
        }
    }
    for pos in &board.pieces.get(&Piece{piece_type: PieceType::Bishop, team: board.to_move}).expect("Error: pieces access").clone() {
        for dir in ([(-1, -1), (1, -1), (-1, 1), (1, 1)] as [(i32, i32); 4]).iter() {
            gen_line_moves(board, &mut moves, pos, *dir, PieceType::Bishop);
        }
    }
    for pos in &board.pieces.get(&Piece{piece_type: PieceType::Queen, team: board.to_move}).expect("Error: pieces access").clone() {
        for dir in ([(-1, -1), (1, -1), (-1, 1), (1, 1), (-1, 0), (1, 0), (0, -1), (0, 1)] as [(i32, i32); 8]).iter() {
            gen_line_moves(board, &mut moves, pos, *dir, PieceType::Queen);
        }
    }
    for pos in &board.pieces.get(&Piece{piece_type: PieceType::King, team: board.to_move}).expect("Error: pieces access").clone() {
        for dir in ([(-1, -1), (1, -1), (-1, 1), (1, 1), (-1, 0), (1, 0), (0, -1), (0, 1)] as [(i32, i32); 8]).iter() {
            gen_line_moves(board, &mut moves, pos, *dir, PieceType::King);
        }
    }
    for pos in &board.pieces.get(&Piece{piece_type: PieceType::Knight, team: board.to_move}).expect("Error: pieces access").clone() {
        gen_knight_moves(board, &mut moves, pos);
    }
    for pos in &board.pieces.get(&Piece{piece_type: PieceType::Pawn, team: board.to_move}).expect("Error: pieces access").clone() {
        gen_pawn_moves(board, &mut moves, pos);
    }
    gen_castle_moves(board, &mut moves);
    moves
}

fn gen_knight_moves(board: &mut Board, moves: &mut Vec<Move>, pos: &Pos) -> () {
    for (x, y) in [(-1, -2), (-2, -1), (1, -2), (-2, 1), (-1, 2), (2, -1), (1, 2), (2, 1)].iter() {
        if pos.0 as i32 + x >= 0 && pos.0 as i32 + x <= 7 && pos.1 as i32 + y >= 0 && pos.1 as i32 + y <= 7 {
            let p0 = (pos.0 as i32 + x) as usize;
            let p1 = (pos.1 as i32 + y) as usize;
            match &board.board[p0][p1] {
                Some(piece) if board.to_move != piece.team => {
                    let m = basic_move(board, Piece{piece_type: PieceType::Knight, team: board.to_move}, pos, &Pos(p0, p1), Some(*piece));
                    add_if_no_check(board, moves, m);
                },
                None => {
                    add_if_no_check(board, moves, basic_move(board, Piece{piece_type: PieceType::Knight, team: board.to_move}, pos, &Pos(p0, p1), None));
                },
                _ => ()
            }
        }
    }
}

fn gen_pawn_moves(board: &mut Board, moves: &mut Vec<Move>, pos: &Pos) -> () {
    let dir: i32 = if board.to_move {1} else {-1};
    let one_dir = (pos.0 as i32 + dir) as usize;
    let one_space = match board.board[one_dir][pos.1] {
        None => true,
        _ => false
    };
    if one_space {
        if (board.to_move && pos.0 == 6) || (!board.to_move && pos.0 == 1) {
            add_promotion_moves(board, moves, pos, &Pos(one_dir, pos.1), None);
        }
        else {
            add_if_no_check(board, moves, basic_move(board, Piece{piece_type: PieceType::Pawn, team: board.to_move}, pos, &Pos(one_dir, pos.1), None));
        }
    };
    if ((board.to_move && pos.0 == 1) || (!board.to_move && pos.0 == 6)) && one_space {
        match board.board[(pos.0 as i32 + (dir * 2)) as usize][pos.1] {
            None => add_if_no_check(board, moves, Move{
                piece: Piece{piece_type: PieceType::Pawn, team: board.to_move},
                from: *pos,
                to: Pos((pos.0 as i32 + (dir * 2)) as usize, pos.1),
                promote: false,
                castle_rook: None,
                captured: None,
                running: Some(Pos((pos.0 as i32 + (dir * 2)) as usize, pos.1)),
                was_running: board.running,
                can_castle: board.can_castle,
                prev_can_castle: board.can_castle,
                notation_data: MoveNotationData::Move
            }),
            _ => ()
        }
    };
    if pos.1 > 0 {
        match board.board[one_dir][pos.1 - 1] {
            Some(piece) if board.to_move != piece.team => {
                if one_dir == 0 || one_dir == 7 {
                    add_promotion_moves(board, moves, pos, &Pos(one_dir, pos.1 - 1), Some(piece));
                }
                else
                {
                    add_if_no_check(board, moves, basic_move(board, Piece{piece_type: PieceType::Pawn, team: board.to_move}, pos, &Pos(one_dir, pos.1 - 1), Some(piece)));
                }
            },
            None => if board.running == Some(Pos(pos.0, pos.1 - 1)) {
                match board.board[pos.0][pos.1 - 1] {
                    Some(piece) if board.to_move != piece.team => add_if_no_check(board, moves, Move{
                        piece: Piece{piece_type: PieceType::Pawn, team: board.to_move},
                        from: *pos,
                        to: Pos(one_dir, pos.1 - 1),
                        promote: false,
                        castle_rook: None,
                        captured: Some((piece, Pos(pos.0, pos.1 - 1))),
                        running: None,
                        was_running: board.running,
                        can_castle: board.can_castle,
                        prev_can_castle: board.can_castle,
                        notation_data: MoveNotationData::Move
                    }),
                    _ => ()
                }
            },
            _ => ()
        }
    };
    if pos.1 < 7 {
        match board.board[one_dir][pos.1 + 1] {
            Some(piece) if board.to_move != piece.team => {
                if one_dir == 0 || one_dir == 7 {
                    add_promotion_moves(board, moves, pos, &Pos(one_dir, pos.1 + 1), Some(piece));
                }
                else
                {
                    add_if_no_check(board, moves, basic_move(board, Piece{piece_type: PieceType::Pawn, team: board.to_move}, pos, &Pos(one_dir, pos.1 + 1), Some(piece)));
                }
            }
            None =>  if board.running == Some(Pos(pos.0, pos.1 + 1)) {
                match board.board[pos.0][pos.1 + 1] {
                    Some(piece) if board.to_move != piece.team => add_if_no_check(board, moves, Move{
                        piece: Piece{piece_type: PieceType::Pawn, team: board.to_move},
                        from: *pos,
                        to: Pos(one_dir, pos.1 + 1),
                        promote: false,
                        castle_rook: None,
                        captured: Some((piece, Pos(pos.0, pos.1 + 1))),
                        running: None,
                        was_running: board.running,
                        can_castle: board.can_castle,
                        prev_can_castle: board.can_castle,
                        notation_data: MoveNotationData::Move
                    }),
                    _ => ()
                }
            },
            _ => ()
        }
    };
}

fn gen_castle_moves(board: &mut Board, moves: &mut Vec<Move>) -> () {
    let r = if board.to_move {0} else {7};
    if board.can_castle[if board.to_move {CASTLE_WHITE + KING_SIDE_CASTLE} else {CASTLE_BLACK + KING_SIDE_CASTLE}] {
        match (board.board[r][5], board.board[r][6]) {
            (None, None) => if !is_in_check(board, board.to_move) { add_if_no_check(board, moves,
                Move{
                    piece: Piece{piece_type: PieceType::King, team: board.to_move},
                    from: Pos(r, 4),
                    to: Pos(r, 6),
                    promote: false,
                    castle_rook: Some((Pos(r, 7), Pos(r,5))),
                    captured: None,
                    running: None,
                    was_running: board.running,
                    can_castle: update_can_castle(&Pos(r,4), board.can_castle),
                    prev_can_castle: board.can_castle,
                    notation_data: MoveNotationData::KSC
                }
            )},
            _ => ()
        };
    }
    if board.can_castle[if board.to_move {CASTLE_WHITE + QUEEN_SIDE_CASTLE} else {CASTLE_BLACK + QUEEN_SIDE_CASTLE}] {
        match (board.board[r][1], board.board[r][2], board.board[r][3]) {
            (None, None, None) => if !is_in_check(board, board.to_move) { add_if_no_check(board, moves,
                Move{
                    piece: Piece{piece_type: PieceType::King, team: board.to_move},
                    from: Pos(r, 4),
                    to: Pos(r, 2),
                    promote: false,
                    castle_rook: Some((Pos(r, 0), Pos(r,3))),
                    captured: None,
                    running: None,
                    was_running: board.running,
                    can_castle: update_can_castle(&Pos(r,4), board.can_castle),
                    prev_can_castle: board.can_castle,
                    notation_data: MoveNotationData::QSC
                }
            )},
            _ => ()
        };
    }
}

fn gen_line_moves(board: &mut Board, moves: &mut Vec<Move>, pos: &Pos, (r_dir, f_dir): (i32, i32), piece_type: PieceType) -> () {
    fn extend_dir((r, f): (i32, i32)) -> (i32, i32) {
        let rp = if r > 0 {r + 1} else if r < 0 {r - 1} else {r};
        let fp = if f > 0 {f + 1} else if f < 0 {f - 1} else {f};
        (rp, fp)
    }
    let r = pos.0 as i32 + r_dir;
    let f = pos.1 as i32 + f_dir;
    if r >= 0 && r <= 7 && f >= 0 && f <= 7 {
        let ru = r as usize;
        let fu = f as usize;
        match &board.board[ru][fu] {
            None => {
                add_if_no_check(board, moves, basic_move(board, Piece{piece_type: piece_type, team: board.to_move}, pos, &Pos(ru, fu), None));
                if piece_type != PieceType::King {
                    gen_line_moves(board, moves, pos, extend_dir((r_dir, f_dir)), piece_type);
                }
            },
            Some(piece) if board.to_move != piece.team => {
                let m = basic_move(board, Piece{piece_type: piece_type, team: board.to_move}, pos, &Pos(ru, fu), Some(*piece));
                add_if_no_check(board, moves, m);
            },
            _ => ()
        }
    }
}

// Check Detection

fn is_in_check(board: &Board, team: bool) -> bool {
    if DEBUG && board.pieces[&Piece{piece_type: PieceType::King, team: team}].len() == 0 {
        println!("Error: is_in_check: Missing king");
    }
    let pos = board.pieces[&Piece{piece_type: PieceType::King, team: team}][0];
    let mut check = false;
    for (dx, dy) in [(0, -1), (-1, 0), (0, 1), (1, 0), (-1, -1), (1, -1), (-1, 1), (1, 1)].iter() {
        let mut r = pos.0 as i32;
        let mut f = pos.1 as i32;
        let mut first = true;
        loop {
            r += dx;
            f += dy;
            if r < 0 || r > 7 || f < 0 || f > 7 {break}
            match board.board[r as usize][f as usize] {
                Some(Piece{piece_type:PieceType::Bishop, team: pteam}) if (team != pteam) && (dx + dy).abs() % 2 == 0 => {check = true; break}
                Some(Piece{piece_type:PieceType::Rook, team: pteam}) if (team != pteam) && (dx + dy).abs() % 2 == 1 => {check = true; break}
                Some(Piece{piece_type:PieceType::Queen, team: pteam}) if team != pteam => {check = true; break}
                Some(Piece{piece_type:PieceType::King, team: pteam}) if (team != pteam) && first => {check = true; break}
                None => (),
                _ => break
            }
            first = false;
        }
    }
    if !check {
        for (x, y) in [(-1, -2), (-2, -1), (1, -2), (-2, 1), (-1, 2), (2, -1), (1, 2), (2, 1)].iter() {
            if pos.0 as i32 + x >= 0 && pos.0 as i32 + x <= 7 && pos.1 as i32 + y >= 0 && pos.1 as i32 + y <= 7 {
                match board.board[(pos.0 as i32 + x) as usize][(pos.1 as i32 + y) as usize] {
                    Some(Piece{piece_type: PieceType::Knight, team: pteam}) if team != pteam => {check = true; break},
                    _ => ()
                }
            }
        }
    }
    if !check {
        let dir = if board.to_move {-1} else {1};
        for (dx, dy) in [(dir, -1), (dir, 1)].iter() {
            if pos.0 as i32 + dx >= 0 && pos.0 as i32 + dx <= 7 && pos.1 as i32 + dy >= 0 && pos.1 as i32 + dy <= 7 {
                match board.board[(pos.0 as i32 + dx) as usize][(pos.1 as i32 + dy) as usize] {
                    Some(Piece{piece_type: PieceType::Pawn, team: pteam}) if team != pteam => {check = true; break},
                    _ => ()
                }
            }
        }
    }
    check
}

// Board Evaluation and Move Choice

fn cmp_ratings (r1: &Rating, r2: &Rating) -> bool {
    match (r1, r2) {
        (Rating::Evaluation{score: s1}, Rating::Evaluation{score: s2}) => s1 > s2,
        (Rating::Checkmate{score: b1, turns: t1}, Rating::Checkmate{score: b2, turns: t2}) if b1 == b2 => (t1 > t2) ^ b1,
        (Rating::Checkmate{score: b, turns: _}, _) => *b,
        (_, Rating::Checkmate{score: b, turns: _}) => !*b
    }
}

fn join_ratings (raw_rating: &Rating, sub_rating: &Rating) -> Rating {
    match (raw_rating, sub_rating) {
        (Rating::Evaluation{score: rs}, Rating::Evaluation{score: ss}) => Rating::Evaluation{score: rs + ss * 10},
        (Rating::Checkmate{score: b, turns: t}, _) => Rating::Checkmate{score: *b, turns: *t},
        (_, Rating::Checkmate{score: b, turns: t}) => Rating::Checkmate{score: *b, turns: t + 1}
    }
}

fn evaluate_board(board: &mut Board) -> (Rating, Box<Vec<Move>>) {
    if ENGINE_INFO { board.info_eval_count += 1 };
    let moves = gen_moves(board);
    let rating = if moves.len() < 1 {
        if is_in_check(board, board.to_move) {
            Rating::Checkmate{score: !board.to_move, turns: 0}
        }
        else {
            Rating::Evaluation{score: 0}
        }
    }
    else {
        let mut ret = 0;
        ret += 1 * board.pieces[&Piece{piece_type: PieceType::Pawn, team: true}].len() as i64;
        ret += -1 * board.pieces[&Piece{piece_type: PieceType::Pawn, team: false}].len() as i64;
        ret += 3 * board.pieces[&Piece{piece_type: PieceType::Knight, team: true}].len() as i64;
        ret += -3 * board.pieces[&Piece{piece_type: PieceType::Knight, team: false}].len() as i64;
        ret += 3 * board.pieces[&Piece{piece_type: PieceType::Bishop, team: true}].len() as i64;
        ret += -3 * board.pieces[&Piece{piece_type: PieceType::Bishop, team: false}].len() as i64;
        ret += 5 * board.pieces[&Piece{piece_type: PieceType::Rook, team: true}].len() as i64;
        ret += -5 * board.pieces[&Piece{piece_type: PieceType::Rook, team: false}].len() as i64;
        ret += 9 * board.pieces[&Piece{piece_type: PieceType::Queen, team: true}].len() as i64;
        ret += -9 * board.pieces[&Piece{piece_type: PieceType::Queen, team: false}].len() as i64;
        Rating::Evaluation{score: ret}
    };
    (rating, moves)
}

fn fill_move_tree(board: &mut Board, move_tree: &mut MoveTree, depth: u32) -> (Rating, Option<Move>) {
    fn simulate_move(board: &mut Board, mt: &mut MoveTree, m: &Move, depth: u32) -> Rating {
        let temp = if DEBUG { Some(board.board.clone()) } else { None };
        perform_move(board, m);
        let h = hash_board(board);
        increment_position(board, h);
        let ret = if repitition(board) {
                mt.rating
            }
            else {
                fill_move_tree(board, mt, depth - 1).0
            };
        decrement_position(board, h);
        undo_move(board, &m);
        if DEBUG && Some(board.board) != temp {
            panic!("OH NO {}", notate_move(&m));
        };
        ret
    };
    if depth > 0 {
        match &mut move_tree.tree {
            Tree::Leaf => {
                let mut hm: Box<HashMap<Move, Box<MoveTree>>> = Box::new(HashMap::new());
                let (raw_rating, moves) = evaluate_board(board);
                move_tree.raw_rating = raw_rating;
                let mut it = moves.iter();
                let ret = match it.next() {
                    Some(first_move) => {
                        let mut mo = Some(*first_move);
                        let mut first_mt = Box::new(MoveTree{rating: Rating::Evaluation{score: 0}, raw_rating: Rating::Evaluation{score: 0}, tree: Tree::Leaf});
                        let mut max_rating = simulate_move(board, &mut first_mt, &first_move, depth);
                        hm.insert(*first_move, first_mt);
                        for m in it {
                            let mut mt = Box::new(MoveTree{rating: Rating::Evaluation{score: 0}, raw_rating: Rating::Evaluation{score: 0}, tree: Tree::Leaf});
                            let r = simulate_move(board, &mut mt, &m, depth);
                            hm.insert(*m, mt);
                            if cmp_ratings(&r, &max_rating) ^ !board.to_move {
                                max_rating = r;
                                mo = Some(*m);
                            }
                        }
                        move_tree.rating = join_ratings(&move_tree.raw_rating, &max_rating);
                        (move_tree.rating, mo)
                    },
                    None => {
                        move_tree.rating = move_tree.raw_rating;
                        (move_tree.rating, None)
                    }
                };
                move_tree.tree = Tree::Node{branches: hm};
                ret
            },
            Tree::BareNode{moves: ms} => {
                let mut hm: Box<HashMap<Move, Box<MoveTree>>> = Box::new(HashMap::new());
                let mut it = ms.iter();
                let ret = match it.next() {
                    Some(first_move) => {
                        let mut mo = Some(*first_move);
                        let mut first_mt = Box::new(MoveTree{rating: Rating::Evaluation{score: 0}, raw_rating: Rating::Evaluation{score: 0}, tree: Tree::Leaf});
                        let mut max_rating = simulate_move(board, &mut first_mt, &first_move, depth);
                        hm.insert(*first_move, first_mt);
                        for m in it {
                            let mut mt = Box::new(MoveTree{rating: Rating::Evaluation{score: 0}, raw_rating: Rating::Evaluation{score: 0}, tree: Tree::Leaf});
                            let r = simulate_move(board, &mut mt, &m, depth);
                            hm.insert(*m, mt);
                            if cmp_ratings(&r, &max_rating) ^ !board.to_move {
                                max_rating = r;
                                mo = Some(*m);
                            }
                        }
                        move_tree.rating = join_ratings(&move_tree.raw_rating, &max_rating);
                        (move_tree.rating, mo)
                    },
                    None => {
                        move_tree.rating = move_tree.raw_rating;
                        (move_tree.rating, None)
                    }
                };
                move_tree.tree = Tree::Node{branches: hm};
                ret
            },
            Tree::Node{branches: br} => {
                let mut it = br.iter_mut();
                match it.next() {
                    Some((first_move, first_mt)) => {
                        let mut mo: Option<Move> = Some(first_move.clone());
                        let mut max_rating = simulate_move(board, first_mt, &first_move, depth);
                        for (m, mt) in it {
                            let r = simulate_move(board, mt, &m, depth);
                            if cmp_ratings(&r, &max_rating) ^ !board.to_move {
                                max_rating = r;
                                mo = Some(*m);
                            }
                        }
                        (join_ratings(&move_tree.raw_rating, &max_rating), mo)
                    },
                    None => panic!("fill_move_tree: Tree::Node")
                }
            }
        }
    }
    else {
        let rating = match move_tree.tree {
            Tree::Leaf => {
                let (rating, moves) = evaluate_board(board);
                move_tree.rating = rating;
                move_tree.raw_rating = rating;
                move_tree.tree = Tree::BareNode{moves: moves};
                rating
            },
            _ => move_tree.raw_rating
        };
        (rating, None)
    }
}

fn adjust_move_tree(move_tree: &mut MoveTree, m: &Move) -> Box<MoveTree> {
    match &mut move_tree.tree {
        Tree::Node{branches: br} => match br.remove(m) {
            Some(mt) => mt,
            _ => panic!("Error: adjust_move_tree: No entry for {}.", notate_move(&m))
        },
        _ => {
            Box::new(MoveTree{rating: Rating::Evaluation{score: 0}, raw_rating: Rating::Evaluation{score: 0}, tree: Tree::Leaf})
        }
    }
}

// Repitition Detection

fn hash_board(board: &Board) -> u64 {
    let mut hasher = DefaultHasher::new();
    board.board.hash(&mut hasher);
    board.to_move.hash(&mut hasher);
    board.running.hash(&mut hasher);
    board.can_castle.hash(&mut hasher);
    hasher.finish()
}

fn repitition(board: &Board) -> bool {
    match board.positions.get(&hash_board(board)) {
        Some(n) => *n >= 2,
        None => false
    }
}

// Move Input Methods

fn cpu_move(board: &mut Board, move_tree: &mut MoveTree, depth: u32) -> Move {
    let now = Instant::now();
    match fill_move_tree(board, move_tree, depth) {
        (rating, Some(m)) => {
            if ENGINE_INFO {
                match rating {
                    Rating::Evaluation{score: s} => println!("Evaluated at a {}.", s),
                    Rating::Checkmate{score: s, turns: t} => println!("Evaluated at a mate in {} in favor of {}.", t, if s {"white"} else {"black"})
                };
                println!("Took {} seconds to pick a move.", now.elapsed().as_secs_f64());
                println!("Evaluated {} new positions.", board.info_eval_count);
                board.info_eval_count = 0;
            };
            m
        },
        _ => {
            println!("Error: cpu_move");
            basic_move(board, Piece{piece_type: PieceType::Pawn, team: true}, &Pos(8,8), &Pos(8,8), None)
        }
    }
}

fn player_move(moves: &Vec<Move>, stdin: std::io::Stdin) -> Move {
    let mut s;
    let ret;
    loop {
        println!("Player move: ");
        s = String::new();
        stdin.read_line(&mut s).expect("");
        s.pop();
        if s == "moves" {
            for m in moves {
                println!("{}", notate_move(&m))
            };
            continue;
        }
        let mo = moves.iter().fold(None, |acc, m| if notate_move(&m) == s {Some(m)} else {acc});
        match mo {
            None => {
                println!("Try again. List all possible moves with \"moves\".");
                continue;
            },
            Some(m) => {
                ret = *m;
                break;
            }
        }
    }
    ret
}

// Output

fn notate_pos(pos: &Pos) -> String {
    let a = match pos.0 {
        0 => "1",
        1 => "2",
        2 => "3",
        3 => "4",
        4 => "5",
        5 => "6",
        6 => "7",
        _ => "8"
    };
    let b = match pos.1 {
        0 => "a",
        1 => "b",
        2 => "c",
        3 => "d",
        4 => "e",
        5 => "f",
        6 => "g",
        _ => "h"
    };
    format!("{}{}", b, a)
}

fn notate_move(m: &Move) -> String {
    match m.notation_data {
        MoveNotationData::Move => format!("{}-{}", notate_pos(&m.from), notate_pos(&m.to)),
        MoveNotationData::KSC => format!("O-O"),
        MoveNotationData::QSC => format!("O-O-O"),
        MoveNotationData::PQ => format!("{}-{}-{}", notate_pos(&m.from), notate_pos(&m.to), "Q"),
        MoveNotationData::PK => format!("{}-{}-{}", notate_pos(&m.from), notate_pos(&m.to), "K"),
        MoveNotationData::PB => format!("{}-{}-{}", notate_pos(&m.from), notate_pos(&m.to), "B"),
        MoveNotationData::PR => format!("{}-{}-{}", notate_pos(&m.from), notate_pos(&m.to), "R"),
    }
}

fn print_board(board: &Board) -> () {
    let board_iter = board.board.iter().map(|r| r.iter().map(|po| match po {
        None => ' ',
        Some(Piece{piece_type: pt, team: true}) => match pt {
            PieceType::Pawn => '♙',
            PieceType::Rook => '♖',
            PieceType::Knight => '♘',
            PieceType::Bishop => '♗',
            PieceType::Queen => '♕',
            PieceType::King => '♔'
            },
        Some(Piece{piece_type: pt, team: false}) => match pt {
            PieceType::Pawn => '♟',
            PieceType::Rook => '♜',
            PieceType::Knight => '♞',
            PieceType::Bishop => '♝',
            PieceType::Queen => '♛',
            PieceType::King => '♚'
            }
        }
    ));
    println!("   ┌───┬───┬───┬───┬───┬───┬───┬───┐");
    let mut first = true;
    let mut tile = false;
    let mut or = 8;
    for r in board_iter.rev() {
        if !first {
            println!("   ├───┼───┼───┼───┼───┼───┼───┼───┤");
        }
        first = false;
        print!(" {} ", or);
        or -= 1;
        for p in r {
            if tile {
                print!("│░{}░", p);
            }
            else {
                print!("│ {} ", p);
            }
            tile = !tile;
        }
        tile = !tile;
        println!("│");
    }
    println!("   └───┴───┴───┴───┴───┴───┴───┴───┘");
    println!("     a   b   c   d   e   f   g   h");
}

// Interface

fn main() {
    use std::io::{stdin, stdout, Write};
    let mut s;

    println!("Enter white's CPU depth or press enter for player input.");
    let _ = stdout().flush();
    s = String::new();
    stdin().read_line(&mut s).expect("");
    s.pop();
    let white = match s.parse::<u32>() {
        Ok(i) => Some(i),
        _ => {
            println!("White will use player input.");
            None
        }
    };

    println!("Enter black's CPU depth or press enter for player input.");
    let _ = stdout().flush();
    s = String::new();
    stdin().read_line(&mut s).expect("");
    s.pop();
    let black = match s.parse::<u32>() {
        Ok(i) => Some(i),
        _ => {
            println!("Black will use player input.");
            None
        }
    };

    let mut board = initial_board();
    let mut white_moves = 0;
    let mut black_moves = 0;
    let mut history = Vec::new();
    let mut move_tree = Box::new(MoveTree{rating: Rating::Evaluation{score: 0}, raw_rating: Rating::Evaluation{score: 0}, tree: Tree::Leaf});
    let h = hash_board(&board);
    increment_position(&mut board, h);
    print_board(&board);
    loop {
        let moves = gen_moves(&mut board);
        let m;
        if moves.len() < 1 {
            if is_in_check(&board, true) {
                println!("Checkmate, black wins.");
            }
            else {
                println!("Stalemate, draw.");
            }
            break;
        };
        match white {
            None => {
                m = player_move(&moves, stdin());
                perform_move(&mut board, &m);
            },
            Some(d) => {
                if white_moves >= MAX_CPU_MOVES {
                    println!("Max moves exceeded, white resigns. Black wins.");
                    break;
                }
                else{
                    m = cpu_move(&mut board, &mut move_tree, d);
                    perform_move(&mut board, &m);
                    println!("White plays {}.", notate_move(&m));
                    white_moves += 1;
                }
            }
        };
        print_board(&board);
        history.push(m);
        if repitition(&board) {
            println!("Threefold repitition, draw.");
            break;
        }
        let h = hash_board(&board);
        increment_position(&mut board, h);
        move_tree = adjust_move_tree(&mut move_tree, &m);
        let omoves = gen_moves(&mut board);
        let m;
        if omoves.len() < 1 {
            if is_in_check(&board, false) {
                println!("Checkmate, white wins.");
            }
            else {
                println!("Stalemate, draw.");
            }
            break;
        }
        match black {
            None => {
                m = player_move(&omoves, stdin());
                perform_move(&mut board, &m);
            },
            Some(d) => {
                if black_moves >= MAX_CPU_MOVES {
                    println!("Max moves exceeded, black resigns. White wins.");
                    break;
                }
                else{
                    m = cpu_move(&mut board, &mut move_tree, d);
                    perform_move(&mut board, &m);
                    println!("Black plays {}.", notate_move(&m));
                    black_moves += 1;
                }
            }
        }
        print_board(&board);
        history.push(m);
        if repitition(&board) {
            println!("Threefold repitition, draw.");
            break;
        }
        let h = hash_board(&board);
        increment_position(&mut board, h);
        move_tree = adjust_move_tree(&mut move_tree, &m);
    }
    println!("Move history:");
    for m in history {
        println!("{}", notate_move(&m));
    }
}
