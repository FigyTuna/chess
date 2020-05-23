
use std::collections::HashSet;

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
enum PieceType {
    Pawn{moved: bool, running: bool},
    Rook{moved: bool},
    Knight,
    Bishop,
    Queen,
    King{moved: bool}
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct Pos(usize, usize);

#[derive(Copy, Clone)]
struct Piece {
    piece_type: PieceType,
    team: bool
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
enum Move {
    Move{from: Pos, to: Pos},
    Capture{from: Pos, to: Pos},
    Run{from: Pos, to: Pos},
    EnPassant{from: Pos, to: Pos, captured: Pos},
    Promote{from: Pos, to: Pos, piece_type: PieceType},
    Castle{king_from: Pos, king_to: Pos, rook_from: Pos, rook_to: Pos}
}

const MAX_CPU_MOVES: i32 = 100;

const INITIAL_BOARD: [[Option<Piece>; 8]; 8] = [
    [
        Some(Piece {piece_type: PieceType::Rook{moved: false}, team: true}),
        Some(Piece {piece_type: PieceType::Knight, team: true}),
        Some(Piece {piece_type: PieceType::Bishop, team: true}),
        Some(Piece {piece_type: PieceType::Queen, team: true}),
        Some(Piece {piece_type: PieceType::King{moved: false}, team: true}),
        Some(Piece {piece_type: PieceType::Bishop, team: true}),
        Some(Piece {piece_type: PieceType::Knight, team: true}),
        Some(Piece {piece_type: PieceType::Rook{moved: false}, team: true})
    ],
    [
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: true}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: true}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: true}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: true}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: true}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: true}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: true}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: true})
    ],
    [None; 8],
    [None; 8],
    [None; 8],
    [None; 8],
    [
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: false}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: false}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: false}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: false}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: false}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: false}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: false}),
        Some(Piece {piece_type: PieceType::Pawn{moved: false, running: false}, team: false}),
    ],
    [
        Some(Piece {piece_type: PieceType::Rook{moved: false}, team: false}),
        Some(Piece {piece_type: PieceType::Knight, team: false}),
        Some(Piece {piece_type: PieceType::Bishop, team: false}),
        Some(Piece {piece_type: PieceType::Queen, team: false}),
        Some(Piece {piece_type: PieceType::King{moved: false}, team: false}),
        Some(Piece {piece_type: PieceType::Bishop, team: false}),
        Some(Piece {piece_type: PieceType::Knight, team: false}),
        Some(Piece {piece_type: PieceType::Rook{moved: false}, team: false}),
    ]
];

fn move_piece(po: &Option<Piece>, run: bool) -> Option<Piece> {
    match po {
        Some(Piece{piece_type: PieceType::Pawn{moved: _, running: _}, team: t}) => Some(Piece{piece_type: PieceType::Pawn{moved: true, running: run}, team: *t}),
        Some(Piece{piece_type: PieceType::Rook{moved: _}, team: t}) => Some(Piece{piece_type: PieceType::Rook{moved: true}, team: *t}),
        Some(Piece{piece_type: PieceType::King{moved: _}, team: t}) => Some(Piece{piece_type: PieceType::King{moved: true}, team: *t}),
        Some(Piece{piece_type: PieceType::Bishop, team: t}) => Some(Piece{piece_type: PieceType::Bishop, team: *t}),
        Some(Piece{piece_type: PieceType::Knight, team: t}) => Some(Piece{piece_type: PieceType::Knight, team: *t}),
        Some(Piece{piece_type: PieceType::Queen, team: t}) => Some(Piece{piece_type: PieceType::Queen, team: *t}),
        None => None
    }
}

fn perform_move(board: &mut [[Option<Piece>; 8]; 8], m: &Move) -> () {
    match m {
        Move::Move{from: f, to: t} => {
            board[t.0][t.1] = move_piece(&board[f.0][f.1], false);
            board[f.0][f.1] = None;
        },
        Move::Capture{from: f, to: t} => {
            board[t.0][t.1] = move_piece(&board[f.0][f.1], false);
            board[f.0][f.1] = None;
        },
        Move::Run{from: f, to: t} => {
            board[t.0][t.1] = move_piece(&board[f.0][f.1], true);
            board[f.0][f.1] = None;
        },
        Move::EnPassant{from: f, to: t, captured: c} => {
            board[t.0][t.1] = board[f.0][f.1];
            board[f.0][f.1] = None;
            board[c.0][c.1] = None;
        }
        Move::Castle{king_from: kf, king_to: kt, rook_from: rf, rook_to: rt} => {
            board[kt.0][kt.1] = move_piece(&board[kf.0][kf.1], false);
            board[kf.0][kf.1] = None;
            board[rt.0][rt.1] = move_piece(&board[rf.0][rf.1], false);
            board[rf.0][rf.1] = None;
        },
        Move::Promote{from: f, to: t, piece_type: pt} => {
            let team = match board[f.0][f.1] {
                Some(Piece{piece_type: _, team: t}) => t,
                _ => true
            };
            board[t.0][t.1] = Some(Piece{piece_type: *pt, team: team});
            board[f.0][f.1] = None;
        }
    }
}

fn gen_moves(board: &[[Option<Piece>; 8]; 8], team: bool) -> HashSet<Move> {
    let mut moves:HashSet<Move> = HashSet::new();
    let mut r: usize = 0;
    let mut f: usize;
    for ri in board.iter() {
        f = 0;
        for po in ri {
            match po {
                Some(Piece{piece_type:_, team: pteam}) if team != *pteam => (),
                Some(Piece{piece_type:PieceType::Rook{moved:_moved}, team: _}) => {
                    for dir in ([(-1, 0), (1, 0), (0, -1), (0, 1)] as [(i32, i32); 4]).iter() {
                        gen_line_moves(board, team, &mut moves, Pos(r, f), *dir, false)
                    }
                },
                Some(Piece{piece_type:PieceType::Bishop, team: _}) => {
                    for dir in ([(-1, -1), (1, -1), (-1, 1), (1, 1)] as [(i32, i32); 4]).iter() {
                        gen_line_moves(board, team, &mut moves, Pos(r, f), *dir, false)
                    }
                },
                Some(Piece{piece_type:PieceType::Queen, team: _}) => {
                    for dir in ([(-1, -1), (1, -1), (-1, 1), (1, 1), (-1, 0), (1, 0), (0, -1), (0, 1)] as [(i32, i32); 8]).iter() {
                        gen_line_moves(board, team, &mut moves, Pos(r, f), *dir, false)
                    }
                },
                Some(Piece{piece_type:PieceType::King{moved: _}, team: _}) => {
                    for dir in ([(-1, -1), (1, -1), (-1, 1), (1, 1), (-1, 0), (1, 0), (0, -1), (0, 1)] as [(i32, i32); 8]).iter() {
                        gen_line_moves(board, team, &mut moves, Pos(r, f), *dir, true)
                    };
                },
                Some(Piece{piece_type:PieceType::Pawn{moved: m, running: _}, team: _}) => {
                    gen_pawn_moves(board, team, &mut moves, Pos(r, f), m)
                },
                Some(Piece{piece_type:PieceType::Knight, team: _}) => {
                    gen_knight_moves(board, team, &mut moves, Pos(r, f))
                }
                _ => ()
            };
            f += 1;
        }
        r += 1;
    }
    gen_castle_moves(board, team, &mut moves);
    moves
}

fn gen_knight_moves(board: &[[Option<Piece>; 8]; 8], team: bool, moves: &mut HashSet<Move>, pos: Pos) -> () {
    for (x, y) in [(-1, -2), (-2, -1), (1, -2), (-2, 1), (-1, 2), (2, -1), (1, 2), (2, 1)].iter() {
        if pos.0 as i32 + x >= 0 && pos.0 as i32 + x <= 7 && pos.1 as i32 + y >= 0 && pos.1 as i32 + y <= 7 {
            let p0 = (pos.0 as i32 + x) as usize;
            let p1 = (pos.1 as i32 + y) as usize;
            match board[p0][p1] {
                Some(Piece{piece_type: _, team: pteam}) if team != pteam => {
                    add_if_no_check(&board, team, moves, Move::Capture{from: pos, to: Pos(p0, p1)});
                },
                None => {
                    add_if_no_check(&board, team, moves, Move::Move{from: pos, to: Pos(p0, p1)});
                },
                _ => ()
            }
        }
    }
}

fn gen_pawn_moves(board: &[[Option<Piece>; 8]; 8], team: bool, moves: &mut HashSet<Move>, pos: Pos, moved: &bool) -> () {
    let dir: i32 = if team {1} else {-1};
    let one_dir = (pos.0 as i32 + dir) as usize;
    let one_space = match board[one_dir][pos.1] {
        None => true,
        _ => false
    };
    if one_space {
        if (team && pos.0 == 6) || (!team && pos.0 == 1) {
            add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1), piece_type: PieceType::Bishop});
            add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1), piece_type: PieceType::Knight});
            add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1), piece_type: PieceType::Rook{moved: true}});
            add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1), piece_type: PieceType::Queen});
        }
        else {
            add_if_no_check(&board, team, moves, Move::Move{from: pos, to: Pos(one_dir, pos.1)});
        }
    };
    if !*moved && one_space {
        match board[(pos.0 as i32 + (dir * 2)) as usize][pos.1] {
            None => {add_if_no_check(&board, team, moves, Move::Run{from: pos, to: Pos((pos.0 as i32 + (dir * 2)) as usize, pos.1)});},
            _ => ()
        }
    };
    if pos.1 > 0 {
        match board[one_dir][pos.1 - 1] {
            Some(Piece{piece_type: _, team: pteam}) if team != pteam => {
                if one_dir == 0 || one_dir == 7 {
                    add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1 - 1), piece_type: PieceType::Bishop});
                    add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1 - 1), piece_type: PieceType::Knight});
                    add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1 - 1), piece_type: PieceType::Rook{moved: true}});
                    add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1 - 1), piece_type: PieceType::Queen});
                }
                else
                {
                    add_if_no_check(&board, team, moves, Move::Capture{from: pos, to: Pos(one_dir, pos.1 - 1)});
                }
            }
            _ => ()
        };
        match board[pos.0][pos.1 - 1] {
            Some(Piece{piece_type: PieceType::Pawn{moved: _, running:true}, team: pteam}) if team != pteam => {
                add_if_no_check(&board, team, moves, Move::EnPassant{from: pos, to: Pos(one_dir, pos.1 - 1), captured: Pos(pos.0, pos.1 - 1)});
            }
            _ => ()
        }
    };
    if pos.1 < 7 {
        match board[one_dir][pos.1 + 1] {
            Some(Piece{piece_type: _, team: pteam}) if team != pteam => {
                if one_dir == 0 || one_dir == 7 {
                    add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1 + 1), piece_type: PieceType::Bishop});
                    add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1 + 1), piece_type: PieceType::Knight});
                    add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1 + 1), piece_type: PieceType::Rook{moved: true}});
                    add_if_no_check(&board, team, moves, Move::Promote{from: pos, to: Pos(one_dir, pos.1 + 1), piece_type: PieceType::Queen});
                }
                else
                {
                    add_if_no_check(&board, team, moves, Move::Capture{from: pos, to: Pos(one_dir, pos.1 + 1)});
                }
            }
            _ => ()
        };
        match board[pos.0][pos.1 + 1] {
            Some(Piece{piece_type: PieceType::Pawn{moved: _, running:true}, team: pteam}) if team != pteam => {
                add_if_no_check(&board, team, moves, Move::EnPassant{from: pos, to: Pos(one_dir, pos.1 + 1), captured: Pos(pos.0, pos.1 + 1)});
            }
            _ => ()
        }
    };
}

fn gen_castle_moves(board: &[[Option<Piece>; 8]; 8], team: bool, moves: &mut HashSet<Move>) -> () {
    let r = if team {0} else {7};
    match board[r][4] {
        Some(Piece{piece_type: PieceType::King{moved:false}, team: _}) => {
            if !is_in_check(board, team, Pos(r, 4)){
                match (board[r][0], board[r][1], board[r][2], board[r][3]) {
                    (Some(Piece{piece_type: PieceType::Rook{moved:false}, team: _}), None, None, None) => {
                        add_if_no_check(&board, team, moves, Move::Castle{king_from: Pos(r, 4), king_to: Pos(r, 2), rook_from: Pos(r, 0), rook_to: Pos(r, 3)});
                    }
                    _ => ()
                };
                match (board[r][7], board[r][6], board[r][5]) {
                    (Some(Piece{piece_type: PieceType::Rook{moved:false}, team: _}), None, None) => {
                        add_if_no_check(&board, team, moves, Move::Castle{king_from: Pos(r, 4), king_to: Pos(r, 6), rook_from: Pos(r, 7), rook_to: Pos(r, 5)});
                    }
                    _ => ()
                }
            }
        },
        _ => ()
    }
}

fn gen_line_moves(board: &[[Option<Piece>; 8]; 8], team: bool, moves: &mut HashSet<Move>, pos: Pos, (r_dir, f_dir): (i32, i32), king: bool) -> () {
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
        match board[ru][fu] {
            None => {
                add_if_no_check(&board, team, moves, Move::Move{from: pos, to: Pos(ru, fu)});
                if !king {
                    gen_line_moves(board, team, moves, pos, extend_dir((r_dir, f_dir)), false)
                }
            },
            Some(Piece{piece_type: _ptype, team:pteam}) if team != pteam => {
                add_if_no_check(&board, team, moves, Move::Capture{from: pos, to: Pos(ru, fu)});
            },
            _ => ()
        }
    }
}

fn add_if_no_check(board: &[[Option<Piece>; 8]; 8], team: bool, moves: &mut HashSet<Move>, m: Move) -> () {
    let mut b = board.clone();
    perform_move(&mut b, &m);
    if !is_in_check_find(&b, team) {
        moves.insert(m);
    }
}

fn is_in_check_find(board: &[[Option<Piece>; 8]; 8], team: bool) -> bool {
    let mut p = Pos(8, 8);
    let mut x;
    let mut y = 0;
    for r in board.iter() {
        x = 0;
        for po in r {
            match po {
                Some(Piece{piece_type: PieceType::King{moved: _}, team: pteam}) if team == *pteam => p = Pos(y, x),
                _ => ()
            }
            x += 1;
        }
        y += 1;
    }
    is_in_check(&board, team, p)
}

fn is_in_check(board: &[[Option<Piece>; 8]; 8], team: bool, pos: Pos) -> bool {
    let mut check = false;
    for (dx, dy) in [(0, -1), (-1, 0), (0, 1), (1, 0), (-1, -1), (1, -1), (-1, 1), (1, 1)].iter() {
        let mut r = pos.0 as i32;
        let mut f = pos.1 as i32;
        let mut first = true;
        loop {
            r += dx;
            f += dy;
            if r < 0 || r > 7 || f < 0 || f > 7 {break}
            match board[r as usize][f as usize] {
                Some(Piece{piece_type:PieceType::Bishop, team: pteam}) if (team != pteam) && (dx + dy).abs() % 2 == 0 => {check = true; break}
                Some(Piece{piece_type:PieceType::Rook{moved: _}, team: pteam}) if (team != pteam) && (dx + dy).abs() % 2 == 1 => {check = true; break}
                Some(Piece{piece_type:PieceType::Queen, team: pteam}) if team != pteam => {check = true; break}
                Some(Piece{piece_type:PieceType::King{moved:_}, team: pteam}) if (team != pteam) && first => {check = true; break}
                None => (),
                _ => break
            }
            first = false;
        }
    }
    if !check {
        for (x, y) in [(-1, -2), (-2, -1), (1, -2), (-2, 1), (-1, 2), (2, -1), (1, 2), (2, 1)].iter() {
            if pos.0 as i32 + x >= 0 && pos.0 as i32 + x <= 7 && pos.1 as i32 + y >= 0 && pos.1 as i32 + y <= 7 {
                match board[(pos.0 as i32 + x) as usize][(pos.1 as i32 + y) as usize] {
                    Some(Piece{piece_type: PieceType::Knight, team: pteam}) if team != pteam => {check = true; break},
                    _ => ()
                }
            }
        }
    }
    if !check {
        let dir = if team {1} else {-1};
        for (dx, dy) in [(dir, -1), (dir, 1)].iter() {
            if pos.0 as i32 + dx >= 0 && pos.0 as i32 + dx <= 7 && pos.1 as i32 + dy >= 0 && pos.1 as i32 + dy <= 7 {
                match board[(pos.0 as i32 + dx) as usize][(pos.1 as i32 + dy) as usize] {
                    Some(Piece{piece_type: PieceType::Pawn{moved: _, running: _}, team: pteam}) if team != pteam => {check = true; break},
                    _ => ()
                }
            }
        }
    }
    check
}

fn pick_move(moves: &HashSet<Move>) -> Move {
    let i = 13 % moves.len();
    let mut v = Vec::new();
    for m in moves.iter() {
        v.push(m);
    };
    *v[i]
}

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
    match m {
        Move::Move{from: f, to: t} => format!("{}-{}", notate_pos(f), notate_pos(t)),
        Move::Capture{from: f, to: t} => format!("{}-{}", notate_pos(f), notate_pos(t)),
        Move::Run{from: f, to: t} => format!("{}-{}", notate_pos(f), notate_pos(t)),
        Move::EnPassant{from: f, to: t, captured: _} => format!("{}-{}", notate_pos(f), notate_pos(t)),
        Move::Castle{king_from: _, king_to: t, rook_from: _, rook_to: _} => if t.1 == 2 {format!("O-O-O")} else {format!("O-O")},
        Move::Promote{from: f, to: t, piece_type: pt} => {
            let p = match pt {
                PieceType::Bishop => "B",
                PieceType::Knight => "K",
                PieceType::Rook{moved: _} => "R",
                _ => "Q",
            };
            format!("{}-{}-{}", notate_pos(f), notate_pos(t), p)
        }
    }
}

fn print_board(board: &[[Option<Piece>; 8]; 8]) -> () {
    let board_iter = board.iter().map(|r| r.iter().map(|po| match po {
        None => ' ',
        Some(Piece{piece_type: pt, team: true}) => match pt {
            PieceType::Pawn{moved:_, running: _} => '♙',
            PieceType::Rook{moved:_} => '♖',
            PieceType::Knight => '♘',
            PieceType::Bishop => '♗',
            PieceType::Queen => '♕',
            PieceType::King{moved:_} => '♔'
            },
        Some(Piece{piece_type: pt, team: false}) => match pt {
            PieceType::Pawn{moved:_, running: _} => '♟',
            PieceType::Rook{moved:_} => '♜',
            PieceType::Knight => '♞',
            PieceType::Bishop => '♝',
            PieceType::Queen => '♛',
            PieceType::King{moved:_} => '♚'
            }
        }
    ));
    println!("┌───┬───┬───┬───┬───┬───┬───┬───┐");
    let mut first = true;
    let mut tile = false;
    for r in board_iter.rev() {
        if !first {
            println!("├───┼───┼───┼───┼───┼───┼───┼───┤");
        }
        first = false;
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
    println!("└───┴───┴───┴───┴───┴───┴───┴───┘");
}

fn cpu_move(_board: &[[Option<Piece>; 8]; 8], moves: &HashSet<Move>) -> Move {
    pick_move(moves)
}

fn player_move(moves: &HashSet<Move>, stdin: std::io::Stdin) -> Move {
    let mut s;
    let ret;
    loop {
        println!("Enter a move. List all moves with 'moves'.");
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
                println!("Try again.");
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

fn main() {
    use std::io::{stdin, stdout, Write};
    let mut s;
    let mut b = INITIAL_BOARD;
    println!("White is player? 'y' for yes.");
    let _=stdout().flush();
    s = String::new();
    stdin().read_line(&mut s).expect("");
    s.pop();
    let white = s == "y";
    println!("Black is player? 'y' for yes.");
    let _=stdout().flush();
    s = String::new();
    stdin().read_line(&mut s).expect("");
    s.pop();
    let black = s == "y";
    let mut white_moves = 0;
    let mut black_moves = 0;
    loop {
        print_board(&b);
        let moves = gen_moves(&b, true);
        if moves.len() < 1 {
            if is_in_check_find(&b, true) {
                println!("Checkmate, black wins.");
            }
            else {
                println!("Stalemate, draw.");
            }
            break;
        }
        if white {
            let m = player_move(&moves, stdin());
            perform_move(&mut b, &m);
        }
        else {
            if white_moves >= MAX_CPU_MOVES {
                println!("Max moves exceeded, white resigns. Black wins.");
                break;
            }
            else{
                let m = cpu_move(&b, &moves);
                perform_move(&mut b, &m);
                println!("White plays {}.", notate_move(&m));
                white_moves += 1;
            }
        }
        print_board(&b);
        let omoves = gen_moves(&b, false);
        if omoves.len() < 1 {
            if is_in_check_find(&b, false) {
                println!("Checkmate, white wins.");
            }
            else {
                println!("Stalemate, draw.");
            }
            break;
        }
        if black {
            let m = player_move(&omoves, stdin());
            perform_move(&mut b, &m);
        }
        else {
            if black_moves >= MAX_CPU_MOVES {
                println!("Max moves exceeded, black resigns. White wins.");
                break;
            }
            else{
                let m = cpu_move(&b, &omoves);
                perform_move(&mut b, &m);
                println!("Black plays {}.", notate_move(&m));
                black_moves += 1;
            }
        }
    }
}
