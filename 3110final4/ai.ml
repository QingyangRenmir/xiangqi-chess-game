open State
open Piece
open Board

type piece = Piece.t

let s = Black

let available_piece (board : Board.t) : Piece.t list =
  let list = ref [] in
  for i = 0 to 8 do
    for j = 0 to 8 do
      let piece = get_piece board (i, j) in
      match piece with
      | None -> ()
      | Some x ->
          if Piece.get_side x = Black then list := x :: !list else ()
    done
  done;
  !list

let choose_piece board list_piece =
  list_piece |> List.length |> ( + ) (-1) |> Random.int
  |> List.nth list_piece

let get_coordinate piece =
  let c = get_coord piece in
  match get_c piece with
  | Soldier -> (c, (fst c - 1, snd c))
  | Cannon ->
      ( c,
        List.nth
          [
            (fst c, snd c + Random.int (8 - snd c));
            (fst c + Random.int (8 - fst c), snd c);
          ]
          (Random.int 1) )
  | Rook ->
      ( c,
        List.nth
          [
            (fst c, snd c + Random.int (8 - snd c));
            (fst c + Random.int (8 - fst c), snd c);
          ]
          (Random.int 1) )
  | Horse ->
      ( c,
        List.nth
          [
            (fst c + 2, snd c + 1);
            (fst c - 2, snd c + 1);
            (fst c + 2, snd c - 1);
            (fst c - 2, snd c - 1);
            (fst c + 1, snd c + 2);
            (fst c + 1, snd c - 2);
            (fst c - 1, snd c + 2);
            (fst c - 1, snd c - 2);
          ]
          (Random.int 7) )
  | Elephant ->
      ( c,
        List.nth
          [
            (fst c + 2, snd c + 2);
            (fst c - 2, snd c + 2);
            (fst c + 2, snd c - 2);
            (fst c - 2, snd c - 2);
          ]
          (Random.int 3) )
  | Advisor ->
      ( c,
        List.nth
          [
            (fst c + 1, snd c + 1);
            (fst c - 1, snd c + 1);
            (fst c + 1, snd c - 1);
            (fst c - 1, snd c - 1);
          ]
          (Random.int 3) )
  | General ->
      ( c,
        List.nth
          [
            (fst c, snd c - 1);
            (fst c, snd c + 1);
            (fst c + 1, snd c);
            (fst c - 1, snd c);
          ]
          (Random.int 3) )

let rec make_legal_move state list_piece =
  let piece = choose_piece (State.get_current_board state) list_piece in
  let coord = get_coordinate piece in
  match State.move (fst coord) (snd coord) state with
  | Illegal -> make_legal_move state list_piece
  | Legal _ -> coord

let process_state (state : State.t) : int array array =
  let board3d =
    state |> State.get_current_board |> Mlearn.translate_board
  in
  Mlearn.make_2d [ board3d ]

let calc_dis board1 board2 =
  let l = Array.length board1 in
  let sum_dif = ref 0 in
  for i = 0 to l - 1 do
    sum_dif :=
      ((board1.(i) - board2.(i)) * (board1.(i) - board2.(i))) + !sum_dif
  done;
  sqrt (Int.to_float !sum_dif)

let compare_tuple t1 t2 = compare (snd t1) (snd t2)

let xvec = Mlearn.x_vec

let yvec = Mlearn.y_vec

(** [ml_coord] takes the state of the game, find the first 20 closest
    states from data, and returns the move of it. If no legal move is
    found, it randomly generates a move. Returns ((x1,y1),(x2,y2))*)
let ml_coord state =
  let coordinate = ref ((0, 0), (0, 0)) in
  let vec_board = process_state state in
  let () = print_endline "finish process state" in
  let order_dis = ref [] in
  (* for i = 0 to Array.length Mlearn.x_vec - 1 do *)
  for i = 0 to Array.length xvec - 1 do
    let () = print_endline "start calc dis" in
    order_dis := (i, calc_dis xvec.(i) vec_board.(0)) :: !order_dis
  done;
  let () = print_endline "before sort" in
  let order_dis = List.sort compare_tuple !order_dis in
  let () = print_endline "sorted dis" in
  for j = 20 downto 0 do
    let coord = yvec.(fst (List.nth order_dis j)) in
    match Array.to_list coord with
    | [ x1; y1; x2; y2 ] -> (
        let c = ((x1, y1), (x2, y2)) in
        match State.move (fst c) (snd c) state with
        | Illegal -> ()
        | Legal _ -> coordinate := c )
    | _ -> failwith "something wrong"
  done;

  if !coordinate = ((0, 0), (0, 0)) then (
    let () = print_endline "no matching" in
    let list_piece = available_piece (State.get_current_board state) in
    coordinate := make_legal_move state list_piece;
    !coordinate )
  else
    let () = print_endline "should work" in
    !coordinate

let make_command state level =
  let coord = ref ((0, 0), (0, 0)) in
  if level = "easy" then
    let list_piece = available_piece (State.get_current_board state) in
    let piece =
      choose_piece (State.get_current_board state) list_piece
    in
    coord := make_legal_move state list_piece
  else coord := ml_coord state;
  match !coord with
  | (x, y), (x', y') ->
      "move " ^ string_of_int x ^ "," ^ string_of_int y ^ " "
      ^ string_of_int x' ^ "," ^ string_of_int y'
