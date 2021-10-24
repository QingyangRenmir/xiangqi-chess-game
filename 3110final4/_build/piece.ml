type side =
  | Red
  | Black

type rank =
  | General
  | Advisor
  | Elephant
  | Horse
  | Rook
  | Cannon
  | Soldier

(** coord (2, 3) means the i (y-coordinate) is 2 and j (x-coordinate) is
    3. *)
type coord = int * int

type piece = {
  character : rank;
  side : side;
  coordinate : coord;
}

type t = piece

let create_piece r s c = { character = r; side = s; coordinate = c }

let get_c piece = piece.character

let get_side piece = piece.side

let string_of_side side = if side = Red then "Red" else "Black"

let change_coord piece coord =
  create_piece (get_c piece) (get_side piece) coord

let get_coord piece = piece.coordinate

let extract = function
  | Some x -> x
  | None -> raise (Invalid_argument "extract None")

let rank_of_piece piece = piece.character

let char_of_piece piece =
  if piece = None then '+'
  else
    match (extract piece).character with
    | General -> 'G'
    | Advisor -> 'A'
    | Elephant -> 'E'
    | Horse -> 'H'
    | Rook -> 'R'
    | Cannon -> 'C'
    | Soldier -> 'S'

let string_of_piece piece =
  let col = if get_side piece = Red then "Red" else "Black" in
  match get_c piece with
  | General -> col ^ " General"
  | Advisor -> col ^ " Advisor"
  | Elephant -> col ^ " Elephant"
  | Horse -> col ^ " Horse"
  | Rook -> col ^ " Rook"
  | Cannon -> col ^ " Cannon"
  | Soldier -> col ^ " Soldier"
