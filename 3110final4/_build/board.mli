(** Representation of a static board of our game. *)

open Piece

(** [t] is a 2d array that represents the entire board, with every
    piece's property *)
type t

type graveyard = {
  red_graveyard : Piece.t list;
  black_graveyard : Piece.t list;
}

type score = {
  red_score : int;
  black_score : int;
}

(** Let piece, maximum is 70 (w/o general) + 20 (general) = 90, piece is
    captured piece, score_specific is the current score of the
    particular side*)
val update_score : Piece.t -> int -> int

(** [generate_graveyard] is an empty graveyard initialized to be used*)
val generate_graveyard : unit -> graveyard

(** [get_red_g gy] is list of black captured pieces from [gy]*)
val get_red_g : graveyard -> Piece.t list

(** [get_black_g gy] is list of black captured pieces from [gy]*)
val get_black_g : graveyard -> Piece.t list

(** [generate_score] is a score value initialized to be used*)
val generate_score : unit -> score

(** [get_red_score score] is an int value of red [score]*)
val get_red_score : score -> int

(** [get_black_score score] is an int value of black [score]*)
val get_black_score : score -> int

(** return the number of piece of the particular [rank], in the
    gravelist specified by [grave] and [side]*)
val count_pieces : Piece.rank -> graveyard -> Piece.side -> int

(** [generate_board] is the initial state of board *)
val generate_board : unit -> t

(** [get_piece board coord] is the piece that locates on the coordinate
    [coord] on the board [board]*)
val get_piece : t -> coord -> Piece.t option

(**[print_board board] prints the representation of the board [board],
   integrated with graveyard *)
val print_board : t -> graveyard -> score -> unit

(**[print_rev_board board] prints the representation of the board from
   other side[board], integrated with graveyard *)
val print_rev_board : t -> graveyard -> score -> unit

(** [turned_board board] is the resulting board after rotating the board
    [board] for 180 degree *)
val turned_board : t -> t

(** [update_board board start dest] is the updated board [board] by
    moving the piece at [start] to [dest] and setting the postion
    [start] to None *)
val update_board : t -> coord -> coord -> t
