(** Representation of data for 1 player hard mode *)

(** [vectorized_board_state] is a 3D int array of shape 9 x 10 x 14 that
    represent the board state. [\[vectorized_board_state\].(0).(0)]
    representing 14 unique pieces and is an one-hot representation of
    piece. *)
type vectorized_board_state = int array array array

(** [move] represent a move in a certain round. It has the form ((x1,
    y1),(x2, y2)), the first coordinate is the start of move and the
    second coordinate is the destiny. *)
type move = (int * int) * (int * int)

(** [populate_train filename train_data] is a representation of our data
    stored in the csv file [filename]*)
val populate_train : string -> string array array -> string array array

(** [vec_piece rank side] is the one hot representation of a piece of
    [rank] and from [side]*)
val vec_piece : Piece.rank -> Piece.side -> int list

(** [translate_lines board yaxis] takes a single axis [yaxis] of [board]
    and turn it into vectorized row. *)
val translate_lines : Board.t -> int -> int array array

(** [translate_board] takes in a complete board for one round and uses
    [translate_lines] to vectorized it, returns a
    [vectorized_baord_state] of [9 x 10 x 14] in size*)
val translate_board : Board.t -> vectorized_board_state

(** [get_start_coord rank board start_x] is the coordinate of the piece
    on column [start_x] of board [board] refered by string [rank]*)
val get_start_coord : string -> Board.t -> string -> int * int

(** [get_end_coord start state oper side end_x] is the ending coordinate
    of the piece starting at [start] after operation [oper] and [end_x]
    on board of [state] *)
val get_end_coord :
  int * int -> State.t -> string -> string -> string -> int * int

(** [translate_coord state_ref move_str] is the start and end coord for
    board in [state_ref] after translating [move_str]*)
val translate_coord : State.t ref -> string -> move

(** [cal_game_length] calculates the number of total moves in each game,
    returns an int list of size number of games *)
val cal_game_length : string array array -> int list

(** [simulate_round] takes the input train data and translate each board
    state and move from WXF notation to readable format, run every move
    through [State.move] to produce the corresponding board state and
    return all information in a [(vectorized_board_state * move) array]*)
val simulate_round :
  string array array -> (vectorized_board_state * move) array

(** [make_2d] flattens the 3d list of [vectorized_board_state] into a 2d
    array array, the shape of 2d array is length of
    [vectorized_board_state list] x [1260] *)
val make_2d : vectorized_board_state list -> int array array

(** [x_vec] is a fully processed [int array array] of all board status
    from training data*)
val x_vec : int array array

(** [y_vec] is a fully processed [int array array] of all moves from
    training data*)
val y_vec : int array array
