(** Representation of the commmand we will parse.*)

(** The type [coords] represents the coordinates the player want to move
    a piece from and to*)
type coords = (int * int) list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a pair of coordinate *)
type command =
  | Move of coords
  | Quit
  | Help
  | History

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words, if any, become the
    coordinates. Make sure no space between x,y of the coordinate.
    Examples:

    - [parse " move 2,3 4,5"] is [Move \[(2,3); (4,5)\]]
    - [parse "move 2, 3 4, 5"] raises [Malformed]
    - [parse "quit"] is [Quit].

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} if the verb is neither "quit" nor "move", or if the
    verb is "quit" and there is a non-empty object phrase, or if the
    verb is "move" and there is an empty object phrase.*)
val parse : string -> command
