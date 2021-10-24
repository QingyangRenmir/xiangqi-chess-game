(** length 2 list of int tuple*)
type coords = (int * int) list

type command =
  | Move of coords
  | Quit
  | Help
  | History

(*3 chances of withdraw*)

exception Empty

exception Malformed

let get_head = function [] -> "" | h :: t -> h

let get_tail = function [] -> [] | _ :: t -> t

let rec words = function
  | [] -> []
  | h :: t -> if h <> "" then h :: words t else words t

let str_to_tuple str =
  let strlst = String.split_on_char ',' str in
  try
    match strlst with
    | [ x; y ] -> (int_of_string x, int_of_string y)
    | _ -> (-1, -1)
  with Failure _ -> raise Malformed

let rec check_coords_in_range tuple_lst =
  match tuple_lst with
  | [] -> true
  | h :: t -> fst h >= 0 && snd h >= 0 && check_coords_in_range t

let check_good_coords tuple_lst =
  List.length tuple_lst = 2 && check_coords_in_range tuple_lst

let parse str =
  let splitted =
    str |> String.trim |> String.split_on_char ' ' |> words
  in
  let verb = get_head splitted in
  if verb = "" then raise Empty
  else
    let coordinates = get_tail splitted in
    let coords = List.map str_to_tuple coordinates in
    match verb with
    | "quit" -> if List.length coords = 0 then Quit else raise Malformed
    | "move" ->
        if check_good_coords coords then Move coords
        else raise Malformed
    | "help" -> if List.length coords = 0 then Help else raise Malformed
    | "history" ->
        if List.length coords = 0 then History else raise Malformed
    | _ -> raise Malformed
