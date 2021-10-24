open Stdlib
open Printf
open State
open Command
open Board
open Piece

exception Illegal_state

(** [get_result_state result] is the state from result [result], if
    result is legal, then gives state; otherwise, raise Illegal_state
    exception *)

type history = string list

type gallery = (Board.t * graveyard * score) list

let get_1st (a, b, c) = a

let get_2nd (a, b, c) = b

let get_3rd (a, b, c) = c

let get_result_state result =
  match result with
  | State.Legal t -> t
  | State.Illegal -> raise Illegal_state

(** [valid_command] is an recursive function that handles exceptions
    with mal inputs. If the input is legal, return parse command, else
    return a warning message and ask for new input. This step is
    repeated until a legal input is received. *)
let rec valid_command command =
  try parse command with
  | Empty -> (
      print_endline "Command is Empty";
      match read_line () with input -> valid_command input)
  | Malformed -> (
      print_endline
        "Command is Malformed, should be: 'quit' or 'move x1,y1 x2,y2' \
         or 'help' or 'history' Please try again";
      print_string "> ";
      match read_line () with input -> valid_command input)
  | Illegal_state -> (
      print_endline "This is an illegal move, try again! \n";
      print_string "> ";
      match read_line () with input -> valid_command input)

let rec help_user confuse =
  match confuse with
  | "Soldier" ->
      print_endline "Soldier can move 1 step foward before river";
      print_endline "Can move 1 step forward, left, right after river"
  | "Cannon" ->
      print_endline "Cannons move orthogonally without jumping";
      print_endline "Can only capture by jumping over a single piece"
  | "Rook" ->
      print_endline "Rook moves any distance orthogonally";
      print_endline "May not jump over intervening pieces"
  | "Horse" ->
      print_endline "Horse moves in any 2*1 or 1*2 size like 日";
      print_endline "Tripping horse leg: illegal move when";
      print_endline "the coord 1 step towards the moving occupied"
  | "Elephant" ->
      print_endline "Elephant moves 2 points diagonally like 田";
      print_endline "Cannot move if one piece is stuck on midpoint";
      print_endline "Cannot move across river"
  | "Advisor" ->
      print_endline "Advisor moves diagonally 1 step";
      print_endline "Cannot leave palace 田 within advisors' route"
  | "General" ->
      print_endline "General moves orthogonally 1 step";
      print_endline "Cannot leave palace 田. Once captured, Game OVER"
  | _ ->
      print_endline "Oops! invalid input, try again~";
      print_string "> ";
      let confuse2 = read_line () in
      help_user confuse2

let rec help_history history =
  match history with
  | [] -> print_string ""
  | h :: t ->
      print_endline h;
      help_history t

(*[move_command] is of the form Move (coords), and is a valid command*)
let string_of_move move_command =
  match move_command with
  | Move coords ->
      let str_int1 = string_of_int (fst (List.nth coords 0)) in
      let str_int2 = string_of_int (snd (List.nth coords 0)) in
      let str_int3 = string_of_int (fst (List.nth coords 1)) in
      let str_int4 = string_of_int (snd (List.nth coords 1)) in
      let str_tuple1 = str_int1 ^ "," ^ str_int2 in
      let str_tuple2 = str_int3 ^ "," ^ str_int4 in
      "move " ^ str_tuple1 ^ " " ^ str_tuple2
  | _ -> ""

let rec help_board gallery =
  Printf.printf "\027[33m%s\027[0m"
    "Wanna look at another No.i board, or quit?\n";
  Printf.printf "\027[33m%s\027[0m" "  [type i to indicate, or quit]\n";
  print_string "> ";
  let request2 = read_line () in
  match request2 with
  | "quit" -> exit 0
  (*meaning that request2 is a valid integer step numnber*)
  | i ->
      let keyword =
        try
          let respond_step = int_of_string request2 in
          let galle = List.nth gallery respond_step in
          print_board (get_1st galle) (get_2nd galle) (get_3rd galle);
          print_endline ""
        with _ -> Printf.printf "\027[33m%s\027[0m" "invalid input~\n"
      in
      keyword;
      help_board gallery

(*Prints all step except for the last step of the win*)
let rec help_request request history gallery =
  match request with
  | "steps" ->
      help_history history;
      (*continue asking to next round*)
      Printf.printf "\027[33m%s\027[0m"
        "Wanna look at, board history, or quit?\n";
      Printf.printf "\027[33m%s\027[0m" "  [type boards/quit]\n";
      print_string "> ";
      let request2 = read_line () in
      help_request request2 history gallery
  | "boards" ->
      Printf.printf "\027[33m%s\027[0m"
        "Which step of the board do you want to look at?\n";
      Printf.printf "\027[33m%s\027[0m"
        "  [type i to indicate No.i board]\n";
      print_string "> ";
      let printed =
        try
          let respond_step = int_of_string (read_line ()) in
          let galle = List.nth gallery respond_step in
          print_board (get_1st galle) (get_2nd galle) (get_3rd galle);
          print_endline ""
        with _ -> Printf.printf "\027[33m%s\027[0m" "invalid input~\n"
      in
      printed;
      (*continue asking to next round*)
      help_board gallery
  | "quit" -> exit 0
  | _ ->
      Printf.printf "\027[33m%s\027[0m"
        "Oops! invalid input, try again~\n";
      print_string "> ";
      let request2 = read_line () in
      help_request request2 history gallery

(*[move_command] is of the form Move (coords), and is a valid command*)

(** [play_game_help] is the helper function that updates each move
    according to the command and pass the turn to the other side*)

(*history is a list of move commands, accumulating along the game*)
let rec play_game_help diff st mode history gallery =
  let cur_board = get_current_board st in
  let cur_turn = get_current_turn st in
  let cur_grave = get_current_grave st in
  let cur_score = get_current_score st in
  (*--->not sure how to use it for now*)
  let cur_total_step = st |> get_current_step |> get_total_step in
  (*Print current board: depending on current turn direct or reversed*)
  Printf.printf "\027[33m%s\027[0m" "\nCurrent Board:\n ";
  if cur_turn = Red then Board.print_board cur_board cur_grave cur_score
  else
    Board.print_rev_board
      (Board.turned_board cur_board)
      cur_grave cur_score;
  (*Print current turn*)
  print_endline "\027[33m\nCurrent Turn: \027[0m";
  if mode = 2 || (mode = 1 && cur_turn = Red) then (
    if Piece.string_of_side cur_turn = "Red" then
      Printf.printf "\027[31;1m\n%s\n\027[0m"
        (Piece.string_of_side cur_turn)
    else
      Printf.printf "\027[34;1m\n%s\n\027[0m"
        (Piece.string_of_side cur_turn);
    (*Print first qustion*)
    let str_ins =
      "\n\
       What do you want to do next? (you can move or quit or help or history)\n\
       Example: 'move 9,4 8,4' moves the red General up one step."
    in
    (*Asking for and Retriving command message, and executing the effect
      according to specific command*)
    Printf.printf "\027[33m%s\n\027[0m" str_ins;
    (* print_endline "\n\ What do you want to do next? (you can move or
       quit)\n\ Example: 'move 9,4 8,4' moves the red General up one
       step."; *)
    print_string "> ";
    let msg = read_line () in
    let command = valid_command msg in
    (*command = parsed message [command] or raises Exception*)
    (*command = Move of coords*)
    try
      match command with
      | Move [ (x1, y1); (x2, y2) ] ->
          let start = (x1, y1) in
          let dest = (x2, y2) in
          let new_st_result = State.move start dest st in
          let new_st = get_result_state new_st_result in
          let win_cond = State.check_winner new_st in
          (*If there will be a winning in the upcomping state*)
          if win_cond <> None then
            let winner = Option.get win_cond |> Piece.string_of_side in
            let loser = if winner = "Red" then "Black" else "Red" in
            let w_final_score =
              if win_cond = Some Red then
                get_red_score (get_current_score new_st)
              else get_black_score (get_current_score new_st)
            in
            let lo_final_score =
              if win_cond = Some Red then
                get_black_score (get_current_score new_st)
              else get_red_score (get_current_score new_st)
            in
            let () =
              Printf.printf "\027[33m%s\027[0m" "\nCurrent Board:\n ";
              let new_board = get_current_board new_st in
              if cur_turn = Red then
                Board.print_board new_board
                  (get_current_grave new_st)
                  (get_current_score new_st)
              else
                Board.print_rev_board
                  (Board.turned_board new_board)
                  (get_current_grave new_st)
                  (get_current_score new_st);
              let str1 = "  CONGRATULATIONS! " ^ winner ^ " win! " in
              let str2 =
                "          " ^ winner ^ "'s score is a Brilliant "
                ^ string_of_int w_final_score
                ^ "!"
              in
              let str3 =
                "          " ^ loser ^ "'s score is an Awesome "
                ^ string_of_int lo_final_score
                ^ "! :D Catch up!"
              in
              print_endline "    ";
              print_endline "    ";
              Printf.printf "\027[32;1m%s\n\027[0m" str1;
              Printf.printf "\027[32;1m%s\n\027[0m" str2;
              Printf.printf "\027[32;1m%s\n\027[0m" str3;
              (*Ask for request input*)
              print_endline "";
              Printf.printf "\027[33m%s\027[0m"
                "Now that you are done with the game,\n";
              Printf.printf "\027[33m%s\027[0m"
                "Wanna look at all your steps, board history, or quit?\n";
              Printf.printf "\027[33m%s\027[0m"
                "  [type steps/boards/quit]\n";
              print_string "> ";
              (*deal with the user request, recursively, to execute
                corresponding functionalities*)
              let request = read_line () in
              help_request request
                (history
                @ [
                    "Step "
                    ^ string_of_int cur_total_step
                    ^ ": " ^ string_of_side cur_turn ^ " "
                    ^ string_of_move command;
                  ])
                (gallery
                @ [
                    (cur_board, cur_grave, cur_score);
                    ( new_board,
                      get_current_grave new_st,
                      get_current_score new_st );
                  ])
            in
            exit 0
            (*If no winning detected, continue the game with updated
              state*)
          else
            (*Command is move x1,y1 x2,y2; and no winning in next round*)
            let step_hint =
              "Step " ^ string_of_int cur_total_step ^ ": "
            in
            play_game_help diff new_st mode
              (history
              @ [
                  step_hint ^ string_of_side cur_turn ^ " "
                  ^ string_of_move command;
                ])
              (gallery @ [ (cur_board, cur_grave, cur_score) ])
      | Quit ->
          let str_bye = "~ Bye Bye ~\n" in
          Printf.printf "\027[32;1m%s\n\027[0m" str_bye;
          exit 0
      | Help ->
          print_endline "Forgot rules ?";
          print_endline "Don't panic! We're here to help you!";
          print_endline
            "Enter Soldier/Cannon/Rook/Horse/Elephant/Advisor/General";
          print_string "> ";
          (*deal with the confuse message, recursively*)
          let confuse = read_line () in
          help_user confuse;
          play_game_help diff st mode history gallery
      | History ->
          print_endline "Wanna look back at the path you come from?";
          if history = [] then
            print_string "Check back later after you make a move!"
          else help_history history;
          play_game_help diff st mode history gallery
      | _ ->
          print_endline "unknown command!";
          play_game_help diff st mode history gallery
    with
    | Illegal_state ->
        print_endline "This is an illegal move, try again! \n";
        play_game_help diff st mode history gallery
    | Invalid_argument _ ->
        print_endline
          "please enter the coordinate within the board, Please try \
           again!";
        play_game_help diff st mode history gallery)
  else
    (*mode = 1 and Side = Black*)
    Printf.printf "\027[34;1m\n%s\n\027[0m"
      (Piece.string_of_side cur_turn);
  (* print_endline "1"; *)
  if diff <> "easy" || diff <> "hard" then
    let diff = "easy" in
    let c = Ai.make_command st diff in
    (* print_endline "2"; *)
    (* print_endline c; *)
    let command = Command.parse c in
    match command with
    | Move [ (x1, y1); (x2, y2) ] ->
        let start = (x1, y1) in
        let dest = (x2, y2) in
        let new_st_result = State.move start dest st in
        let step_hint = "Step " ^ string_of_int cur_total_step ^ ": " in
        play_game_help diff
          (get_result_state new_st_result)
          mode
          (history
          @ [
              step_hint ^ string_of_side cur_turn ^ " "
              ^ string_of_move command;
            ])
          (gallery @ [ (cur_board, cur_grave, cur_score) ])
    | _ -> failwith "ai module error"

(** [play_game f] starts the adventure in file [f]. *)
let play_game mode diff =
  let init_st = State.init_state in
  let init_history = [] in
  let init_gallery = [] in
  play_game_help diff init_st mode init_history init_gallery

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let str_welcome =
    "\n\n~ Welcome to the 3110 Chinese Chess Game engine ~\n"
  in
  Printf.printf "\027[33m%s\027[0m" str_welcome;
  let str_mode =
    "\nOne player or Two players mode: 1 for one, 2 for two\n"
  in
  Printf.printf "\027[33m%s\027[0m" str_mode;
  print_string "> ";
  let msg = read_line () in
  let mode = ref 1 in
  let diff = ref "easy" in
  if msg = string_of_int 1 then mode := 1 else mode := 2;
  if !mode = 1 then (
    let str_diff = "\nPlease choose difficulty level : easy / hard\n" in
    Printf.printf "\027[33m%s\027[0m" str_diff;
    print_string "> ";
    let msg2 = read_line () in
    diff := msg2)
  else ();

  print_string "Start playing, You are the red side\n";
  play_game !mode !diff

(* Execute the game engine. *)

let () = main ()
