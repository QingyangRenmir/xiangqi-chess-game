open OUnit2
open Piece
open Command
open State
open Board
open Mlearn

exception Illegal_state

(** TEST PLAN *)

(** Since our system is a terminal based chess game, we did Test Driven
    Development and wrote automated tests for our basic functions to
    make them work as expected before we proceed to larger ones. We also
    play our game very frequently to find out what's wrong with our
    implementation and how they are reflected to the game interface *)

(** Main modules like State, Command, Piece, Board, MLearn are all
    automatically tested by OUnit tests shown below. However, for Main
    and AI modules, we did manual testing through "make play". We
    interactively playtest our code through our interface. As in A2, we
    find the drawbacks of our game, either in implementation or user
    experience , by playing the game ourselves. *)

(** This test plan is thorough and reasonable because it is very hard to
    generate automated tests for main and ai. A state of our chess game
    contains too much information and those information are cumulative,
    which means one cannot generate them from nothing but can only get
    the state you want by moving tens or hundreds of moves step by step.
    In fact, we also have some automated tests for these game state, but
    most of them are close to start board, and these tests work to show
    our functions work as intended and that would be enough. As you can
    see, it's tedious to write states for automated testing, even harder
    than what we did in A1: Enigma. That's why we manually test them
    through playing our games*)

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [get_result_state result] is the state from result [result], if
    result is legal, then gives state; otherwise, raise Illegal_state
    exception *)
let get_result_state result =
  match result with
  | State.Legal t -> t
  | State.Illegal -> raise Illegal_state

let string_of_int_tuple tp =
  "(" ^ string_of_int (fst tp) ^ ", " ^ string_of_int (snd tp) ^ ")"

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let string_of_piece_op piece_op =
  let rank = Char.escaped (char_of_piece piece_op) in
  let coord = piece_op |> extract |> Piece.get_coord in
  rank ^ " at "
  ^ string_of_int (fst coord)
  ^ ","
  ^ string_of_int (snd coord)

let string_of_piece piece =
  let coord = Piece.get_coord piece in
  Piece.string_of_piece piece
  ^ " at "
  ^ string_of_int (fst coord)
  ^ ","
  ^ string_of_int (snd coord)

let string_of_move move =
  "("
  ^ string_of_int_tuple (fst move)
  ^ ", "
  ^ string_of_int_tuple (snd move)
  ^ ")"

let advisor = create_piece Advisor Red (9, 3)

let general = create_piece General Black (0, 4)

let rook = create_piece Rook Red (9, 0)

let moved_advisor = create_piece Advisor Red (8, 4)

let start_board = generate_board ()

let updated_board = update_board start_board (9, 3) (8, 4)

let start_state = State.init_state

let rcannon_capture_bhorse =
  get_result_state (State.move (7, 1) (0, 1) start_state)

let brook_cap_rcannon =
  get_result_state (State.move (0, 0) (0, 1) rcannon_capture_bhorse)

let piece_tests =
  [
    ("get advisor" >:: fun _ -> assert_equal (get_c advisor) Advisor);
    ("get side" >:: fun _ -> assert_equal (get_side advisor) Red);
    ("get general" >:: fun _ -> assert_equal (get_c general) General);
    ("get black side" >:: fun _ -> assert_equal (get_side general) Black);
    (*added rook tests*)
    ("get rook1" >:: fun _ -> assert_equal (get_side rook) Red);
    ("get rook2" >:: fun _ -> assert_equal (get_c rook) Rook);
  ]

let state_tests =
  [
    ( "Illegal turn" >:: fun _ ->
      assert_equal Illegal (move (3, 0) (2, 0) init_state) );
    ( "black graveyard" >:: fun _ ->
      assert_equal
        [ create_piece Horse Black (0, 1) ]
        (get_black_g (get_current_grave rcannon_capture_bhorse))
        (*Due to modification of the graveyard sytem, uss this for now *)
        (* (State.get_current_black_g rcannon_capture_bhorse) *)
        ~printer:(pp_list string_of_piece) );
    ( "red graveyard" >:: fun _ ->
      assert_equal
        [ create_piece Cannon Red (0, 1) ]
        (get_red_g (get_current_grave brook_cap_rcannon))
        ~printer:(pp_list string_of_piece) );
  ]

let command_err name str excep =
  name >:: fun ctxt -> assert_raises excep (fun () -> parse str)

let command_tests =
  [
    ("quit" >:: fun _ -> assert_equal Quit (parse "quit"));
    ( "move good" >:: fun _ ->
      assert_equal (Move [ (2, 3); (4, 5) ]) (parse "move 2,3 4,5") );
    command_err "empty" "" Empty;
    command_err "coordinate wrong" "move 1 2" Malformed;
    command_err "coordinate space" "move 1, 3 2, 5" Malformed;
    command_err "coordinate length wrong" "move 1,2 3,4 5,6" Malformed;
    ( "move good rook" >:: fun _ ->
      assert_equal (Move [ (9, 0); (8, 0) ]) (parse "move 9,0 8,0") );
    ( "move good elepant" >:: fun _ ->
      assert_equal (Move [ (9, 2); (7, 4) ]) (parse "move 9,2 7,4") );
    ( "move good general" >:: fun _ ->
      assert_equal (Move [ (0, 4); (1, 4) ]) (parse "move 0,4 1,4") );
    ( "move good horse" >:: fun _ ->
      assert_equal (Move [ (9, 7); (7, 6) ]) (parse "move 9,7 7,6") );
    ( "move good soldier" >:: fun _ ->
      assert_equal (Move [ (3, 8); (4, 8) ]) (parse "move 3,8 4,8") );
    ( "move good cannon" >:: fun _ ->
      assert_equal (Move [ (7, 1); (5, 1) ]) (parse "move 7,1 5,1") );
    (*added tests*)
    ( "move good cannon2" >:: fun _ ->
      assert_equal (Move [ (7, 1); (0, 1) ]) (parse "move 7,1 0,1") );
    ( "move good general2" >:: fun _ ->
      assert_equal (Move [ (9, 4); (8, 4) ]) (parse "move 9,4 8,4") );
  ]

let board_tests =
  [
    ( "get piece test" >:: fun _ ->
      assert_equal (Some advisor)
        (get_piece start_board (9, 3))
        ~printer:string_of_piece_op );
    ( "update board test" >:: fun _ ->
      assert_equal (Some moved_advisor)
        (get_piece updated_board (8, 4))
        ~printer:string_of_piece_op );
  ]

let string_of_2d_array array =
  let str = ref "" in
  for i = 0 to Array.length array.(0) - 1 do
    str :=
      string_of_int array.(0).(i)
      ^ ";" ^ !str
      ^ if i mod 14 = 0 then "\n" else ""
  done;
  !str ^ ""

let ai_tests = []

let dummy_array = Array.make 65299 (Array.make 4 "")

let data = Mlearn.populate_train "moves.csv" dummy_array

let red_horse_out =
  get_result_state (State.move (9, 7) (7, 6) start_state)

let blue_horse_out =
  get_result_state (State.move (0, 7) (2, 6) red_horse_out)

let red_rook_out =
  get_result_state (State.move (9, 8) (7, 8) blue_horse_out)

let r_random_move =
  get_result_state (State.move (9, 0) (8, 0) start_state)

let mlearn_tests1 =
  [
    (* ( "populate_train test" >:: fun _ -> assert_equal (Array.to_list
       [| "57390689"; "36"; "black"; "h5+4" |]) (Array.to_list
       data.(Array.length data - 1)) ~printer:(pp_list pp_string) ); *)
    ( "translate_lines test" >:: fun _ ->
      assert_equal
        (Array.to_list (Mlearn.translate_lines start_board 7).(1))
        [ 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0 ]
        ~printer:(pp_list string_of_int) );
    ( "translate_board test: red cannon" >:: fun _ ->
      assert_equal
        (Array.to_list (Mlearn.translate_board start_board).(7).(1))
        [ 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0 ]
        ~printer:(pp_list string_of_int) );
    ( "translate_board test: blue rook" >:: fun _ ->
      assert_equal
        (Array.to_list (Mlearn.translate_board start_board).(0).(0))
        [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 ]
        ~printer:(pp_list string_of_int) );
    ( "translate_board test: blue general" >:: fun _ ->
      assert_equal
        (Array.to_list (Mlearn.translate_board start_board).(0).(4))
        [ 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0 ]
        ~printer:(pp_list string_of_int) );
    ( "get start coord: red\n       right cannon" >:: fun _ ->
      assert_equal (7, 7)
        (Mlearn.get_start_coord "C" start_board "2")
        ~printer:string_of_int_tuple );
    ( "get start coord: black advisor" >:: fun _ ->
      assert_equal (0, 3)
        (Mlearn.get_start_coord "a" start_board "4")
        ~printer:string_of_int_tuple );
    ( "get end\n       coord: red normal +" >:: fun _ ->
      assert_equal (7, 8)
        (Mlearn.get_end_coord (9, 8) start_state "+" "Red" "2")
        ~printer:string_of_int_tuple );
    ( "get end coord: red normal ." >:: fun _ ->
      assert_equal (7, 4)
        (Mlearn.get_end_coord (7, 7) start_state "." "Red" "5")
        ~printer:string_of_int_tuple );
    ( "get\n       end coord: red horse +" >:: fun _ ->
      assert_equal (7, 6)
        (Mlearn.get_end_coord (9, 7) start_state "+" "Red" "3")
        ~printer:string_of_int_tuple );
    ( "get end coord: blue normal +" >:: fun _ ->
      assert_equal (2, 0)
        (Mlearn.get_end_coord (0, 0) rcannon_capture_bhorse "+" "Black"
           "2")
        ~printer:string_of_int_tuple );
    ( "get end coord: blue normal ." >:: fun _ ->
      assert_equal (2, 4)
        (Mlearn.get_end_coord (2, 1) rcannon_capture_bhorse "." "Black"
           "5")
        ~printer:string_of_int_tuple );
    ( "get end coord: blue horse +" >:: fun _ ->
      assert_equal (2, 6)
        (Mlearn.get_end_coord (0, 7) rcannon_capture_bhorse "+" "Black"
           "7")
        ~printer:string_of_int_tuple );
    ( "get end coord: red normal -" >:: fun _ ->
      assert_equal (8, 7)
        (Mlearn.get_end_coord (7, 7) start_state "-" "Red" "1")
        ~printer:string_of_int_tuple );
    ( "get\n       end coord: red horse -" >:: fun _ ->
      assert_equal (9, 7)
        (Mlearn.get_end_coord (7, 6) blue_horse_out "-" "Red" "2")
        ~printer:string_of_int_tuple );
    ( "get end coord: blue normal -" >:: fun _ ->
      assert_equal (1, 1)
        (Mlearn.get_end_coord (2, 1) red_horse_out "-" "Black" "1")
        ~printer:string_of_int_tuple );
    ( "get end coord: blue horse -" >:: fun _ ->
      assert_equal (0, 7)
        (Mlearn.get_end_coord (2, 6) red_rook_out "-" "Black" "8")
        ~printer:string_of_int_tuple );
  ]

let start_state_ref = ref start_state

let h23_ref = ref r_random_move

let start_state = State.init_state

let intermediate1 =
  get_result_state (State.move (7, 1) (8, 1) start_state)

let intermediate2 =
  get_result_state (State.move (0, 4) (1, 4) intermediate1)

let consecutive_cannon =
  get_result_state (State.move (8, 1) (8, 7) intermediate2)

let mlearn_tests2 =
  [
    ( "get start coord: first black data h2+3" >:: fun _ ->
      assert_equal (0, 1)
        (Mlearn.get_start_coord "h"
           (State.get_current_board r_random_move)
           "2")
        ~printer:string_of_int_tuple );
    ( "get end coord: first black\n       data h2+3" >:: fun _ ->
      assert_equal (2, 2)
        (Mlearn.get_end_coord (0, 1) r_random_move "+" "Black" "3")
        ~printer:string_of_int_tuple );
    ( "translate_coord" >:: fun _ ->
      assert_equal
        ((7, 7), (7, 4))
        (Mlearn.translate_coord start_state_ref "C2.5")
        ~printer:string_of_move );
    ( "translate_coord: h2+3" >:: fun _ ->
      assert_equal
        ((0, 1), (2, 2))
        (Mlearn.translate_coord h23_ref "h2+3")
        ~printer:string_of_move );
    ( "translate_coord: A6+5" >:: fun _ ->
      assert_equal
        ((9, 3), (8, 4))
        (Mlearn.translate_coord start_state_ref "A6+5")
        ~printer:string_of_move );
    ( "get_start_coord: ++" >:: fun _ ->
      assert_equal (7, 7)
        (Mlearn.get_start_coord "C"
           (get_current_board consecutive_cannon)
           "+")
        ~printer:string_of_int_tuple );
    ( "get_start_coord: -+" >:: fun _ ->
      assert_equal (8, 7)
        (Mlearn.get_start_coord "C"
           (get_current_board consecutive_cannon)
           "-")
        ~printer:string_of_int_tuple );
    ( "cal_game_length: full game" >:: fun _ ->
      assert_equal
        [ 73; 73; 54; 100; 57; 92 ]
        (Mlearn.cal_game_length (Array.sub data 0 449))
        ~printer:(pp_list string_of_int) );
    ( "cal_game_length: game not complete" >:: fun _ ->
      assert_equal [ 73; 27 ]
        (Mlearn.cal_game_length (Array.sub data 0 100))
        ~printer:(pp_list string_of_int) );
  ]

let suite =
  "test suite forProject"
  >::: (* let () = print_board (get_current_board consecutive_cannon)
          (Board.generate_graveyard ()) (Board.generate_score ()) in *)
  List.flatten
    [
      state_tests;
      command_tests;
      piece_tests;
      board_tests;
      ai_tests;
      mlearn_tests1;
      mlearn_tests2;
    ]

let _ = run_test_tt_main suite
