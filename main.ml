(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gchateau <gchateau@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 10:42:43 by gchateau          #+#    #+#             *)
(*   Updated: 2015/06/21 17:11:32 by gchateau         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main () =
  let game = Megagrid.megagrid_at_start in
  let rec p1_input mgrid =
	Megagrid.display mgrid;
	print_endline "O's turn to play.";
	let rec tmp_input () = try Megagrid.mplay_move mgrid (Input.read_position ()) Megagrid.Grid.O with
	  | Failure "Illegal move."		-> print_endline "Illegal move."; tmp_input ()
	in
	let mg = tmp_input () in
	  if Megagrid.mcheck_win mg then
		begin
		  print_endline "O wins the game!";
		  let tmp_retry () = Input.read_retry () in
			if tmp_retry () then
			  begin
				print_char '\n';
				p1_input game
			  end
			else exit 0
		end
	  else p2_input (mg)
  and p2_input mgrid =
	Megagrid.display mgrid;
	print_endline "X's turn to play.";
	let rec tmp_input () = try Megagrid.mplay_move mgrid (Input.read_position ()) Megagrid.Grid.X with
	  | Failure "Illegal move."		-> print_endline "Illegal move."; tmp_input ()
	in
	let mg = tmp_input () in
	  if Megagrid.mcheck_win mg then
		begin
		  print_endline "X wins the game!";
		  let tmp_retry () = Input.read_retry () in
			if tmp_retry () then
			  begin
				print_char '\n';
				p1_input game
			  end
			else exit 0
		end
	  else p1_input (mg)
  in
  p1_input game

(* ************************************************************************** *)
let () = main ()
