(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gchateau <gchateau@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 10:42:43 by gchateau          #+#    #+#             *)
(*   Updated: 2015/06/21 20:10:03 by gchateau         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main () =
  let game = Megagrid.megagrid_at_start in
  let rec p1_input mgrid =
	Megagrid.display mgrid;
	print_endline "O's turn to play.";
	let rec mv_input () =
	  let tmp = Input.read_position () in
		if Megagrid.legal_move mgrid tmp then
		  begin
			let res = Megagrid.mplay_move mgrid tmp Megagrid.Grid.O in
			  if Megagrid.mcheck_win res then
				begin
				  print_endline "O wins the game!\n";
				  Megagrid.display res;
				  match Input.read_retry () with
					| true	-> print_char '\n'; p1_input game
					| false	-> exit 0
				end
			  else p2_input (res)			
		  end
		else (print_endline "Illegal move."; mv_input ())
	in mv_input ()
  and p2_input mgrid =
	Megagrid.display mgrid;
	print_endline "X's turn to play.";
	let rec mv_input () =
	  let tmp = Input.read_position () in
		if Megagrid.legal_move mgrid tmp then
		  begin
			let res = Megagrid.mplay_move mgrid tmp Megagrid.Grid.X in
			  if Megagrid.mcheck_win res then
				begin
				  print_endline "X wins the game!\n";
				  Megagrid.display res;
				  match Input.read_retry () with
					| true	-> print_char '\n'; p1_input game
					| false	-> exit 0
				end
			  else p1_input (res)			
		  end
		else (print_endline "Illegal move."; mv_input ())
	in mv_input ()
  in
  p1_input game

(* ************************************************************************** *)
let () = main ()
