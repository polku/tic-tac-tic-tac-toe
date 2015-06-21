(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gchateau <gchateau@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 10:42:43 by gchateau          #+#    #+#             *)
(*   Updated: 2015/06/21 15:22:21 by gchateau         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_tuple (a, b) =
  print_string "(";
  print_int a;
  print_string ", ";
  print_int b;
  print_endline ")";
  (a, b)

let main ac av =
  let game = Megagrid.megagrid_at_start in
  let rec p1_input mgrid =
	Megagrid.display mgrid;
	print_endline "\nO's turn to play.";
	let rec tmp_input () = try Megagrid.mplay_move mgrid (print_tuple (Input.read_position ())) Megagrid.Grid.O with
	  | Failure "Illegal move."		-> print_endline "Illegal move."; tmp_input ()
	in
	p2_input (tmp_input ());
  and p2_input mgrid =
	Megagrid.display mgrid;
	print_endline "\nX's turn to play.";
	let rec tmp_input () = try Megagrid.mplay_move mgrid (print_tuple (Input.read_position ())) Megagrid.Grid.X with
	  | Failure "Illegal move."		-> print_endline "Illegal move."; tmp_input ()
	in
	p1_input (tmp_input ());
  in
  p1_input game

(* ************************************************************************** *)
let () =
  let argv = Array.to_list Sys.argv in
	main (List.length argv) argv
