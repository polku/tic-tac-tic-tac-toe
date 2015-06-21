(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Input.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gchateau <gchateau@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 10:45:49 by gchateau          #+#    #+#             *)
(*   Updated: 2015/06/21 14:20:58 by gchateau         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let parse str =
  let str = String.trim str in
  let len = String.length str in
  let rec loop i s nl =
	if i >= len then nl @ [(int_of_string s)]
	else match (String.get str i) with
	  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'	->
		  loop (i + 1) (s ^ (String.sub str i 1)) nl
	  | ' '	->
		  if (String.length s) = 0 then loop (i + 1) s nl
		  else loop (i + 1) "" (nl @ [(int_of_string s)])
	  | _	-> []
  in
	if len = 0 then []
	else loop 0 "" []

let rec read_position () =
	let lst = parse (read_line ()) in
	  match lst with
		| [y; x] when x < 10  && x > 0 && y < 10 && x > 0	-> print_char '\n'; (x, y)
		| [y; x] when x > 9 || x < 1 || y > 9 || y < 1		-> print_endline "Illegal move."; read_position ()
		| _													-> print_endline "Incorrect format."; read_position ()
