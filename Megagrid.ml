
module Grid = 
	struct

		type case = X | O | Empty of int * int

		type line = case list

		type grid = Xwin | Owin | Playing of line list

		type move = int * int

		let grid_at_start = Playing ([
								[Empty (0, 0); Empty (0, 1); Empty (0, 2);] ; 
								[Empty (1, 0); Empty (1, 1); Empty (1, 2);] ; 
								[Empty (2, 0); Empty (2, 1); Empty (2, 2);] ; 
							])


		(** Return a list of string representations of lines *)
		let grid_to_string grid = 
			let case_to_string c = match c with 
				| X -> "X"
				| O -> "O"
				| Empty (_, _) -> "_"
			in
			let line_to_string l = match l with
				| [a; b; c;] -> (case_to_string a) ^ " " ^ (case_to_string b) ^ " " ^ (case_to_string c)
				| _ -> ""
			in
			let g_to_string grid =
				List.map line_to_string grid
			in
			match grid with
				| Xwin -> ["x x x"; "x x x"; "x x x"]
				| Owin -> ["o o o"; "o o o"; "o o o"]
				| Playing a -> g_to_string a



		(* Check win in a grid *)
		let check_win grid = match grid with
			| Xwin | Owin -> true
			| Playing([ [a ; b ; c] ; [_ ; _ ; _] ; [_ ; _ ; _] ; ]) when a = b && b = c && (a = X || a = O) -> true
			| Playing([ [_ ; _ ; _] ; [a ; b ; c] ; [_ ; _ ; _] ; ]) when a = b && b = c && (a = X || a = O) -> true
			| Playing([ [_ ; _ ; _] ; [_ ; _ ; _] ; [a ; b ; c] ; ]) when a = b && b = c && (a = X || a = O) -> true
			| Playing([ [a ; _ ; _] ; [b ; _ ; _] ; [c ; _ ; _] ; ]) when a = b && b = c && (a = X || a = O) -> true
			| Playing([ [_ ; a ; _] ; [_ ; b ; _] ; [_ ; c ; _] ; ]) when a = b && b = c && (a = X || a = O) -> true
			| Playing([ [_ ; _ ; a] ; [_ ; _ ; b] ; [_ ; _ ; c] ; ]) when a = b && b = c && (a = X || a = O) -> true
			| Playing([ [a ; _ ; _] ; [_ ; b ; _] ; [_ ; _ ; c] ; ]) when a = b && b = c && (a = X || a = O) -> true
			| Playing([ [_ ; _ ; a] ; [_ ; b ; _] ; [c ; _ ; _] ; ]) when a = b && b = c && (a = X || a = O) -> true
			| Playing([ [a ; b ; c] ; [d ; e ; f] ; [g ; h ; i] ; ]) when (a = X || a = O) && (b = X || b = O) && 
																	(c = X || c = O) && (d = X || d = O) && 
																	(e = X || e = O) && (f = X || f = O) && 
																	(g = X || g = O) && (h = X || h = O) &&
																	(i = X || i = O)
															-> true
			| _ -> false


(*   Updated: 2015/06/21 16:26:13 by gchateau         ###   ########.fr       *)
		let play_move grid move player =
			let getX () = match move with
				| (x, _) -> x
			in
			let getY () = match move with
				| (_, y) -> y
			in
			let new_case c xp yp = match c with
				| Empty (x, y) when x = getX () && y = getY () -> player
				| X when xp = getX () && yp = getY () -> failwith "Illegal move."
				| O when xp = getX () && yp = getY () -> failwith "Illegal move."
				| _ -> c 
			in
			let new_line l num =
				let rec loop n =
					if n = 3 then []
					else (new_case (List.nth l n) n num) :: loop (n + 1)
				in
				loop 0
			in
			let rec gloop n =
				if n = 3 then []
				else (new_line (List.nth grid n) n) :: gloop (n+1)
			in
			if check_win (Playing(gloop 0)) && player = X then Xwin
			else if check_win (Playing(gloop 0)) && player = O then Owin
			else Playing(gloop 0)

		let identity g = match g with
			| Playing a -> Playing a
			| _ -> g

	end

(* Megagrid functions *)

type megaline = Grid.grid list

type megagrid = megaline list

let megagrid_at_start = [ 
							[ Grid.grid_at_start ; Grid.grid_at_start ; Grid.grid_at_start ;] ;
							[ Grid.grid_at_start ; Grid.grid_at_start ; Grid.grid_at_start ;] ;
							[ Grid.grid_at_start ; Grid.grid_at_start ; Grid.grid_at_start ;] ;
						]

let mgrid_to_string mgrid = 
	let mline_to_string mline = match mline with
		| [a ; b ; c;] -> (List.nth (Grid.grid_to_string a) 0) ^ " | " ^ (List.nth (Grid.grid_to_string b) 0) ^ " | " ^ (List.nth (Grid.grid_to_string c) 0)
		| _ -> ""
	in
	List.map mline_to_string mgrid 

let mgrid_to_string mgrid =
	let get_mline n =
		List.nth mgrid n
	in
	let rec loop ml line =
		let get_str m g l =
			List.nth (Grid.grid_to_string (List.nth (get_mline m) g)) l
		in
		let get_line m l =
			[ get_str m 0 l ^ " | " ^ get_str m 1 l ^ " | " ^ get_str m 2 l ]
		in
		match (ml, line) with
			| (x, y) when x > 2 -> []
			| (x, y) when y = 2 -> (get_line x y) @ ["=-=-=-=-=-=-=-=-=-=-="] @(loop (x+1) 0)
			| (x, y) -> (get_line x y) @ (loop x (y+1))
	in
  	loop 0 0

let display mgrid =
	let lines = mgrid_to_string mgrid in
	List.iter print_endline lines ;
	print_endline "                     "

(** Check win on the megagrid*)
let mcheck_win mgrid = match mgrid with 
	| [ [a ; b ; c] ; [_ ; _ ; _] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = Grid.Xwin || a = Grid.Owin) -> true
	| [ [_ ; _ ; _] ; [a ; b ; c] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = Grid.Xwin || a = Grid.Owin) -> true
	| [ [_ ; _ ; _] ; [_ ; _ ; _] ; [a ; b ; c] ; ] when a = b && b = c && (a = Grid.Xwin || a = Grid.Owin) -> true
	| [ [a ; _ ; _] ; [b ; _ ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = Grid.Xwin || a = Grid.Owin) -> true
	| [ [_ ; a ; _] ; [_ ; b ; _] ; [_ ; c ; _] ; ] when a = b && b = c && (a = Grid.Xwin || a = Grid.Owin) -> true
	| [ [_ ; _ ; a] ; [_ ; _ ; b] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = Grid.Xwin || a = Grid.Owin) -> true
	| [ [a ; _ ; _] ; [_ ; b ; _] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = Grid.Xwin || a = Grid.Owin) -> true
	| [ [_ ; _ ; a] ; [_ ; b ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = Grid.Xwin || a = Grid.Owin) -> true
	| [ [a ; b ; c] ; [d ; e ; f] ; [g ; h ; i] ; ] when (a = Grid.Xwin || a = Grid.Owin) && (b = Grid.Xwin || b = Grid.Owin) && 
														(c = Grid.Xwin || c = Grid.Owin) && (d = Grid.Xwin || d = Grid.Owin) && 
														(e = Grid.Xwin || e = Grid.Owin) && (f = Grid.Xwin || f = Grid.Owin) && 
														(g = Grid.Xwin || g = Grid.Owin) && (h = Grid.Xwin || h = Grid.Owin) &&
															(i = Grid.Xwin || i = Grid.Owin)
															-> true
	| _ -> false

let posX (x, _) = x
let posY (_, y) = y

let gPos n = match n with
  | 1 | 2 | 3    -> 0
  | 4 | 5 | 6    -> 1
  | 7 | 8 | 9    -> 2
  | _            -> failwith "Illegal move."

let cPos n = match n with
  | 1 | 4 | 7    -> 0
  | 2 | 5 | 8    -> 1
  | 3 | 6 | 9    -> 2
  | _            -> failwith "Illegal move."

let print_debug x y gx gy cx cy str =
  print_endline str;
  print_string "x: ";
  print_int x;
  print_string "; y: ";
  print_int y;
  print_string "\nx_grid: ";
  print_int gx;
  print_string "; y_grid: ";
  print_int gy;
  print_string "\nx_case: ";
  print_int cx;
  print_string "; y_case: ";
  print_int cy;
  print_endline "\n"

let posX (x, _) = x
let posY (_, y) = y

let gPos n = match n with
  | 1 | 2 | 3	-> 0
  | 4 | 5 | 6	-> 1
  | 7 | 8 | 9	-> 2
  | _			-> failwith "Illegal move."

let cPos n = match n with
  | 1 | 4 | 7	-> 0
  | 2 | 5 | 8	-> 1
  | 3 | 6 | 9	-> 2
  | _			-> failwith "Illegal move."

(** Play a move int he megagrid first finding the grid to play in *)
let mplay_move mgrid move player =
	let y_grid = gPos (posY move) in 
	let x_grid = gPos (posX move) in
	let y_case = cPos (posX move) in 
	let x_case = cPos (posY move) in
	let new_grid g x y = match g with 
		| Grid.Playing (a) when x = x_grid && y = y_grid -> Grid.play_move a (x_case,y_case) player
		| Grid.Xwin when x = x_grid && y = y_grid -> failwith "Illegal move."
		| Grid.Owin when x = x_grid && y = y_grid -> failwith "Illegal move."
		| _ -> g
	in
	let new_line l num =
		let rec loop n =
			if n = 3 then []
			else (new_grid (List.nth l n) n num) :: loop (n + 1)
		in
		loop 0
	in
	let rec mloop n =
		if n = 3 then []
		else (new_line (List.nth mgrid n) n) :: mloop (n+1)
	in
	mloop 0
	(* + check win et ajout variant pour victoire finale *)
