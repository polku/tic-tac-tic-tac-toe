
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


		(** Play a move in a grid, return grid updated *)
		let play_move grid move player =
			let getX () = match move with
				| (x, _) -> x
			in
			let getY () = match move with
				| (_, y) -> y
			in
			let new_grid () =
				let new_case c = match c with
					| Empty (x, y) when x = getX () && y = getY () -> player
					| X | O -> failwith "Illegal move"
					| _ -> c 
				in
				let new_line n =
					List.map new_case (List.nth grid n)
				in
				[ new_line 0 ; new_line 1 ; new_line 2 ]
			in
			new_grid ()

		(* Check win in a grid *)
		let check_win grid = match grid with
			| [ [a ; b ; c] ; [_ ; _ ; _] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
			| [ [_ ; _ ; _] ; [a ; b ; c] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
			| [ [_ ; _ ; _] ; [_ ; _ ; _] ; [a ; b ; c] ; ] when a = b && b = c && (a = X || a = O) -> true
			| [ [a ; _ ; _] ; [b ; _ ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
			| [ [_ ; a ; _] ; [_ ; b ; _] ; [_ ; c ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
			| [ [_ ; _ ; a] ; [_ ; _ ; b] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = X || a = O) -> true
			| [ [a ; _ ; _] ; [_ ; b ; _] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = X || a = O) -> true
			| [ [_ ; _ ; a] ; [_ ; b ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
			| [ [a ; b ; c] ; [d ; e ; f] ; [g ; h ; i] ; ] when (a = X || a = O) && (b = X || b = O) && 
																	(c = X || c = O) && (d = X || d = O) && 
																	(e = X || e = O) && (f = X || f = O) && 
																	(g = X || g = O) && (h = X || h = O) &&
																	(i = X || i = O)
															-> true
			| _ -> false

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
	List.iter print_endline lines

(* *)
let m_check_win mgrid = match mgrid with 
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



let () =
	display megagrid_at_start ;

