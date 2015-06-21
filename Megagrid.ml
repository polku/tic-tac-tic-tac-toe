
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


		let play_move grid move player =
			let getX () = match move with
				| (x, _) -> x
			in
			let getY () = match move with
				| (_, y) -> y
			in
			let new_case c xp yp = match c with
				| Empty (x, y) when x = getX () && y = getY () -> player
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
			if check_win (Playing(gloop 0)) && player = X then (print_endline "X win a grid!"; Xwin)
			else if check_win (Playing(gloop 0)) && player = O then (print_endline "O win a grid!"; Owin)
			else Playing(gloop 0)


		let legal_move grid move =
            let getX () = match move with
              | (x, _) -> x
            in
            let getY () = match move with
              | (_, y) -> y
            in
            let get_case () =
                List.nth (List.nth grid (getX ())) (getY ())
            in
            match get_case () with 
              | Empty(_, _) -> true
              | _ -> false


		let identity g = match g with
			| Playing a -> Playing a
			| _ -> g

	end

(* Megagrid functions *)

type megaline = Grid.grid list

type megagrid = Error | Mgrid of megaline list

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
  | _            -> -1

let cPos n = match n with
  | 1 | 4 | 7    -> 0
  | 2 | 5 | 8    -> 1
  | 3 | 6 | 9    -> 2
  | _            -> -1

(** Play a move int he megagrid first finding the grid to play in *)
let mplay_move mgrid move player =
	let y_grid = gPos (posY move) in 
	let x_grid = gPos (posX move) in
	let y_case = cPos (posX move) in 
	let x_case = cPos (posY move) in
	let new_grid g x y = match g with 
		| Grid.Playing (a) when x = x_grid && y = y_grid -> Grid.play_move a (x_case,y_case) player
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

let legal_move mgrid move =
    let get_case () =
        List.nth (List.nth mgrid (gPos (posX move))) (gPos (posY move))
    in
    let y_case = cPos (posX move) in 
    let x_case = cPos (posY move) in
    let thegrid = get_case () in
    match thegrid with
      | Grid.Xwin | Grid.Owin -> false
      | Grid.Playing(t) -> Grid.legal_move t (x_case, y_case)
