
type case = X | O | Empty of int * int

type player = Px | Po

type line = case list

type grid = line list

type move = int * int

type megaline = grid list

type megagrid = megaline list

let grid_at_start = [
						[Empty (0, 0); Empty (0, 1); Empty (0, 2);] ; 
						[Empty (1, 0); Empty (1, 1); Empty (1, 2);] ; 
						[Empty (2, 0); Empty (2, 1); Empty (2, 2);] ; 
					]

let megagrid_at_start = [ 
							[ grid_at_start ; grid_at_start ; grid_at_start ;] ;
							[ grid_at_start ; grid_at_start ; grid_at_start ;] ;
							[ grid_at_start ; grid_at_start ; grid_at_start ;] ;
						]
let size = 3

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
	List.map line_to_string grid

let display grid =
	let lines = grid_to_string grid in
	List.iter print_endline lines


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

let check_win grid = match grid with
	| [ [a ; b ; c] ; [_ ; _ ; _] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
	| [ [_ ; _ ; _] ; [a ; b ; c] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
	| [ [_ ; _ ; _] ; [_ ; _ ; _] ; [a ; b ; c] ; ] when a = b && b = c && (a = X || a = O) -> true
	| [ [a ; _ ; _] ; [b ; _ ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
	| [ [_ ; a ; _] ; [_ ; b ; _] ; [_ ; c ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
	| [ [_ ; _ ; a] ; [_ ; _ ; b] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = X || a = O) -> true
	| [ [a ; _ ; _] ; [_ ; b ; _] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = X || a = O) -> true
	| [ [_ ; _ ; a] ; [_ ; b ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = X || a = O) -> true
	| _ -> false


let () =
	display grid_at_start ;
	let win1 = play_move grid_at_start (0,0) X in
	display win1 ;
	let win2 = play_move win1 (0,1) X in
	display win2 ;
	let win3 = play_move win2 (0,1) X in
	display win3 ;
	if check_win win3 then print_endline "gagne"
	else print_endline "pas gagne"

