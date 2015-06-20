
type case = X | O | Empty of int * int

type line = case list

type grid = Xwin | Owin | Playing of line list

type move = int * int

let grid_at_start = Playing ([
						[Empty (0, 0); Empty (0, 1); Empty (0, 2);] ; 
						[Empty (1, 0); Empty (1, 1); Empty (1, 2);] ; 
						[Empty (2, 0); Empty (2, 1); Empty (2, 2);] ; 
					])

let size = 3

(* Return a list of strings representations of lines *)
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






(* display for a grid alone FOR DEBUG *)
let display grid =
	let lines = grid_to_string grid in
	List.iter print_endline lines

(* play move in a grid, return grid updated *)
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

(* check win in a grid *)
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



(* Megagrid functions *)

type megaline = grid list

type megagrid = megaline list

let megagrid_at_start = [ 
							[ grid_at_start ; grid_at_start ; grid_at_start ;] ;
							[ grid_at_start ; grid_at_start ; grid_at_start ;] ;
							[ grid_at_start ; grid_at_start ; grid_at_start ;] ;
						]

(* Return list of line *)
let mgrid_to_string mgrid = 
	let g_to_string grid = match grid with
		| Xwin -> ["x x x"; "x x x"; "x x x"]
		| Owin -> ["o o o"; "o o o"; "o o o"]
		| Playing a -> grid_to_string a
	in
	let mline_to_string mline = match mline with
		| [a ; b ; c;] -> (List.nth (g_to_string a) 0) ^ " " ^ (List.nth (g_to_string a) 1) ^ " " ^ (List.nth (g_to_string a) 2)
		| _ -> ""
	in


(* *)
let m_check_win megagrid = match mgrid
	| [ [a ; b ; c] ; [_ ; _ ; _] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; _ ; _] ; [a ; b ; c] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; _ ; _] ; [_ ; _ ; _] ; [a ; b ; c] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [a ; _ ; _] ; [b ; _ ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; a ; _] ; [_ ; b ; _] ; [_ ; c ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; _ ; a] ; [_ ; _ ; b] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [a ; _ ; _] ; [_ ; b ; _] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; _ ; a] ; [_ ; b ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| _ -> false

(* *)
let m_play_move megagrid move player = 
(* joue puis copy megagrid en veirfiant si gagne pour remp par Xwin ou Owin *)

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


