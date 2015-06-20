
type case = X | O | Empty of int * int

type line = case list

type grid = Xwin | Owin | Playing of line list

type move = int * int

let grid_at_start = Playing ([
						[Empty (0, 0); Empty (0, 1); Empty (0, 2);] ; 
						[Empty (1, 0); Empty (1, 1); Empty (1, 2);] ; 
						[Empty (2, 0); Empty (2, 1); Empty (2, 2);] ; 
					])

let test1_grid = Playing ([
						[X; Empty (0, 1); Empty (0, 2);] ; 
						[Empty (1, 0); X; Empty (1, 2);] ; 
						[Empty (2, 0); Empty (2, 1); O;] ; 
					])

let test2_grid = Xwin

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
	let g_to_string grid =
		List.map line_to_string grid
	in
	match grid with
		| Xwin -> ["x x x"; "x x x"; "x x x"]
		| Owin -> ["o o o"; "o o o"; "o o o"]
		| Playing a -> g_to_string a


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

let mgrid_test1 = [ 
						[ grid_at_start ; grid_at_start ; test2_grid ;] ;
						[ grid_at_start ; test1_grid ; grid_at_start ;] ;
						[ grid_at_start ; grid_at_start ; grid_at_start ;] ;
					]


let mgrid_to_string mgrid = 
	let mline_to_string mline = match mline with
		| [a ; b ; c;] -> (List.nth (grid_to_string a) 0) ^ " | " ^ (List.nth (grid_to_string b) 0) ^ " | " ^ (List.nth (grid_to_string c) 0)
		| _ -> ""
	in
	List.map mline_to_string mgrid 

let display mgrid =
	let lines = mgrid_to_string mgrid in
	List.iter print_endline lines

(* *)
let m_check_win mgrid = match mgrid with 
	| [ [a ; b ; c] ; [_ ; _ ; _] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; _ ; _] ; [a ; b ; c] ; [_ ; _ ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; _ ; _] ; [_ ; _ ; _] ; [a ; b ; c] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [a ; _ ; _] ; [b ; _ ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; a ; _] ; [_ ; b ; _] ; [_ ; c ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; _ ; a] ; [_ ; _ ; b] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [a ; _ ; _] ; [_ ; b ; _] ; [_ ; _ ; c] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| [ [_ ; _ ; a] ; [_ ; b ; _] ; [c ; _ ; _] ; ] when a = b && b = c && (a = Xwin || a = Owin) -> true
	| _ -> false


let maxgrid_pos n = match n with
  | 1 | 2 | 3   -> 0
  | 4 | 5 | 6   -> 1
  | 7 | 8 | 9   -> 2
  | _           -> -1

let mingrid_pos n = match n with
  | 1 | 4 | 7   -> 0
  | 2 | 5 | 8   -> 1
  | 3 | 6 | 9   -> 2
  | _           -> -1

let move_getx (x, y) = x
let move_gety (x, y) = y

(* *)
let m_play_move megagrid move player =
  let max_a = maxgrid_pos (move_gety move) in
  let max_b = maxgrid_pos (move_getx move) in
  let min_x = mingrid_pos (move_getx move) in
  let min_y = mingrid_pos (move_gety move) in
    let rec loop i ln nl =
      if i > 2 then nl
      else
        begin
          if i = max_b then
            loop (i + 1) ln (nl @ [(play_move (List.nth ln i) (min_x, min_y) player)])
          else
            loop (i + 1) ln (nl @ [(List.nth ln i)])
        end
    in
    let rec rebuilt i nl =
      if i > 2 then nl
      else
        begin
          if i = max_a then
            rebuilt (i + 1) (nl @ [(loop 0 (List.nth megagrid i) [])])
          else
            rebuilt (i + 1) (nl @ [List.nth megagrid i])
        end
    in rebuilt 0 []

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


