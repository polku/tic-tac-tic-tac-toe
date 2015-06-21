module Grid :
  sig

    type case = X | O | Empty of int * int
    type line
    type grid
    type move

    val grid_at_start : grid

    val grid_to_string : grid -> string list

    val check_win : grid -> bool

    val play_move : case list list -> int * int -> case -> grid

    val legal_move : case list list -> int * int -> bool

    val identity : grid -> grid

  end

type megaline
type megagrid

val megagrid_at_start : Grid.grid list list

val mgrid_to_string : Grid.grid list list -> string list

val display : Grid.grid list list -> unit

val mcheck_win : Grid.grid list list -> bool

val mplay_move : Grid.grid list list -> int * int -> Grid.case -> Grid.grid list list

val legal_move : Grid.grid list list -> int * int -> bool
