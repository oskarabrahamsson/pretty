
type state

val empty : unit -> state

val open_block : state -> int -> unit
val close_block : state -> unit
val print_string : state -> string -> unit
val print_break : state -> int -> unit
val exec : state -> Pretty.token

