type token

val string : string -> token
val break : int -> token
val block : int -> token list -> token
val print : out_channel -> int -> token -> unit

