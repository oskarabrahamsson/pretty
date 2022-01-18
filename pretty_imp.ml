
(* This is a stateful (or non-functional) version of the pretty-printer library
   from ML for the working programmer by L.C. Paulson.

   The original interface looks something like this:

     type token
     val string : string -> token
     val break : int -> token
     val block : int -> token list -> token

     val print : outstream -> int -> token -> unit

   i.e. a token type, several constructors for the token type, and a function
   that given an outstream, a margin, and a token, formats and prints the output
   all at once.

   The imperative version exports this interface:

     type state
     val open_block : state -> int -> unit
     val close_block : state -> unit
     val print_string : state -> string -> unit
     val print_break : state -> int -> unit
     val exec : state -> Pretty.token

   so, just a builder for Pretty.tokens.

 *)

type token_queue = Token_queue of Pretty.token list *   (* stack of tokens *)
                                  int                   (* block indent    *)

let tq_empty ind = Token_queue ([], ind)
let tq_enqueue (Token_queue (ts, ind)) tok = Token_queue (tok::ts, ind)
let tq_to_block (Token_queue (ts, ind)) = Pretty.block ind (List.rev ts)

type state = St of token_queue list ref (* intermediate result *)

let empty () : state = St (ref [tq_empty 0])

let st_insert (St qs) tok =
  match !qs with
  | [] -> failwith "Pretty_imp: Impossible: empty token_queue stack in state!"
  | tq::tqs ->
      qs := tq_enqueue tq tok :: tqs

let st_remove (St qs) =
  match !qs with
  | [] -> failwith ("Pretty_imp.close_block:" ^
                    "Impossible: empty token_queue stack in state!")
  | tq::tqs ->
      begin
        qs := tqs;
        tq
      end

let print_string st str = st_insert st (Pretty.string str)
let print_break st len = st_insert st (Pretty.break len)

let open_block (St st) indent =
  st := tq_empty indent :: !st

let close_block st =
  let tq = st_remove st in
  let tok = tq_to_block tq in
  st_insert st tok

let exec (St st) =
  match !st with
  | [Token_queue ([tok], _)] -> tok
  | [Token_queue ([], _)] -> failwith "Pretty_imp.exec: Empty token queue!"
  | _ :: _ -> failwith "Pretty_imp.exec: Unclosed blocks!"
  | _ -> failwith "Pretty_imp.exec: Impossible: empty token_stack queue!"

