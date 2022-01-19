module type APP_LIST = sig
  type 'a app_list

  val append : 'a app_list -> 'a app_list -> 'a app_list
  val list : 'a list -> 'a app_list
  val empty : 'a app_list

  val map : ('a -> 'b) -> 'a app_list -> 'b app_list
  val iter : ('a -> unit) -> 'a app_list -> unit
  val to_list : 'a app_list -> 'a list
  val concat : string app_list -> string
end (* sig *)

module App_list : APP_LIST = struct
  type 'a app_list =
    | Append of 'a app_list * 'a app_list
    | List of 'a list
    | Empty
  ;;

  let append l r =
    match l, r with
    | _, Empty -> l
    | Empty, _ -> r
    | _, _ -> Append (l, r)
  ;;

  let list = function
    | [] -> Empty
    | xs -> List xs
  ;;

  let empty = Empty
  ;;

  let rec map f l =
    match l with
    | Empty -> Empty
    | List xs -> List (List.map f xs)
    | Append (l, r) -> Append (map f l, map f r)
  ;;

  let rec iter f l =
    match l with
    | Empty -> ()
    | List xs -> List.iter f xs
    | Append (l, r) ->
        begin
          iter f l;
          iter f r
        end
  ;;

  let rec to_list l =
    match l with
    | Empty -> []
    | List xs -> xs
    | Append (l, r) -> to_list l @ to_list r
  ;;

  let rec concat_aux sofar l =
    match l with
    | Empty -> sofar
    | List xs -> String.concat "" xs :: sofar
    | Append (l, r) -> concat_aux (concat_aux sofar l) r
  ;;

  let concat l = String.concat "" (concat_aux [] l)
  ;;

end (* struct *)

module type PRETTY_CORE = sig
  type token
  val string : string -> token
  val space : token
  val newline : token
  val break : int -> token
  val block : int -> token list -> token
  val print : int -> token -> string App_list.app_list
end (* sig *)

module Pretty_core : PRETTY_CORE = struct

  type token =
      | Block of token list * int * int
      | String of string
      | Break of int
      | Newline
  ;;

  let rec breakdist after = function
    | [] -> after
    | t :: es ->
        match t with
        | Block (_, _, len) -> len + breakdist after es
        | String s -> String.length s + breakdist after es
        | Break _ | Newline -> 0
  ;;

  let print margin tok =
    let space = ref margin in
    let blanks n =
      begin
        space := !space - n;
        App_list.list (List.init n (fun _ -> " "))
      end in
    let newline () =
      begin
        space := margin;
        App_list.list ["\n"]
      end in
    let rec printing blockspace after toks =
      match toks with
      | Block (bes, indent, len) :: es ->
          let out1 = printing (!space - indent) (breakdist after es) bes in
          let out2 = printing blockspace after es in
          App_list.append out1 out2
      | String s :: es ->
          let _ = space := !space - String.length s in
          let out1 = App_list.list [s] in
          let out2 = printing blockspace after es in
          App_list.append out1 out2
      | Break len :: es ->
          if len + breakdist after es <= !space then
            let out1 = blanks len in
            let out2 = printing blockspace after es in
            App_list.append out1 out2
          else
            let out1 = newline () in
            let out2 = blanks (margin - blockspace) in
            let out3 = printing blockspace after es in
            App_list.append out1 (App_list.append out2 out3)
      | Newline :: es ->
          let out1 = newline () in
          let out2 = blanks (margin - blockspace) in
          let out3 = printing blockspace after es in
          App_list.append out1 (App_list.append out2 out3)
      | [] -> App_list.empty in
    printing margin 0 [tok]
  ;;

  let string s = String s
  ;;

  let break l = Break l
  ;;

  let newline = Newline
  ;;

  let space = String " "
  ;;

  let block =
    let length =
      function
      | Block (_, _, len) -> len
      | String s -> String.length s
      | Break len -> len in
    let sum = List.fold_left (fun s t -> s + length t) 0 in
    fun indent toks -> Block (toks, indent, sum toks)
  ;;
end (* struct *)

module type PRETTY_IMP = sig
  type state
  val empty : unit -> state
  val open_block : state -> int -> unit
  val open_hvblock : state -> int -> unit
  val open_hblock : state -> int -> unit
  val close_block : state -> unit
  val print_string : state -> string -> unit
  val print_break : state -> int -> int -> unit
  val print_space : state -> unit
  val print_newline : state -> unit
  val to_token : state -> Pretty_core.token
end (* sig *)

module Pretty_imp : PRETTY_IMP = struct
  type token_queue =
    Token_queue of Pretty_core.token list *   (* stack of tokens *)
                   int                        (* block indent    *)
  ;;

  let tq_empty ind = Token_queue ([], ind)
  ;;

  let tq_enqueue (Token_queue (ts, ind)) tok =
    Token_queue (tok::ts, ind)
  ;;

  let tq_to_block (Token_queue (ts, ind)) =
    Pretty_core.block ind (List.rev ts)
  ;;

  type state =
    St of token_queue list ref (* intermediate result *)
  ;;

  let empty () : state = St (ref [tq_empty 0])
  ;;

  let st_insert (St qs) tok =
    match !qs with
    | [] ->
        qs := [tq_enqueue (tq_empty 0) tok]
    | tq::tqs ->
        qs := tq_enqueue tq tok :: tqs
  ;;

  let st_remove (St qs) =
    match !qs with
    | [] -> failwith ("Pretty_imp.close_block:" ^
                      "Impossible: empty token_queue stack in state!")
    | tq::tqs ->
        begin
          qs := tqs;
          tq
        end
  ;;

  let print_string st str = st_insert st (Pretty_core.string str)
  ;;

  let print_break st len _ = st_insert st (Pretty_core.break len)
  ;;

  let print_newline st = st_insert st Pretty_core.newline
  ;;

  let print_space st = st_insert st Pretty_core.space
  ;;

  let open_block (St st) indent =
    st := tq_empty indent :: !st
  ;;

  let open_hblock = open_block
  ;;

  let open_hvblock = open_block
  ;;

  let close_block st =
    let tq = st_remove st in
    let tok = tq_to_block tq in
    st_insert st tok
  ;;

  let to_token (St st) =
    match !st with
    | [Token_queue ([tok], _)] -> tok
    | [Token_queue ([], _)] -> failwith "Pretty_imp.exec: Empty token queue!"
    | _ :: _ -> failwith "Pretty_imp.exec: Unclosed blocks!"
    | _ -> failwith "Pretty_imp.exec: Impossible: empty token_stack queue!"
  ;;

end (* struct *)

module type PRETTY = sig
  type state

  val margin : int ref

  val empty : unit -> state

  val open_block : state -> int -> unit
  val open_hvblock : state -> int -> unit
  val open_hblock : state -> int -> unit
  val close_block : state -> unit

  val print_string : state -> string -> unit
  val print_break : state -> int -> int -> unit
  val print_newline : state -> unit
  val print_space : state -> unit

  val print_stdout : (state -> 'a -> unit) -> 'a -> unit
  val print_with_fmt : (state -> 'a -> unit) -> Format.formatter -> 'a -> unit
  val print_to_string : (state -> 'a -> unit) -> 'a -> string
end (* sig *)

module Pretty : PRETTY = struct

  include Pretty_imp;;

  let margin = ref 78;;

  let print_stdout printer data =
    let st = empty () in
    printer st data;
    let tok = to_token st in
    let apps = Pretty_core.print (!margin) tok in
    App_list.iter (output_string stdout) apps;;

  let print_with_fmt printer fmt data =
    let st = empty () in
    printer st data;
    let tok = to_token st in
    let apps = Pretty_core.print (!margin) tok in
    App_list.iter (Format.pp_print_string fmt) apps;;

  let print_to_string printer data =
    let st = empty () in
    printer st data;
    let tok = to_token st in
    let apps = Pretty_core.print (!margin) tok in
    App_list.concat apps;;

end (* struct *)

