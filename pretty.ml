(* This is an implementation of the pretty printer from L.C. Paulson's
   ML for the working programmer. The printer has been extended to support
   different kinds of 'blocks' with behavior that correspond to those supported
   by the OCaml Format library:

   - Within a (horizontal) hblock, break hints never split the line,
   - within a (vertical) vblock, break hints always split the line,
   - within a (horizontal/vertical) hvblock, break hints split the line when
     the block does not fit on the current line,
   - within a compacting block (the standard block), a break hint never splits
     the line unless there is no more space on the current line.

   (I think the blocks are slightly broken at the moment.)
 *)

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
  val break : int -> int -> token
  val block : int -> token list -> token
  val hvblock : int -> token list -> token
  val hblock : token list -> token
  val vblock : int -> token list -> token
  val print : int -> token -> string App_list.app_list
end (* sig *)

module Pretty_core : PRETTY_CORE = struct

  type block_type =
    | Horizontal
    | Vertical
    | Horizontal_vertical
    | Compacting
  ;;

  type token =
      | Block of block_type * token list * int * int
      | String of string
      | Break of int * int
  ;;

  let rec breakdist after = function
    | [] -> after
    | t :: es ->
        match t with
        | Block (_, _, _, len) -> len + breakdist after es
        | String s -> String.length s + breakdist after es
        | Break _ -> 0
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
    let rec printing block_type blockspace after toks =
      match toks with
      | Block (typ, bes, indent, len) :: es ->
          (* If a Horizontal_vertical block fits on the line, print it as
             if it were Horizontal, otherwise print it as if it were
             vertical. *)
          let typ' = match typ with
                     | Horizontal_vertical ->
                         if len <= !space then Horizontal else Vertical
                     | _ -> typ in
          let out1 = printing typ' (!space - indent)
                                  (breakdist after es) bes in
          let out2 = printing block_type blockspace after es in
          App_list.append out1 out2
      | String s :: es ->
          let _ = space := !space - String.length s in
          let out1 = App_list.list [s] in
          let out2 = printing block_type blockspace after es in
          App_list.append out1 out2
      | Break (len, offset) :: es -> (* Depends on the block type: *)
          begin
            match block_type with
            | Horizontal -> (* Never split the line at this level *)
                let out1 = blanks len in
                let out2 = printing block_type blockspace after es in
                App_list.append out1 out2
            | Vertical -> (* Always split the line at this level *)
                let out1 = newline () in
                let out2 = blanks offset in
                let out3 = blanks (margin - blockspace) in
                let out4 = printing block_type blockspace after es in
                App_list.append out1
                  (App_list.append out2
                    (App_list.append out3 out4))
            | Horizontal_vertical -> (* should not happen *)
                failwith "Pretty_core.print: Horizontal_vertical box"
            | Compacting ->
                if len + breakdist after es <= !space then
                  let out1 = blanks len in
                  let out2 = printing block_type blockspace after es in
                  App_list.append out1 out2
                else
                  let out1 = newline () in
                  let out2 = blanks offset in
                  let out3 = blanks (margin - blockspace) in
                  let out4 = printing block_type blockspace after es in
                  App_list.append out1
                    (App_list.append out2
                      (App_list.append out3 out4))
          end
      | [] -> App_list.empty in
    printing Compacting margin 0 [tok]
  ;;

  let string s = String s
  ;;

  let break l i = Break (l, i)
  ;;

  let space = Break (1, 0)
  ;;

  let mk_block typ =
    let length =
      function
      | Block (_, _, _, len) -> len
      | String s -> String.length s
      | Break (len, _) -> len in
    let sum = List.fold_left (fun s t -> s + length t) 0 in
    fun indent toks -> Block (typ, toks, indent, sum toks)
  ;;

  let block = mk_block Compacting
  ;;

  let hblock = mk_block Horizontal 0
  ;;

  let vblock = mk_block Vertical
  ;;

  let hvblock = mk_block Horizontal_vertical
  ;;

end (* struct *)

module type PRETTY_IMP = sig
  type state
  val empty : unit -> state
  val open_block : state -> int -> unit
  val open_hvblock : state -> int -> unit
  val open_hblock : state -> unit
  val open_vblock : state -> int -> unit
  val close_block : state -> unit
  val print_string : state -> string -> unit
  val print_break : state -> int -> int -> unit
  val print_space : state -> unit
  (* Badly chosen name: means 'close all blocks and print a newline character'.
   *)
  val print_newline : state -> unit
  val to_token : state -> Pretty_core.token
end (* sig *)

module Pretty_imp : PRETTY_IMP = struct

  type block_type =
    | H_block
    | V_block
    | Hv_block
    | C_block
  ;;

  type token_queue =
    Token_queue of Pretty_core.token list *   (* stack of tokens *)
                   int                    *   (* block indent    *)
                   block_type                 (* block type      *)
  ;;

  let tq_empty ind typ = Token_queue ([], ind, typ)
  ;;

  let tq_enqueue (Token_queue (ts, ind, typ)) tok =
    Token_queue (tok::ts, ind, typ)
  ;;

  let tq_to_block (Token_queue (ts, ind, typ)) =
    match typ with
    | H_block -> Pretty_core.hblock (List.rev ts)
    | Hv_block -> Pretty_core.hvblock ind (List.rev ts)
    | V_block -> Pretty_core.vblock ind (List.rev ts)
    | C_block -> Pretty_core.block ind (List.rev ts)
  ;;

  type state =
    St of token_queue list ref (* intermediate result *)
  ;;

  let empty () : state = St (ref [tq_empty 0 C_block])
  ;;

  let st_insert (St qs) tok =
    match !qs with
    | [] ->
        qs := [tq_enqueue (tq_empty 0 C_block) tok]
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

  let print_break st l i = st_insert st (Pretty_core.break l i)
  ;;

  let print_space st = st_insert st Pretty_core.space
  ;;

  let new_block (St st) indent typ =
    st := tq_empty indent typ :: !st

  let open_block st indent = new_block st indent C_block
  ;;

  let open_hblock st = new_block st 0 H_block
  ;;

  let open_hvblock st indent = new_block st indent Hv_block
  ;;

  let open_vblock st indent = new_block st indent V_block
  ;;

  let close_block st =
    let tq = st_remove st in
    let tok = tq_to_block tq in
    st_insert st tok
  ;;

  let rec st_flush st =
    try close_block st;
        st_flush st
    with Failure _ -> ()
  ;;

  let print_newline st =
    st_flush st;
    st_insert st (Pretty_core.string "\n")
  ;;

  let to_token (St st) =
    match !st with
    | [Token_queue ([tok], _, _)] -> tok
    | [Token_queue ([], _, _)] -> failwith "Pretty_imp.exec: Empty token queue!"
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
  val open_hblock : state -> unit
  val open_vblock : state -> int -> unit
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

  let margin = ref 60;;

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

