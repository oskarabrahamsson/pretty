type token =
    | Block of token list * int * int
    | String of string
    | Break of int

let rec breakdist after = function
  | [] -> after
  | Block (_, _, len) :: es -> len + breakdist after es
  | String s :: es -> String.length s + breakdist after es
  | Break _ :: _ -> 0

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
        let out1 = printing indent len bes in
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
    | [] -> App_list.empty in
  printing margin 0 [tok]

let string s = String s

let break l = Break l

let block =
  let length =
    function
    | Block (_, _, len) -> len
    | String s -> String.length s
    | Break len -> len in
  let sum = List.fold_left (fun s t -> s + length t) 0 in
  fun indent toks -> Block (toks, indent, sum toks)

