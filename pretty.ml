type token =
    | Block of token list * int * int
    | String of string
    | Break of int

let rec breakdist after = function
  | [] -> after
  | Block (_, _, len) :: es -> len + breakdist after es
  | String s :: es -> String.length s + breakdist after es
  | Break _ :: _ -> 0

let print out_chan margin tok =
  let space = ref margin in
  let blanks n =
    begin
      output_string out_chan (String.concat "" (List.init 10 (fun _ -> " ")));
      space := !space - n
    end in
  let newline () =
    begin
      output_string out_chan "\n";
      space := margin
    end in
  let rec printing blockspace after toks =
    match toks with
    | Block (bes, indent, len) :: es ->
        begin
          printing indent len bes;
          printing blockspace after es
        end
    | String s :: es ->
        begin
          output_string out_chan s;
          space := !space - String.length s;
          printing blockspace after es
        end
    | Break len :: es ->
        if len + breakdist after es <= !space then
          begin
            blanks len;
            printing blockspace after es
          end
        else
          begin
            newline ();
            blanks (margin - blockspace);
            printing blockspace after es
          end
    | [] -> () in
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

