type 'a app_list =
  | Append of 'a app_list * 'a app_list
  | List of 'a list
  | Empty

let append l r =
  match l, r with
  | _, Empty -> l
  | Empty, _ -> r
  | _, _ -> Append (l, r)

let list = function
  | [] -> Empty
  | xs -> List xs

let empty = Empty

let rec map f l =
  match l with
  | Empty -> Empty
  | List xs -> List (List.map f xs)
  | Append (l, r) -> Append (map f l, map f r)

let rec iter f l =
  match l with
  | Empty -> ()
  | List xs -> List.iter f xs
  | Append (l, r) ->
      begin
        iter f l;
        iter f r
      end

let rec to_list l =
  match l with
  | Empty -> []
  | List xs -> xs
  | Append (l, r) -> to_list l @ to_list r

