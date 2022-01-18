(** An [app_list] is a binary tree with list leaves that can be mapped over
    or app'd over in linear time. *)
type 'a app_list

(** [append] appends two app_lists. *)
val append : 'a app_list -> 'a app_list -> 'a app_list

(** [list] creates an app_list from a single list *)
val list : 'a list -> 'a app_list

(** [empty] is the empty app_list *)
val empty : 'a app_list

val map : ('a -> 'b) -> 'a app_list -> 'b app_list
val iter : ('a -> unit) -> 'a app_list -> unit
val to_list : 'a app_list -> 'a list
