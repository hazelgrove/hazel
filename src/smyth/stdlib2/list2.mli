val pure_bind : 'a list -> ('a -> 'b) -> 'b list

val pure : 'a -> 'a list

val bind : 'a list -> ('a -> 'b list) -> 'b list

val concat_map : ('a -> 'b list) -> 'a list -> 'b list

val maximum : 'a list -> 'a option

val repeat : int -> 'a -> 'a list

val sequence : 'a list list -> 'a list list

val filter_somes : 'a option list -> 'a list

val intersperse : 'a -> 'a list -> 'a list

(* Inclusive on both ends *)
val range : low:int -> high:int -> int list

val remove_first : 'a -> 'a list -> 'a list

(* Should only use on comparable types *)
val permutations : 'a list -> 'a list list

val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

val hd_opt : 'a list -> 'a option

val tl_opt : 'a list -> 'a list option

val uncons : 'a list -> ('a * 'a list) option

val is_empty : 'a list -> bool

val transpose : 'a list list -> 'a list list

val collapse_equal : 'a list -> 'a option

val index_left : 'a list -> (int * 'a) list

val index_right : 'a list -> ('a * int) list

val find_map : ('a -> 'b option) -> 'a list -> 'b option

val sum : int list -> int

val fsum : float list -> float

val average : float list -> float option

val take : int -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list

val cartesian_product : 'a list -> 'b list -> ('a * 'b) list

val count : ('a -> bool) -> 'a list -> int
