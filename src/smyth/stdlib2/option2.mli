val map : ('a -> 'b) -> 'a option -> 'b option

val pure_bind : 'a option -> ('a -> 'b) -> 'b option

val bind : 'a option -> ('a -> 'b option) -> 'b option

val and_then : ('a -> 'b option) -> 'a option -> 'b option

val guard : bool -> unit option

val sequence : 'a option list -> 'a list option

val with_default : 'a -> 'a option -> 'a

val sequence_fst : 'a option * 'b -> ('a * 'b) option

val sequence_snd : 'a * 'b option -> ('a * 'b) option

val filter : ('a -> bool) -> 'a option -> 'a option

module Syntax : sig
  val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option

  val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
end
