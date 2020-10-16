val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result

val pure_bind : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

val and_then : ('a -> ('b, 'e) result) -> ('a, 'e) result -> ('b, 'e) result

val guard : 'e -> bool -> (unit, 'e) result

val sequence : ('a, 'e) result list -> ('a list, 'e) result

val to_option : ('a, 'e) result -> 'a option

val with_default : 'a -> ('a, 'e) result -> 'a

val unwrap : ('a -> 'b) -> ('e -> 'b) -> ('a, 'e) result -> 'b

module Syntax : sig
  val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

  val ( let* ) :
    ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
end
