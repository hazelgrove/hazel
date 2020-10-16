val identity : 'a -> 'a

val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
