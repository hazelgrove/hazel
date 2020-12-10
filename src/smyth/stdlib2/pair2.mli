val pair : 'a -> 'b -> 'a * 'b

val map_fst : ('a -> 'c) -> 'a * 'b -> 'c * 'b

val map_snd : ('b -> 'c) -> 'a * 'b -> 'a * 'c

val lift_snd_result : 'a * ('b, 'e) result -> ('a * 'b, 'e) result
