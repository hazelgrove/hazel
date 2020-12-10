type 'a gen = unit -> 'a

val constant : 'a -> 'a gen

val from : 'a * 'a list -> 'a gen

val pair : 'a gen -> 'b gen -> ('a * 'b) gen

val triple : 'a gen -> 'b gen -> 'c gen -> ('a * 'b * 'c) gen

val nat : int gen

val bool : bool gen

val nat_list : int list gen

val nested_nat_list : int list list gen

val bool_list : bool list gen

val nat_tree : int Tree2.t gen

val bool_tree : bool Tree2.t gen

val io_trial :
     n:int
  -> k:int
  -> ('a -> 'b)
  -> 'a gen
  -> 'a gen option
  -> ('a * 'b) list list
