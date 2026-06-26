(* Reason can't reference OCaml's (++) operator (its own ++ maps to ^),
   so notty's attr-combining operator is re-exported here as a plain
   function for NottyIO.re. *)
let cat = Notty.A.( ++ )
