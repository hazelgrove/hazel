exception NoLayout

(* Returns a string visual representation of the given Layout
   Should appear similar to how it does in the hazel editor*)
val string_of_layout : UHAnnot.t Pretty.Layout.t -> string

(* Returns a string visual representation of the given UHExp
   Should appear similar to how it does in the hazel editor*)
val print_exp : UHExp.t -> string
