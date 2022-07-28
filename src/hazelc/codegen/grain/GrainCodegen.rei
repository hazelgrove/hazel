[@deriving sexp]
type opts = {print_final_expr: bool};

/*
   Generate Grain code from Anf.
 */
let codegen: (~opts: opts, Mir.Anf.prog) => Grain.modl;
