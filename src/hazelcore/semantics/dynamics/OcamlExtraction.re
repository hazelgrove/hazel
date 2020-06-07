module Typ = {
    type ocaml_typ = string;

    let extract : HTyp.t -> ocaml_typ = ...
}

module Pat = {
    type ocaml_pat = string;

    let extract : DHPat.t -> ocaml_pat = ...
}

module Exp = {
    type ocaml_exp = string;

    type extract_result = option(ocaml_exp, HTyp.t);

    let extract : (Contexts.t, DHExp.t) -> extract_result = ...

    /* Theorem: If extract(ctx, d) = Some(ce, ty) and ctx |- d : ty then ce :_ocaml Typ.extract(ty) */
}