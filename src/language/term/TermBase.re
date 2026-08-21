let continue = x => x;
let stop = (_, x) => x;

/*
   This megafile contains the definitions of the expression data types in
   Hazel. They are all in one file because they are mutually recursive, and
   OCaml doesn't let us have mutually recursive files. Any definition that
   is not mutually recursive across the whole data structure should be
   defined in Any.re, Exp.re, Typ.re, Pat.re, TPat.re, etc...

   Each module has:

   - A type definition for the term

   - A map_term function that allows you to apply a function to every term in
     the data structure with the following type:

     map_term:
     (
       ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
       ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
       ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
       ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
       ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
       ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
       t
     ) =>
     t;

     Each argument to `map_term` specifies what should happen at each node in the
     data structure. Each function takes two arguments: a `continue` function that
     allows the map to continue on all the children nodes, and the current node
     itself. If you don't explicitly call the `continue` function, the map will
     not traverse the children nodes. If you don't provide a function for a
     specific kind of node, the map will simply continue at that node without
     any additional action.

   - A fast_equal function that compares two terms for equality, it performs
     structural equality except for the case of closures, where it just compares
     the id of the closure.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type any_t = Grammar.any_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type exp_t = Grammar.exp_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type exp_term = Grammar.exp_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type pat_t = Grammar.pat_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type pat_term = Grammar.pat_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_t = Grammar.typ_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_term = Grammar.typ_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type tpat_t = Grammar.tpat_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type tpat_term = Grammar.tpat_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type rul_t = Grammar.rul_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type rul_term = Grammar.rul_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type mod_t = Grammar.mod_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type mod_term = Grammar.mod_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type sig_t = Grammar.sig_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type sig_term = Grammar.sig_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type mpat_t = Grammar.mpat_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type mpat_term = Grammar.mpat_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type proof_t = Grammar.proof_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type proof_term = Grammar.proof_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type prul_t = Grammar.prul_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type prul_term = Grammar.prul_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_filter_kind_t = Grammar.stepper_filter_kind_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type type_hole = Grammar.type_hole(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type type_provenance = Grammar.type_provenance(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type filter = Grammar.filter(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type deferral_position_t = Grammar.deferral_position_t;

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x: any_t,
      ) => {
    let rec_call = (y: any_t): any_t =>
      switch (y) {
      | Exp(x) =>
        Exp(
          Exp.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_mod,
            ~f_sig,
            ~f_mpat,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | Pat(x) =>
        Pat(
          Pat.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_mod,
            ~f_sig,
            ~f_mpat,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | Typ(x) =>
        Typ(
          Typ.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_mod,
            ~f_sig,
            ~f_mpat,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | TPat(x) =>
        TPat(
          TPat.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_mod,
            ~f_sig,
            ~f_mpat,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | Rul(x) =>
        Rul(
          Rul.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_mod,
            ~f_sig,
            ~f_mpat,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      /* Drv terms have their own traversal machinery in DrvTermBase; the
         generic Any.map_term doesn't descend into them. */
      | Drv(x) => Drv(x)
      | Mod(x) =>
        Mod(
          Mod.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_mod,
            ~f_sig,
            ~f_mpat,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | Sig(x) =>
        Sig(
          Sig.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_mod,
            ~f_sig,
            ~f_mpat,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | MPat(x) =>
        MPat(
          MPat.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_mod,
            ~f_sig,
            ~f_mpat,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | Proof(x) =>
        Proof(
          Proof.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | PRul(x) =>
        PRul(
          PRul.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_any,
            ~f_proof,
            x,
          ),
        )
      | Any () => Any()
      };
    x |> f_any(rec_call);
  };
}
and Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position = deferral_position_t;
  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position = deferral_position_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let pat_map_term =
      Pat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let typ_map_term =
      Typ.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let tpat_map_term =
      TPat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let any_map_term =
      Any.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let mpat_map_term =
      MPat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let proof_map_term =
      Proof.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_any,
        ~f_proof,
      );
    let flt_map_term =
      StepperFilterKind.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Atom(_)
        | DrvQuote(_)
        | Constructor(_)
        | Label(_)
        | ExplicitNonlabel
        | Deferral(_)
        | Var(_)
        | LivelitName(_)
        | Undefined => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | DynamicErrorHole(e, err) => DynamicErrorHole(exp_map_term(e), err)
        | ListLit(ts) => ListLit(List.map(exp_map_term, ts))
        | Fun(p, e, t, f) =>
          Fun(
            pat_map_term(p),
            exp_map_term(e),
            Option.map(typ_map_term, t),
            f,
          )
        | TypFun(tp, e, f) => TypFun(tpat_map_term(tp), exp_map_term(e), f)
        | TupLabel(label, e) =>
          TupLabel(exp_map_term(label), exp_map_term(e))
        | Tuple(xs) => Tuple(List.map(exp_map_term, xs))
        | TupleExtension(e1, e2) =>
          TupleExtension(exp_map_term(e1), exp_map_term(e2))
        | Dot(e1, e2) => Dot(exp_map_term(e1), exp_map_term(e2))
        | Let(p, e1, e2) =>
          Let(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | Theorem(p, e1, pf, e2) =>
          Theorem(
            pat_map_term(p),
            exp_map_term(e1),
            proof_map_term(pf),
            exp_map_term(e2),
          )
        | ProofObject(t) => ProofObject(exp_map_term(t))
        | Forall(p, e) => Forall(pat_map_term(p), exp_map_term(e))
        | ForallWhere(p, g, e) =>
          ForallWhere(pat_map_term(p), exp_map_term(g), exp_map_term(e))
        | FunWhere(p, g, e) =>
          FunWhere(pat_map_term(p), exp_map_term(g), exp_map_term(e))
        | FixF(p, e, env) => FixF(pat_map_term(p), exp_map_term(e), env)
        | TyAlias(tp, t, e) =>
          TyAlias(tpat_map_term(tp), typ_map_term(t), exp_map_term(e))
        | Use(t, e) => Use(typ_map_term(t), exp_map_term(e))
        | Ap(op, e1, e2) => Ap(op, exp_map_term(e1), exp_map_term(e2))
        | TypAp(e, t) => TypAp(exp_map_term(e), typ_map_term(t))
        | DeferredAp(e, es) =>
          DeferredAp(exp_map_term(e), List.map(exp_map_term, es))
        | If(e1, e2, e3) =>
          If(exp_map_term(e1), exp_map_term(e2), exp_map_term(e3))
        | Seq(e1, e2) => Seq(exp_map_term(e1), exp_map_term(e2))
        | Test(e) => Test(exp_map_term(e))
        | HintedTest(e, h) => HintedTest(exp_map_term(e), exp_map_term(h))
        | Filter(f, e) => Filter(flt_map_term(f), exp_map_term(e))
        | Closure(env, e) => Closure(env, exp_map_term(e))
        | Parens(e) => Parens(exp_map_term(e))
        | Projector(data, e) => Projector(data, exp_map_term(e))
        | Cons(e1, e2) => Cons(exp_map_term(e1), exp_map_term(e2))
        | ListConcat(e1, e2) =>
          ListConcat(exp_map_term(e1), exp_map_term(e2))
        | UnOp(op, e) => UnOp(op, exp_map_term(e))
        | BinOp(op, e1, e2) =>
          BinOp(op, exp_map_term(e1), exp_map_term(e2))
        | BuiltinFun(str) => BuiltinFun(str)
        | Match(e, rls) =>
          Match(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        | Asc(e, t) => Asc(exp_map_term(e), typ_map_term(t))
        | Module(items) =>
          Module(
            List.map(
              Mod.map_term(
                ~f_exp,
                ~f_pat,
                ~f_typ,
                ~f_tpat,
                ~f_rul,
                ~f_mod,
                ~f_sig,
                ~f_mpat,
                ~f_any,
                ~f_proof,
              ),
              items,
            ),
          )
        | ModuleExp(mp, def, body) =>
          ModuleExp(
            mpat_map_term(mp),
            exp_map_term(def),
            exp_map_term(body),
          )
        },
    };
    x |> f_exp(rec_call);
  };
}
and Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term; // The second Typ.t field is only meaningful in dynamic patterns
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let pat_map_term =
      Pat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let typ_map_term =
      Typ.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let any_map_term =
      Any.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Wild
        | Atom(_)
        | Constructor(_)
        | Label(_)
        | Var(_)
        | ExplicitNonlabel => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | ListLit(ts) => ListLit(List.map(pat_map_term, ts))
        | Ap(e1, e2) => Ap(pat_map_term(e1), pat_map_term(e2))
        | Cons(e1, e2) => Cons(pat_map_term(e1), pat_map_term(e2))
        | Tuple(xs) => Tuple(List.map(pat_map_term, xs))
        | TupLabel(label, e) =>
          TupLabel(pat_map_term(label), pat_map_term(e))
        | Parens(e) => Parens(pat_map_term(e))
        | Projector(data, p) => Projector(data, pat_map_term(p))
        | Asc(e, t) => Asc(pat_map_term(e), typ_map_term(t))
        },
    };
    x |> f_pat(rec_call);
  };
}
and Typ: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  type sum_map = ConstructorMap.t(t);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  type sum_map = ConstructorMap.t(t);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let typ_map_term =
      Typ.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let any_map_term =
      Any.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let tpat_map_term =
      TPat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let exp_map_term =
      Exp.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Unknown(Hole(EmptyHole))
        | Unknown(Hole(Invalid(_)))
        | Unknown(SynSwitch)
        | Unknown(Internal)
        | Atom(_)
        | DrvQuoteTy(_)
        | Label(_)
        | ExplicitNonlabel
        | Var(_) => term
        | List(t) => List(typ_map_term(t))
        | Unknown(Hole(MultiHole(things))) =>
          Unknown(Hole(MultiHole(List.map(any_map_term, things))))
        | Prod(xs) => Prod(List.map(typ_map_term, xs))
        | TupLabel(label, e) =>
          TupLabel(typ_map_term(label), typ_map_term(e))
        | Parens(e) => Parens(typ_map_term(e))
        | Projector(data, t) => Projector(data, typ_map_term(t))
        | Arrow(t1, t2) => Arrow(typ_map_term(t1), typ_map_term(t2))
        | Sum(variants) =>
          Sum(
            List.map(
              fun
              | ConstructorMap.Variant(c, ann, t) => {
                  /* We turn a variant back into its original term (see MakeTerm.parse_sum_term)
                   * in order to map over it. The main reason this was implemented is so that
                   * id renaming passes work. */
                  switch (
                    typ_map_term({
                      term: Var(c),
                      annotation: IdTagged.IdTag.mk_internal(ann.ids),
                    })
                  ) {
                  | {term: Var(c), annotation: {ids, _}} =>
                    ConstructorMap.Variant(
                      c,
                      {
                        ids,
                        secondary: ann.secondary,
                      },
                      Option.map(typ_map_term, t),
                    )
                  | t => BadEntry(typ_map_term(t))
                  };
                }
              | ConstructorMap.BadEntry(t) =>
                ConstructorMap.BadEntry(typ_map_term(t)),
              variants,
            ),
          )
        | ProdProjection(t1, t2) =>
          ProdProjection(typ_map_term(t1), typ_map_term(t2))
        | ProdExtension(t1, t2) =>
          ProdExtension(typ_map_term(t1), typ_map_term(t2))
        | Rec(tp, t) => Rec(tpat_map_term(tp), typ_map_term(t))
        | Poly(tp, t) => Poly(tpat_map_term(tp), typ_map_term(t))
        | ProofOf(e) => ProofOf(exp_map_term(e))
        | Sig(items) =>
          Sig(
            List.map(
              Sig.map_term(
                ~f_exp,
                ~f_pat,
                ~f_typ,
                ~f_tpat,
                ~f_rul,
                ~f_mod,
                ~f_sig,
                ~f_mpat,
                ~f_any,
                ~f_proof,
              ),
              items,
            ),
          )
        },
    };
    x |> f_typ(rec_call);
  };
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;

  let tyvar_of_utpat: t => option(string);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let any_map_term =
      Any.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        },
    };
    x |> f_tpat(rec_call);
  };

  let tyvar_of_utpat = ({term, _}: t) =>
    switch (term) {
    | Var(x) => Some(x)
    | _ => None
    };
}
and Rul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = rul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = rul_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = rul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = rul_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let pat_map_term =
      Pat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let any_map_term =
      Any.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | Rules(e, rls) =>
          Rules(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        },
    };
    x |> f_rul(rec_call);
  };
}
and PRul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = prul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = prul_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = prul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = prul_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_proof);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_proof);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_proof);
    let proof_map_term =
      Proof.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_any,
        ~f_proof,
      );
    let rec_call = ({term, _} as pr: t) => {
      ...pr,
      term:
        switch (term) {
        | Invalid(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | ProofRules(e, rls) =>
          ProofRules(
            exp_map_term(e),
            List.map(
              ((p, body)) => (pat_map_term(p), proof_map_term(body)),
              rls,
            ),
          )
        },
    };
    rec_call(x);
  };
}
and Mod: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = mod_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = mod_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = mod_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = mod_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let pat_map_term =
      Pat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let typ_map_term =
      Typ.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let tpat_map_term =
      TPat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let any_map_term =
      Any.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let mpat_map_term =
      MPat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let rec_call = ({term, _} as m: t) => {
      ...m,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | ModLet(p, e) => ModLet(pat_map_term(p), exp_map_term(e))
        | ModType(tp, t) => ModType(tpat_map_term(tp), typ_map_term(t))
        | ModExp(e) => ModExp(exp_map_term(e))
        | ModuleMod(mp, e) => ModuleMod(mpat_map_term(mp), exp_map_term(e))
        },
    };
    x |> f_mod(rec_call);
  };
}
and Sig: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = sig_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = sig_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = sig_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = sig_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let pat_map_term =
      Pat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let typ_map_term =
      Typ.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let tpat_map_term =
      TPat.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let any_map_term =
      Any.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let rec_call = ({term, _} as s: t) => {
      ...s,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | SigLet(p) => SigLet(pat_map_term(p))
        | SigType(tp, t) => SigType(tpat_map_term(tp), typ_map_term(t))
        },
    };
    x |> f_sig(rec_call);
  };
}

and MPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = mpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = mpat_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = mpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = mpat_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let _ = (f_exp, f_pat, f_typ, f_tpat, f_rul);
    let any_map_term =
      Any.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let typ_map_term =
      Typ.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    let rec rec_call = ({term, _} as mp: t) => {
      ...mp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | Asc(inner, ty) => Asc(f_mpat(rec_call, inner), typ_map_term(ty))
        },
    };
    x |> f_mpat(rec_call);
  };
}
and Proof: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = proof_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = proof_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = proof_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = proof_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        ~f_proof=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_proof);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_proof);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_proof);
    let rec proof_map_term = ({term, _} as p: t) => {
      ...p,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | Seq(p1, p2) => Seq(proof_map_term(p1), proof_map_term(p2))
        | AxiomStep({at_idx, at_exp, direction, equality, instantiation}) =>
          AxiomStep({
            at_idx: exp_map_term(at_idx),
            at_exp: exp_map_term(at_exp),
            direction,
            equality: exp_map_term(equality),
            instantiation:
              Option.map(
                ((v, i)) => (exp_map_term(v), exp_map_term(i)),
                instantiation,
              ),
          })
        | AlgebriteStep({at_idx, at_exp, with_exp}) =>
          AlgebriteStep({
            at_idx: exp_map_term(at_idx),
            at_exp: exp_map_term(at_exp),
            with_exp: exp_map_term(with_exp),
          })
        | EvalStep({at_idx, at_exp}) =>
          EvalStep({
            at_idx: exp_map_term(at_idx),
            at_exp: exp_map_term(at_exp),
          })
        | Induction(e, cases) =>
          Induction(
            exp_map_term(e),
            List.map(
              ((pt, body)) => (pat_map_term(pt), proof_map_term(body)),
              cases,
            ),
          )
        | Forall(x, body) => Forall(pat_map_term(x), proof_map_term(body))
        | Assume(e, body) => Assume(exp_map_term(e), proof_map_term(body))
        | Generalize(e, body) =>
          Generalize(exp_map_term(e), proof_map_term(body))
        | Revert(e, inst, body) =>
          Revert(
            exp_map_term(e),
            Option.map(
              ((v, i)) => (exp_map_term(v), exp_map_term(i)),
              inst,
            ),
            proof_map_term(body),
          )
        | Contradiction(e, inst) =>
          Contradiction(
            exp_map_term(e),
            Option.map(
              ((v, i)) => (exp_map_term(v), exp_map_term(i)),
              inst,
            ),
          )
        | Have(e, sub, body) =>
          Have(exp_map_term(e), proof_map_term(sub), proof_map_term(body))
        },
    };
    x |> f_proof(proof_map_term);
  };
}
and StepperFilterKind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = stepper_filter_kind_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_mod: (Mod.t => Mod.t, Mod.t) => Mod.t=?,
      ~f_sig: (Sig.t => Sig.t, Sig.t) => Sig.t=?,
      ~f_mpat: (MPat.t => MPat.t, MPat.t) => MPat.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_proof: (Proof.t => Proof.t, Proof.t) => Proof.t=?,
      t
    ) =>
    t;

  let map: (Exp.t => Exp.t, t) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = stepper_filter_kind_t;

  let map = (mapper, filter: t): t => {
    switch (filter) {
    | Filter({act, pat}) =>
      Filter({
        act,
        pat: mapper(pat),
      })
    | Residue(idx, act) => Residue(idx, act)
    };
  };

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_mod=continue,
        ~f_sig=continue,
        ~f_mpat=continue,
        ~f_any=continue,
        ~f_proof=continue,
      ) => {
    let exp_map_term =
      Exp.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_mod,
        ~f_sig,
        ~f_mpat,
        ~f_any,
        ~f_proof,
      );
    (
      fun
      | Filter({pat: e, act}) =>
        Filter({
          pat: exp_map_term(e),
          act,
        })
      | Residue(i, a) => Residue(i, a):
        t => t
    );
  };
};
