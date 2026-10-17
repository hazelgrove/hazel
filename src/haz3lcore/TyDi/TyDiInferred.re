/* Typing `:` makes an empty type annotation. While the caret is in it, the
   completion buffer shows the type the annotated pattern or expression gets
   without the annotation, abbreviated past max_columns; Tab writes the full
   type. The empty annotation itself means `?`, as always. */
open Util;
open OptUtil.Syntax;
open Language;

let max_columns = 28;

let is_hole = (hole: Id.t, ty: Typ.t): bool =>
  Typ.rep_id(ty) == hole
  && (
    switch (Typ.term_of(ty)) {
    | Unknown(Hole(EmptyHole)) => true
    | _ => false
    }
  );

type annotated =
  | Pat(Pat.t)
  | Exp(Exp.t);

/* The program with the annotation whose type is `hole` removed, and what
   that annotation was on. */
let without_annotation =
    (hole: Id.t, term: Exp.t): option((Exp.t, annotated)) => {
  let found = ref(None);
  let stripped =
    Exp.map_term(
      ~f_pat=
        (continue, p: Pat.t) =>
          switch (Pat.term_of(p)) {
          | Asc(inner, ty) when is_hole(hole, ty) =>
            found := Some(Pat(inner));
            continue(inner);
          | _ => continue(p)
          },
      ~f_exp=
        (continue, e: Exp.t) =>
          switch (Exp.term_of(e)) {
          | Asc(inner, ty) when is_hole(hole, ty) =>
            found := Some(Exp(inner));
            continue(inner);
          | _ => continue(e)
          },
      term,
    );
  let+ annotated = found^;
  (stripped, annotated);
};

let rec strip_polys = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Poly(_, body) => strip_polys(body)
  | _ => ty
  };

/* Written type variables are bound by their definition, so a let's scheme
   is suggested without its `poly`s. */
let inferred_type = (m: Statics.Map.t, annotated): option(Typ.t) =>
  switch (annotated) {
  | Exp(e) =>
    let+ info = Statics.Map.lookup_exp(Exp.rep_id(e), m);
    info.ty;
  | Pat(p) =>
    let+ info = Statics.Map.lookup_pat(Pat.rep_id(p), m);
    let ty = strip_polys(info.ty);
    switch (Pat.term_of(p), Typ.term_of(ty)) {
    /* `let f(x) : ` annotates the result */
    | (Ap(_), Arrow(_, ret)) => ret
    | _ => ty
    };
  };

let columns = Unicode.Width.columns_of_string;

/* `…, …` reads as one gap */
let merge_ellipses = (s: string): string => {
  let run = Abbreviate.flat_ellipses ++ ", " ++ Abbreviate.flat_ellipses;
  let n = String.length(run);
  let rec go = (i, acc) =>
    if (i >= String.length(s)) {
      acc;
    } else if (i + n <= String.length(s) && String.sub(s, i, n) == run) {
      /* drop the `…, ` and look again at the second `…` */
      go(
        i + n - String.length(Abbreviate.flat_ellipses),
        acc,
      );
    } else {
      go(i + 1, acc ++ String.make(1, s.[i]));
    };
  go(0, "");
};

/* Abbreviate marks what it drops as Unknown(Internal); show those as `…`
   and every real unknown as `?`. Its budget undercounts types, so shrink
   the budget until the text fits. */
let abbreviated = (budget: int, ty: Typ.t): string => {
  let unknowns_as_holes =
    Typ.map_term(
      ~f_typ=
        (continue, t: Typ.t) =>
          switch (Typ.term_of(t)) {
          | Unknown(_) => {
              ...t,
              term: Unknown(Hole(EmptyHole)),
            }
          | _ => continue(t)
          },
      ty,
    );
  let elided_as_ellipses =
    Typ.map_term(~f_typ=(continue, t: Typ.t) =>
      switch (Typ.term_of(t)) {
      | Unknown(Internal) => {
          ...t,
          term: Var(Abbreviate.flat_ellipses),
        }
      | _ => continue(t)
      }
    );
  let rec go = available => {
    let s =
      Abbreviate.abbreviate_typ(~available, unknowns_as_holes)
      |> elided_as_ellipses
      |> BuiltinsPrinter.typ_to_string
      |> merge_ellipses;
    columns(s) <= budget || available <= 0 ? s : go(available - 1);
  };
  go(budget);
};

/* A function's result is kept over its arguments */
let rec shown = (budget: int, ty: Typ.t): string => {
  let full = BuiltinsPrinter.typ_to_string(ty);
  switch (Typ.term_of(ty)) {
  | _ when columns(full) <= budget => full
  | Arrow(arg, ret) when budget > 6 =>
    let ret = shown(budget - 5, ret);
    let parens =
      switch (Typ.term_of(arg)) {
      | Arrow(_) => true
      | _ => false
      };
    let arg =
      shown(budget - columns(ret) - 4 - (parens ? 2 : 0), arg)
      |> (s => parens && s != Abbreviate.flat_ellipses ? "(" ++ s ++ ")" : s);
    arg ++ " -> " ++ ret;
  | _ => abbreviated(budget, ty)
  };
};

let set_buffer =
    (~settings: CoreSettings.t, statics: CachedStatics.t, z: Zipper.t)
    : option(Zipper.t) => {
  let* hole = Zipper.hole_beside_caret(z);
  let* (stripped, annotated) = without_annotation(hole, statics.term);
  let* root =
    Statics.Map.lookup_exp(Exp.rep_id(statics.term), statics.info_map);
  let (m, _) = Statics.mk(~ana=root.ana, settings, root.ctx, stripped);
  let* ty = inferred_type(m, annotated);
  let* () =
    switch (Typ.term_of(ty)) {
    | Unknown(_) => None
    | _ => Some()
    };
  Some(
    Zipper.set_buffer(
      z,
      ~mode=Abbreviated(BuiltinsPrinter.typ_to_string(ty)),
      ~content=TyDi.mk_unparsed_buffer(shown(max_columns, ty)),
    ),
  );
};
