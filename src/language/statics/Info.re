open Util;
open OptUtil.Syntax;

/* INFO.re — cursor statics bundle per AST node.

   Statics supplies elab_syn_ty, marks, optional warnings, and inspector message
   payloads; Info stores/queries the resulting per-node data.

   elab_syn_ty is the synthesized type of the elaborated expression (before
   hole fixing). Note: it can be influenced by the analytic type in some cases
   (e.g. via expectation/meet interaction), so it is not purely synthetic.
   Marks on Info are statics errors; use marks_of for problem display.

   See Message.re for inspector payload types; statics computes marks/messages.
   */

/* ==================================== TYPES ======================================== */

/* The ids of a term's ancestors in the AST */
[@deriving (show({with_path: false}), sexp, yojson)]
type ancestors = list(Id.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type label_inference('a) =
  | SingletonLabelInference({
      label: LabeledTuple.label,
      pre_labeled_info: 'a,
    })
  | MultiLabelInference({
      reordered: bool,
      introduced_labels: list(LabeledTuple.label),
    });

[@deriving (show({with_path: false}), sexp, yojson)]
type slice_child_mode =
  | SliceKeep
  | SliceOmit
  | SliceSource
  | SliceTrack
  | SliceMap
  | SlicePrune
  | SliceAscribe
  | SliceAlias
  | SliceModule
  | SliceAlternative
  | SliceMatched;

[@deriving (show({with_path: false}), sexp, yojson)]
type slice_child = {
  mode: slice_child_mode,
  child: Id.t,
  pattern: option(Id.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type slice_scratch = {
  children: list(slice_child),
  patterns: list(Id.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type exp = {
  user_term: Exp.t, /* The term under consideration */
  elab_term: Exp.t,
  ancestors, /* Ascending list of containing term ids */
  ctx: Ctx.t, /* Typing context for the term */
  ana: Typ.t, /* Parental type expectations  */
  elab_syn_ty: Typ.t, /* Synthesized type of the elaborated expression */
  marks: list(Mark.t), /* Error marks from statics */
  co_ctx: CoCtx.t, /* Locally free variables */
  cls: Cls.t, /* DERIVED: Syntax class (i.e. form name) */
  message: Message.t, /* DERIVED: non-error inspector payload (Exp only) */
  warnings: list(Warning.list_item),
  ty: Typ.t, /* DERIVED: Type after nonempty hole fixing */
  label_inference: option(label_inference(exp)), /* Label inference information for the tuple */
  inferred_label: option(LabeledTuple.label), /* Inferred label for an expression within the tuple */
  label_sort: bool, /* When in the position of a label */
  dot_labels: list(string), /* Available labels when in dot-access position */
  slice_children: list(slice_child),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pat = {
  user_term: Pat.t,
  elab_term: Pat.t,
  ancestors,
  ctx: Ctx.t,
  co_ctx: CoCtx.t,
  ana: Typ.t,
  elab_syn_ty: Typ.t,
  marks: list(Mark.t),
  cls: Cls.t,
  message: Message.t, /* DERIVED: non-error inspector payload (Pat only) */
  warnings: list(Warning.list_item),
  ty: Typ.t,
  constraint_: Coverage.Constraint.t,
  label_inference: option(label_inference(pat)),
  inferred_label: option(LabeledTuple.label),
  label_sort: bool /* When in the position of a label */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type typ = {
  user_term: Typ.t,
  ancestors,
  ctx: Ctx.t,
  expects: TypExpectation.t,
  cls: Cls.t,
  marks: list(Mark.t),
  message: option(Message.t), /* Some(TypOk(_)) when marks = [] */
  warnings: list(Warning.list_item),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tpat = {
  user_term: TPat.t,
  ancestors,
  ctx: Ctx.t,
  cls: Cls.t,
  marks: list(Mark.t),
  message: option(Message.t), /* Some(TPatOk(_)) when marks = [] */
  warnings: list(Warning.list_item),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type mod_ = {
  id: Id.t,
  user_term: Mod.t,
  cls: Cls.t,
  sort: Sort.t,
  ctx: Ctx.t,
  ancestors,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type sig_ = {
  id: Id.t,
  user_term: Sig.t,
  cls: Cls.t,
  sort: Sort.t,
  ctx: Ctx.t,
  ancestors,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type mpat = {
  id: Id.t,
  user_term: MPat.t,
  cls: Cls.t,
  sort: Sort.t,
  ctx: Ctx.t,
  ancestors,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type secondary = {
  id: Id.t, // Id of term static info is sourced from
  cls: Cls.t, // Cls of secondary, not source term
  sort: Sort.t, // from source term
  ctx: Ctx.t // from source term
};

/* The static information collated for each term */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | InfoDrv(DrvInfo.t)
  | InfoExp(exp)
  | InfoPat(pat)
  | InfoTyp(typ)
  | InfoTPat(tpat)
  | InfoMod(mod_)
  | InfoSig(sig_)
  | InfoMPat(mpat)
  | InfoSliceScratch(slice_scratch)
  | Secondary(secondary);

/* ==================================== Getters ==================================== */

let sort_of: t => Sort.t =
  fun
  | InfoDrv(drv) => Drv(DrvInfo.sort_of(drv))
  | InfoExp({cls: Mod(_), _}) => Mod
  | InfoExp(_) => Exp
  | InfoPat(_) => Pat
  | InfoTyp(_) => Typ
  | InfoTPat(_) => TPat
  | InfoMod(_) => Mod
  | InfoSig(_) => Sig
  | InfoMPat(_) => MPat
  | InfoSliceScratch(_) => Exp
  | Secondary(s) => s.sort;

/* The grammar's mold system uses a single `Drv(Exp)` outer sort for all of
   `Drv(Jdmt)`, `Drv(Ctx)`, `Drv(Prop)`, and `Drv(Exp)` (see DrvSort.re on the
   "remolding issue"). Statics disambiguates these sub-sorts, so the info_map
   is the source of truth when we need the precise Drv sub-sort (e.g. to pick
   a CSS class like `.token.Drv` vs `.token.Exp`). For any non-Drv mold or
   when no InfoDrv entry is present, we defer to the mold's outer sort. */
let refine_sort_from_mold =
    (~info_map: Id.Map.t(t), ~id: Id.t, mold_out: Sort.t): Sort.t =>
  switch (mold_out) {
  | Drv(_) =>
    switch (Id.Map.find_opt(id, info_map)) {
    | Some(InfoDrv(drv)) => Drv(DrvInfo.sort_of(drv))
    | _ => mold_out
    }
  | _ => mold_out
  };

let class_of: t => string =
  fun
  | InfoDrv(drv) => DrvInfo.sort_of(drv) |> DrvSort.class_of
  | _ as i => sort_of(i) |> Sort.show;

let cls_of: t => Cls.t =
  fun
  | InfoDrv(drv) => DrvInfo.cls_of(drv)
  | InfoExp({cls, _})
  | InfoPat({cls, _})
  | InfoTyp({cls, _})
  | InfoTPat({cls, _})
  | InfoMod({cls, _})
  | InfoSig({cls, _})
  | InfoMPat({cls, _})
  | Secondary({cls, _}) => cls
  | InfoSliceScratch(_) => Exp(Exp.EmptyHole);

/* User-facing class name, context-aware where the static `Cls.show`
   loses information. Currently specializes tpat `Var` based on the
   statics' classification: a bound type variable with kind
   `Abstract` is a parameter, anything else is an alias. Other sorts
   fall back to the plain `Cls.show(cls_of(info))`. */
let cls_text_of: t => string =
  fun
  | InfoTPat({cls: TPat(Var), message, _}) =>
    switch (message) {
    | Some(TPatOk(TypeParameter(_))) => "Type parameter"
    | Some(TPatOk(TypeAlias(_))) => "Type alias"
    | _ => Cls.show(TPat(Var))
    }
  | info => Cls.show(cls_of(info));

let any_of: t => option(Any.t) =
  fun
  | InfoDrv({term, _}) => Some(Drv(term))
  | InfoExp({user_term, _}) => Some(Exp(user_term))
  | InfoPat({user_term, _}) => Some(Pat(user_term))
  | InfoTyp({user_term, _}) => Some(Typ(user_term))
  | InfoTPat({user_term, _}) => Some(TPat(user_term))
  | InfoMod({user_term, _}) => Some(Mod(user_term))
  | InfoSig({user_term, _}) => Some(Sig(user_term))
  | InfoMPat({user_term, _}) => Some(MPat(user_term))
  | InfoSliceScratch(_) => None
  | Secondary(_) => None;

let ctx_of: t => Ctx.t =
  fun
  | InfoDrv(_) => Ctx.empty_pre_elaboration
  | InfoExp({ctx, _})
  | InfoPat({ctx, _})
  | InfoTyp({ctx, _})
  | InfoTPat({ctx, _})
  | InfoMod({ctx, _})
  | InfoSig({ctx, _})
  | InfoMPat({ctx, _})
  | Secondary({ctx, _}) => ctx
  | InfoSliceScratch(_) => Ctx.empty_pre_elaboration;

let ancestors_of: t => ancestors =
  fun
  | InfoDrv(drv) => DrvInfo.ancestors_of(drv)
  | InfoExp({ancestors, _})
  | InfoPat({ancestors, _})
  | InfoTyp({ancestors, _})
  | InfoTPat({ancestors, _})
  | InfoMod({ancestors, _})
  | InfoSig({ancestors, _})
  | InfoMPat({ancestors, _}) => ancestors
  | InfoSliceScratch(_) => []
  | Secondary(_) => []; //TODO

let parent_id_of: t => option(Id.t) =
  info => info |> ancestors_of |> ListUtil.hd_opt;

let id_of: t => Id.t =
  fun
  | InfoDrv(drv) => DrvInfo.id_of(drv)
  | InfoExp(i) => Exp.rep_id(i.user_term)
  | InfoPat(i) => Pat.rep_id(i.user_term)
  | InfoTyp(i) => Typ.rep_id(i.user_term)
  | InfoTPat(i) => TPat.rep_id(i.user_term)
  | InfoMod({id, _})
  | InfoSig({id, _})
  | InfoMPat({id, _}) => id
  | InfoSliceScratch(_) => Id.mk()
  | Secondary(s) => s.id;

let marks_of: t => list(Mark.t) =
  fun
  | InfoExp({marks, _})
  | InfoPat({marks, _}) => marks
  | InfoTyp({marks, _})
  | InfoTPat({marks, _}) => marks
  | InfoDrv(_) /* Drv errors are tracked separately via DrvInfo.error_of. */
  | InfoMod(_)
  | InfoSig(_)
  | InfoMPat(_)
  | InfoSliceScratch(_)
  | Secondary(_) => [];

/* Determines whether any term is in an error hole. Drv info uses its own
   status representation; everything else reports via the unified marks list. */
let is_error = (ci: t): bool =>
  switch (ci) {
  | InfoDrv(drv) => DrvInfo.is_error(drv)
  | _ => marks_of(ci) != []
  };

let warnings_of: t => list(Warning.list_item) =
  fun
  | InfoExp({warnings, _})
  | InfoPat({warnings, _})
  | InfoTyp({warnings, _})
  | InfoTPat({warnings, _}) => warnings
  | InfoDrv(_)
  | InfoMod(_)
  | InfoSig(_)
  | InfoMPat(_)
  | InfoSliceScratch(_)
  | Secondary(_) => [];

let is_warning = (ci: t): bool => warnings_of(ci) != [];

/* A term is "typable" if it can meaningfully be assigned a type and will
   have a runtime value. This includes expressions and patterns, but excludes
   types, type patterns, and certain expression forms (deferrals, labels,
   type aliases) that don't produce useful values for probing/statics display. */
let is_typable_term: option(t) => bool =
  fun
  | Some(
      InfoExp({
        user_term: {term: Deferral(_) | Label(_) | TyAlias(_), _},
        _,
      }),
    ) =>
    false
  | Some(
      InfoTyp(_) | InfoTPat(_) | InfoMod(_) | InfoSig(_) | InfoMPat(_) |
      InfoDrv(_) |
      InfoSliceScratch(_) |
      Secondary(_),
    ) =>
    false
  | Some(InfoExp(_) | InfoPat(_)) => true
  | None => false;

let exp_co_ctx: exp => CoCtx.t = ({co_ctx, _}) => co_ctx;
let exp_ty: exp => Typ.t = ({ty, _}) => ty;
let pat_ctx: pat => Ctx.t = ({ctx, _}) => ctx;
let pat_ty: pat => Typ.t = ({ty, _}) => ty;
let pat_constraint: pat => Coverage.Constraint.t =
  ({constraint_, _}) => constraint_;

/* Thin dispatcher; detection logic lives in Mark.is_syntax_error. */
let is_syntax_error = (ci: t): bool =>
  Mark.is_syntax_error(sort_of(ci), marks_of(ci));

let is_label = (info: t): bool =>
  switch (info) {
  | InfoTyp({message: Some(Message.TypOk(Message.EmptyLabel)), _})
  | InfoTyp({user_term: {term: Label(_), _}, _})
  | InfoExp({user_term: {term: Label(_), _}, _})
  | InfoPat({user_term: {term: Label(_), _}, _})
  | InfoPat({label_sort: true, _})
  | InfoExp({label_sort: true, _}) => true
  | _ => false
  };

/* Extract the projector kind from an info, if it represents a projector term */
let projector_kind_of = (info: t): option(ProjectorKind.t) =>
  switch (info) {
  | InfoExp({user_term: {term: Projector({kind, _}, _), _}, _}) =>
    Some(kind)
  | InfoPat({user_term: {term: Projector({kind, _}, _), _}, _}) =>
    Some(kind)
  | InfoTyp({user_term: {term: Projector({kind, _}, _), _}, _}) =>
    Some(kind)
  | _ => None
  };

/* If the info represents some kind of name binding which
   exists in the context, return the id where the binding occurs.

   `Var(name)` in `InfoTyp` is a type-variable reference only when the
   typ is in `TypeExpected` position. In a constructor-declaration
   position (`+ A(a)` / `+ B(b)` etc.) the `Var(name)` is the
   constructor's name being introduced, not a tvar reference; we
   return `None` there so the `var_highlight_ids` fallback uses the
   declaration's own id as the binding site. */
let get_binding_site = (info: t): option(Id.t) => {
  /* Recover a constructor reference's binding-site id in order of
     preference:
       1. Type-directed `lookup_ctr_for_ana` — picks the right binding
          when multiple sum types in scope share a constructor name.
       2. If no `ConstructorEntry` is in scope (e.g. constructor
          declared inside a module), find it on the analysis target's
          sum constructors via `Typ.get_sum_constructors`.
       3. Last-resort `VarEntry` lookup (capitalized module names
          parse as `Constructor` but bind as variables). */
  /* For a constructor used as a function (`C(arg)`) the analysis
     target is `Arrow(_, result)`; peel arrows to get the underlying
     sum type. */
  let rec result_of_ana = (ty: Typ.t): Typ.t =>
    switch (ty.term) {
    | Arrow(_, out) => result_of_ana(out)
    | Parens(inner)
    | Projector(_, inner) => result_of_ana(inner)
    | _ => ty
    };
  let ctr_id_from_ana =
      (~ctx: Ctx.t, ~ana: Typ.t, name: string): option(Id.t) => {
    let* sum = Typ.get_sum_constructors(ctx, result_of_ana(ana));
    let* variant =
      List.find_opt(
        (v: ConstructorMap.variant(Typ.t)) =>
          switch (v) {
          | Variant(ctr, _, _) => ctr == name
          | BadEntry(_) => false
          },
        sum,
      );
    switch (variant) {
    | Variant(_, ann, _) =>
      switch (ann.ids) {
      | [id, ..._] when !Id.equal(id, Id.invalid) => Some(id)
      | _ => None
      }
    | BadEntry(_) => None
    };
  };
  let ctr_binding_id =
      (~ctx: Ctx.t, ~ana: option(Typ.t), name: string): option(Id.t) =>
    switch (Ctx.lookup_ctr_for_ana(ctx, name, ana)) {
    | Some(entry) when entry.id != Id.invalid => Some(entry.id)
    | _ =>
      switch (ana) {
      | Some(ana) =>
        switch (ctr_id_from_ana(~ctx, ~ana, name)) {
        | Some(_) as id => id
        | None =>
          let* entry = Ctx.lookup_var(ctx, name);
          entry.id == Id.invalid ? None : Some(entry.id);
        }
      | None =>
        let* entry = Ctx.lookup_var(ctx, name);
        entry.id == Id.invalid ? None : Some(entry.id);
      }
    };
  switch (info) {
  | InfoExp({user_term: {term: Var(name), _}, ctx, _}) =>
    let* entry = Ctx.lookup_var(ctx, name);
    entry.id == Id.invalid ? None : Some(entry.id);
  | InfoExp({user_term: {term: Constructor(name, _), _}, ctx, ana, _}) =>
    ctr_binding_id(~ctx, ~ana=Some(ana), name)
  | InfoPat({user_term: {term: Constructor(name, _), _}, ctx, ana, _}) =>
    ctr_binding_id(~ctx, ~ana=Some(ana), name)
  | InfoTyp({
      user_term: {term: Var(_), _},
      expects:
        TypExpectation.ConstructorExpected(_) |
        TypExpectation.VariantExpected(_),
      _,
    }) =>
    None
  | InfoTyp({user_term: {term: Var(name), _}, ctx, _}) =>
    let* id = Ctx.lookup_tvar_id(ctx, name);
    id == Id.invalid ? None : Some(id);
  | _ => None
  };
};

let typ_is_constructor_expected = t =>
  switch (t) {
  | {
      expects:
        TypExpectation.ConstructorExpected(_) |
        TypExpectation.VariantExpected(_),
      _,
    } =>
    true
  | _ => false
  };

let rec pre_labeled_info = (info: t): t =>
  switch (info) {
  | InfoExp({
      label_inference:
        Some(SingletonLabelInference({pre_labeled_info: pli, _})),
      _,
    }) =>
    pre_labeled_info(InfoExp(pli))
  | InfoPat({
      label_inference:
        Some(SingletonLabelInference({pre_labeled_info: pli, _})),
      _,
    }) =>
    pre_labeled_info(InfoPat(pli))
  | _ => info
  };
