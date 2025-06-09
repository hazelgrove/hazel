open ProjectorBase;
open Util;

let init =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~copy_ed: ed => ed,
      kind: ProjectorCore.Kind.t,
      any: Term.Any.t,
      ed: unit => option(ed),
    )
    : option(ProjectorCore.model(ed, ed_a, ed_f)) => {
  open ProjectorCore.Kind;
  let.gadt W(kind_gadt) = kind;
  let methods = ProjectorCore.to_module(kind_gadt);
  switch (methods.init(~copy_ed, any, ed)) {
  | None => None
  | Some(model) => Some(ProjectorCore.V(kind_gadt, model, Calc.Pending))
  };
};

let make_term =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~mk_term_ed,
      ~sort: Sort.t,
      V(k, m, exp_cache): ProjectorCore.model(ed, ed_a, ed_f),
    )
    : (ProjectorCore.model(ed, ed_a, ed_f), Calc.t(Any.t)) => {
  let methods = ProjectorCore.to_module(k);
  let (ed', term) = methods.mk_term(~mk_term_ed, ~sort, ~prev=exp_cache, m);
  (V(k, ed', term |> Calc.save), term);
};
