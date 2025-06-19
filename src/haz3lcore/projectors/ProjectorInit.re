open Util;

let init =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~editor_module,
      kind: ProjectorCore.Kind.t,
      any: Language.Any.t,
      ed: unit => option(ed),
    )
    : option(ProjectorCore.model(ed, ed_a, ed_f)) => {
  open ProjectorCore.Kind;
  let.gadt W(kind_gadt) = kind;
  let methods = ProjectorCore.to_module(editor_module, kind_gadt);
  let init =
    methods
    |> (
      (
        type p_m,
        type p_a,
        type p_f,
        module Methods:
          ProjectorInterface.PROJECTOR with
            type model' = p_m and
            type action' = p_a and
            type focus' = p_f and
            type editor_model = ed,
        any,
        ed,
      ) => {
        Methods.init(any, ed);
      }
    );
  switch (init(any, ed)) {
  | None => None
  | Some(model) => Some(ProjectorCore.V(kind_gadt, model, Calc.Pending))
  };
};

let make_term =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~editor_module,
      ~sort: Sort.t,
      V(k, m, exp_cache): ProjectorCore.model(ed, ed_a, ed_f),
    )
    : (ProjectorCore.model(ed, ed_a, ed_f), Calc.t(Language.Any.t)) => {
  let methods = ProjectorCore.to_module(editor_module, k);
  let mk_term =
    methods
    |> (
      (
        type p_m,
        type p_a,
        type p_f,
        module Methods:
          ProjectorInterface.PROJECTOR with
            type model' = p_m and
            type action' = p_a and
            type focus' = p_f and
            type editor_model = ed,
        ~sort,
        ~prev,
        m,
      ) => {
        Methods.mk_term(~sort, ~prev, m);
      }
    );
  let (ed', term) = mk_term(~sort, ~prev=exp_cache, m);
  (V(k, ed', term |> Calc.save), term);
};
