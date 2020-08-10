[@deriving sexp]
type edit_state = (ZExp.t, HTyp.t, MetaVarGen.t);

type type_mode =
  | Syn
  | Ana(HTyp.t);

let tuple_zip =
    (
      ~get_tuple_elements: Skel.t('op) => list(Skel.t('op)),
      skel: Skel.t('op),
      ty: HTyp.t,
    )
    : option(list((Skel.t('op), HTyp.t))) => {
  let skels = skel |> get_tuple_elements;
  let tys = ty |> HTyp.get_prod_elements;
  switch (ListUtil.opt_zip(skels, tys)) {
  | Some(_) as zipped => zipped
  | None =>
    switch (skels, tys) {
    | ([_], _) => Some([(skel, ty)])
    | (_, [Hole]) =>
      skels |> List.map(skel => (skel, HTyp.Hole)) |> Option.some
    | _ => None
    }
  };
};

type syn_fixer('term, 'extra_input, 'extra_output) =
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool,
    ~extra_input: 'extra_input,
    'term
  ) =>
  ('term, 'extra_output, MetaVarGen.t, bool);
type ana_fixer('term, 'extra_input, 'extra_output) =
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool,
    ~extra_input: 'extra_input,
    'term,
    HTyp.t
  ) =>
  ('term, 'extra_output, MetaVarGen.t, bool);

let syn_count = ref(0);
let syn_fixed_count = ref(0);

// Wraps a syn_fixer to check `fixed` is true. If it isn't, then the value
// didn't change and we can ensure pointer stability by returning the original
// term.
let stable_syn_fixer =
    (f: syn_fixer('term, 'extra_input, 'extra_output))
    : syn_fixer('term, 'extra_input, 'extra_output) =>
  (ctx, u_gen, ~renumber_empty_holes, ~extra_input, term) => {
    let (fixed_term, extra_output, u_gen, fixed) =
      f(ctx, u_gen, ~renumber_empty_holes, ~extra_input, term);
    if (fixed) {
      syn_fixed_count := syn_fixed_count^ + 1;
    };
    syn_count := syn_count^ + 1;
    (fixed ? fixed_term : term, extra_output, u_gen, fixed);
  };

let ana_count = ref(0);
let ana_fixed_count = ref(0);
let stable_ana_fixer =
    (f: ana_fixer('term, 'extra_input, 'extra_output))
    : ana_fixer('term, 'extra_input, 'extra_output) =>
  (ctx, u_gen, ~renumber_empty_holes, ~extra_input, term, ty) => {
    let (fixed_term, extra_output, u_gen, fixed) =
      f(ctx, u_gen, ~renumber_empty_holes, ~extra_input, term, ty);
    if (fixed) {
      ana_fixed_count := ana_fixed_count^ + 1;
    };
    ana_count := ana_count^ + 1;
    (fixed ? fixed_term : term, extra_output, u_gen, fixed);
  };

let set_hole_reason =
    (u_gen: MetaVarGen.t, reason: ErrStatus.HoleReason.t, err: ErrStatus.t) =>
  switch (err) {
  | InHole(r, _) when r == reason => (err, u_gen, false)
  | _ =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let err = ErrStatus.InHole(reason, u);
    (err, u_gen, true);
  };

let set_inconsistent_branches =
    (
      u_gen: MetaVarGen.t,
      rule_types: list(HTyp.t),
      case_err: CaseErrStatus.t,
    ) =>
  switch (case_err) {
  | InconsistentBranches(rule_types', _) when rule_types == rule_types' => (
      case_err,
      u_gen,
      false,
    )
  | _ =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (InconsistentBranches(rule_types, u), u_gen, true);
  };
