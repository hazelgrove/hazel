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

// Wraps a syn_fixer to check `changed` is true. If it isn't, then the value
// didn't change and we can ensure pointer stability by returning the original
// term.
let stable_syn_fixer =
    (f: syn_fixer('term, 'extra_input, 'extra_output))
    : syn_fixer('term, 'extra_input, 'extra_output) =>
  (ctx, u_gen, ~renumber_empty_holes, ~extra_input, term) => {
    let (fixed_term, extra_output, u_gen, changed) =
      f(ctx, u_gen, ~renumber_empty_holes, ~extra_input, term);
    (changed ? fixed_term : term, extra_output, u_gen, changed);
  };
let stable_ana_fixer =
    (f: ana_fixer('term, 'extra_input, 'extra_output))
    : ana_fixer('term, 'extra_input, 'extra_output) =>
  (ctx, u_gen, ~renumber_empty_holes, ~extra_input, term, ty) => {
    let (fixed_term, extra_output, u_gen, changed) =
      f(ctx, u_gen, ~renumber_empty_holes, ~extra_input, term, ty);
    (changed ? fixed_term : term, extra_output, u_gen, changed);
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
