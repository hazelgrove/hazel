open Language;

/* Term-level refactorings, built on the segment<->term roundtrip:
 * parse the buffer to a term (via canonical completion), transform the
 * term, print with roundtrip settings, and splice back as the new
 * zipper. Ids survive the trip, so probes, statics, and incremental
 * caches survive the refactor. */

let roundtrip_settings: ExpToSegment.Settings.t = {
  secondary: PreserveExact,
  parenthesization: Structural,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  use_literal_lexemes: true,
  project_tables: false,
};

/* Structural printing adds no defensive parens, so a non-atomic
 * definition must be wrapped before substitution or the inlined text
 * can reassociate (`let x = 1 + 2 in x * x` -> `1 + 2 * 1 + 2`) */
let needs_parens = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(_)
  | Atom(_)
  | EmptyHole
  | Parens(_)
  | ListLit(_)
  | Tuple([])
  | Ap(Forward, _, _)
  | TypAp(_)
  | Dot(_)
  | Constructor(_, _) => false
  | _ => true
  };

/* Whitespace attaches to adjacent LEAF nodes: a region's boundary
 * spacing lives on its first leaf's `before` and last leaf's `after`.
 * Strip those when a region moves to a new slot. */
/* map_term visits constructor args in evaluation order (right to
 * left), so "first visited" tricks are unreliable. Determine a
 * region's boundary whitespace TEXTUALLY, by printing it and taking
 * the leading/trailing Secondary runs, then drop those pieces from
 * the term's annotations by id. */
let secondary_run_ids = (seg: Segment.t): list(Id.t) => {
  let rec go = (acc, seg: Segment.t) =>
    switch (seg) {
    | [Piece.Secondary(w), ...rest] => go([w.id, ...acc], rest)
    | _ => acc
    };
  go([], seg);
};

let secondary_run_pieces = (seg: Segment.t): list(Secondary.t) => {
  let rec go = (acc, seg: Segment.t) =>
    switch (seg) {
    | [Piece.Secondary(w), ...rest] =>
      go(
        [
          {
            Secondary.id: w.id,
            content: w.content,
          },
          ...acc,
        ],
        rest,
      )
    | _ => List.rev(acc)
    };
  go([], seg);
};

let drop_secondary = (ids: list(Id.t), e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let keep = (ws: list(Secondary.t)) =>
          ws |> List.filter((w: Secondary.t) => !List.mem(w.id, ids));
        let (before, after) = e.annotation.secondary;
        {
          ...e,
          annotation: {
            ...e.annotation,
            secondary: (keep(before), keep(after)),
          },
        }
        |> cont;
      },
    e,
  );

let strip_boundaries = (e: Exp.t): Exp.t => {
  let seg = ExpToSegment.exp_to_segment(~settings=roundtrip_settings, e);
  let leading = secondary_run_ids(seg);
  let trailing = secondary_run_ids(List.rev(seg));
  drop_secondary(leading @ trailing, e);
};

let strip_leading = (e: Exp.t): Exp.t => {
  let seg = ExpToSegment.exp_to_segment(~settings=roundtrip_settings, e);
  drop_secondary(secondary_run_ids(seg), e);
};

let strip_trailing = (e: Exp.t): Exp.t => {
  let seg = ExpToSegment.exp_to_segment(~settings=roundtrip_settings, e);
  drop_secondary(secondary_run_ids(List.rev(seg)), e);
};

/* The inserted copy takes over the replaced occurrence's stored
 * whitespace (its slot in the line); the definition keeps its own
 * interior spacing */
let inserted = (def: Exp.t, at: Exp.t): Exp.t => {
  let secondary = at.annotation.secondary;
  let def = strip_boundaries(def);
  if (needs_parens(def)) {
    {
      annotation: {
        ...IdTagged.IdTag.mk_internal([Id.mk()]),
        secondary,
      },
      term: Parens(def),
    };
  } else {
    let (before, after) = secondary;
    let (def_before, def_after) = def.annotation.secondary;
    {
      ...def,
      annotation: {
        ...def.annotation,
        secondary: (def_before @ before, def_after @ after),
      },
    };
  };
};

/* Does the pattern bind this name? (occurrences under such binders are
 * shadowed and must not be substituted) */
let binds = (x: string, p: Pat.t): bool => {
  let found = ref(false);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p: Pat.t) => {
          switch (IdTagged.term_of(p)) {
          | Var(y) when y == x => found := true
          | _ => ()
          };
          cont(p);
        },
      p,
    );
  found^;
};

/* Shadow-aware substitution of def for x, preserving each occurrence's
 * whitespace slot. Does not rename binders: a free variable of def
 * that is rebound between the let and a use site can be captured
 * (matching the editor-wide Substitution limitation). */
let rec subst = (x: string, def: Exp.t, e: Exp.t): Exp.t => {
  let go = subst(x, def);
  let (term, rewrap) = Exp.unwrap(e);
  switch (term) {
  | Var(y) when y == x => inserted(def, e)
  | Let(p, d, body) =>
    rewrap(Let(p, go(d), binds(x, p) ? body : go(body)))
  | Fun(p, body, t, n) when binds(x, p) => rewrap(Fun(p, body, t, n))
  | FixF(p, body, env) when binds(x, p) => rewrap(FixF(p, body, env))
  | Match(scrut, rules) =>
    rewrap(
      Match(
        go(scrut),
        rules
        |> List.map(((p, body)) => (p, binds(x, p) ? body : go(body))),
      ),
    )
  | _ =>
    /* one level of generic descent: map each direct child, without
       re-entering this node (map_term's f is called on the node
       itself first, so guard on identity once) */
    Exp.map_term(
      ~f_exp={
        let entered = ref(false);
        (cont, e': Exp.t) =>
          if (entered^) {
            go(e');
          } else {
            entered := true;
            cont(e');
          };
      },
      e,
    )
  };
};

/* Every substituted occurrence carries the def's ids; re-id all but
 * one so the buffer never contains duplicates (one copy keeps the
 * originals, so probes on the definition follow it somewhere) */
/* Fresh ids for every node AND secondary piece, keeping whitespace
 * content and lexemes (Exp.replace_all_ids drops secondary, which
 * would strip a duplicated copy's spacing) */
let refresh_ids = (e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let refresh_sec = (ws: list(Secondary.t)) =>
          ws
          |> List.map((w: Secondary.t) =>
               {
                 ...w,
                 id: Id.mk(),
               }
             );
        let (before, after) = e.annotation.secondary;
        {
          ...e,
          annotation: {
            ids: [Id.mk()],
            secondary: (refresh_sec(before), refresh_sec(after)),
            incomplete: [],
            lexeme: e.annotation.lexeme,
          },
        }
        |> cont;
      },
    e,
  );

let dedupe_ids = (e: Exp.t): Exp.t => {
  let seen = ref(Id.Map.empty);
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let ids = IdTagged.ids(e);
        if (List.exists(id => Id.Map.mem(id, seen^), ids)) {
          refresh_ids(e);
        } else {
          List.iter(id => seen := Id.Map.add(id, (), seen^), ids);
          cont(e);
        };
      },
    e,
  );
};

let var_pat_name = (p: Pat.t): option(string) =>
  switch (IdTagged.term_of(p)) {
  | Var(name) => Some(name)
  | _ => None
  };

/* Find the Let bound to `target` (one of its tile ids) with a simple
 * var pattern; replace it with its body, the bound var substituted
 * (capture-avoiding via Substitution). Returns the transformed program
 * and a focus id for the caret. */
let inline_let = (target: Id.t, program: Exp.t): option((Exp.t, Id.t)) => {
  let focus = ref(None);
  let program' =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) =>
          switch (IdTagged.term_of(e)) {
          | Let(p, def, body)
              when
                focus^ == None
                && List.mem(target, IdTagged.ids(e))
                && var_pat_name(p) != None =>
            let x = Option.get(var_pat_name(p));
            focus := Some(Exp.rep_id(def));
            let body' = subst(x, def, body) |> strip_leading;
            /* the result takes over the let's slot in its context */
            let (let_before, let_after) = e.annotation.secondary;
            let (b_before, b_after) = body'.annotation.secondary;
            {
              ...body',
              annotation: {
                ...body'.annotation,
                secondary: (let_before @ b_before, b_after @ let_after),
              },
            };
          | _ => cont(e)
          },
      program,
    );
  focus^ |> Option.map(f => (dedupe_ids(program'), f));
};

/* === Registry ===
 * A refactoring is one record; menus and dispatch iterate the
 * registry, so adding a transform means adding a kind + an entry. */

type impl = {
  label: string,
  tooltip: string,
  /* Some((program', focus_id)) when applicable at `target` */
  prepare:
    (~info_map: Statics.Map.t, ~target: Id.t, Exp.t) => option((Exp.t, Id.t)),
};

/* Replace a Let node (matched by predicate at target) with a rewrite
 * of its parts; shared slot handling: the result takes over the let's
 * whitespace slot */
let rewrite_let =
    (
      ~target: Id.t,
      ~matches: (Pat.t, Exp.t, Exp.t) => bool,
      ~rewrite: (Pat.t, Exp.t, Exp.t) => (Exp.t, Id.t),
      program: Exp.t,
    )
    : option((Exp.t, Id.t)) => {
  let focus = ref(None);
  let program' =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) =>
          switch (IdTagged.term_of(e)) {
          | Let(p, def, body)
              when
                focus^ == None
                && List.mem(target, IdTagged.ids(e))
                && matches(p, def, body) =>
            let (result, f) = rewrite(p, def, body);
            focus := Some(f);
            let result = strip_leading(result);
            let (let_before, let_after) = e.annotation.secondary;
            let (b_before, b_after) = result.annotation.secondary;
            {
              ...result,
              annotation: {
                ...result.annotation,
                secondary: (let_before @ b_before, b_after @ let_after),
              },
            };
          | _ => cont(e)
          },
      program,
    );
  focus^ |> Option.map(f => (dedupe_ids(program'), f));
};

let inline_let_impl: impl = {
  label: "Inline Let",
  tooltip: "Replace this let by substituting its definition",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_let(
      ~target,
      ~matches=(p, _, _) => var_pat_name(p) != None,
      ~rewrite=
        (p, def, body) => {
          let x = Option.get(var_pat_name(p));
          (subst(x, def, body), Exp.rep_id(def));
        },
      program,
    ),
};

/* Statics-gated: the binding's pattern var carries an UnusedVar
 * warning (co-ctx says the body never uses it) */
let pat_unused = (~info_map: Statics.Map.t, p: Pat.t): bool =>
  switch (Id.Map.find_opt(Pat.rep_id(p), info_map)) {
  | Some(InfoPat({warnings, _})) =>
    warnings
    |> List.exists((w: Warning.list_item) =>
         switch (w) {
         | Pat(UnusedVar(_)) => true
         }
       )
  | _ => false
  };

let remove_unused_let_impl: impl = {
  label: "Remove Unused Let",
  tooltip: "Delete this binding: its variable is never used",
  prepare: (~info_map, ~target, program) =>
    rewrite_let(
      ~target,
      ~matches=
        (p, _, _) => var_pat_name(p) != None && pat_unused(~info_map, p),
      ~rewrite=(_, _, body) => (body, Exp.rep_id(body)),
      program,
    ),
};

/* Replace an arbitrary node (matched at target); the replacement takes
 * over the node's whitespace slot */
let rewrite_node =
    (
      ~target: Id.t,
      ~rewrite: Exp.t => option((Exp.t, Id.t)),
      program: Exp.t,
    )
    : option((Exp.t, Id.t)) => {
  let focus = ref(None);
  let program' =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) =>
          if (focus^ == None && List.mem(target, IdTagged.ids(e))) {
            switch (rewrite(e)) {
            | Some((result, f)) =>
              focus := Some(f);
              /* the replacement takes over the node's textual slot:
                 leading/trailing whitespace runs (which live on leaf
                 annotations) move to the new node's outer secondary */
              let seg_e =
                ExpToSegment.exp_to_segment(~settings=roundtrip_settings, e);
              let lead = secondary_run_pieces(seg_e);
              let trail = List.rev(secondary_run_pieces(List.rev(seg_e)));
              let ids = List.map((w: Secondary.t) => w.id, lead @ trail);
              let result = drop_secondary(ids, result);
              let (rb, ra) = result.annotation.secondary;
              {
                ...result,
                annotation: {
                  ...result.annotation,
                  secondary: (lead @ rb, ra @ trail),
                },
              };
            | None => cont(e)
            };
          } else {
            cont(e);
          },
      program,
    );
  focus^ |> Option.map(f => (dedupe_ids(program'), f));
};

let fresh = (term): Exp.t => {
  annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
  term,
};
let fresh_pat = (term): Pat.t => {
  annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
  term,
};

let bool_pat = (p: Pat.t): option(bool) =>
  switch (IdTagged.term_of(p)) {
  | Atom(Bool(b)) => Some(b)
  | _ => None
  };

let if_to_case_impl: impl = {
  label: "Convert to Case",
  tooltip: "Rewrite this if/then/else as a case on true and false",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~target,
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | If(c, t, alt) =>
            Some((
              fresh(
                Match(
                  c,
                  [
                    (fresh_pat(Atom(Bool(true))), t),
                    (fresh_pat(Atom(Bool(false))), alt),
                  ],
                ),
              ),
              Exp.rep_id(c),
            ))
          | _ => None
          },
      program,
    ),
};

let case_to_if_impl: impl = {
  label: "Convert to If",
  tooltip: "Rewrite this true/false case as if/then/else",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~target,
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Match(scrut, [(p1, e1), (p2, e2)]) =>
            switch (bool_pat(p1), bool_pat(p2)) {
            | (Some(true), Some(false)) =>
              Some((
                fresh(If(scrut, e1, strip_trailing(e2))),
                Exp.rep_id(scrut),
              ))
            | (Some(false), Some(true)) =>
              Some((
                fresh(If(scrut, e2, strip_trailing(e1))),
                Exp.rep_id(scrut),
              ))
            | _ => None
            }
          | _ => None
          },
      program,
    ),
};

let pad = (e: Exp.t): Exp.t => {
  let sp = (): list(Secondary.t) => [
    {
      id: Id.mk(),
      content: Whitespace(" "),
    },
  ];
  {
    ...e,
    annotation: {
      ...e.annotation,
      secondary: (sp(), sp()),
    },
  };
};

/* every var/pat-var name in the program (over-approximates scope) */
let used_names = (program: Exp.t): list(string) => {
  let names = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | Var(x) => names := [x, ...names^]
          | _ => ()
          };
          cont(e);
        },
      ~f_pat=
        (cont, p: Pat.t) => {
          switch (IdTagged.term_of(p)) {
          | Var(x) => names := [x, ...names^]
          | _ => ()
          };
          cont(p);
        },
      program,
    );
  names^;
};

let fresh_name = (program: Exp.t): string => {
  let used = used_names(program);
  let rec pick = n => {
    let cand = "x" ++ string_of_int(n);
    List.mem(cand, used) ? pick(n + 1) : cand;
  };
  List.mem("x", used) ? pick(1) : "x";
};

let pad_pat = (p: Pat.t): Pat.t => {
  let sp = (): list(Secondary.t) => [
    {
      id: Id.mk(),
      content: Whitespace(" "),
    },
  ];
  {
    ...p,
    annotation: {
      ...p.annotation,
      secondary: (sp(), sp()),
    },
  };
};

let extractable = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(_)
  | EmptyHole
  | Let(_)
  | Seq(_)
  | Filter(_) => false
  | _ => true
  };

let extract_let_impl: impl = {
  label: "Extract to Let",
  tooltip: "Bind this expression to a fresh variable in place",
  prepare: (~info_map as _, ~target, program) => {
    let x = fresh_name(program);
    let at_root = List.mem(target, IdTagged.ids(program));
    rewrite_node(
      ~target,
      ~rewrite=
        e =>
          extractable(e)
            ? {
              let def = pad(e |> strip_leading |> strip_trailing);
              let let_node =
                fresh(
                  Let(pad_pat(fresh_pat(Var(x))), def, fresh(Var(x))),
                );
              /* anywhere but the top level, the let must not capture
                 its surroundings when reparsed */
              let node = at_root ? let_node : fresh(Parens(let_node));
              Some((node, Exp.rep_id(def)));
            }
            : None,
      program,
    );
  },
};

/* unshadowed-use check is conservative: any occurrence counts */
let mentions = (x: string, e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | Var(y) when y == x => found := true
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  found^;
};

let eta_reduce_impl: impl = {
  label: "Eta-Reduce",
  tooltip: "Simplify `fun x -> f(x)` to `f`",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~target,
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Fun(p, body, _, _) =>
            switch (var_pat_name(p), IdTagged.term_of(body)) {
            | (Some(x), Ap(Forward, f, arg)) =>
              switch (IdTagged.term_of(arg)) {
              | Var(y) when y == x && !mentions(x, f) =>
                let f = f |> strip_leading |> strip_trailing;
                Some((f, Exp.rep_id(f)));
              | _ => None
              }
            | _ => None
            }
          | _ => None
          },
      program,
    ),
};

let negate_if_impl: impl = {
  label: "Negate & Swap Branches",
  tooltip: "Flip this if: negate the condition, swap then/else",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~target,
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | If(c, t, alt) =>
            let c = c |> strip_leading |> strip_trailing;
            let c = needs_parens(c) ? fresh(Parens(c)) : c;
            let pad_l = (e: Exp.t) => {
              let (b, _) = pad(e).annotation.secondary;
              {
                ...e,
                annotation: {
                  ...e.annotation,
                  secondary: (b, snd(e.annotation.secondary)),
                },
              };
            };
            let cond = pad(fresh(UnOp(Bool(Not), c)));
            let t' = pad(alt |> strip_leading |> strip_trailing);
            let alt' = pad_l(t |> strip_leading |> strip_trailing);
            Some((fresh(If(cond, t', alt')), Exp.rep_id(c)));
          | _ => None
          },
      program,
    ),
};

let impl: Action.refactor => impl =
  fun
  | InlineLet => inline_let_impl
  | RemoveUnusedLet => remove_unused_let_impl
  | IfToCase => if_to_case_impl
  | CaseToIf => case_to_if_impl
  | ExtractLet => extract_let_impl
  | EtaReduce => eta_reduce_impl
  | NegateIf => negate_if_impl;

let all: list(Action.refactor) = [
  InlineLet,
  RemoveUnusedLet,
  ExtractLet,
  EtaReduce,
  IfToCase,
  CaseToIf,
  NegateIf,
];

/* Menu support: which refactorings apply at the current indication */
let menu_items =
    (~info_map: Statics.Map.t, z: Zipper.t)
    : list((Action.refactor, string, string)) =>
  switch (Indicated.index(z)) {
  | None => []
  | Some(target) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    all
    |> List.filter_map(kind => {
         let i = impl(kind);
         Option.is_some(i.prepare(~info_map, ~target, term))
           ? Some((kind, i.label, i.tooltip)) : None;
       });
  };

let go =
    (~info_map: Statics.Map.t, kind: Action.refactor, z: Zipper.t)
    : option(Zipper.t) =>
  switch (Indicated.index(z)) {
  | None => None
  | Some(target) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    switch (impl(kind).prepare(~info_map, ~target, term)) {
    | None => None
    | Some((term', focus)) =>
      let seg =
        ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term')
        |> SpaceNormalize.go;
      let z' = {
        ...Zipper.unzip(seg),
        refractors: z.refractors,
      };
      Some(
        switch (Move.jump_to_id_indicated(z', focus)) {
        | Some(z'') => z''
        | None => z'
        },
      );
    };
  };
