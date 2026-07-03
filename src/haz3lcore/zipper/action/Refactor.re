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

let applicable = (z: Zipper.t): bool =>
  switch (Indicated.index(z)) {
  | None => false
  | Some(target) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let found = ref(false);
    let _ =
      Exp.map_term(
        ~f_exp=
          (cont, e: Exp.t) =>
            switch (IdTagged.term_of(e)) {
            | Let(p, _, _)
                when
                  List.mem(target, IdTagged.ids(e))
                  && var_pat_name(p) != None =>
              found := true;
              e;
            | _ => cont(e)
            },
        term,
      );
    found^;
  };

let go = (kind: Action.refactor, z: Zipper.t): option(Zipper.t) =>
  switch (kind) {
  | InlineLet =>
    switch (Indicated.index(z)) {
    | None => None
    | Some(target) =>
      let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
      switch (inline_let(target, term)) {
      | None => None
      | Some((term', focus)) =>
        let seg =
          ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term');
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
    }
  };
