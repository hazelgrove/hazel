/* (^ticket(721)) A livelit's commit that changes only its model's literals.

   After a step of a livelit's slider the page writes the new model into the
   program (LivelitProj.commit_model), and the program's statics were worked
   out again for it: re-analyzing the edited top-level item took 12 to 28 ms of
   the edit on the Witch slide (run 42826692242). But a change to a model's
   literals can change the program's values, not its types. So when the commit
   keeps the model's ids ([same_shape_ids]) and changes only literals inside the
   model ([literal_changes]), the statics are kept, with the new literals put in
   wherever the statics hold terms: the record of each term from the literal up
   to the program's root (its term and its elaborated term), the program's term,
   and the program's elaboration, where the use's model sits inside its
   expansion (HazelJitRun.model_of). Each term is copied only along the path to
   the literal; everything off it stays the very same object, as the compiled
   run's comparison and the worker's caches want. Anything that does not line
   up answers None, and the statics are worked out again, as before.

   What is not brought up to date: each record's derived inspector message and
   label inference, which hold no literal's value the page shows; and the
   per-item analysis's own memory of the item (DefStatics), which sees the item
   changed at the next edit that works the statics out, and analyzes it then. */
open Util;
open Language;

let keep = (_, x) => x;
let same_class = ModelIds.same_class;
let literal_changes = ModelIds.literal_changes;

/* every id in [e], each node's first */
let ids_in = (e: Exp.t): Id.Map.t(unit) => {
  let out = ref(Id.Map.empty);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, x) => {
          out := Id.Map.add(Exp.rep_id(x), (), out^);
          cont(x);
        },
      e,
    );
  out^;
};

/* a use's model: the argument of its outermost application */
let rec model_of_use = (e: Exp.t): option(Exp.t) =>
  switch (e.term) {
  | Parens(x) => model_of_use(x)
  | Ap(_, _, m) => Some(m)
  | _ => None
  };

/* [e] with the node of id [target] replaced by [f] of it, copying only the
   path to it: below the root, a node is entered only if [path] holds its id.
   None if the walk does not meet [target], or [f] answers None */
let along =
    (~path: Id.t => bool, ~target: Id.t, f: Exp.t => option(Exp.t), e: Exp.t)
    : option(Exp.t) => {
  /* None: not met yet; Some(true): replaced; Some(false): [f] said None */
  let met = ref(None);
  let root = ref(true);
  let go = (cont, x: Exp.t) =>
    if (met^ != None) {
      x;
    } else if (Exp.rep_id(x) == target) {
      switch (f(x)) {
      | Some(y) =>
        met := Some(true);
        y;
      | None =>
        met := Some(false);
        x;
      };
    } else if (root^ || path(Exp.rep_id(x))) {
      root := false;
      cont(x);
    } else {
      x;
    };
  let e' =
    Exp.map_term(
      ~f_exp=go,
      ~f_pat=keep,
      ~f_typ=keep,
      ~f_tpat=keep,
      ~f_sig=keep,
      ~f_mpat=keep,
      e,
    );
  met^ == Some(true) ? Some(e') : None;
};

/* a literal node given [lit]'s value */
let set_literal = (lit: Exp.t, x: Exp.t): option(Exp.t) =>
  switch (x.term, lit.term) {
  | (Atom(a), Atom(b)) when same_class(a, b) =>
    Some({
      term: lit.term,
      annotation: {
        ...x.annotation,
        lexeme: lit.annotation.lexeme,
      },
    })
  | _ => None
  };

/* the use's elaborated projector node, its model's literal [l] given [lit]'s
   value inside its expansion */
let swap_model = (l: Id.t, lit: Exp.t, p: Exp.t): option(Exp.t) => {
  OptUtil.Syntax.(
    switch (p.term) {
    | Projector(data, inner) =>
      let* m = HazelJitRun.model_of(inner);
      let* m' = along(~path=_ => true, ~target=l, set_literal(lit), m);
      let+ inner' = HazelJitRun.with_model(inner, m');
      {
        ...p,
        term: Projector(data, inner'),
      };
    | _ => None
    }
  );
};

/* why the last patch was not made, for the check's mark */
let refused = ref("");

let no = (why: string): option('a) => {
  refused := why;
  None;
};

/* [s] with literal [l] given [lit]'s value: in each record from [l] up to the
   root, the program's term and its elaboration. [projector]: the use whose
   model holds [l], whose elaborated node holds the model inside its expansion */
let patch_one =
    (~projector: Id.t, s: CachedStatics.t, (l, lit): (Id.t, Exp.t))
    : option(CachedStatics.t) => {
  OptUtil.Syntax.(
    switch (Statics.Map.lookup(l, s.info_map)) {
    | Some(InfoExp(li)) when List.mem(projector, li.ancestors) =>
      let chain = [l, ...li.ancestors];
      let on =
        List.fold_left(
          (m, id) => Id.Map.add(id, (), m),
          Id.Map.empty,
          chain,
        );
      /* a node on the path, or one statics has no record of (a cast or other
         node the elaboration put in), which may stand between two that are */
      let path = id => Id.Map.mem(id, on) || !Id.Map.mem(id, s.info_map);
      let user = along(~path, ~target=l, set_literal(lit));
      let elab_above = along(~path, ~target=projector, swap_model(l, lit));
      let elab_inside = along(~path=_ => true, ~target=l, set_literal(lit));
      let* info_map =
        List.fold_left(
          (acc, id) => {
            let* m = acc;
            switch (Statics.Map.lookup(id, m)) {
            | Some(InfoExp(i)) =>
              let* user_term =
                switch (user(i.user_term)) {
                | Some(t) => Some(t)
                | None => no("a record's term")
                };
              let* elab_term =
                switch (
                  List.mem(projector, i.ancestors)
                    ? elab_inside(i.elab_term) : elab_above(i.elab_term)
                ) {
                | Some(t) => Some(t)
                | None => no("a record's elaborated term")
                };
              Some(
                Id.Map.add(
                  id,
                  Info.InfoExp({
                    ...i,
                    user_term,
                    elab_term,
                  }),
                  m,
                ),
              );
            | Some(_) => no("a record of another sort on the path")
            | None => Some(m)
            };
          },
          Some(s.info_map),
          chain,
        );
      let* term =
        switch (user(s.term)) {
        | Some(t) => Some(t)
        | None => no("the program's term")
        };
      let+ elaborated =
        switch (elab_above(s.elaborated)) {
        | Some(t) => Some(t)
        | None => no("the program's elaboration")
        };
      {
        ...s,
        term,
        elaborated,
        info_map,
      };
    | Some(InfoExp(_)) => no("the literal not in the use")
    | _ => no("no record of the literal")
    }
  );
};

/* every literal in [changes], each in turn, or None */
let patch =
    (~projector: Id.t, s: CachedStatics.t, changes: list((Id.t, Exp.t)))
    : option(CachedStatics.t) =>
  List.fold_left(
    (acc, change) => Option.bind(acc, s => patch_one(~projector, s, change)),
    Some(s),
    changes,
  )
  |> Option.map((s: CachedStatics.t) =>
       {
         ...s,
         completion: None,
       }
     );

/* the last commit found to change only a model's literals: the zipper it
   made, the use's projector, and each literal's new value */
type note = {
  n_zipper: Zipper.t,
  n_projector: Id.t,
  n_changes: list((Id.t, Exp.t)),
};
let noted: ref(option(note)) = ref(None);

/* After a commit (Editor.Update.update): [old] the projector's syntax before,
   [seg] the syntax the commit wrote, [z] the zipper it made. Noted when the
   syntax stood bare (SetSyntax keeps it bare; a paren layer it put back would
   carry a new id) and the two differ only in literals of the use's model, with
   the same ids */
let note_commit =
    (~projector: Id.t, ~old: Segment.t, ~seg: Segment.t, z: Zipper.t): unit => {
  noted := None;
  if (Segment.unparenthesize(old) == old) {
    let written =
      seg
      |> Segment.unparenthesize
      |> Segment.trim_secondary(Right)
      |> Segment.trim_secondary(Left);
    switch (MakeTerm.for_projection(old), MakeTerm.for_projection(written)) {
    | (Some(Exp(o)), Some(Exp(n))) =>
      switch (literal_changes(o, n), model_of_use(o)) {
      | (Some([_, ..._] as changes), Some(model)) =>
        let inside = ids_in(model);
        if (List.for_all(((id, _)) => Id.Map.mem(id, inside), changes)) {
          noted :=
            Some({
              n_zipper: z,
              n_projector: projector,
              n_changes: changes,
            });
        };
      | _ => ()
      }
    | _ => ()
    };
  };
};

/* The statics for [z] patched from [s], when [z] is the zipper the noted
   commit made; the note is taken either way */
let patched =
    (~zipper: Zipper.t, s: CachedStatics.t): option(CachedStatics.t) =>
  switch (noted^) {
  | Some(n) when n.n_zipper === zipper =>
    noted := None;
    patch(~projector=n.n_projector, s, n.n_changes);
  | Some(_) =>
    noted := None;
    None;
  | None => None
  };
