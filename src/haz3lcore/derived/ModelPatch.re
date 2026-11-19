/* (^ticket(721)) A livelit's commit that changes only its model's literals.

   After a step of a livelit's slider the page writes the new model into the
   program (LivelitProj.commit_model), and the program's statics are worked out
   again for it: re-analyzing the edited top-level item took 12 to 28 ms of the
   edit on the Witch slide (run 42826692242). But a change to a model's
   literals can change the program's values, not its types. So when the commit
   keeps the model's ids (ModelIds.same_shape_ids) and changes only literals
   inside the model, the statics are worked out as usual (DefStatics), except
   that the edited item is not analyzed again: it is the item it was, patched
   ([item_patch]). Its node is the new one; each record from a literal up to
   the item's root takes its term from the new node and its elaborated term
   from the item's elaboration, which has the literal's new value inside the
   use's expansion (HazelJitRun.model_of). DefStatics then puts the program
   together from its items by its own incremental step, so the statics, their
   elaboration and the worker's request, split along those items (EvalSplit),
   all agree, as after an analysis. Each term is copied only along the path to
   the literal; everything off it stays the very same object.

   Patching the statics after DefStatics instead (build 44) left them
   matching no analysis of DefStatics', and the worker's request went whole,
   about 70 ms more (run 42838899176). Not brought up to date: each record's
   derived inspector message and label inference, which hold no literal's
   value the page shows. Anything that does not line up answers None, and the
   item is analyzed, as before. */
open Util;
open Language;

let keep = (_, x) => x;

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

exception Found(Exp.t);

/* the node of id [target] in [e], down the same path [along] walks, the very
   object, with nothing copied */
let find_along =
    (~path: Id.t => bool, ~target: Id.t, e: Exp.t): option(Exp.t) => {
  let root = ref(true);
  let go = (cont, x: Exp.t) =>
    if (Exp.rep_id(x) == target) {
      raise(Found(x));
    } else if (root^ || path(Exp.rep_id(x))) {
      root := false;
      cont(x);
    } else {
      x;
    };
  switch (
    Exp.map_term(
      ~f_exp=go,
      ~f_pat=keep,
      ~f_typ=keep,
      ~f_tpat=keep,
      ~f_sig=keep,
      ~f_mpat=keep,
      e,
    )
  ) {
  | exception (Found(x)) => Some(x)
  | _ => None
  };
};

/* a literal node given [lit]'s value */
let set_literal = (lit: Exp.t, x: Exp.t): option(Exp.t) =>
  switch (x.term, lit.term) {
  | (Atom(a), Atom(b)) when ModelIds.same_class(a, b) =>
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
        term: (Projector(data, inner'): Exp.term),
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

/* a commit's literal changes: the ids of the literals it changed, and the
   use whose model holds them, when there is one (a livelit's; without one, as
   in the tests, the literal stands in the elaboration as itself). Each new
   value is the new node's own, with the spelling it was parsed with */
type changes = {
  c_projector: option(Id.t),
  c_literals: list(Id.t),
};

/* item [q] of DefStatics, as it was, with [n] its new node: patched, and the
   keys of its map that changed, or None. A plain item only (no members,
   nothing nested), whose map holds every changed literal */
let item_patch =
    (c: changes, q: DefStatics.item, n: Exp.t)
    : option((DefStatics.item, list(Id.t))) => {
  open OptUtil.Syntax;
  let* () =
    q.d_members == [] && q.d_nest == None && Exp.rep_id(n) == q.d_id
      ? Some() : no("not a plain item");
  let* () =
    List.for_all(l => Id.Map.mem(l, q.d_map), c.c_literals) ? Some() : None; /* another item's literals: analyzed as usual */
  /* the hollow the item was analyzed as (its body a hole), made new */
  let* root =
    switch (Statics.Map.lookup_exp(q.d_id, q.d_map)) {
    | Some(r) => Some(r)
    | None => no("no record of the item's root")
    };
  let* hollow =
    switch (DefStatics.split_body(n), DefStatics.split_body(root.user_term)) {
    | (Some((_, rebuild)), Some((hole, _))) =>
      Some({
        ...n,
        term: rebuild(hole),
      })
    | (None, None) => Some(n) /* the trailing expression: as itself */
    | _ => no("the item's hollow")
    };
  let patch_one = (acc, l) => {
    let* (map, elab, keys) = acc;
    let* li =
      switch (Statics.Map.lookup_exp(l, map)) {
      | Some(li) => Some(li)
      | None => no("no record of the literal")
      };
    let chain = [l, ...li.ancestors];
    let on =
      List.fold_left((m, id) => Id.Map.add(id, (), m), Id.Map.empty, chain);
    /* a node on the path, or one statics has no record of (a cast or other
       node the elaboration put in), which may stand between two that are */
    let path = id => Id.Map.mem(id, on) || !Id.Map.mem(id, map);
    let* lit =
      switch (find_along(~path, ~target=l, hollow)) {
      | Some(lit) => Some(lit)
      | None => no("the literal in the new node")
      };
    let inside = (i: Info.exp) =>
      switch (c.c_projector) {
      | Some(p) => List.mem(p, i.ancestors)
      | None => false
      };
    let patch_elab = e =>
      switch (c.c_projector) {
      | Some(p) when List.mem(p, chain) =>
        along(~path, ~target=p, swap_model(l, lit), e)
      | Some(_) => None
      | None => along(~path, ~target=l, set_literal(lit), e)
      };
    let* elab' =
      switch (patch_elab(elab)) {
      | Some(e) => Some(e)
      | None => no("the item's elaboration")
      };
    List.fold_left(
      (acc, id) => {
        let* (map, keys) = acc;
        switch (Statics.Map.lookup(id, map)) {
        | Some(Info.InfoExp(i) as old) =>
          let* user_term =
            switch (find_along(~path, ~target=id, hollow)) {
            | Some(t) => Some(t)
            | None => no("a record's term in the new node")
            };
          let* elab_term =
            switch (find_along(~path, ~target=id, elab)) {
            | Some(shared) when shared === i.elab_term =>
              find_along(~path, ~target=id, elab')
            | _ =>
              inside(i)
                ? along(
                    ~path=_ => true,
                    ~target=l,
                    set_literal(lit),
                    i.elab_term,
                  )
                : patch_elab(i.elab_term)
            };
          let info =
            Info.InfoExp({
              ...i,
              user_term,
              elab_term,
            });
          /* every key filed under the old record */
          let ids =
            List.filter(
              k =>
                switch (Statics.Map.lookup(k, map)) {
                | Some(x) => x === old
                | None => false
                },
              i.user_term.annotation.ids,
            );
          Some((
            List.fold_left((m, k) => Id.Map.add(k, info, m), map, ids),
            ids @ keys,
          ));
        | Some(_) => no("a record of another sort on the path")
        | None => Some((map, keys))
        };
      },
      Some((map, keys)),
      chain,
    )
    |> Option.map(((map, keys)) => (map, elab', keys));
  };
  let+ (map, elab, keys) =
    List.fold_left(patch_one, Some((q.d_map, q.d_elab, [])), c.c_literals);
  (
    {
      ...q,
      d_node: n,
      d_map: map,
      d_elab: elab,
    },
    List.sort_uniq(compare, keys),
  );
};

/* the last commit found to change only a model's literals: the zipper it
   made, and its changes */
type note = {
  n_zipper: Zipper.t,
  n_changes: changes,
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
      switch (ModelIds.literal_changes(o, n), model_of_use(o)) {
      | (Some([_, ..._] as literals), Some(model)) =>
        let inside = ids_in(model);
        if (List.for_all(((id, _)) => Id.Map.mem(id, inside), literals)) {
          noted :=
            Some({
              n_zipper: z,
              n_changes: {
                c_projector: Some(projector),
                c_literals: List.map(fst, literals),
              },
            });
        };
      | _ => ()
      }
    | _ => ()
    };
  };
};

/* the noted commit's changes, when [zipper] is the zipper it made; the note is
   taken either way */
let take = (~zipper: Zipper.t): option(changes) =>
  switch (noted^) {
  | Some(n) when n.n_zipper === zipper =>
    noted := None;
    Some(n.n_changes);
  | Some(_) =>
    noted := None;
    None;
  | None => None
  };

/* [f] (the statics worked out, by DefStatics) with the items that hold [c]'s
   literals patched rather than analyzed */
let with_item_patch = (c: changes, f: unit => 'a): 'a => {
  refused := "";
  DefStatics.items_patched := 0;
  DefStatics.item_patch := Some(item_patch(c));
  switch (f()) {
  | x =>
    DefStatics.item_patch := None;
    x;
  | exception e =>
    DefStatics.item_patch := None;
    raise(e);
  };
};
