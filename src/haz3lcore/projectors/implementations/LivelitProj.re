open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* Bumped on every optimistic-table change. ProjectorView's ViewCache keys
   livelit entries on this (like AppBridge.version for apps): the underlying
   syntax only reaches the cache key via statics_map identity, which the
   statics debounce holds stale on the commit frame — without this bump the
   event-time optimistic view would sit invisible until the deferred statics
   refresh. */
let optimistic_version: ref(int) = ref(0);

/* Commit-vs-ephemeral decision for an event-time update result: a model
   persists to the program text only when it is checkpointable (carries no
   captured environment); otherwise it lives solely in the optimistic table
   and the syntax commit is skipped (see opt_ephemeral below). */
let commit_decision =
    (new_model: TermBase.Exp.t)
    : [
        | `Commit
        | `Ephemeral
      ] =>
  MvuShape.is_checkpointable(new_model) ? `Commit : `Ephemeral;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  /* The statics at the projector's id describe the Projector node itself,
     and a slide's ^^livelit(...) invoke adds a Parens layer — look through
     both to find the application. */
  let rec strip_wrappers = (term: TermBase.Exp.term): TermBase.Exp.term =>
    switch (term) {
    | Parens({term, _})
    | Projector(_, {term, _}) => strip_wrappers(term)
    | term => term
    };

  let get_model = (info: info) =>
    switch (info.statics) {
    | Some(InfoExp({user_term, _})) =>
      switch (strip_wrappers(user_term.term)) {
      | Ap(_dir, {term: LivelitName(llname), _}, model) =>
        Some((llname, model))
      | _ => None
      }
    | _ => None
    };

  /* --- Splices in a livelit's model ---------------------------------
   *
   * A splice is a region of the CLIENT's program held inside the widget:
   * edited in place, and typed in the surrounding scope, since splices
   * are transparent to statics. An author opts a model field in by
   * PARENTHESIZING its value:
   *
   *     let init : Model = (pct=50, lo=(0), hi=(100));
   *
   * The parens are the durable marker, and they have to be, because a
   * splice piece prints as nothing but its content and the deck loads
   * from text -- so a splice cannot round-trip and `init` rebuilds it
   * from the parens on every load. The client's code stays INSIDE the
   * parens, which is what makes the rewrap idempotent.
   *
   * Only labeled fields whose value is exactly a parenthesized
   * expression are eligible. No invocation in the shipped deck matches,
   * so this is inert for every livelit that has not asked for it. */

  /* Split a segment into (leading secondary, core, trailing secondary). */
  let split_outer_secondary =
      (seg: Base.segment): (Base.segment, Base.segment, Base.segment) => {
    let (lead, rest) = Segment.take_while_secondary(seg);
    let (rev_trail, rev_core) =
      Segment.take_while_secondary(List.rev(rest));
    (lead, List.rev(rev_core), List.rev(rev_trail));
  };

  /* Split a field at its tuple-label separator, returning the prefix
   * through the "=" tile and the value pieces after it. */
  let split_at_label_sep =
      (field: Base.segment): option((Base.segment, Base.segment)) => {
    let rec go = (prefix, ps: Base.segment) =>
      switch (ps) {
      | [] => None
      | [Base.Tile({label: ["="], _}) as eq, ...rest] =>
        Some((List.rev([eq, ...prefix]), rest))
      | [p, ...rest] => go([p, ...prefix], rest)
      };
    go([], field);
  };

  let map_comma_groups =
      (f: Base.segment => Base.segment, seg: Base.segment): Base.segment =>
    Segment.split_at_commas(seg)
    |> Aba.map_a(f)
    |> Aba.join(Fun.id, p => [p])
    |> List.concat;

  /* A parenthesized expression: Convex on both sides. The application's
   * own argument tile carries the same ["(", ")"] label but a postfix
   * mold, so the nibs are what tell them apart. */
  let as_parens = (p: Base.piece): option((Base.tile, Base.segment)) =>
    switch (p) {
    | Tile(
        {
          label: ["(", ")"],
          mold: {nibs: ({shape: Convex, _}, {shape: Convex, _}), _},
          children: [inner],
          _,
        } as t,
      ) =>
      Some((t, inner))
    | _ => None
    };

  /* `lo=(0)` -> `lo=(<splice 0>)`. Returns None when the field is not
   * marked, or is already spliced. */
  let wrap_marked_field = (field: Base.segment): option(Base.segment) => {
    open OptUtil.Syntax;
    let* (label_prefix, value) = split_at_label_sep(field);
    let (lead, core, trail) = split_outer_secondary(value);
    let* (t, inner) =
      switch (core) {
      | [p] => as_parens(p)
      | _ => None
      };
    let (ilead, icore, itrail) = split_outer_secondary(inner);
    switch (icore) {
    | [Base.Splice(_)] => None /* idempotent: already spliced */
    | _ =>
      let inner' = ilead @ [Piece.mk_splice(icore)] @ itrail;
      Some(
        label_prefix
        @ lead
        @ [
          Base.Tile({
            ...t,
            children: [inner'],
          }),
        ]
        @ trail,
      );
    };
  };

  /* Rewrite the model tuple's marked fields. [seg] is the whole
   * invocation: the `^name` tile followed by the application's
   * argument tile. Returns None when nothing was marked, so an
   * unmarked livelit installs no syntax override at all. */
  let splice_marked_fields = (seg: Base.segment): option(Base.segment) => {
    let wrapped = ref(0);
    let wrap = (field: Base.segment) =>
      switch (wrap_marked_field(field)) {
      | Some(field') =>
        incr(wrapped);
        field';
      | None => field
      };
    /* `^name((a=1, b=(2)))` puts the tuple's own parens inside the
     * application's, so descend one layer when there is one. */
    let rewrite_arg = (arg: Base.segment): Base.segment =>
      switch (arg) {
      | [p] =>
        switch (as_parens(p)) {
        | Some((t, inner)) => [
            Base.Tile({
              ...t,
              children: [map_comma_groups(wrap, inner)],
            }),
          ]
        | None => map_comma_groups(wrap, arg)
        }
      | _ => map_comma_groups(wrap, arg)
      };
    let seg' =
      List.map(
        (p: Base.piece) =>
          switch (p) {
          | Tile({label: ["(", ")"], children: [arg], _} as t)
              when Option.is_none(as_parens(p)) =>
            Base.Tile({
              ...t,
              children: [rewrite_arg(arg)],
            })
          | _ => p
          },
        seg,
      );
    wrapped^ > 0 ? Some(seg') : None;
  };

  /* Does this field value hold a splice, possibly under parens? */
  let rec holds_splice = (e: TermBase.Exp.t): bool =>
    switch (e.term) {
    | Splice(_) => true
    | Parens(e') => holds_splice(e')
    | _ => false
    };

  let rec tuple_fields = (e: TermBase.Exp.t): list(TermBase.Exp.t) =>
    switch (e.term) {
    | Parens(inner) => tuple_fields(inner)
    | Tuple(xs) => xs
    | _ => []
    };

  /* The labels of the model's spliced fields. Empty for every livelit
     that did not mark a field, which is how the splice-aware paths below
     stay inert for the rest of the deck. */
  let spliced_field_labels = (model: TermBase.Exp.t): list(string) =>
    List.filter_map(
      x =>
        switch (Exp.match_tup_label(x)) {
        | Some((name, v)) when holds_splice(v) => Some(name)
        | _ => None
        },
      tuple_fields(model),
    );

  /* A spliced field is the CLIENT's code, so the livelit's own update may
     not overwrite it: on commit a spliced field keeps the term that is in
     the syntax, and only unspliced fields take the new value. That is what
     lets splices survive an interaction at all -- SetTerm re-attaches them
     by id (ExpToSegment.reuse_splices), which can only match ids the
     committed term still carries. */
  let preserve_spliced_fields =
      (~from as old_model: TermBase.Exp.t, new_model: TermBase.Exp.t)
      : TermBase.Exp.t => {
    let labels = spliced_field_labels(old_model);
    if (labels == []) {
      new_model;
    } else {
      let olds = tuple_fields(old_model);
      let old_named = name =>
        List.find_opt(
          x =>
            switch (Exp.match_tup_label(x)) {
            | Some((n, _)) => n == name
            | None => false
            },
          olds,
        );
      let keep = (x: TermBase.Exp.t) =>
        switch (Exp.match_tup_label(x)) {
        | Some((name, _)) when List.mem(name, labels) =>
          switch (old_named(name)) {
          | Some(o) => o
          | None => x
          }
        | _ => x
        };
      /* Rebuild through any parens layer, so the argument keeps the shape
         the application expects. */
      let rec go = (e: TermBase.Exp.t): TermBase.Exp.t =>
        switch (e.term) {
        | Parens(inner) => {
            ...e,
            term: Parens(go(inner)),
          }
        | Tuple(xs) => {
            ...e,
            term: Tuple(List.map(keep, xs)),
          }
        | _ => e
        };
      go(new_model);
    };
  };

  let init = (any: Language.Any.t, seg: Base.segment) =>
    switch (any) {
    | Exp({term: Ap(_dir, {term: LivelitName(_), _}, _), _})
    | Exp({
        term: Parens({term: Ap(_dir, {term: LivelitName(_), _}, _), _}),
        _,
      }) =>
      Some(((), splice_marked_fields(seg) |> Option.map(s => Syntax(s))))
    | _ => None
    };

  /* Shape analogue of last_good_view: if statics info or the livelit
     entry is transiently unavailable (mid-commit), falling back to the
     default inline shape would collapse a Block-sized placeholder and
     jump the layout. Reuse the last known shape instead. */
  let last_good_shape: Hashtbl.t(Id.t, ProjectorCore.Shape.t) =
    Hashtbl.create(16);

  /* Widen the author's declared shape by what its splices actually hold.
     The author sizes the widget for its controls; the client decides how
     long the code in a splice is, and only the editor knows that. Getting
     this wrong is not cosmetic: `.livelit > *:not(svg)` sets overflow
     hidden, so an undersized box does not overlap, it CLIPS -- the splice
     becomes invisible and unclickable while keyboard navigation still
     enters it, leaving an invisible caret. Over-reserving only wastes
     space, so err that way.

     Known gap: a splice whose content wraps to several rows still reports
     one row here, because the declared shape's vertical is the author's
     and promoting Inline to Block would move the text around them. A
     multi-line splice is clipped vertically. */
  let widen_for_splices =
      (info, splice_size: View.splice_size, shape: ProjectorCore.Shape.t) => {
    let extra =
      List.fold_left(
        (acc, s: Base.splice) => {
          let size: Util.Point.t = splice_size(s.id);
          acc + size.col;
        },
        0,
        Segment.direct_splices(info.syntax),
      );
    extra == 0
      ? shape
      : {
        ...shape,
        horizontal: shape.horizontal + extra,
      };
  };

  let placeholder = (_model, info, splice_size) => {
    let looked_up =
      switch (get_model(info), info.statics) {
      | (Some((llname, _)), Some(InfoExp(exp))) =>
        switch (Ctx.lookup_livelit(exp.ctx, llname)) {
        | Some(ll) => Some(ll.shape)
        | None => None
        }
      | _ => None
      };
    let shape =
      switch (looked_up) {
      | Some(shape) =>
        Hashtbl.replace(last_good_shape, info.id, shape);
        shape;
      | None =>
        switch (Hashtbl.find_opt(last_good_shape, info.id)) {
        | Some(shape) => shape
        | None => ProjectorCore.Shape.inline(32)
        }
      };
    widen_for_splices(info, splice_size, shape);
  };

  let replace_model_term =
      (updated_model_term: TermBase.Exp.t, start_term: TermBase.Any.t)
      : TermBase.Any.t =>
    switch (start_term) {
    | Exp({term: Ap(dir, name, _model), _} as rest) =>
      Exp({
        ...rest,
        term: Ap(dir, name, updated_model_term),
      })
    | Exp(
        {term: Parens({term: Ap(dir, name, _model), _} as inner), _} as rest,
      ) =>
      Exp({
        ...rest,
        term:
          Parens({
            ...inner,
            term: Ap(dir, name, updated_model_term),
          }),
      })
    | _ =>
      print_endline("Warning - LivelitProj.replace_model_term: not an Ap");
      start_term;
    };
  let splice_rows = (_, _, _) => Id.Map.empty;
  let update = (_model, _info, action) =>
    switch (action) {
    | _ => print_endline("Warning - LivelitProj.update: No action")
    };

  /* Absent when the projector isn't drawn at the code site (docked to the
     sidebar, or culled from the viewport) */
  /* Focus the container — but never steal focus from a control INSIDE
     the livelit's own GUI (a text input keeps focus across the click). */
  let focus_pointer = (id: Id.t) =>
    switch (JsUtil.get_elem_by_id_opt(Id.cls(id))) {
    | None => ()
    | Some(el) =>
      let inside =
        switch (
          Js_of_ocaml.Js.Opt.to_option(
            Js_of_ocaml.Dom_html.document##.activeElement,
          )
        ) {
        | Some(active) =>
          Js_of_ocaml.Js.to_bool(
            Js_of_ocaml.Js.Unsafe.meth_call(
              el,
              "contains",
              [|Js_of_ocaml.Js.Unsafe.inject(active)|],
            ),
          )
        | None => false
        };
      if (!inside) {
        el##focus;
      };
    };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: None,
    };

  /* Dynamics on: the view fold-in (Statics' Projector case) samples the
     live HTML of a user-defined livelit at this projector's id */
  let dynamics = true;
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;
  let context_actions = (_, _, ~splice as _) => [];

  /* The projector's sample stream carries both the view's HTML and the
     use's own value; the live view is the latest HTML-shaped sample.
     Samples are mid-run values — close_value, not strip_wrappers, so the
     HTML's handlers keep their definitions. */
  let live_html = (info: ProjectorBase.info): option(TermBase.Exp.t) =>
    switch (info.dynamics) {
    | None => None
    | Some({samples, _}) =>
      List.fold_left(
        (acc, s: Sample.t) => {
          let v = MvuShape.close_value(s.value);
          if (MvuShape.is_html(v)) {
            switch (acc) {
            | Some((best, _)) when best >= s.seq => acc
            | _ => Some((s.seq, v))
            };
          } else {
            acc;
          };
        },
        None,
        samples,
      )
      |> Option.map(snd)
    };

  /* Extract a member from the evaluated definition. A definition is a
     module; under Modules II it evaluates to a Module whose items are
     ModVal(x, v) bindings, read by name (the last binding wins, as for
     Dot). The labeled-tuple reading is kept for values that still arrive
     in that shape. Member order and helper count don't matter either way. */
  let record_field =
      (record: TermBase.Exp.t, label: string): option(TermBase.Exp.t) => {
    let record = MvuShape.strip_wrappers(record);
    switch (record.term) {
    | Module(items) =>
      List.fold_left(
        (acc, item: TermBase.Mod.t) =>
          switch (item.term) {
          | ModVal(x, v) when x == label => Some(v)
          | _ => acc
          },
        None,
        items,
      )
    | _ =>
      switch (MvuShape.of_tuple(record)) {
      | Some(fs) =>
        List.find_map(
          f =>
            switch (MvuShape.of_field(f)) {
            | Some((l, v)) when l == label => Some(v)
            | _ => None
            },
          fs,
        )
      | None => None
      }
    };
  };

  /* The latest sampled value at some id (e.g. the model argument) */
  let latest_value = (samples: list(Sample.t)): option(TermBase.Exp.t) =>
    List.fold_left(
      (acc, s: Sample.t) =>
        switch (acc) {
        | Some((best, _)) when best >= s.seq => acc
        | _ => Some((s.seq, s.value))
        },
      None,
      samples,
    )
    |> Option.map(((_, v)) => MvuShape.close_value(v));

  /* An action from a handler. With the model's live value in hand, commit
     the TRANSITION — ^name.update(model, action) — as the new argument:
     the main evaluation normalizes it, so the last interaction stays in the
     program where update's probes (and the stepper) can see it, and the
     next commit collapses it to its value again. Without a sampled model
     value (dynamics off), fall back to evaluating update here, at event
     time, in the builtin environment, and committing the result. */
  /* Optimistic rendering for discrete actions. The authoritative
     pipeline (SetSyntax -> statics -> worker eval -> fresh sample) takes
     on the order of a second; the next model and its view are computable
     at event time in milliseconds, since update/view evaluate in the
     builtin env. Each action commits to the syntax immediately (every
     discrete action is its own history step) while the widget renders
     the optimistic view NOW, with handlers bound to the optimistic model
     so rapid successive actions chain correctly. The entry yields to the
     authoritative sample when the sample's content converges with it —
     guaranteed while definitions are closed, because both sides evaluate
     the same view on the same model — and is dropped whenever the syntax
     model stops matching what we committed (external edit, undo). An
     update result that cannot be checkpointed skips the commit entirely
     and lives only here (see opt_ephemeral below). */
  type optimistic_entry = {
    opt_model: TermBase.Exp.t, /* the newest local model, already a value */
    opt_html: TermBase.Exp.t, /* view(opt_model), evaluated at event time */
    /* Squished prints of every syntax state that is legitimately "ours"
       while commits are in flight: the pre-burst base, then each commit,
       oldest first. During a rapid burst the syntax lags the local model
       by several commits, so renders may see ANY of these. Printed text
       is the identity: the syntax literally came from printing the
       committed terms, and structural comparison trips over
       print/reparse asymmetries (LivelitName vs Var, evaluated negative
       atoms vs unary minus). */
    opt_outstanding: list(string),
    /* Highest outstanding index observed in the syntax; a later render
       matching an EARLIER index means the syntax rewound (undo), which
       drops the entry. */
    mutable opt_matched: int,
    /* Transient (gesture) updates or an uncommittable model have changed
       the model since the last commit; the next committing event must
       flush even if its own update is a no-op. */
    opt_dirty: bool,
    /* The model is not checkpointable, so it never committed: this entry
       is the state's only home. Never yield it to sample convergence
       (samples keep coming from the unchanged stale syntax); undo and
       external edits still drop it — inherent, nothing to restore from. */
    opt_ephemeral: bool,
  };
  let optimistic: Hashtbl.t(Id.t, optimistic_entry) = Hashtbl.create(16);

  /* One console warning per projector id: an unserializable model is a
     standing property of the definition, not per-event news. */
  let warned_ephemeral: Hashtbl.t(Id.t, unit) = Hashtbl.create(4);
  let warn_ephemeral = (~id: Id.t, ~ll_name: string) =>
    if (!Hashtbl.mem(warned_ephemeral, id)) {
      Hashtbl.add(warned_ephemeral, id, ());
      Js_of_ocaml.Firebug.console##warn(
        Js_of_ocaml.Js.string(
          "livelit ^"
          ++ ll_name
          ++ ": model is not serializable; state will not persist to the program text",
        ),
      );
    };

  let squish = str =>
    String.to_seq(str)
    |> Seq.filter(c => c != ' ' && c != '\n' && c != '\t')
    |> String.of_seq;

  let event_inject =
      (
        ~id: Id.t,
        ~print_term: TermBase.Exp.t => string,
        ~ll_name: string,
        ~def_elab: TermBase.Exp.t,
        ~model: TermBase.Exp.t,
        ~model_value: option(TermBase.Exp.t),
        ~commit_model: TermBase.Exp.t => Ui_effect.t(unit),
        ~repaint: unit => Ui_effect.t(unit),
        gesture: HazelDOM.gesture,
        action: TermBase.Exp.t,
      )
      : Ui_effect.t(unit) => {
    let fail = msg => {
      print_endline("LivelitProj: " ++ msg);
      Ui_effect.Ignore;
    };
    let ap = IdTagged.FreshGrammar.Exp.ap;
    /* Base model for this action. The optimistic entry wins over the
       handler's captured model: a rapid successor event fires from a DOM
       still showing the PREVIOUS render, whose handlers close over the
       pre-commit model — composing from there would silently stomp the
       in-flight action. The optimistic table is the newest local truth. */
    let base_value =
      switch (Hashtbl.find_opt(optimistic, id)) {
      | Some(e) => Some(e.opt_model)
      | None => model_value
      };
    let base = Option.value(base_value, ~default=model);
    /* What goes in the syntax: the update redex when the base model is a
       committable value (keeps the interaction visible to probes and the
       stepper), independent of whether the optimistic path succeeds. */
    let redex =
      switch (base_value) {
      | Some(mv)
          /* A spliced model cannot go through the redex: the redex is a
             fresh `^name.update((value, action))` term carrying no Splice
             nodes, so committing it would drop the client's code. Such a
             model commits the merged term instead (see commit_model). */
          when
            spliced_field_labels(model) == []
            && MvuShape.is_checkpointable(mv)
            && MvuShape.is_checkpointable(action) =>
        Some(
          UserLivelit.mk_update_redex(
            ~name=ll_name,
            ~model_value=mv,
            ~action,
          ),
        )
      | _ => None
      };
    /* ~committed=None: an ephemeral store — the syntax is not changing
       (uncommittable model), so like a transient event it leaves the
       set of "ours" syntax states alone. */
    let store_entry = (new_model, record, ~committed) =>
      switch (record_field(record, "view")) {
      | Some(view_fn) =>
        switch (MvuShape.safe_evaluate(ap(Forward, view_fn, new_model))) {
        | Ok(html) when MvuShape.is_html(html) =>
          let prior = Hashtbl.find_opt(optimistic, id);
          /* Transient and ephemeral events change nothing in the syntax,
             so the set of syntax states that count as "ours" is
             unchanged; only committing events append their commit. */
          let outstanding =
            switch (committed, gesture, prior) {
            | (None, _, Some(e))
            | (Some(_), HazelDOM.Transient, Some(e)) => e.opt_outstanding
            | (None, _, None)
            | (Some(_), HazelDOM.Transient, None) => [
                squish(print_term(model)),
              ]
            | (Some(c), HazelDOM.Commit, Some(e)) =>
              e.opt_outstanding @ [squish(print_term(c))]
            | (Some(c), HazelDOM.Commit, None) => [
                squish(print_term(model)),
                squish(print_term(c)),
              ]
            };
          /* cap the ring; a burst outrunning this many in-flight
             commits falls back to the authoritative path */
          let outstanding = {
            let n = List.length(outstanding);
            n > 64
              ? List.filteri((i, _) => i >= n - 64, outstanding)
              : outstanding;
          };
          incr(optimistic_version);
          Hashtbl.replace(
            optimistic,
            id,
            {
              opt_model: new_model,
              opt_html: html,
              opt_outstanding: outstanding,
              opt_matched:
                switch (prior) {
                | Some(e) => e.opt_matched
                | None => 0
                },
              opt_dirty:
                Option.is_none(committed) || gesture == HazelDOM.Transient,
              opt_ephemeral: Option.is_none(committed),
            },
          );
        | _ =>
          if (Hashtbl.mem(optimistic, id)) {
            incr(optimistic_version);
            Hashtbl.remove(optimistic, id);
          }
        }
      | None => ()
      };
    /* Event-time evaluation of the next model (and, best-effort, its
       view for the optimistic entry). `Skip: the update was a no-op, so
       neither commit nor store — without this, the click the browser
       fires after every drag (and any handler returning the model
       unchanged) would pollute history with identity steps. A committing
       no-op still flushes when transient updates left the entry dirty. */
    let next_model =
      switch (MvuShape.safe_evaluate(def_elab)) {
      | Error(e) => `Error("definition error: " ++ e)
      | Ok(record) =>
        switch (record_field(record, "update")) {
        | None => `Error("definition is missing update")
        | Some(update_fn) =>
          let applied =
            ap(
              Forward,
              update_fn,
              IdTagged.FreshGrammar.Exp.tuple([base, action]),
            );
          switch (MvuShape.safe_evaluate(applied)) {
          | Error(e) => `Error("update error: " ++ e)
          | Ok(new_model) when commit_decision(new_model) == `Ephemeral =>
            /* The model carries a closure, so it cannot live in the
               syntax tree. Degrade gracefully instead of wedging: keep
               the widget running off the optimistic entry and skip the
               syntax commit (including the redex — its value could not
               persist either). Warned once; undo/external edits drop
               the ephemeral state. */
            warn_ephemeral(~id, ~ll_name);
            store_entry(new_model, record, ~committed=None);
            `Ephemeral;
          | Ok(new_model) =>
            let unchanged =
              squish(print_term(new_model)) == squish(print_term(base));
            let dirty_prior =
              switch (Hashtbl.find_opt(optimistic, id)) {
              | Some(e) => e.opt_dirty
              | None => false
              };
            if (unchanged && (gesture == HazelDOM.Transient || !dirty_prior)) {
              `Skip;
            } else {
              let committed =
                switch (redex) {
                | Some(r) => r
                | None => new_model
                };
              store_entry(new_model, record, ~committed=Some(committed));
              `Ok(committed);
            };
          };
        }
      };
    switch (gesture, next_model) {
    | (_, `Skip) => Ui_effect.Ignore
    | (_, `Ephemeral) =>
      /* Nothing committed; repaint so the optimistic view shows. */
      repaint()
    | (Transient, `Ok(_)) =>
      /* Live preview only: the optimistic entry above is the whole
         effect; a quiet non-historic action makes the frame repaint. */
      repaint()
    | (Transient, `Error(e)) => fail(e)
    | (Commit, `Ok(committed)) => commit_model(committed)
    | (Commit, `Error(_)) when Option.is_some(redex) =>
      /* The redex commit does not need the event-time evaluation to have
         succeeded (e.g. a definition that is not closed still works via
         the program's own evaluation). */
      commit_model(Option.get(redex))
    | (Commit, `Error(e)) => fail(e)
    };
  };

  /* User-defined livelit: render the view, a Hazel HTML value, via
     HazelDOM. Preferred source is the live sample the view fold-in recorded
     during the main evaluation; without one (dynamics off, or not yet
     evaluated) the captured definition is evaluated here instead, which
     requires it to be closed. Actions run through update and commit to the
     syntax, so each use's model lives in its own Ap argument, like builtin
     livelits. */
  /* Last successfully rendered view per projector instance. After a
     commit, the syntax model is briefly an unevaluated update-transition
     that the render-time fallback cannot resolve (its ^name reference is
     free in the builtin env), so until the main evaluation delivers a
     fresh sample the view would flash an error. Instead, show the last
     good render, dimmed and inert (its handlers close over the stale
     model, so letting clicks through could silently drop the in-flight
     edit). Display-only cache; entries overwrite on every successful
     render. */
  let last_good_view: Hashtbl.t(Id.t, Node.t) = Hashtbl.create(16);

  let user_view =
      (
        ~id: Id.t,
        ~print_term: TermBase.Exp.t => string,
        ~ll_name: string,
        ~def_elab: TermBase.Exp.t,
        ~model: TermBase.Exp.t,
        ~model_value: option(TermBase.Exp.t),
        ~commit_model: TermBase.Exp.t => Ui_effect.t(unit),
        ~repaint: unit => Ui_effect.t(unit),
        ~view_term: TermBase.Exp.t => Node.t,
        ~splice_view_at: int => option(Node.t),
        ~live: option(TermBase.Exp.t),
      )
      : Node.t => {
    let err = msg =>
      switch (Hashtbl.find_opt(last_good_view, id)) {
      | Some(node) =>
        Node.div(~attrs=[Attr.classes(["livelit-pending"])], [node])
      | None =>
        Node.div(
          ~attrs=[Attr.classes(["livelit-user-error"])],
          [Node.text(msg)],
        )
      };
    let ok = (node: Node.t): Node.t => {
      Hashtbl.replace(last_good_view, id, node);
      node;
    };
    let seed = (~model, ~model_value): HazelDOM.t => {
      inject:
        event_inject(
          ~id,
          ~print_term,
          ~ll_name,
          ~def_elab,
          ~model,
          ~model_value,
          ~commit_model,
          ~repaint,
        ),
      view_term,
      splice_view: splice_view_at,
      commit: HazelDOM.State,
    };
    /* Optimistic entry: render it (interactive, full brightness) until
       the authoritative sample content-converges with it or the syntax
       model stops matching what we committed (external edit / undo). */
    let opt =
      switch (Hashtbl.find_opt(optimistic, id)) {
      | None => None
      | Some(entry) =>
        /* An ephemeral entry never converges: no commit is in flight, so
           the sample forever reflects the stale syntax — yielding to it
           would silently revert the state. */
        let converged =
          !entry.opt_ephemeral
          && (
            switch (live) {
            | Some(l) => Exp.fast_equal(l, entry.opt_html)
            | None => false
            }
          );
        let model_print = squish(print_term(model));
        let idx = {
          let rec find = (i, xs) =>
            switch (xs) {
            | [] => None
            | [x, ..._] when x == model_print => Some(i)
            | [_, ...rest] => find(i + 1, rest)
            };
          find(0, entry.opt_outstanding);
        };
        let drop = () => {
          incr(optimistic_version);
          Hashtbl.remove(optimistic, id);
          None;
        };
        switch (idx) {
        | _ when converged => drop()
        | None =>
          /* syntax shows something we never committed: external edit */
          drop()
        | Some(i) when i < entry.opt_matched =>
          /* syntax rewound to an earlier state: undo */
          drop()
        | Some(i) =>
          entry.opt_matched = i;
          Some(entry);
        };
      };
    switch (opt, live) {
    | (Some(entry), _) =>
      ok(
        HazelDOM.go(
          seed(~model=entry.opt_model, ~model_value=Some(entry.opt_model)),
          entry.opt_html,
        ),
      )
    | (None, Some(html)) =>
      ok(HazelDOM.go(seed(~model, ~model_value), html))
    | (None, None) =>
      let ap = IdTagged.FreshGrammar.Exp.ap;
      switch (MvuShape.safe_evaluate(def_elab)) {
      | Error(e) => err("livelit definition error: " ++ e)
      | Ok(record) =>
        switch (record_field(record, "view")) {
        | None => err("livelit definition is missing view")
        | Some(view_fn) =>
          switch (MvuShape.safe_evaluate(ap(Forward, view_fn, model))) {
          | Error(e) => err("livelit view error: " ++ e)
          | Ok(html) when MvuShape.is_html(html) =>
            ok(HazelDOM.go(seed(~model, ~model_value), html))
          | Ok(_) => err("livelit view did not produce HTML")
          }
        }
      };
    };
  };

  let view =
      (
        {info, parent, local_quiet, view_seg, splices, splice_view, _}:
          View.args(model, action),
      ) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };

    let node =
      switch (get_model(info)) {
      | Some((ll_name, model)) =>
        let ll = Ctx.lookup_livelit(ctx, ll_name);

        /* Write an updated model back into the Ap's argument position.
           A model with no splices keeps the original SetSyntax path.
           A model WITH splices cannot: SetSyntax reprints the projector's
           whole segment, and a splice prints as nothing but its content,
           so the client's code would be flattened into the model on the
           first interaction. SetTerm regenerates the segment from the
           term and re-attaches splices by id, which works only because
           `preserve_spliced_fields` kept those nodes in what we commit.
           The caret is not preserved on either path -- SetTerm rebuilds
           the zipper from the root -- so a widget action still evicts the
           caret from a splice being edited. */
        let commit_model = (new_model: TermBase.Exp.t) =>
          switch (spliced_field_labels(model)) {
          | [] =>
            let updated_segment =
              info.utility.lift_syntax(
                ~inline=true,
                replace_model_term(new_model),
                info.syntax,
              );
            switch (updated_segment) {
            | Some(s) => parent(SetSyntax(s))
            | None =>
              print_endline("Warning - LivelitProj.view: lift_syntax failed");
              Ui_effect.Ignore;
            };
          | [_, ..._] =>
            let merged = preserve_spliced_fields(~from=model, new_model);
            switch (info.utility.seg_to_term(info.syntax)) {
            | Some(t) =>
              parent(SetTerm(replace_model_term(merged, t), true))
            | None =>
              print_endline("Warning - LivelitProj.view: seg_to_term failed");
              Ui_effect.Ignore;
            };
          };

        switch (ll) {
        | Some({user_def: Some(def_elab), _}) =>
          let view_term = term =>
            Exp(term)
            |> info.utility.term_to_seg(~inline=true)
            |> view_seg(~background=false, Exp);
          let model_value =
            Option.bind(info.dynamics_at(Exp.rep_id(model)), latest_value);
          Node.div(
            ~attrs=[
              Attr.classes([ll_name, "user-livelit"]),
              Attr.id(Id.cls(info.id)),
            ],
            [
              user_view(
                ~id=info.id,
                ~ll_name,
                ~print_term=
                  term =>
                    /* Model terms contain no projectors or refractors, so
                       trivial handlers suffice (the real ones live above
                       this module in the dependency order). */
                    Segment.to_string(
                      ~refractor_seg_to_seg=(rs, seg) => (rs, seg),
                      ~projector_to_segment=_ => [],
                      info.utility.term_to_seg(~inline=true, Exp(term)),
                    ),
                ~def_elab,
                ~model,
                ~model_value,
                ~commit_model,
                /* Non-historic Layout-level no-op: repaints the frame so a
                   transient (drag) update becomes visible without an edit. */
                ~repaint=() => local_quiet(),
                ~view_term,
                /* Positional access to this livelit's own splices, in
                   document order, for `Html.splice(n)` in the user's view. */
                ~splice_view_at=
                  i =>
                    List.nth_opt(splices, i)
                    |> Option.map((s: Base.splice) => splice_view(s.id)),
                ~live=live_html(info),
              ),
            ],
          );
        | Some(ll) =>
          let action_callback = (action: LivelitCtx.action_exp) =>
            commit_model(ll.update(action, model));

          let list_contents = ll.view(model, action_callback);
          Node.div(
            ~attrs=[Attr.class_(ll_name), Attr.id(Id.cls(info.id))],
            [list_contents],
          );
        | None =>
          print_endline("Warning - LivelitProj.view: not found in context");
          Node.text("No livelit found");
        };
      | None =>
        print_endline("Warning - LivelitProj.view: get is empty");
        Node.text("No livelit found");
      };

    View.mk(node);
  };
};
