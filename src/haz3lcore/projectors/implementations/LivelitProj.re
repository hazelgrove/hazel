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

  let init = (any: Language.Any.t) =>
    switch (any) {
    | Exp({term: Ap(_dir, {term: LivelitName(_), _}, _), _})
    | Exp({
        term: Parens({term: Ap(_dir, {term: LivelitName(_), _}, _), _}),
        _,
      }) =>
      Some()
    | _ => None
    };

  /* Shape analogue of last_good_view: if statics info or the livelit
     entry is transiently unavailable (mid-commit), falling back to the
     default inline shape would collapse a Block-sized placeholder and
     jump the layout. Reuse the last known shape instead. */
  let last_good_shape: Hashtbl.t(Id.t, ProjectorCore.Shape.t) =
    Hashtbl.create(16);

  let placeholder = (_model, info) => {
    let looked_up =
      switch (get_model(info), info.statics) {
      | (Some((llname, _)), Some(InfoExp(exp))) =>
        switch (Ctx.lookup_livelit(exp.ctx, llname)) {
        | Some(ll) => Some(ll.size)
        | None => None
        }
      | _ => None
      };
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
  let update = (_model, _info, action) =>
    switch (action) {
    | _ => print_endline("Warning - LivelitProj.update: No action")
    };

  /* Absent when the projector isn't drawn at the code site (docked to the
     sidebar, or culled from the viewport) */
  let focus_pointer = (id: Id.t) =>
    switch (JsUtil.get_elem_by_id_opt(Id.cls(id))) {
    | None => ()
    | Some(el) => el##focus
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

  /* Extract a member from the evaluated definition record: by label when
     labeled (modules desugar to labeled tuples, any size); positional only
     for a plain (init, update, view, expand) tuple */
  let record_field =
      (record: TermBase.Exp.t, label: string, index: int)
      : option(TermBase.Exp.t) =>
    switch (MvuShape.of_tuple(MvuShape.strip_wrappers(record))) {
    | Some(fs) =>
      switch (
        List.find_map(
          f =>
            switch (MvuShape.of_field(f)) {
            | Some((l, v)) when l == label => Some(v)
            | _ => None
            },
          fs,
        )
      ) {
      | Some(v) => Some(v)
      | None when List.length(fs) >= 4 =>
        let f = List.nth(fs, index);
        switch (MvuShape.of_field(f)) {
        | Some((_, v)) => Some(v)
        | None => Some(f)
        };
      | None => None
      }
    | None => None
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
     model stops matching what we committed (external edit, undo). */
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
  };
  let optimistic: Hashtbl.t(Id.t, optimistic_entry) = Hashtbl.create(16);

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
    /* What goes in the syntax: the update redex when the base model is a
       committable value (keeps the interaction visible to probes and the
       stepper), independent of whether the optimistic path succeeds. */
    let redex =
      switch (base_value) {
      | Some(mv)
          when
            MvuShape.is_checkpointable(mv)
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
    /* Event-time evaluation of the next model (and, best-effort, its
       view for the optimistic entry). */
    let next_model =
      switch (MvuShape.safe_evaluate(def_elab)) {
      | Error(e) => Error("definition error: " ++ e)
      | Ok(record) =>
        switch (record_field(record, "update", 1)) {
        | None => Error("definition is missing update")
        | Some(update_fn) =>
          let base = Option.value(base_value, ~default=model);
          let applied =
            ap(
              Forward,
              update_fn,
              IdTagged.FreshGrammar.Exp.tuple([base, action]),
            );
          switch (MvuShape.safe_evaluate(applied)) {
          | Error(e) => Error("update error: " ++ e)
          | Ok(new_model) when !MvuShape.is_checkpointable(new_model) =>
            /* the model persists in the syntax tree, so it must be
               closure-free */
            Error("update produced an uncommittable model")
          | Ok(new_model) =>
            let committed =
              switch (redex) {
              | Some(r) => r
              | None => new_model
              };
            switch (record_field(record, "view", 2)) {
            | Some(view_fn) =>
              switch (
                MvuShape.safe_evaluate(ap(Forward, view_fn, new_model))
              ) {
              | Ok(html) when MvuShape.is_html(html) =>
                let prior = Hashtbl.find_opt(optimistic, id);
                let outstanding =
                  switch (prior) {
                  | Some(e) =>
                    e.opt_outstanding @ [squish(print_term(committed))]
                  | None => [
                      squish(print_term(model)),
                      squish(print_term(committed)),
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
            Ok(committed);
          };
        }
      };
    switch (next_model, redex) {
    | (Ok(committed), _) => commit_model(committed)
    | (Error(_), Some(r)) =>
      /* The redex commit does not need the event-time evaluation to have
         succeeded (e.g. a definition that is not closed still works via
         the program's own evaluation). */
      commit_model(r)
    | (Error(e), None) => fail(e)
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
        ~view_term: TermBase.Exp.t => Node.t,
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
        ),
      view_term,
      commit: HazelDOM.State,
    };
    /* Optimistic entry: render it (interactive, full brightness) until
       the authoritative sample content-converges with it or the syntax
       model stops matching what we committed (external edit / undo). */
    let opt =
      switch (Hashtbl.find_opt(optimistic, id)) {
      | None => None
      | Some(entry) =>
        let converged =
          switch (live) {
          | Some(l) => Exp.fast_equal(l, entry.opt_html)
          | None => false
          };
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
        switch (record_field(record, "view", 2)) {
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

  let view = ({info, parent, view_seg, _}: View.args(model, action)) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };

    let node =
      switch (get_model(info)) {
      | Some((ll_name, model)) =>
        let ll = Ctx.lookup_livelit(ctx, ll_name);

        /* Write an updated model back into the Ap's argument position */
        let commit_model = (new_model: TermBase.Exp.t) => {
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
                ~ll_name,
                ~def_elab,
                ~model,
                ~model_value,
                ~commit_model,
                ~view_term,
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
