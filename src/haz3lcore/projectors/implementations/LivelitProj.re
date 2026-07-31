open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

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

  let placeholder = (_model, info) => {
    switch (get_model(info), info.statics) {
    | (Some((llname, _)), Some(InfoExp(exp))) =>
      /* Get the livelit size */
      switch (Ctx.lookup_livelit(exp.ctx, llname)) {
      | Some(ll) => ll.size
      | None =>
        /* Default size */
        ProjectorCore.Shape.inline(32)
      }
    | _ =>
      /* Default size */
      ProjectorCore.Shape.inline(32)
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
  let event_inject =
      (
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
    switch (model_value) {
    | Some(mv)
        when
          MvuShape.is_checkpointable(mv)
          && MvuShape.is_checkpointable(action) =>
      commit_model(
        UserLivelit.mk_update_redex(~name=ll_name, ~model_value=mv, ~action),
      )
    | _ =>
      let ap = IdTagged.FreshGrammar.Exp.ap;
      switch (MvuShape.safe_evaluate(def_elab)) {
      | Error(e) => fail("definition error: " ++ e)
      | Ok(record) =>
        switch (record_field(record, "update", 1)) {
        | None => fail("definition is missing update")
        | Some(update_fn) =>
          let applied =
            ap(
              Forward,
              update_fn,
              IdTagged.FreshGrammar.Exp.tuple([model, action]),
            );
          switch (MvuShape.safe_evaluate(applied)) {
          | Error(e) => fail("update error: " ++ e)
          | Ok(new_model) =>
            /* the model persists in the syntax tree, so it must be
               closure-free */
            MvuShape.is_checkpointable(new_model)
              ? commit_model(new_model)
              : fail("update produced an uncommittable model")
          };
        }
      };
    };
  };

  /* User-defined livelit: render the view, a Hazel HTML value, via
     HazelDOM. Preferred source is the live sample the view fold-in recorded
     during the main evaluation; without one (dynamics off, or not yet
     evaluated) the captured definition is evaluated here instead, which
     requires it to be closed. Actions run through update and commit to the
     syntax, so each use's model lives in its own Ap argument, like builtin
     livelits. */
  let user_view =
      (
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
      Node.div(
        ~attrs=[Attr.classes(["livelit-user-error"])],
        [Node.text(msg)],
      );
    let seed: HazelDOM.t = {
      inject:
        event_inject(
          ~ll_name,
          ~def_elab,
          ~model,
          ~model_value,
          ~commit_model,
        ),
      view_term,
      commit: HazelDOM.State,
    };
    switch (live) {
    | Some(html) => HazelDOM.go(seed, html)
    | None =>
      let ap = IdTagged.FreshGrammar.Exp.ap;
      switch (MvuShape.safe_evaluate(def_elab)) {
      | Error(e) => err("livelit definition error: " ++ e)
      | Ok(record) =>
        switch (record_field(record, "view", 2)) {
        | None => err("livelit definition is missing view")
        | Some(view_fn) =>
          switch (MvuShape.safe_evaluate(ap(Forward, view_fn, model))) {
          | Error(e) => err("livelit view error: " ++ e)
          | Ok(html) when MvuShape.is_html(html) => HazelDOM.go(seed, html)
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
