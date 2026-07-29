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

  let dynamics = false;
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;

  /* User-defined livelit: evaluate the captured definition record and
     render its view, a Hazel HTML value, via HazelDOM. The definition must
     be closed — it evaluates in the builtin environment here — so helpers
     live among the module's members. Actions from handlers run through
     update and commit to the syntax, so each use's model lives in its own
     Ap argument, like builtin livelits. */
  let user_view =
      (
        ~def_elab: TermBase.Exp.t,
        ~model: TermBase.Exp.t,
        ~commit_model: TermBase.Exp.t => Ui_effect.t(unit),
        ~view_term: TermBase.Exp.t => Node.t,
      )
      : Node.t => {
    let err = msg =>
      Node.div(
        ~attrs=[Attr.classes(["livelit-user-error"])],
        [Node.text(msg)],
      );
    let ap = IdTagged.FreshGrammar.Exp.ap;
    switch (MvuShape.safe_evaluate(def_elab)) {
    | Error(e) => err("livelit definition error: " ++ e)
    | Ok(record) =>
      switch (MvuShape.of_tuple(MvuShape.strip_wrappers(record))) {
      | Some(fs) =>
        /* by label when labeled (modules desugar to labeled tuples, any
           size); positional only for a plain (init, update, view, expand)
           tuple */
        let field = (label, index) =>
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
          };
        switch (field("update", 1), field("view", 2)) {
        | (Some(update_fn), Some(view_fn)) =>
          let inject = (action: TermBase.Exp.t): Ui_effect.t(unit) => {
            let applied =
              ap(
                Forward,
                update_fn,
                IdTagged.FreshGrammar.Exp.tuple([model, action]),
              );
            switch (MvuShape.safe_evaluate(applied)) {
            | Error(e) =>
              print_endline("LivelitProj: update error: " ++ e);
              Ui_effect.Ignore;
            | Ok(new_model) =>
              /* the model persists in the syntax tree, so it must be
                 closure-free */
              MvuShape.is_checkpointable(new_model)
                ? commit_model(new_model)
                : {
                  print_endline(
                    "LivelitProj: update produced an uncommittable model",
                  );
                  Ui_effect.Ignore;
                }
            };
          };
          switch (MvuShape.safe_evaluate(ap(Forward, view_fn, model))) {
          | Error(e) => err("livelit view error: " ++ e)
          | Ok(html) when MvuShape.is_html(html) =>
            let seed: HazelDOM.t = {
              inject,
              view_term,
              commit: HazelDOM.State,
            };
            HazelDOM.go(seed, html);
          | Ok(_) => err("livelit view did not produce HTML")
          };
        | _ => err("livelit definition is missing update or view")
        };
      | None => err("livelit definition did not evaluate to a record")
      }
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
          Node.div(
            ~attrs=[
              Attr.classes([ll_name, "user-livelit"]),
              Attr.id(Id.cls(info.id)),
            ],
            [user_view(~def_elab, ~model, ~commit_model, ~view_term)],
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
