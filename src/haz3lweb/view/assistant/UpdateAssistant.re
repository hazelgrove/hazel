open Haz3lcore;
include UpdateAction;

/* NOTE: this is duplicated from Update */
let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) => {
  let ed_init = Editors.get_editor(model.editors);
  switch (Haz3lcore.Perform.go(~settings=model.settings.core, a, ed_init)) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok(ed) => Ok({...model, editors: Editors.put_editor(ed, model.editors)})
  };
};

let reset_buffer = (model: Model.t) => {
  let ed = model.editors |> Editors.get_editor;
  let z = ed.state.zipper;
  switch (z.selection.mode) {
  | Buffer(_) =>
    switch (Perform.go_z(~settings=model.settings.core, Destruct(Left), z)) {
    | Error(_) => model
    | Ok(z) =>
      let ed = Editor.new_state(Destruct(Left), z, ed);
      //TODO(andrew): fix double action
      {...model, editors: Editors.put_editor(ed, model.editors)};
    }
  | _ => model
  };
};

let apply =
    (
      {settings, _} as model: Model.t,
      update: agent_action,
      ~schedule_action,
      ~state,
      ~main,
    )
    : Result.t(Model.t) => {
  let editor = model.editors |> Editors.get_editor;
  let z = editor.state.zipper;
  switch (update) {
  | Prompt(TyDi) =>
    let ctx_init = Editors.get_ctx_init(~settings, model.editors);
    switch (TyDi.set_buffer(~settings=settings.core, ~ctx=ctx_init, z)) {
    | None => Ok(model)
    | Some(z) =>
      let ed = Editor.new_state(Pick_up, z, editor);
      //TODO: add correct action to history (Pick_up is wrong)
      let editors = Editors.put_editor(ed, model.editors);
      Ok({...model, editors});
    };
  | AcceptSuggestion =>
    switch (z.selection.mode) {
    | Normal => Ok(model)
    | Buffer(Parsed) => perform_action(model, Unselect(Some(Right)))
    | Buffer(Unparsed) =>
      switch (TyDi.get_buffer(z)) {
      | None => Ok(model)
      /* This case shouldn't happen if we assume that we prevalidate
       * everything we put in the unparsed buffer*/
      | Some(completion) when String.contains(completion, ' ') =>
        /* Slightly hacky. We assume that if a completion string has
         * spaces in it, that means it will have a hole in it. This
         * is a non-essential invariant currently maintained in TyDi.
         * In such a case, we insert the completion as normal by
         * pasting, then return to the beginning and advance to the
         * first hole. This should be revisited if completions are
         * refactored to use a more structured buffer format */
        module M = (val Editor.Meta.module_of_t(editor.state.meta));
        let start = Zipper.caret_point(M.measured, z);
        let rec do_actions = (model, actions: list(UpdateAction.t)) =>
          switch (actions) {
          | [] => Ok(model)
          | [hd, ...tl] =>
            switch (main(model, hd, state, ~schedule_action)) {
            | Error(err) => Error(err)
            | Ok(model) => do_actions(model, tl)
            }
          };
        /* TODO(andrew): use zipper-level actions here to avoid
         * measured recomputation at editor-level */
        do_actions(
          model,
          [
            Paste(completion),
            PerformAction(Move(Goal(Point(start)))),
            PerformAction(MoveToNextHole(Right)),
            PerformAction(Move(Local(Left(ByToken)))),
          ],
        );
      | Some(completion) =>
        main(model, Paste(completion), state, ~schedule_action)
      }
    }
  };
};
