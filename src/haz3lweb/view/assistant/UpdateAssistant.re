open Haz3lcore;
include UpdateAction;

let apply =
    (model: Model.t, update: agent_action, ~schedule_action, ~state, ~main)
    : Result.t(Model.t) => {
  switch (update) {
  //| Prompt(TyDi) => Ok(set_buffer(model))
  | AcceptSuggestion =>
    let editor = model.editors |> Editors.get_editor;
    let z = editor.state.zipper;
    let trim = AssistantExpander.trim;
    switch (z.selection.mode) {
    | Normal => Ok(model)
    | Buffer(Parsed) =>
      UpdateAction.perform_action(model, Unselect(Some(Right)))
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
        let start =
          Zipper.caret_point(M.measured, editor.state.meta.projected.z);
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
            PerformAction(Paste(trim(completion))),
            PerformAction(Move(Goal(Point(start)))),
            PerformAction(MoveToNextHole(Right)),
            PerformAction(Move(Local(Left(ByToken)))),
          ],
        );
      | Some(completion) =>
        main(
          model,
          PerformAction(Paste(trim(completion))),
          state,
          ~schedule_action,
        )
      }
    };
  };
};
