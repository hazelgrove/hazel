open Haz3lcore;
include UpdateAction;
open Util;
open OptUtil.Syntax;

let assistant_action_to_editor_actions =
    (
      {globals: {settings, _}, active_editor: selection, _} as model: Model.t,
      agent_action,
    )
    : list(Action.t) =>
  {
    let* selection = selection;
    let+ editor =
      Editors.get_selected_editor(~selection, model.editors, model.results);
    let z = editor.state.zipper;
    switch (agent_action) {
    | Prompt(TyDi) =>
      let ctx_init = Editors.get_ctx_init(~settings, model.editors);
      switch (TyDi.suggest(~settings=settings.core, ~ctx=ctx_init, z)) {
      | None => []
      | Some(suggestion) => [Action.Suggest(suggestion)]
      };
    | AcceptSuggestion =>
      print_endline("accepting suggestion");
      switch (z.selection.mode) {
      | Normal => []
      | Buffer(Parsed) => [Unselect(Some(Right))]
      | Buffer(Unparsed) =>
        switch (TyDi.get_buffer(z)) {
        | None => []
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
          [
            Paste(AssistantExpander.trim(completion)),
            Move(Goal(Point(start))),
            MoveToNextHole(Right),
            Move(Local(Left(ByToken))),
          ];
        | Some(completion) => [Paste(AssistantExpander.trim(completion))]
        }
      };
    };
  }
  |> Option.to_list
  |> List.flatten;
