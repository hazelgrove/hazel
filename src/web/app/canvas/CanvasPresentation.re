/* Accepted tool edits become shared presentation states. Parsing and tool
   validation remain atomic. Each definition's model is materialized only
   when it reaches the screen, using the incremental statics machinery. */
open Haz3lcore;
let capture =
    (
      ~enabled=true,
      ~settings: Language.CoreSettings.t,
      ~label,
      ~avatar,
      before: CodeWithStatics.Model.t,
      after: CodeWithStatics.Model.t,
    ) => {
  let old = Zipper.unselect_and_zip(before.editor.state.zipper);
  let accepted = Zipper.unselect_and_zip(after.editor.state.zipper);
  let states =
    try({
      let operations = enabled ? DefinitionSteps.plan(old, accepted) : [];
      let (final, states) =
        List.fold_left(
          ((seg, states), op) => {
            let next = DefinitionSteps.apply(op, seg);
            /* Identity-only repair travels with the preceding edit, without another
               visual dwell. Durable syntax ids still exactly match the accepted edit. */
            let states =
              EditIdentity.same_content(seg, next)
                ? switch (states) {
                  | [(_, target), ...rest] => [(next, target), ...rest]
                  | [] => []
                  }
                : [(next, op.DefinitionSteps.target), ...states];
            (next, states);
          },
          (old, []),
          operations,
        );
      if (enabled && !Segment.equiv_mod_grout(final, accepted)) {
        failwith(
          "definition operations did not reconstruct the accepted edit",
        );
      };
      states;
    }) {
    | exn =>
      CanvasLog.log(
        "presentation: cannot split tool; showing accepted edit atomically: "
        ++ Printexc.to_string(exn),
      );
      [];
    };
  let snapshot = seg => {
    let saved_slot = DefStatics.slot^;
    Fun.protect(
      ~finally=() => DefStatics.slot := saved_slot,
      () => {
        let z = Zipper.unzip(DefinitionSteps.materialize(seg));
        let z = {
          ...z,
          refractors: after.editor.state.zipper.refractors,
        };
        let statics =
          CachedStatics.init_compositional(
            ~settings,
            ~stitch=x => x,
            ~root=after.editor.root,
            z,
          );
        CodeWithStatics.Model.mk(
          ~statics,
          Editor.Model.mk(~root=after.editor.root, z),
        );
      },
    );
  };
  /* Tool handlers may return before statics have been calculated. Never
     present that cache miss as an empty graph: materialize the accepted
     program just as we do an intermediate definition. */
  let final_model =
    lazy(
      Id.Map.is_empty(after.statics.info_map) ? snapshot(accepted) : after
    );
  CanvasBuffer.seed(before);
  CanvasBuffer.note_tool();
  /* No future samples leak into intermediate states. The last beat is the
     exact accepted model, including its refractors and current evaluation. */
  let rec publish = states =>
    switch (states) {
    | [] => CanvasBuffer.push_lazy(~label, ~avatar, final_model)
    | [(_, target)] =>
      CanvasBuffer.push_lazy(
        ~label,
        ~avatar=
          Option.fold(~none=avatar, ~some=id => Some((id, "edit")), target),
        final_model,
      )
    | [(seg, target), ...rest] =>
      CanvasBuffer.push_lazy(
        ~label,
        ~avatar=
          Option.fold(~none=avatar, ~some=id => Some((id, "edit")), target),
        lazy(snapshot(seg)),
      );
      publish(rest);
    };
  publish(List.rev(states));
};
