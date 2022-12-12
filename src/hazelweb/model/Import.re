let import = (e: UHExp.t, model: Model.t): Model.t => {
  let ze = e |> ZExp.place_before;
  let edit_state =
    ze |> Statics_Exp.fix_and_renumber_holes_z(Contexts.initial);
  let program = edit_state |> Program.mk(~width=model.cell_width);
  let new_model = model |> Model.put_program(program) /* Update undo history. */;

  new_model
  |> Model.put_undo_history(
       {
         let history = model |> Model.get_undo_history;
         let prev_cardstacks = model |> Model.get_cardstacks;
         let new_cardstacks = new_model |> Model.get_cardstacks;
         UndoHistory.push_edit_state(
           history,
           prev_cardstacks,
           new_cardstacks,
           ModelAction.Import(e),
         );
       },
     );
};
