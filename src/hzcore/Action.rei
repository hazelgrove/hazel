type t =
  // TODO Mark
  | Move(Path.t)
  | Delete({
      dir: Direction.t,
      into_layout_whitespace: bool,
    })
  | Construct(unit); // TODO

module Error = {
  type t = unit;
};

type editor_state = (program_state, IdGen.t);

let perform:
  (t, program_state)
  => Result.t((EditState.t(program_state), HistoryEntry.t), Error.t);

let perform:
  (t, editor_state)
  => Result.t(
    list((t, editor_state, HistoryEntry.t)),
    Error.t,
  ) = {
    let prim_actions = perform_compound(t, editor_state);
    fold(perform_primitive, prim_actions)
  }

let perform_compound:
  (t, program_state) => list(t) = [t]

let perform_primitive :
  (t, editor_state) => Result.t((editor_state, HistoryEntry.t), Error.t) = {
    let (program_state, id_gen) = editor_state;
    let cmd = monadic_perform_primitive(t, program_state);
    // TODO pattern match here
    let ((program_state, history_entry), id_gen') = cmd(id_gen);
    Ok(((program_state, id_gen'), history_entry))
  }

let monadic_perform_primitive : (t, program_state) => IdGenCmd.t(Result.t((program_state, HistoryEntry.t), Error.t))

type compound_state = list((t, program_state, HistoryEntry.t));

module PerformCompoundCmd = {
  type t('a) = compound_state => ('a, compound_state);

  let return : 'a => t('a);
  let bind : t('a) => ('a => t('b)) => t('b);

  let add_prim_action : Action.t => t(program_state);
}
