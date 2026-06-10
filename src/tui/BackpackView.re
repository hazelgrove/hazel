open Haz3lcore;

/* Floating display of the "backpack" — shards picked up during
   restructuring that Tab will put back down. The web shows these
   stacked above the caret (Backpack.re); the TUI overlays a one-line
   chip on the row above the caret. Token list and suppression rules
   mirror the web view. */

let tokens = (editor: Editor.Model.t): list(string) => {
  let z = editor.state.zipper;
  /* Tiles bisected by a selection show as incomplete, so the web
     suppresses the backpack during (non-buffer) selections; same here */
  Selection.is_empty(z.selection) || Selection.is_buffer(z.selection)
    ? Zipper.local_backpack(z)
      @ editor.syntax.cached_backpack
      |> Util.ListUtil.dedup
      |> List.map(Tile.effective_label)
      |> List.filter_map(
           fun
           | [] => None
           | [hd, ..._] => Some(hd),
         )
    : [];
};

/* The chip: head shard (next to put down) first and emphasized */
let chip = (~can_put_down: bool, tokens: list(string)): Frame.row => {
  let base =
    can_put_down
      ? Style.reverse(Style.fg(Theme.hole_yellow))
      : Style.reverse(Style.fg(Theme.exp));
  switch (tokens) {
  | [] => []
  | [head, ...rest] =>
    [(base, " \xe2\x87\xa7 "), (Style.bold(base), head)]  /* ⇧ */
    @ (rest == [] ? [] : [(base, " " ++ String.concat(" ", rest))])
    @ [(base, " ")]
  };
};

let view = (editor: Editor.Model.t): option(Frame.row) =>
  switch (tokens(editor)) {
  | [] => None
  | ts =>
    Some(chip(~can_put_down=Zipper.can_put_down(editor.state.zipper), ts))
  };
