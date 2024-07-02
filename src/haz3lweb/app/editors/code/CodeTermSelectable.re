module Model = CodeSelectable.Model;

module Update = CodeSelectable.Update;

module Selection = CodeSelectable.Selection;

module View = {
  open Haz3lcore;
  type event = CodeEditable.View.event;

  let get_ancestors = (id: Id.t, m: Statics.Map.t) => {
    switch (Id.Map.find_opt(id, m)) {
    | Some(Info.InfoExp({ancestors, _})) => Some([id, ...ancestors])
    | _ => None
    };
  };

  let rec least_common_ancestor =
          (ancestors: list(list(Id.t))): option(Id.t) => {
    switch (ancestors) {
    | [] => None
    | [a] => Some(List.hd(a))
    | [x, y, ...rest] =>
      open Util.OptUtil.Syntax;
      let rec chop_x = (
        fun
        | [] => None
        | [x, ...xs] when List.mem(x, y) => Some([x, ...xs])
        | [_x, ...xs] => chop_x(xs)
      );
      let* x' = chop_x(x);
      least_common_ancestor([x', ...rest]);
    };
  };

  let get_selected_id = (m: Model.t) => {
    let z = m.editor.state.zipper;
    {
      let sel = z.selection.content;
      let ids =
        List.filter_map(
          fun
          | Piece.Tile({id, _}) => Some(id)
          | _ => None,
          sel,
        );
      let ancestors =
        List.filter_map(id => get_ancestors(id, m.statics.info_map), ids);
      least_common_ancestor(ancestors);
    }
    |> Util.OptUtil.get_opt(() =>
         switch (Haz3lcore.Indicated.piece(z)) {
         | Some((Tile({id, _}), _, _)) => Some(id)
         | Some((Grout(_) | Secondary(_), _, _))
         | _ => None
         }
       );
  };

  let view =
      (
        ~globals: Globals.t,
        ~inject: Update.t => 'a,
        ~overlays=[],
        ~selected: bool,
        model: Model.t,
      ) => {
    let overlays = {
      module Deco =
        Deco.Deco({
          let editor = model.editor;
          let globals = globals;
        });
      overlays @ (selected ? Deco.taken_step(get_selected_id(model)) : []);
    };
    CodeSelectable.View.view(~globals, ~inject, ~overlays, model, ~selected);
  };
};
