open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;
open Web;

/* This defines the projector selection menu/toggle at the bottom right */

// TODO(matt nominating andrew): move this into the projector code

// module Applicable = {
//   /* If there are applicable projectors, we distinguish the first
//    * one, which will be the current active projector if the indicated
//    * term is already projected */
//   type t = option((ProjectorCore.Kind.t, list(ProjectorCore.Kind.t)));

//   /* Determines what term to target for projection. This logic
//    * should be kept in sync with the projector add/remove logic
//    * in ProjectorPerform */
//   let target_seg =
//       (cursor: Cursor.cursor(Editors.Update.t)): option(Segment.t) => {
//     let* seg =
//       switch (cursor.selection) {
//       | None => None
//       | Some([]) =>
//         switch (cursor.indicated_piece) {
//         | Some(Tile(_) as p)
//         | Some(Projector(_) as p) => Some([p])
//         | Some(Grout(_))
//         | Some(Secondary(_))
//         | None => None
//         }
//       | Some(seg) => Some(seg)
//       };
//     let* () = Segment.deep_tile_complete(seg) ? Some() : None;
//     let* () = Segment.is_padded(seg) ? None : Some();
//     let* skel =
//       switch (Segment.skel(seg)) {
//       | exception _ => None
//       | skel => Some(skel)
//       };
//     let* () =
//       switch (Segment.sort_of(skel, seg)) {
//       | Exp
//       | Pat
//       | Typ
//       | TPat => Some()
//       | Rul
//       | Any => None
//       };
//     Some(seg);
//   };

//   // TODO(matt|andrew): make this work more generally for different sorts
//   let target_term = seg =>
//     seg
//     |> Zipper.unzip
//     |> Editor.Model.of_zipper
//     |> Editor.Update.make_term(~sort=Exp)
//     |> snd
//     |> Calc.get_value;

//   let target_ed = (seg: Segment.t, ()): option('a) =>
//     switch (seg) {
//     | []
//     | [Projector(_)] => None
//     | s => Some(s |> Zipper.unzip |> Editor.Model.of_zipper)
//     };

//   /* Is a projector of `kind` applicable to the target term? */
//   let is_applicable =
//       (cursor: Cursor.cursor(Editors.Update.t), kind: ProjectorCore.Kind.t)
//       : option(ProjectorCore.Kind.t) => {
//     let* target_seg = target_seg(cursor);
//     let term = target_term(target_seg);
//     let ed = target_ed(target_seg);
//     let+ _ = Projector.Model.mk(kind, term, ed);
//     kind;
//   };

//   /* If the current indicated term is a projector, return its kind */
//   let indicated_kind =
//       (editor: option(Editor.Model.t)): option(ProjectorCore.Kind.t) => {
//     let* editor = editor;
//     let* (piece, _, _) = Indicated.for_index(editor |> Editor.Model.get_z);
//     switch (piece) {
//     | Projector(p) => Some(Projector.Model.get_kind(p.model))
//     | _ => None
//     };
//   };

//   /* The string names of all projectors applicable to the currently
//    * indicated syntax, with the currently applied projection (if any)
//    * lifted to the top of the list */
//   let lift_active_projector =
//       (
//         cursor: Cursor.cursor(Editors.Update.t),
//         applicable_projectors: list(ProjectorCore.Kind.t),
//       )
//       : list(ProjectorCore.Kind.t) => {
//     switch (indicated_kind(cursor.editor)) {
//     | None => applicable_projectors
//     | Some(k) => ListUtil.lift(k, applicable_projectors)
//     };
//   };

//   let is_read_only = (cursor: Cursor.cursor(Editors.Update.t)): bool =>
//     switch (cursor.editor) {
//     | None => true
//     | _ => cursor.editor_read_only
//     };

//   let projectors = (cursor): t =>
//     if (is_read_only(cursor)) {
//       None;
//     } else {
//       let list =
//         ProjectorCore.Kind.projectors
//         |> List.filter_map(is_applicable(cursor))
//         |> lift_active_projector(cursor);
//       switch (list) {
//       | [] => None
//       | [hd, ...tl] => Some((hd, tl))
//       };
//     };
// };
let knob =
  div(
    ~attrs=[clss(["toggle-knob"])],
    [create("img", ~attrs=[Attr.src("img/noun-fold-1593402.svg")], [])],
  );

let toggle_view =
    (
      unproject: option(ContextualAction.t),
      applicable_projectors: list(ContextualAction.t),
    ) =>
  switch (unproject, applicable_projectors) {
  | (None, []) =>
    div(~attrs=[clss(["toggle-switch", "inactive"])], [knob])
  | (None, [first, ..._]) =>
    div(
      ~attrs=[
        clss(["toggle-switch"]),
        Attr.on_mousedown(_ =>
          first.update_action |> Option.value(~default=Ui_effect.Ignore)
        ),
      ],
      [knob],
    )
  | (Some(unproject), _) =>
    div(
      ~attrs=[
        clss(["toggle-switch", "active"]),
        Attr.on_mousedown(_ =>
          unproject.update_action |> Option.value(~default=Ui_effect.Ignore)
        ),
      ],
      [knob],
    )
  };

let keyboard_shortcut_of = (kind: ProjectorCore.Kind.t): string =>
  switch (kind) {
  // | Fold => "Option-f"
  // | Probe => "Option-v"
  | Info => "Option-t"
  | _ => "Option-l"
  };

/* A selection input for contetually applicable projectors */
let select_view =
    (
      current_projector: option(string),
      applicable_projectors: list(ContextualAction.t),
    ) => {
  switch (current_projector, applicable_projectors) {
  | (None, []) => select(~attrs=[Attr.id("projector-select")], [])
  | (x, y) =>
    let current_option =
      x
      |> Option.map(name =>
           option(~attrs=[Attr.title(name)], [text(name)])
         );
    let current_effect = x |> Option.map(name => (name, Effect.Ignore));
    let applicable_options =
      y
      |> List.map((shortcut: ContextualAction.t) =>
           option(
             ~attrs=[
               Attr.title(
                 shortcut.label
                 ++ (
                   switch (shortcut.hotkey) {
                   | None => ""
                   | Some(hotkey) => " (" ++ hotkey ++ ")"
                   }
                 ),
               ),
             ],
             [text(shortcut.label)],
           )
         );
    let applicable_effects =
      y
      |> List.map((shortcut: ContextualAction.t) =>
           (
             shortcut.label,
             shortcut.update_action |> Option.value(~default=Effect.Ignore),
           )
         );
    let options = Option.to_list(current_option) @ applicable_options;
    let effects = Option.to_list(current_effect) @ applicable_effects;
    select(
      ~attrs=[
        Attr.id("projector-select"),
        Attr.title("select projector"),
        Attr.on_change((_, name) => {
          switch (List.assoc_opt(name, effects)) {
          | None => Ui_effect.Ignore
          | Some(effect) => effect
          }
        }),
      ],
      options,
    );
  // switch (applicable_projectors) {
  // | None => select(~attrs=[Attr.id("projector-select")], [])
  // | Some((active, rest)) =>
  //   let value = ProjectorCore.Kind.name(active);
  //   select(
  //     ~attrs=[
  //       Attr.id("projector-select"),
  //       Attr.title(keyboard_shortcut_of(active)),
  //       Attr.on_change((_, name) => {
  //         JsUtil.set_select_value("projector-select", value);
  //         inject(SetIndicated(Specific(ProjectorCore.Kind.of_name(name))));
  //       }),
  //     ],
  //     [active, ...rest]
  //     |> List.map(k =>
  //          option(
  //            ~attrs=[Attr.title(keyboard_shortcut_of(k))],
  //            [text(ProjectorCore.Kind.name(k))],
  //          )
  //        ),
  //   );
  // };
  };
};

let view = (cursor: Cursor.t) => {
  let applicable_projectors =
    List.filter(
      (p: ContextualAction.t) => p.section == Some("projectors"),
      cursor.contextual_actions,
    );
  let unproject =
    List.find_opt(
      (p: ContextualAction.t) => p.label == "Unproject",
      applicable_projectors,
    );
  div(
    ~attrs=[Attr.id("projectors")],
    [select_view(cursor.current_projector, applicable_projectors)]
    @ [toggle_view(unproject, applicable_projectors)],
  );
};
