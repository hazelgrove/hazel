open Util;
open Virtual_dom.Vdom;
open Node;

/* Reusable instructor-mode header editors for exercises.
   Provides pencil/confirm/cancel UI for editing a title, module name,
   and prompt (with markdown rendering for the prompt via ExplainThis). */

/* The DOM ids below are also recognized by Page.View.is_input_field so
   that keyboard shortcuts are bypassed while these inputs have focus.
   Only one exercise is visible at a time, so reusing these ids across
   exercise kinds is safe. */
let title_input_id = "title-input-box";
let module_name_input_id = "module-name-input";
let prompt_input_id = "prompt-input-box";

/* Pencil/confirm/cancel buttons rendered with the design-system Button.
   The `.edit-icon` wrapper preserves the existing positioning rules
   (see cell.css). */
let edit_icon = (~tooltip="", icon, action) =>
  div(
    ~attrs=[Attr.class_("edit-icon")],
    [Components.button(~tooltip, [icon], action)],
  );

/* Title: bold headline, editable in instructor mode. */
let title_view =
    (
      ~instructor_mode: bool,
      ~is_editing: bool,
      ~title: string,
      ~on_focus_textbox: _ => Ui_effect.t(unit),
      ~toggle_editing: _ => Ui_effect.t(unit),
      ~update_title: string => Ui_effect.t(unit),
    )
    : Node.t => {
  let placeholder = title == "" ? "Untitled Exercise" : title;
  let confirm = ev => {
    let new_value =
      Obj.magic(Js_of_ocaml.Js.some(JsUtil.get_elem_by_id(title_input_id)))##.value;
    Effect.Many([update_title(new_value), toggle_editing(ev)]);
  };
  let body =
    instructor_mode
      ? is_editing
          ? div(
              ~attrs=[Attr.class_("title-edit")],
              [
                input(
                  ~attrs=[
                    Attr.class_("title-text"),
                    Attr.id(title_input_id),
                    Attr.value(title),
                    Attr.on_focus(on_focus_textbox),
                  ],
                  (),
                ),
                edit_icon(Icons.confirm, confirm),
                edit_icon(Icons.cancel, toggle_editing),
              ],
            )
          : div(
              ~attrs=[Attr.class_("title-edit")],
              [
                div(
                  ~attrs=[
                    Attr.classes([
                      "title-text",
                      title == "" ? "title-placeholder" : "",
                    ]),
                  ],
                  [text(placeholder)],
                ),
                edit_icon(Icons.pencil, toggle_editing),
              ],
            )
      : Components.heading(~level=`H1, [text(title)]);
  CellCommon.simple_cell_view([
    div(~attrs=[Attr.class_("title-cell")], [body]),
  ]);
};

/* Module name: only visible in instructor mode. */
let module_name_view =
    (
      ~instructor_mode: bool,
      ~is_editing: bool,
      ~module_name: string,
      ~on_focus_textbox: _ => Ui_effect.t(unit),
      ~toggle_editing: _ => Ui_effect.t(unit),
      ~update_module_name: string => Ui_effect.t(unit),
    )
    : Node.t => {
  let placeholder = module_name == "" ? "Unnamed Module" : module_name;
  let confirm = ev => {
    let new_value =
      Obj.magic(
        Js_of_ocaml.Js.some(JsUtil.get_elem_by_id(module_name_input_id)),
      )##.value;
    Effect.Many([toggle_editing(ev), update_module_name(new_value)]);
  };
  instructor_mode
    ? div(
        ~attrs=[Attr.class_("cell-module-name")],
        [
          is_editing
            ? div(
                ~attrs=[Attr.class_("module-name-edit")],
                [
                  label([text("Module name:")]),
                  input(
                    ~attrs=[
                      Attr.type_("text"),
                      Attr.class_("text-input"),
                      Attr.id(module_name_input_id),
                      Attr.value(module_name),
                      Attr.on_focus(on_focus_textbox),
                    ],
                    (),
                  ),
                  edit_icon(Icons.confirm, confirm),
                  edit_icon(Icons.cancel, toggle_editing),
                ],
              )
            : div(
                ~attrs=[Attr.class_("module-name-text")],
                [
                  text("Module name: "),
                  div(
                    ~attrs=[
                      Attr.classes([
                        module_name == "" ? "module-placeholder" : "",
                      ]),
                    ],
                    [text(placeholder)],
                  ),
                  edit_icon(Icons.pencil, toggle_editing),
                ],
              ),
        ],
      )
    : Node.none;
};

/* Prompt: rendered markdown in both view and student modes; textarea
   when instructor toggles editing. */
let prompt_view =
    (
      ~globals: Globals.t,
      ~inject_explainthis: ExplainThisUpdate.update => Ui_effect.t(unit),
      ~instructor_mode: bool,
      ~is_editing: bool,
      ~prompt: string,
      ~on_focus_textbox: _ => Ui_effect.t(unit),
      ~toggle_editing: _ => Ui_effect.t(unit),
      ~update_prompt: string => Ui_effect.t(unit),
    )
    : Node.t => {
  let placeholder = prompt == "" ? "Empty Prompt" : prompt;
  let (msg, _) =
    ExplainThis.mk_translation(
      ~globals,
      ~inject=inject_explainthis,
      placeholder,
    );
  let confirm = ev => {
    let new_value =
      Obj.magic(Js_of_ocaml.Js.some(JsUtil.get_elem_by_id(prompt_input_id)))##.value;
    Effect.Many([toggle_editing(ev), update_prompt(new_value)]);
  };
  div(
    ~attrs=[Attr.class_("cell-prompt")],
    [
      instructor_mode
        ? is_editing
            ? div(
                ~attrs=[Attr.class_("prompt-edit")],
                [
                  div(
                    ~attrs=[Attr.id("prompt-textarea-container")],
                    [
                      textarea(
                        ~attrs=[
                          Attr.class_("prompt-text"),
                          Attr.id(prompt_input_id),
                          Attr.on_focus(on_focus_textbox),
                          Attr.create("rows", "5"),
                          Attr.create("cols", "30"),
                        ],
                        [text(prompt)],
                      ),
                    ],
                  ),
                  edit_icon(Icons.confirm, confirm),
                  edit_icon(Icons.cancel, toggle_editing),
                ],
              )
            : div(
                ~attrs=[Attr.class_("prompt-edit")],
                [
                  div(
                    ~attrs=[
                      Attr.classes([
                        "prompt-content",
                        prompt == "" ? "prompt-placeholder" : "",
                      ]),
                    ],
                    msg,
                  ),
                  div(
                    ~attrs=[Attr.class_("edit-pencil")],
                    [
                      Components.button(
                        ~tooltip="Edit Prompt",
                        [Icons.pencil],
                        toggle_editing,
                      ),
                    ],
                  ),
                ],
              )
        : div(~attrs=[Attr.class_("prompt-content")], msg),
    ],
  );
};
