open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Js_of_ocaml;

/* ExoTool Projector: a tool selector that lets the user pick
   a Patchwork tool (TLDraw, Petrinaut, CatColab). The underlying
   syntax becomes a String literal with the selected tool ID. */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {tool: string};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetTool(string);

  let init = (a: Language.Any.t): option(model) =>
    switch (a) {
    | Exp(_)
    | Any () => Some({tool: ""})
    | _ => None
    };

  let put = (info, exp: Language.Exp.t): Base.segment =>
    switch (
      info.utility.lift_syntax(
        fun
        | Exp(any) =>
          Exp({
            ...any,
            term: exp.term,
          })
        | _ => failwith("ExoToolProj: put: not expression"),
        Inline.Inline,
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("ExoToolProj: put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (_model, _info) => {
    ProjectorCore.Shape.inline(18);
  };

  let update = (_m: model, _info: info, action: action): model =>
    switch (action) {
    | SetTool(tool) => {tool: tool}
    };

  let view =
      ({model, info, local, parent, _}: View.args(model, action)): View.t => {
    let tool_options =
      [
        Node.create(
          "option",
          ~attrs=[
            Attr.string_property("value", ""),
            Attr.bool_property("disabled", true),
            Attr.bool_property("selected", String.length(model.tool) == 0),
          ],
          [Node.text("Select tool...")],
        ),
      ]
      @ List.map(
          (t: PatchworkToolProj.patchwork_tool) =>
            Node.create(
              "option",
              ~attrs=[
                Attr.string_property("value", t.id),
                Attr.bool_property("selected", model.tool == t.id),
              ],
              [Node.text(t.name)],
            ),
          PatchworkToolProj.tools,
        );

    let tool_select =
      Node.create(
        "select",
        ~attrs=[
          Attr.class_("exotool-select"),
          Attr.on_change((_, value) => {
            let str_exp = Language.IdTagged.FreshGrammar.Exp.string(value);
            let seg = put(info, str_exp);
            Effect.(
              Many([local(SetTool(value)), parent(SetSyntax(seg))])
            );
          }),
          Attr.on_pointerdown(evt => {
            Js.Unsafe.meth_call(evt, "stopPropagation", [||]) |> ignore;
            Effect.Ignore;
          }),
        ],
        tool_options,
      );

    View.mk(
      Node.div(
        ~attrs=[
          Attr.classes(["wrapper", "exotool-wrapper", "cols", "code"]),
        ],
        [Node.text({js|·|js}), tool_select],
      ),
    );
  };
};
