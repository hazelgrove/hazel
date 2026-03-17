open Util;
open Virtual_dom.Vdom;

/* Creates a raw_livelit for a given Patchwork tool ID.
   The resulting livelit renders a <patchwork-view> element
   connected to a hardcoded automerge document. */

let tldraw_doc_url = "automerge:2isSbJyZRp5WJmtCXS3QyY43Twpx";

type tool_config = {
  id: string,
  name: string,
  width: int,
  height: int,
};

let tool_configs: list(tool_config) = [
  {
    id: "petrinaut",
    name: "Petrinaut",
    width: 1050,
    height: 590,
  },
  {
    id: "catcolab",
    name: "CatColab",
    width: 680,
    height: 490,
  },
  {
    id: "tldraw4",
    name: "TLDraw",
    width: 680,
    height: 490,
  },
];

let find_tool_config = (id: string): option(tool_config) =>
  List.find_opt(t => t.id == id, tool_configs);

let mk_exotool_livelit = (tool_id: string): LivelitCtx.raw_livelit => {
  let (tool_width, tool_height) =
    switch (find_tool_config(tool_id)) {
    | Some(t) => (t.width, t.height)
    | None => (680, 490)
    };

  let m = font_metrics^;
  let px_to_grid = (value: int, multiple: float): int =>
    int_of_float(ceil(float_of_int(value) /. multiple));

  {
    name: tool_id,
    id: Id.mk_str("exotool_" ++ tool_id),
    model_t: Typ.temp(Unknown(Internal)),
    model_default: IdTagged.FreshGrammar.Exp.constructor("Null", None),
    expansion_t: Typ.temp(Unknown(Internal)),
    expand: _model =>
      Some(IdTagged.FreshGrammar.Exp.constructor("Null", None)),
    action_t: Typ.temp(Unknown(Internal)),
    update: (_action, model) => model,
    view: (_model, _send_action) => {
      let doc_url = tldraw_doc_url;

      let tool_pane =
        Node.create(
          "patchwork-view",
          ~attrs=[
            Attr.create("doc-url", doc_url),
            Attr.create("tool-id", tool_id),
            Attr.create("tabindex", "0"),
            Attr.create(
              "style",
              Printf.sprintf(
                "width: %dpx; height: %dpx; outline: none;",
                tool_width,
                tool_height,
              ),
            ),
          ],
          [],
        );

      Node.div(
        ~attrs=[Attr.class_("exotool-livelit-wrapper")],
        [tool_pane],
      );
    },
    size:
      ProjectorShape.{
        horizontal: px_to_grid(tool_width, m.col_width) + 1,
        vertical: Tab(px_to_grid(tool_height, m.row_height)),
      },
  };
};
