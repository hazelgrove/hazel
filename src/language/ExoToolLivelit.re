open Util;
open Virtual_dom.Vdom;

/* Creates a raw_livelit for a given Patchwork tool ID.
   The resulting livelit renders a <patchwork-view> element
   connected to a hardcoded automerge document. */

let tldraw_doc_url = "automerge:2isSbJyZRp5WJmtCXS3QyY43Twpx";

/* Known dimension overrides for tools whose default size differs */
let known_dimensions: list((string, (int, int))) = [
  ("petrinaut", (1050, 590)),
];

let dimensions_for = (id: string): (int, int) =>
  switch (List.assoc_opt(id, known_dimensions)) {
  | Some(dims) => dims
  | None => (680, 490)
  };

let mk_exotool_livelit = (tool_id: string): LivelitCtx.raw_livelit => {
  let (tool_width, tool_height) = dimensions_for(tool_id);

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
