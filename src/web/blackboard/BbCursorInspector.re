open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Language;

let div_err = div(~attrs=[clss(["status", "error"])]);
let div_ok = div(~attrs=[clss(["status", "ok"])]);

/* What kind of block the caret is in, said in terms of what the block owes.
   This is the same distinction the background tint draws. */
let modality_note = (m: option(BbInfo.modality)) =>
  switch (m) {
  | None => []
  | Some(Assume) => [
      div(
        ~attrs=[clss(["bb-modality", "bb-assume"])],
        [text("assumed: postulated, and taken on trust")],
      ),
    ]
  | Some(Construct) => [
      div(
        ~attrs=[clss(["bb-modality", "bb-construct"])],
        [text("constructed: a conservative extension, owing a witness")],
      ),
    ]
  };

let bb_view = (~globals as _, info: BbInfo.t) =>
  div(
    ~attrs=[clss(["bb-info"])],
    modality_note(BbInfo.modality_of(info))
    @ [
      switch (BbInfo.error_of(info)) {
      | None => div_ok([])
      | Some(err) => div_err([text(BbInfo.message(err))])
      },
    ],
  );
