open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    |;

  let can_undo = _action => false; //TODO(andrew)
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = unit;

  let init = ();

  module Store =
    Store.F({
      [@deriving (show({with_path: false}), yojson, sexp)]
      type t = unit;
      let default = () => init;

      let key = Store.ProbeSystem;
    });
};

let view =
    (
      ~globals as _: Globals.t,
      ~signal as _,
      ~inject as _: Update.t => Ui_effect.t(unit),
      ~model as _: Model.t,
      ~editor as _: CodeEditable.Model.t,
    ) =>
  div(
    ~attrs=[Attr.id("probesys")],
    [
      div(
        ~attrs=[clss(["header"])],
        [div(~attrs=[clss(["main-title"])], [text("Probe System")])],
      ),
    ],
  );
