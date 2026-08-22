open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* HtmlRenderer — RichProbe renderer for Html-typed sample values: the
   sample renders as the DOM it describes (via HazelDOM) instead of as
   code. Non-interactive: dispatch is a no-op and .rich-html-view sets
   pointer-events: none, so handler attrs in the value are inert. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = unit;
/* the closed Html value (environments substituted) */
[@deriving (show({with_path: false}), sexp, yojson)]
type value = Exp.t;

let update = (m: model, _: action) => m;

let parse = (sort: Sort.t, exp: Exp.t): option(value) =>
  switch (sort) {
  | Sort.Exp =>
    /* close_value, not strip_wrappers: a mid-run sample's subterms sit
       under Closure nodes and would otherwise render with free vars */
    let closed = MvuShape.close_value(exp);
    MvuShape.is_html(closed) ? Some(closed) : None;
  | _ => None
  };

let empty = ();
let init = (_: value) => ();

let drawer_rows = (_: value): int => 8;

let render =
    (
      ~info: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model as _: model,
      ~local as _: action => Ui_effect.t(unit),
      ~parent as _: external_action => Ui_effect.t(unit),
      ~sort as _: Sort.t,
      _: unit,
    )
    : Node.t => {
  /* Unrecognized subterms fall back to an embedded syntax view */
  let view_term = term =>
    Exp(term) |> info.utility.term_to_seg(~inline=true) |> view_seg(Exp);
  let seed: HazelDOM.t = {
    /* inert: events dispatch nothing. Syntax commit so handler payloads
       are never evaluated at event time (State commit would run
       safe_evaluate per event). */
    inject: (_gesture, _msg) => Ui_effect.Ignore,
    view_term,
    commit: HazelDOM.Syntax,
  };
  Node.div(
    ~attrs=[Attr.classes(["rich-html-view"])],
    [HazelDOM.go(seed, value)],
  );
};

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["html-badge"]),
      Attr.title("Click to view as rendered HTML"),
    ],
    [Node.text("</>")],
  );
