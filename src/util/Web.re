open Virtual_dom.Vdom;
open Node;

let clss = Attr.classes;

let div_c = cls => div([Attr.class_(cls)]);
let span_c = cls => span([Attr.class_(cls)]);

let div_if = (p, ats, ns) => p ? div(ats, ns) : div([], []);
let span_if = (p, ats, ns) => p ? span(ats, ns) : span([], []);

let unless = (p, a) => p ? Event.Many([]) : a;

let button = (icon, action) =>
  div([clss(["icon"]), Attr.on_mousedown(action)], [icon]);

let button_d = (icon, action, ~disabled: bool) =>
  div(
    [
      clss(["icon"] @ (disabled ? ["disabled"] : [])),
      Attr.on_mousedown(_ => unless(disabled, action)),
    ],
    [icon],
  );

let link = (icon, url) =>
  div(
    [clss(["icon"])],
    [a(Attr.[href(url), create("target", "_blank")], [icon])],
  );

let toggle = (label, active, action) =>
  div(
    [
      clss(["toggle-switch"] @ (active ? ["active"] : [])),
      Attr.on_click(action),
    ],
    [div([clss(["toggle-knob"])], [text(label)])],
  );
