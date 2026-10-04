open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

let button = (~clss=[], ~tooltip="", icon, action) =>
  div(
    ~attrs=[
      Util.WebUtil.clss(["icon"] @ clss),
      Attr.on_mousedown(action),
      Attr.title(tooltip),
    ],
    [icon],
  );

let button_named = (~tooltip="", icon, action) =>
  div(
    ~attrs=[clss(["named-menu-item"]), Attr.on_click(action)],
    [button(icon, _ => Effect.Ignore), div([text(tooltip)])],
  );

let button_d = (~tooltip="", icon, action, ~disabled: bool) =>
  div(
    ~attrs=[
      clss(["icon"] @ (disabled ? ["disabled"] : [])),
      Attr.title(tooltip),
      Attr.on_mousedown(_ => unless(disabled, action)),
    ],
    [icon],
  );

let link = (~tooltip="", icon, url) =>
  div(
    ~attrs=[clss(["icon"])],
    [
      a(
        ~attrs=Attr.[href(url), title(tooltip), create("target", "_blank")],
        [icon],
      ),
    ],
  );

let toggle = (~tooltip="", label, active, action) =>
  div(
    ~attrs=[
      clss(["toggle-switch"] @ (active ? ["active"] : [])),
      Attr.on_pointerdown(action),
      Attr.title(tooltip),
    ],
    [div(~attrs=[clss(["toggle-knob"])], [text(label)])],
  );

let toggle_named = (~name="", ~tooltip=?, icon, active, action) => {
  let tooltip_attrs =
    switch (tooltip) {
    | Some(t) => [Attr.title(t)]
    | None => []
    };
  div(
    ~attrs=
      [
        clss(["named-menu-item"] @ (active ? ["active"] : [])),
        Attr.on_pointerdown(action),
      ]
      @ tooltip_attrs,
    [
      toggle(~tooltip=Option.value(~default="", tooltip), icon, active, _ =>
        Effect.Ignore
      ),
      div([text(name)]),
    ],
  );
};

let file_select_button_named =
    (~tooltip="", ~accept=[`Extension("json")], id, icon, on_input) =>
  /* https://stackoverflow.com/questions/572768/styling-an-input-type-file-button */
  label(
    ~attrs=[Attr.for_(id)],
    [
      Vdom_input_widgets.File_select.single(
        ~extra_attrs=[Attr.class_("file-select-button"), Attr.id(id)],
        ~accept,
        ~on_input,
        (),
      ),
      div(
        ~attrs=[clss(["named-menu-item"])],
        [
          div(~attrs=[clss(["icon"]), Attr.title(tooltip)], [icon]),
          text(tooltip),
        ],
      ),
    ],
  );

/* The Reset menu's two data resets, one above the other: this browser's
   copy, and the canister's (shown only when the page has a canister). */
let reset_hazel_items = (): list(Node.t) => {
  let reset = (~tooltip, ~question, clear) =>
    button_named(
      Icons.bomb,
      _ => {
        if (Util.JsUtil.confirm(question)) {
          clear();
        };
        Effect.Ignore;
      },
      ~tooltip,
    );
  let local =
    reset(
      ~tooltip="Reset Local Hazel (browser cache)",
      ~question=
        HazelDB.Backend.on
          ? "Reset this browser's copy of Hazel? Its cache and anything kept only here are cleared. What the IC canister holds is kept, and loads again."
          : "Are you SURE you want to reset Hazel to its initial state? You will lose any existing code that you have written, and course staff have no way to restore it!",
      HazelDB.clear_local_and_reload,
    );
  let remote =
    reset(
      ~tooltip="Reset Remote Hazel (IC canister memory)",
      ~question=
        "Are you SURE you want to reset the IC canister's copy of Hazel? Every page using it loses the code and settings it keeps there. Shared decks are kept.",
      HazelDB.clear_remote_and_reload,
    );
  HazelDB.Backend.on ? [local, remote] : [local];
};
