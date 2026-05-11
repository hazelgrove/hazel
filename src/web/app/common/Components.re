/* Components — the workspace surface design system.

   This is where to come for standard UI elements (buttons, headings,
   list rows, etc.) on the workspace surface. Menu items live in
   `Menu.re`. The legacy icon/toggle helpers in `Widgets.re` will be
   migrated over here over time.

   These generate the HTML structure expected by
   src/web/www/style/workspace/workspace.css. See docs/style/elements.md
   for the catalog. */

open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* ============================================================
   Surface base
   ============================================================ */

let surface =
    (
      ~attrs=[],
      ~placement: option([ | `Inline | `Raised | `Tooltip])=?,
      children,
    ) => {
  let placement_cls =
    switch (placement) {
    | Some(`Inline) => ["inline"]
    | Some(`Raised) => ["raised"]
    | Some(`Tooltip) => ["tooltip"]
    | None => []
    };
  div(
    ~attrs=[clss(["workspace-surface"] @ placement_cls)] @ attrs,
    children,
  );
};

/* ============================================================
   Heading
   ============================================================ */

let heading = (~level: [ | `H1 | `H2 | `H3]=`H2, ~attrs=[], children) => {
  let level_cls =
    switch (level) {
    | `H1 => "h1"
    | `H2 => "h2"
    | `H3 => "h3"
    };
  div(~attrs=[clss(["heading", level_cls])] @ attrs, children);
};

let h1 = (~attrs=[], children) => heading(~level=`H1, ~attrs, children);
let h2 = (~attrs=[], children) => heading(~level=`H2, ~attrs, children);
let h3 = (~attrs=[], children) => heading(~level=`H3, ~attrs, children);

/* ============================================================
   Button — visible icon button. Pass `~subtle=true` for the deriver
   style that fades in on parent hover.
   ============================================================ */

let button =
    (
      ~attrs=[],
      ~active=false,
      ~disabled=false,
      ~subtle=false,
      ~tooltip="",
      children,
      action,
    ) => {
  let cls =
    ["button"]
    @ (active ? ["active"] : [])
    @ (disabled ? ["disabled"] : [])
    @ (subtle ? ["subtle"] : []);
  div(
    ~attrs=
      [
        clss(cls),
        Attr.title(tooltip),
        Attr.on_mousedown(_ => unless(disabled, action)),
      ]
      @ attrs,
    children,
  );
};

/* ============================================================
   Toggle
   ============================================================ */

let toggle = (~attrs=[], ~active=false, ~tooltip="", action) =>
  div(
    ~attrs=
      [
        clss(["toggle"] @ (active ? ["active"] : [])),
        Attr.title(tooltip),
        Attr.on_pointerdown(_ => action),
      ]
      @ attrs,
    [div(~attrs=[clss(["knob"])], [])],
  );

/* ============================================================
   Text input
   ============================================================ */

let text_input =
    (
      ~attrs=[],
      ~placeholder="",
      ~value="",
      ~multiline=false,
      on_input: string => Ui_effect.t(unit),
    ) =>
  multiline
    ? textarea(
        ~attrs=
          [
            clss(["text-input"]),
            Attr.placeholder(placeholder),
            Attr.value(value),
            Attr.on_input((_, s) => on_input(s)),
          ]
          @ attrs,
        [],
      )
    : input(
        ~attrs=
          [
            clss(["text-input"]),
            Attr.placeholder(placeholder),
            Attr.value(value),
            Attr.on_input((_, s) => on_input(s)),
          ]
          @ attrs,
        (),
      );

/* ============================================================
   Select — dropdown. `options` is a list of (value, label).
   ============================================================ */

let select_ =
    (
      ~attrs=[],
      ~selected: string,
      ~options: list((string, string)),
      on_change: string => Ui_effect.t(unit),
    ) =>
  select(
    ~attrs=[clss(["select"]), Attr.on_change((_, s) => on_change(s))] @ attrs,
    List.map(
      ((v, label)) =>
        option(
          ~attrs=
            [Attr.value(v)]
            @ (v == selected ? [Attr.create("selected", "selected")] : []),
          [text(label)],
        ),
      options,
    ),
  );

/* ============================================================
   Code inline
   ============================================================ */

let code_inline = (~attrs=[], children) =>
  span(~attrs=[clss(["code-inline"])] @ attrs, children);

/* ============================================================
   Badge
   ============================================================ */

let badge =
    (
      ~tone: [ | `Error | `Warning | `Ok | `Count | `Kbd]=`Count,
      ~attrs=[],
      children,
    ) => {
  let tone_cls =
    switch (tone) {
    | `Error => ["status", "error"]
    | `Warning => ["status", "warning"]
    | `Ok => ["status", "ok"]
    | `Count => ["count"]
    | `Kbd => ["kbd"]
    };
  span(~attrs=[clss(["badge"] @ tone_cls)] @ attrs, children);
};

/* ============================================================
   Divider
   ============================================================ */

let divider = (~vertical=false, ~attrs=[], ()) =>
  Node.hr(
    ~attrs=[clss(["divider"] @ (vertical ? ["vertical"] : []))] @ attrs,
  );

/* ============================================================
   Progress bar — Fill is a continuous bar; Segments is a row of
   status-coded clickable cells.
   ============================================================ */

type progress_segment = [ | `Pass | `Fail | `Indet];

type progress_kind =
  | Fill(float) /* 0.0–1.0 */
  | Segments(list(progress_segment));

let progress_bar = (~label="", ~attrs=[], kind: progress_kind) => {
  let label_node =
    label == "" ? [] : [div(~attrs=[clss(["label"])], [text(label)])];
  let body =
    switch (kind) {
    | Fill(p) =>
      let pct = Printf.sprintf("%d%%", int_of_float(p *. 100.));
      div(
        ~attrs=[clss(["track"])],
        [
          div(
            ~attrs=[clss(["fill"]), Attr.create("style", "width: " ++ pct)],
            [],
          ),
        ],
      );
    | Segments(segs) =>
      div(
        ~attrs=[clss(["track"])],
        List.map(
          seg => {
            let seg_cls =
              switch (seg) {
              | `Pass => "pass"
              | `Fail => "fail"
              | `Indet => "indet"
              };
            div(~attrs=[clss(["segment", seg_cls])], []);
          },
          segs,
        ),
      )
    };
  let segmented_cls =
    switch (kind) {
    | Fill(_) => []
    | Segments(_) => ["segmented"]
    };
  div(
    ~attrs=[clss(["progress-bar"] @ segmented_cls)] @ attrs,
    label_node @ [body],
  );
};

/* ============================================================
   List row — leading / label / trailing slots, state and status
   variants.
   ============================================================ */

type list_row_status = [ | `Error | `Syntax | `Warning | `Hole];

let list_row =
    (
      ~attrs=[],
      ~leading: option(Node.t)=?,
      ~trailing: option(Node.t)=?,
      ~active=false,
      ~expanded=false,
      ~status: option(list_row_status)=?,
      ~on_click=?,
      label,
    ) => {
  let status_cls =
    switch (status) {
    | Some(`Error) => ["error"]
    | Some(`Syntax) => ["syntax"]
    | Some(`Warning) => ["warning"]
    | Some(`Hole) => ["hole"]
    | None => []
    };
  let state_cls =
    (active ? ["active"] : []) @ (expanded ? ["expanded"] : []);
  let event_attrs =
    switch (on_click) {
    | Some(action) => [Attr.on_click(_ => action)]
    | None => []
    };
  let slot = (cls, contents) =>
    switch (contents) {
    | Some(n) => [div(~attrs=[clss([cls])], [n])]
    | None => []
    };
  div(
    ~attrs=
      [clss(["list-row"] @ state_cls @ status_cls)] @ event_attrs @ attrs,
    slot("leading", leading)
    @ [div(~attrs=[clss(["label"])], [label])]
    @ slot("trailing", trailing),
  );
};

/* ============================================================
   Row — horizontal flex group
   ============================================================ */

let row = (~attrs=[], children) =>
  div(~attrs=[clss(["row"])] @ attrs, children);

/* ============================================================
   Tabs — vertical (default), horizontal segmented, or title
   ============================================================ */

let tabs =
    (
      ~direction: [ | `Vertical | `Horizontal | `Title]=`Vertical,
      ~attrs=[],
      children,
    ) => {
  let direction_cls =
    switch (direction) {
    | `Vertical => []
    | `Horizontal => ["horizontal"]
    | `Title => ["title"]
    };
  div(~attrs=[clss(["tabs"] @ direction_cls)] @ attrs, children);
};
