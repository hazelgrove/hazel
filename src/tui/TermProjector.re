open Haz3lcore;

/* Terminal views for projectors ("option A" of
   docs/projector-backend-split.md): a TUI-side registry keyed by
   projector kind, reusing the pure semantics exposed at file level by
   the core implementations (CheckboxProj.toggle, SliderProj.put,
   TypeProj.display_ty, ...). Kinds without a terminal view fall back
   to EditorView's blank-space rendering.

   The cardinal rule: an inline view must fill EXACTLY the cell region
   the projector's Shape reserves in Measured, or the grid (caret,
   selection, underlines) drifts. Glyphs must be width-1 in Hazel's
   Unicode.Width model (beware Extended_Pictographic: ✔ counts as 2,
   ✓ ✗ ⇒ ⋱ count as 1). */

/* What a click inside a projector asks the editor to do; the data
   counterpart of ProjectorBase.external_action's web Ui_effect
   callbacks. App translates these into Action.Project(...). */
type reaction =
  | SetSyntax(Base.segment)
  | Remove;

type t = {
  /* Fill the inline-reserved region (a single row of [width] cells).
     Only consulted for Inline-shaped projectors. None = fall back. */
  inline_view:
    (~model: string, ~info: ProjectorBase.info, ~width: int) =>
    option(Frame.row),
  /* Spans appended after the end of the line containing the projector
     (the web's "offside" view slot) */
  offside_view:
    (~model: string, ~info: ProjectorBase.info) => option(Frame.row),
  /* Click at cell [rel_col] within the inline region */
  on_click:
    (~model: string, ~info: ProjectorBase.info, ~rel_col: int, ~width: int) =>
    option(reaction),
};

let no_inline = (~model as _, ~info as _, ~width as _) => None;
let no_offside = (~model as _, ~info as _) => None;
let no_click = (~model as _, ~info as _, ~rel_col as _, ~width as _) => None;

/* Both backends construct projector info through the same pure core
   function. The TUI doesn't run probes, so dynamics are empty and
   sample focus is the initial one. */
let mk_info =
    (~statics: CachedStatics.t, p: Base.projector): ProjectorBase.info =>
  ProjectorInfo.mk_info(
    p,
    ~sample_focus=Language.Sample.Focus.init,
    ~statics=statics.info_map,
    ~dynamics=Language.Dynamics.Map.empty,
    ~elaborated=Some(statics.elaborated),
  );

let pad_to = (width: int, spans: Frame.row): Frame.row => {
  let cols = Frame.row_cols(spans);
  if (cols < width) {
    spans @ [(Style.default, String.make(width - cols, ' '))];
  } else if (cols > width) {
    Frame.clip_row(spans, ~col_off=0, ~width);
  } else {
    spans;
  };
};

/* === Checkbox: `✓ `/`✗ `; click toggles the underlying bool === */
let checkbox: t = {
  inline_view: (~model as _, ~info, ~width) =>
    switch (CheckboxProj.get(info)) {
    | b =>
      let (style, glyph) =
        b
          ? (Style.bold(Style.fg(Theme.green)), "\xe2\x9c\x93")  /* ✓ */
          : (Style.fg(Theme.red), "\xe2\x9c\x97"); /* ✗ */
      Some(pad_to(width, [(style, glyph)]));
    | exception _ => None
    },
  offside_view: no_offside,
  on_click: (~model as _, ~info, ~rel_col as _, ~width as _) =>
    switch (CheckboxProj.toggle(info)) {
    | seg => Some(SetSyntax(seg))
    | exception _ => None
    },
};

/* === Slider: `[====----]`; click sets the value (0-100) === */
let slider_value = (info: ProjectorBase.info): option(int) =>
  switch (SliderProj.get(info)) {
  | v => int_of_string_opt(Util.Bigint.to_string(v))
  | exception _ => None
  };

let slider: t = {
  inline_view: (~model as _, ~info, ~width) =>
    switch (slider_value(info)) {
    | None => None
    | Some(v) =>
      let v = max(0, min(100, v));
      let inner = max(1, width - 2);
      let filled = v * inner / 100;
      Some(
        pad_to(
          width,
          [
            (Theme.grout, "["),
            (Style.fg(Theme.pat), String.make(filled, '=')),
            (Theme.grout, String.make(inner - filled, '-') ++ "]"),
          ],
        ),
      );
    },
  offside_view: no_offside,
  on_click: (~model as _, ~info, ~rel_col, ~width) => {
    let inner = max(2, width - 2);
    /* clicks on the brackets clamp to the ends */
    let pos = max(0, min(inner - 1, rel_col - 1));
    let v = pos * 100 / (inner - 1);
    switch (SliderProj.put(info, string_of_int(v))) {
    | seg => Some(SetSyntax(seg))
    | exception _ => None
    };
  },
};

/* === TypeProj ("Statics" kind): offside `⇐ Int` after the line === */
let type_display: t = {
  inline_view: no_inline, /* Shape.default: zero-width inline */
  offside_view: (~model, ~info) => {
    let m =
      switch (TypeProj.display_of_sexp(Sexplib.Sexp.of_string(model))) {
      | m => m
      | exception _ => TypeProj.Expected
      };
    let mode = TypeProj.display_mode(m, info.statics);
    let ty =
      TypeProj.display_ty(m, info.statics)
      |> TypeProj.totalize_ty
      |> Language.Typ.pretty_print;
    Some([
      (Style.dim(Style.default), mode ++ " "),
      (Theme.of_base_cls("Typ"), ty),
    ]);
  },
  on_click: no_click,
};

/* === Fold: render the fold text; click unfolds (removes) === */
let fold: t = {
  inline_view: (~model, ~info as _, ~width) => {
    let text =
      switch (FoldProj.t_of_sexp(Sexplib.Sexp.of_string(model))) {
      | m => m.text
      | exception _ => FoldProj.default.text
      };
    Some(pad_to(width, [(Theme.grout, text)]));
  },
  offside_view: no_offside,
  on_click: (~model as _, ~info as _, ~rel_col as _, ~width as _) =>
    Some(Remove),
};

let lookup: ProjectorCore.Kind.t => option(t) =
  fun
  | Fold => Some(fold)
  | Checkbox => Some(checkbox)
  | Slider => Some(slider)
  | Statics => Some(type_display)
  | _ => None;
