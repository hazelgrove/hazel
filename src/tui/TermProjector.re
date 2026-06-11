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
  /* Fill a Block-shaped region: [rows] extra rows below the origin
     row, [width] cells wide, with following editor content resuming at
     column [last_col] of the final row. Must return rows+1 rows (the
     caller pads/truncates and enforces the final row's width). */
  block_view:
    (
      ~model: string,
      ~info: ProjectorBase.info,
      ~width: int,
      ~rows: int,
      ~last_col: int
    ) =>
    option(list(Frame.row)),
};

let no_inline = (~model as _, ~info as _, ~width as _) => None;
let no_offside = (~model as _, ~info as _) => None;
let no_click = (~model as _, ~info as _, ~rel_col as _, ~width as _) => None;
let no_block =
    (~model as _, ~info as _, ~width as _, ~rows as _, ~last_col as _) =>
  None;

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
  block_view: no_block,
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
  block_view: no_block,
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
  block_view: no_block,
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
  block_view: no_block,
};

/* === SliderF: float slider, same bar as Slider scaled 0-100 === */
let sliderf: t = {
  inline_view: (~model as _, ~info, ~width) =>
    switch (SliderFProj.get(info)) {
    | v =>
      let v = max(0., min(100., v));
      let inner = max(1, width - 2);
      let filled = int_of_float(v *. float_of_int(inner) /. 100.);
      let filled = max(0, min(inner, filled));
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
    | exception _ => None
    },
  offside_view: no_offside,
  on_click: (~model as _, ~info, ~rel_col, ~width) => {
    let inner = max(2, width - 2);
    let pos = max(0, min(inner - 1, rel_col - 1));
    let v = float_of_int(pos) *. 100. /. float_of_int(inner - 1);
    switch (SliderFProj.put(info, Printf.sprintf("%g", v))) {
    | seg => Some(SetSyntax(seg))
    | exception _ => None
    };
  },
  block_view: no_block,
};

/* === TextArea: render the string's content in its reserved region
   (read-only; in-place editing needs the focus protocol) === */
let textarea_style = Style.fg(Theme.yellow);

let textarea: t = {
  inline_view: (~model as _, ~info, ~width) =>
    switch (TextAreaProj.get(info)) {
    | s => Some(pad_to(width, [(textarea_style, " " ++ s)]))
    | exception _ => None
    },
  offside_view: no_offside,
  on_click: no_click,
  block_view: (~model as _, ~info, ~width, ~rows, ~last_col as _) =>
    switch (TextAreaProj.get(info)) {
    | s =>
      Some(
        String.split_on_char('\n', s)
        |> Util.ListUtil.take(rows + 1)
        |> List.map(line => pad_to(width, [(textarea_style, " " ++ line)])),
      )
    | exception _ => None
    },
};

/* === Table: box-drawing render of the parsed table === */
let table_cell = (exp: Language.Exp.t): string =>
  switch (
    ProbeText.format_value(~max_length=TableCore.max_column_length, exp)
  ) {
  | s => s
  | exception _ => "?"
  };

let table: t = {
  inline_view: no_inline,
  offside_view: no_offside,
  on_click: no_click,
  block_view: (~model as _, ~info, ~width, ~rows as _, ~last_col as _) =>
    switch (TableProj.get(info)) {
    | None => None
    | Some((headers, data)) =>
      let cells = List.map(List.map(table_cell), data);
      let n_cols = List.length(headers);
      let col_w = i =>
        List.fold_left(
          (acc, row) =>
            switch (List.nth_opt(row, i)) {
            | Some(c) => max(acc, Frame.row_cols([(Style.default, c)]))
            | None => acc
            },
          Frame.row_cols([(Style.default, List.nth(headers, i))]),
          cells,
        );
      let widths = List.init(n_cols, col_w);
      let pad_cell = (w, s) =>
        s
        ++ String.make(
             max(0, w - Frame.row_cols([(Style.default, s)])),
             ' ',
           );
      let line = (style, cs) => [
        (
          style,
          " "
          ++ String.concat(
               " \xe2\x94\x82 ", /* │ */
               List.map2(pad_cell, widths, cs),
             )
          ++ " ",
        ),
      ];
      let sep = [
        (
          Theme.grout,
          widths
          |> List.map(w => List.init(w + 2, _ => "─") |> String.concat(""))  /* ─ */
          |> String.concat("\xe2\x94\xbc") /* ┼ */
        ),
      ];
      Some(
        [
          line(Style.bold(Style.default), headers)
          |> Frame.clip_row(_, ~col_off=0, ~width),
          sep |> Frame.clip_row(_, ~col_off=0, ~width),
        ]
        @ List.map(
            r =>
              line(Style.default, r) |> Frame.clip_row(_, ~col_off=0, ~width),
            cells,
          ),
      );
    | exception _ => None
    },
};

/* === Probe (projector kind): latest sample values offside === */
let probe: t = {
  inline_view: no_inline,
  offside_view: (~model as _, ~info) =>
    switch (info.dynamics) {
    | Some({samples, _}) when samples != [] =>
      let text =
        samples
        |> Util.ListUtil.take(5)
        |> List.map((s: Language.Sample.t) =>
             ProbeText.format_value(~max_length=40, s.value)
           )
        |> String.concat(" \xe2\xab\xbd "); /* ⫽ */
      Some([(Style.fg(Theme.green), "\xe2\x89\xa1 " ++ text)]); /* ≡ */
    | _ => None
    },
  on_click: no_click,
  block_view: no_block,
};

/* === Card: mini card faces — white face, red/black pips === */
let suit_glyph: CardProj.suit => string =
  fun
  | Hearts => "\xe2\x99\xa1" /* ♡ */
  | Diamonds => "\xe2\x99\xa2" /* ♢ */
  | Clubs => "\xe2\x99\xa7" /* ♧ */
  | Spades => "\xe2\x99\xa4" /* ♤ */
  | UnknownS => "?";

let rank_str: CardProj.rank => string =
  fun
  | Ace => "A"
  | Two => "2"
  | Three => "3"
  | Four => "4"
  | Five => "5"
  | Six => "6"
  | Seven => "7"
  | Eight => "8"
  | Nine => "9"
  | Ten => "10"
  | Jack => "J"
  | Queen => "Q"
  | King => "K"
  | UnknownR => "?";

let card_face = ((suit, rank): CardProj.card): Frame.span => {
  let red =
    switch (suit) {
    | Hearts
    | Diamonds => true
    | _ => false
    };
  (
    {
      ...Style.default,
      fg: Ansi256(red ? 160 : 16), /* red / near-black pips */
      bg: Ansi256(255) /* white card face */
    },
    rank_str(rank) ++ suit_glyph(suit),
  );
};

let card: t = {
  inline_view: (~model as _, ~info, ~width) =>
    switch (CardProj.SyntaxTerm.get(info)) {
    | (_, Card(c)) =>
      Some(
        pad_to(
          width,
          [
            (
              {
                ...Style.default,
                bg: Ansi256(255),
              },
              " ",
            ),
            card_face(c),
          ],
        ),
      )
    | (_, Hand(cs)) => Some(pad_to(width, List.map(card_face, cs)))
    | exception _ => None
    },
  offside_view: no_offside,
  on_click: no_click,
  block_view: no_block,
};

/* Inline fallback for kinds without a bespoke terminal view (Livelit,
   Csv, ...): the underlying syntax as a dim one-line chip — strictly
   more informative than the blank-region default. */
let syntax_chip = (~info: ProjectorBase.info, ~width: int): option(Frame.row) =>
  if (width < 2) {
    None;
  } else {
    switch (info.utility.seg_to_string(info.syntax)) {
    | s =>
      let s = String.map(c => c == '\n' ? ' ' : c, s);
      Some(pad_to(width, [(Style.dim(Style.default), s)]));
    | exception _ => None
    };
  };

let lookup: ProjectorCore.Kind.t => option(t) =
  fun
  | Fold => Some(fold)
  | Checkbox => Some(checkbox)
  | Slider => Some(slider)
  | SliderF => Some(sliderf)
  | Statics => Some(type_display)
  | TextArea => Some(textarea)
  | Table => Some(table)
  | Probe => Some(probe)
  | Card => Some(card)
  | Livelit
  | Csv => None; /* covered by the syntax_chip fallback */
