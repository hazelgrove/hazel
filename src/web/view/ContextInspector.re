open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

let alias_view = (s: string): Node.t =>
  div(~attrs=[clss(["typ-alias-view"])], [text(s)]);

let jump_to = entry => Globals.Update.JumpToTile(Language.Ctx.get_id(entry));

/* Γ prints each entry's type whole, as tiles, at every redraw. On the
   Scenes slide one entry, a module of livelit definitions, printed to
   474,304 characters (91,567 type nodes; every member spells out Html.T
   and the command types): 6 s to print and measure, and then the page
   froze laying it out (^ticket(396)). The next largest entry on the
   playground slides is 693 nodes. So past a budget an entry shows an
   outline instead: a module's member names, else an ellipsis. */
let typ_budget = 2000;

let typ_within = (budget: int, ty: Language.Typ.t): bool => {
  let count = ref(0);
  let f = (cont, x: Language.Typ.t) => {
    incr(count);
    count^ > budget ? x : cont(x);
  };
  switch (Language.Typ.map_term(~f_typ=f, ty)) {
  | _ => count^ <= budget
  | exception _ => false
  };
};

let outline_text = (ty: Language.Typ.t): string =>
  switch (ty.term) {
  | Sig(items) =>
    "{ "
    ++ (
      Language.Sig.members(items)
      |> List.map((m: Language.Sig.member) =>
           switch (m) {
           | Val(x, _) => "let " ++ x
           | TypeManifest(x, _)
           | TypeAbstract(x) => "type " ++ x
           }
         )
      |> String.concat("; ")
    )
    ++ " }"
  | _ => "…"
  };

let too_large_view = (ty: Language.Typ.t): Node.t =>
  div(
    ~attrs=[
      clss(["typ-too-large"]),
      Attr.title(
        "This type has more than "
        ++ string_of_int(typ_budget)
        ++ " parts, too many to show here; click to go to its definition",
      ),
    ],
    [text(outline_text(ty))],
  );
/* How types print in Gamma: on one line, as the bar prints them */
let print_settings: Haz3lcore.ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: false,
  show_unknown_as_hole: true,
  use_literal_lexemes: false,
  hole_tiles: false,
  project_tables: false,
};

/* About how many columns of code an entry shows on its one line: the
   CSS's max-width (18em) in code columns, less the unfold toggle's. An
   entry longer than this is cut off, and gets a toggle to show it whole. */
let line_columns = 27;

/* An unfolded entry lays its type out to this many columns. Gamma's box
   has no width of its own (it is as wide as its entries), so an unfolded
   entry widens it: up to 60 columns, fewer on a narrow window. */
let unfold_columns = (globals: Globals.t): int => {
  let px = float_of_int(Js_of_ocaml.Dom_html.window##.innerWidth) -. 64.;
  max(
    line_columns,
    min(60, int_of_float(px /. globals.font_metrics.col_width) - 4),
  );
};

/* Laid-out types, by entry and width. Only unfolded entries are laid out,
   each once while its type stays the same object (as statics keeps an
   unchanged context's types), so a big context costs nothing until an
   entry is opened. */
let laid_out:
  Hashtbl.t((string, int), (Language.Typ.t, Haz3lcore.Segment.t)) =
  Hashtbl.create(16);

let lay_out = (~key, ~width, typ: Language.Typ.t): Haz3lcore.Segment.t =>
  switch (Hashtbl.find_opt(laid_out, (key, width))) {
  | Some((typ', seg)) when typ' === typ => seg
  | _ =>
    let seg =
      typ
      |> Haz3lcore.TypToSegment.typ_to_segment(
           ~settings={
             ...print_settings,
             inline: false,
           },
         )
      |> CodeViewable.lay_out(~width);
    Hashtbl.replace(laid_out, (key, width), (typ, seg));
    seg;
  };

/* Which entry an unfold is for. Not the binding's id alone: the prelude's
   entries share one. The separator tells a type's name from a value's. */
let entry_key = (~name, ~sep, entry: Language.Ctx.entry): string =>
  sep ++ name ++ "@" ++ Haz3lcore.Id.to_string(Language.Ctx.get_id(entry));

/* An entry with a type: its name, a separator and the type on one line,
   cut off if it is long. A long one gets a toggle (not the row, whose
   click jumps to the binding) that shows the type whole, laid out under
   the name, so the names stay in a column. */
let typ_entry_view =
    (
      ~globals: Globals.t,
      ~classes=[],
      ~name: string,
      ~name_node: Node.t,
      ~sep: string,
      entry: Language.Ctx.entry,
      typ: Language.Typ.t,
    )
    : Node.t =>
  /* Past the budget, an outline, and nothing to unfold: printing it at
     all is what froze the page (see typ_budget). Checked before the
     type is printed, even on one line. */
  if (!typ_within(typ_budget, typ)) {
    div(
      ~attrs=[
        Attr.on_click(_ => globals.inject_global(jump_to(entry))),
        clss(["context-entry", "code"] @ classes),
      ],
      [
        div(~attrs=[clss(["unfold", "none"])], []),
        name_node,
        div(~attrs=[clss(["seperator"])], [text(sep)]),
        too_large_view(typ),
      ],
    );
  } else {
    let key = entry_key(~name, ~sep, entry);
    let seg =
      Haz3lcore.TypToSegment.typ_to_segment(~settings=print_settings, typ);
    let columns =
      Util.Unicode.length(name)
      + String.length(sep)
      + 2
      + Util.Unicode.length(
          Haz3lcore.Printer.of_segment(~is_single_line=true, seg),
        );
    let long = columns > line_columns;
    let unfolded = long && List.mem(key, globals.context_unfolded);
    let toggle =
      long
        ? div(
            ~attrs=[
              clss(["unfold"]),
              Attr.title(
                unfolded
                  ? "Show the type on one line"
                  : "Show the whole type, on several lines",
              ),
              Attr.on_click(_ =>
                Effect.Many([
                  Effect.Stop_propagation,
                  globals.inject_global(ToggleContextUnfold(key)),
                ])
              ),
            ],
            [text(unfolded ? "\u{25BE}" : "\u{25B8}")],
          )
        : div(~attrs=[clss(["unfold", "none"])], []);
    let sep = div(~attrs=[clss(["seperator"])], [text(sep)]);
    let attrs = unfolded_class => [
      Attr.on_click(_ => globals.inject_global(jump_to(entry))),
      clss(["context-entry", "code"] @ classes @ unfolded_class),
    ];
    unfolded
      ? div(
          ~attrs=attrs(["unfolded"]),
          [
            div(~attrs=[clss(["entry-head"])], [toggle, name_node, sep]),
            div(
              ~attrs=[clss(["entry-typ"])],
              [
                CodeViewable.view_segment(
                  ~globals,
                  lay_out(~key, ~width=unfold_columns(globals), typ),
                ),
              ],
            ),
          ],
        )
      : div(
          ~attrs=attrs([]),
          [toggle, name_node, sep, CodeViewable.view_segment(~globals, seg)],
        );
  };

let context_entry_view = (~globals, entry: Language.Ctx.entry): Node.t => {
  let div_name = div(~attrs=[clss(["name"])]);
  switch (entry) {
  | VarEntry({name, typ, _})
  | ConstructorEntry({name, typ, _}) =>
    typ_entry_view(
      ~globals,
      ~name,
      ~name_node=div_name([text(name)]),
      ~sep=":",
      entry,
      typ,
    )
  | TVarEntry({name, kind: Singleton(ty), _}) =>
    /* an alias, with what it stands for */
    typ_entry_view(
      ~globals,
      ~name,
      ~name_node=div_name([alias_view(name)]),
      ~sep="::",
      entry,
      ty,
    )
  | TVarEntry({name, kind: Abstract, _}) =>
    div(
      ~attrs=[
        Attr.on_click(_ => globals.inject_global(jump_to(entry))),
        clss(["context-entry", "code"]),
      ],
      [
        div(~attrs=[clss(["unfold", "none"])], []),
        div_name([alias_view(name)]),
        div(~attrs=[clss(["seperator"])], [text("::")]),
        Kind.view(~globals, Abstract),
      ],
    )
  | LivelitEntry({name, expansion_t, _}) =>
    typ_entry_view(
      ~globals,
      ~classes=["livelit-entry"],
      ~name="^" ++ name,
      ~name_node=div_name([text("^" ++ name)]),
      ~sep=":",
      entry,
      expansion_t,
    )
  };
};

let ctx_view = (~globals, ctx: Language.Ctx.t): Node.t =>
  div(
    ~attrs=[clss(["context-inspector"])],
    List.map(
      context_entry_view(~globals),
      ctx
      |> Language.Ctx.filter_shadowed
      |> Language.Ctx.filter_stepper_filter_variables
      |> (x => x.entries)
      |> List.rev,
    ),
  );

let ctx_sorts_view = (~globals, ci: Language.Statics.Info.t) =>
  Language.Info.ctx_of(ci)
  |> Language.Ctx.filter_shadowed
  |> Language.Ctx.filter_stepper_filter_variables
  |> (x => x.entries)
  |> List.rev
  |> List.map(context_entry_view(~globals));

let view = (~globals: Globals.t, ci: option(Language.Statics.Info.t)): Node.t => {
  let clss =
    clss(
      ["context-inspector"]
      @ (globals.settings.context_inspector ? ["visible"] : []),
    );
  switch (ci) {
  | Some(ci) when globals.settings.context_inspector =>
    div(~attrs=[clss], ctx_sorts_view(~globals, ci))
  | _ => div([])
  };
};
