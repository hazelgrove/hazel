open BuiltinsUtil;
module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;

let sum_type = (variants: list((string, option(Typ.t)))): Typ.t =>
  variants
  |> List.map(((name, typ_opt)) =>
       ConstructorMap.Variant(
         name,
         ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ()),
         typ_opt,
       )
     )
  |> sum;

let meta_type: Typ.t = sum_type([("$e", None), ("$v", None)]);

module Ord = {
  let t: Typ.t = sum_type([("Lt", None), ("Eq", None), ("Gt", None)]);

  open IdTagged.FreshGrammar;
  let lt = Exp.constructor("Lt", Some(Some(t)));
  let eq = Exp.constructor("Eq", Some(Some(t)));
  let gt = Exp.constructor("Gt", Some(Some(t)));
  let lt_pat = Pat.constructor("Lt", Some(Some(t)));
  let eq_pat = Pat.constructor("Eq", Some(Some(t)));
  let gt_pat = Pat.constructor("Gt", Some(Some(t)));
};

module Either = {
  let t: Typ.t =
    sum_type([
      ("Left", Some(Unknown(Internal) |> Typ.fresh)),
      ("Right", Some(Unknown(Internal) |> Typ.fresh)),
    ]);

  open IdTagged.FreshGrammar;
  let left =
    Exp.constructor("Left", Some(Some(arrow(unknown(SynSwitch), t))));
  let right =
    Exp.constructor("Right", Some(Some(arrow(unknown(SynSwitch), t))));

  let pat_left =
    Pat.constructor("Left", Some(Some(arrow(unknown(SynSwitch), t))));
  let pat_right =
    Pat.constructor("Right", Some(Some(arrow(unknown(SynSwitch), t))));
};

module Option = {
  let t: Typ.t =
    sum_type([
      ("None", None),
      ("Some", Some(Unknown(Internal) |> Typ.fresh)),
    ]);

  open IdTagged.FreshGrammar;

  // Confirm that we want the type on the constructors for both expressions and patterns
  let none = Exp.constructor("None", Some(Some(t)));

  let some =
    Exp.constructor("Some", Some(Some(arrow(unknown(SynSwitch), t))));

  let pat_none = Pat.constructor("None", Some(Some(t)));

  let pat_some =
    Pat.constructor("Some", Some(Some(arrow(unknown(SynSwitch), t))));

  let builtins: list(hazel_fn) = [
    {
      str: {|fix option_map -> fun (opt, f) -> case opt
               | None => None
               | Some(x) => Some(f(x))
             end|},
      name: "option_map",
      arg: Prod([t, arrow(unknown(Internal), unknown(Internal))]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("option_map"),
              fn(
                Pat.tuple([Pat.var("opt"), Pat.var("f")]),
                match(
                  var("opt"),
                  [
                    (pat_none, none),
                    (
                      Pat.ap(pat_some, Pat.var("x")),
                      ap(Forward, some, ap(Forward, var("f"), var("x"))),
                    ),
                  ],
                ),
                None,
                Some("option_map+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix option_bind -> fun (opt, f) -> case opt
               | None => None
               | Some x => f(x)
             end|},
      name: "option_bind",
      arg: Prod([t, arrow(unknown(Internal), unknown(Internal))]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("option_bind"),
              fn(
                Pat.tuple([Pat.var("opt"), Pat.var("f")]),
                match(
                  var("opt"),
                  [
                    (pat_none, none),
                    (
                      Pat.ap(pat_some, Pat.var("x")),
                      ap(Forward, var("f"), var("x")),
                    ),
                  ],
                ),
                None,
                Some("option_bind+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      name: "option_to_list",
      arg: t.term,
      ret: List(unknown(Internal)),
      str: {|fix option_to_list -> fun opt -> case opt
               | None => []
               | Some x => [x]
             end|},
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("option_to_list"),
              fn(
                Pat.var("opt"),
                match(
                  var("opt"),
                  [
                    (pat_none, list_lit([])),
                    (
                      Pat.ap(pat_some, Pat.var("x")),
                      list_lit([var("x")]),
                    ),
                  ],
                ),
                None,
                Some("option_to_list+"),
              ),
              None,
            )
          )
        );
      },
    },
  ];
};

// Event data types for keyboard and mouse events
module Event = {
  // KeyEvent: { key: String, code: String, ctrl: Bool, shift: Bool, alt: Bool, meta: Bool }
  let key: Typ.t =
    prod([
      string(), // key
      string(), // code
      bool(), // ctrl
      bool(), // shift
      bool(), // alt
      bool() // meta
    ]);

  // MouseEvent: { clientX: Float, clientY: Float, button: Int, ctrl: Bool, shift: Bool, alt: Bool, meta: Bool }
  let mouse: Typ.t =
    prod([
      float(), // clientX
      float(), // clientY
      int(), // button (0=left, 1=middle, 2=right)
      bool(), // ctrl
      bool(), // shift
      bool(), // alt
      bool() // meta
    ]);
};

module HTML = {
  // Helper for elements with attrs and children: (List(Attr), List(HTML))
  let elem_body = () => prod([list(var("Attr")), list(var("HTML"))]);
  // Helper for elements with attrs only: List(Attr)
  let attrs_only = () => list(var("Attr"));

  let t: Typ.t =
    IdTagged.FreshGrammar.Typ.rec_(
      IdTagged.FreshGrammar.TPat.var("HTML"),
      sum_type([
        // === Text content ===
        ("Text", Some(string())),
        // === Primitive value display (convenience) ===
        ("Bool", Some(bool())),
        ("Int", Some(int())),
        ("Float", Some(float())),
        // === Structural elements ===
        ("Div", Some(elem_body())),
        ("Span", Some(elem_body())),
        ("P", Some(elem_body())),
        ("Pre", Some(elem_body())),
        ("Code", Some(elem_body())),
        ("Blockquote", Some(elem_body())),
        // === Headings ===
        ("H1", Some(elem_body())),
        ("H2", Some(elem_body())),
        ("H3", Some(elem_body())),
        ("H4", Some(elem_body())),
        ("H5", Some(elem_body())),
        ("H6", Some(elem_body())),
        // === Lists ===
        ("Ul", Some(elem_body())),
        ("Ol", Some(elem_body())),
        ("Li", Some(elem_body())),
        // === Forms ===
        ("Form", Some(elem_body())),
        ("Label", Some(elem_body())),
        ("Input", Some(attrs_only())),
        ("TextArea", Some(prod([attrs_only(), string()]))), // attrs, content
        ("Button", Some(elem_body())), // Changed: now takes children too
        ("Select", Some(elem_body())),
        ("Option", Some(prod([attrs_only(), string()]))), // attrs, label text
        // === Legacy input variants (for backwards compat) ===
        ("Checkbox", Some(attrs_only())),
        ("Radio", Some(attrs_only())),
        ("Range", Some(attrs_only())),
        // === Links and media ===
        ("A", Some(elem_body())),
        ("Img", Some(attrs_only())),
        // === Tables ===
        ("Table", Some(elem_body())),
        ("Thead", Some(elem_body())),
        ("Tbody", Some(elem_body())),
        ("Tr", Some(elem_body())),
        ("Th", Some(elem_body())),
        ("Td", Some(elem_body())),
        // === Semantic sections ===
        ("Header", Some(elem_body())),
        ("Footer", Some(elem_body())),
        ("Nav", Some(elem_body())),
        ("Main", Some(elem_body())),
        ("Section", Some(elem_body())),
        ("Article", Some(elem_body())),
        ("Aside", Some(elem_body())),
        // === Utility elements ===
        ("Br", None),
        ("Hr", Some(attrs_only())),
        // === Generic element (escape hatch) ===
        // Node(tagName, attrs, children)
        ("Node", Some(prod([string(), attrs_only(), list(var("HTML"))]))),
      ]),
    );

  let attr: Typ.t =
    sum_type([
      // === Identity ===
      ("Id", Some(string())),
      ("Class", Some(string())),
      ("Classes", Some(list(string()))),
      // === Common properties ===
      ("Disabled", Some(bool())),
      ("Placeholder", Some(string())),
      ("Value", Some(string())),
      ("Checked", Some(bool())),
      ("Selected", Some(bool())),
      ("ReadOnly", Some(bool())),
      ("Required", Some(bool())),
      ("AutoFocus", Some(bool())),
      // === Links/media ===
      ("Href", Some(string())),
      ("Src", Some(string())),
      ("Alt", Some(string())),
      ("Title", Some(string())),
      ("Target", Some(string())),
      // === Input specifics ===
      ("Type", Some(string())),
      ("Name", Some(string())),
      ("Min", Some(string())),
      ("Max", Some(string())),
      ("Step", Some(string())),
      ("MaxLength", Some(int())),
      ("Pattern", Some(string())),
      // === Layout ===
      ("Width", Some(string())),
      ("Height", Some(string())),
      ("ColSpan", Some(int())),
      ("RowSpan", Some(int())),
      // === Styling ===
      ("Style", Some(list(prod([string(), string()])))),
      // === Data attributes ===
      ("Data", Some(prod([string(), string()]))), // data-{name}={value}
      // === Event handlers (self-modifying pattern) ===
      // Simple events: Html -> Html
      ("OnClick", Some(arrow(var("HTML"), var("HTML")))),
      ("OnDoubleClick", Some(arrow(var("HTML"), var("HTML")))),
      ("OnMouseEnter", Some(arrow(var("HTML"), var("HTML")))),
      ("OnMouseLeave", Some(arrow(var("HTML"), var("HTML")))),
      ("OnFocus", Some(arrow(var("HTML"), var("HTML")))),
      ("OnBlur", Some(arrow(var("HTML"), var("HTML")))),
      ("OnSubmit", Some(arrow(var("HTML"), var("HTML")))),
      // Events with mouse data: (Html, MouseEvent) -> Html
      (
        "OnMouseDown",
        Some(arrow(prod([var("HTML"), var("MouseEvent")]), var("HTML"))),
      ),
      (
        "OnMouseUp",
        Some(arrow(prod([var("HTML"), var("MouseEvent")]), var("HTML"))),
      ),
      (
        "OnMouseMove",
        Some(arrow(prod([var("HTML"), var("MouseEvent")]), var("HTML"))),
      ),
      // Events with key data: (Html, KeyEvent) -> Html
      (
        "OnKeyDown",
        Some(arrow(prod([var("HTML"), var("KeyEvent")]), var("HTML"))),
      ),
      (
        "OnKeyUp",
        Some(arrow(prod([var("HTML"), var("KeyEvent")]), var("HTML"))),
      ),
      (
        "OnKeyPress",
        Some(arrow(prod([var("HTML"), var("KeyEvent")]), var("HTML"))),
      ),
      // Events with string data: (Html, String) -> Html
      (
        "OnInput",
        Some(arrow(prod([var("HTML"), string()]), var("HTML"))),
      ),
      (
        "OnChange",
        Some(arrow(prod([var("HTML"), string()]), var("HTML"))),
      ),
      // === Legacy/generic attribute (backwards compat) ===
      ("Create", Some(prod([string(), string()]))), // generic attr(name, value)
      ("BoolAttr", Some(prod([string(), bool()]))), // generic bool attr
      // === Legacy event (backwards compat with old OnMousedown casing) ===
      ("OnMousedown", Some(arrow(var("HTML"), var("HTML")))),
    ]);
};

// Command type for side effects (fire-and-forget)
module Cmd = {
  let t: Typ.t =
    IdTagged.FreshGrammar.Typ.rec_(
      IdTagged.FreshGrammar.TPat.var("Cmd"),
      sum_type([
        // === No-op ===
        ("CmdNone", None),
        // === Batch multiple commands ===
        ("Batch", Some(list(var("Cmd")))),
        // === DOM manipulation ===
        ("Focus", Some(string())), // element id
        ("Blur", Some(string())), // element id
        ("ScrollIntoView", Some(string())), // element id
        ("ScrollTo", Some(prod([string(), float(), float()]))), // id, x, y
        // === Clipboard ===
        ("CopyToClipboard", Some(string())),
        // === Time-delayed state update ===
        (
          "Delay",
          Some(prod([float(), arrow(var("HTML"), var("HTML"))])),
        ), // ms, transform
        // === Debugging ===
        ("Log", Some(string())),
      ]),
    );
};

// List of type aliases to add to the context
// Some are sum types (with constructors), others are product types (no constructors)
let type_aliases: list((string, Typ.t)) = [
  ("Ord", Ord.t),
  ("Option", Option.t),
  ("Either", Either.t),
  ("KeyEvent", Event.key),
  ("MouseEvent", Event.mouse),
  ("HTML", HTML.t),
  ("Attr", HTML.attr),
  ("Cmd", Cmd.t),
  ("$Meta", meta_type),
];

let create_type_alias = (name: string, typ: Typ.t): Ctx.entry =>
  Ctx.TVarEntry({
    name,
    id: Id.invalid,
    kind: Ctx.Singleton(typ),
  });

// Convert type aliases to context entries
let types: list(Ctx.entry) =
  List.map(((name, typ)) => create_type_alias(name, typ), type_aliases);

// Add constructors for sum type aliases to the context
// Product types (like KeyEvent, MouseEvent) have no constructors
let constructors: Ctx.t = {
  List.fold_left(
    (ctx, (name, typ)) => {
      switch (Typ.term_of(typ)) {
      | Sum(cons_map) => Ctx.add_ctrs(ctx, name, Id.invalid, cons_map)
      | Rec(_, {term: Sum(cons_map), _}) =>
        Ctx.add_ctrs(ctx, name, Id.invalid, cons_map)
      | _ => ctx // Product types have no constructors to add
      }
    },
    Ctx.empty,
    type_aliases,
  );
};

let builtins = Option.builtins;
let constructor_entries = constructors.entries @ types;
