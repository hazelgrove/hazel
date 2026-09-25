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

// Event data types for keyboard and mouse events.
// Labeled so handlers can use projection (e.key, e.ctrl) instead of
// positional destructuring.
module Event = {
  let field = (name: string, ty: Typ.t): Typ.t =>
    tup_label(label(name), ty);

  // KeyEvent: (key=String, code=String, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)
  let key: Typ.t =
    prod([
      field("key", string()),
      field("code", string()),
      field("ctrl", bool()),
      field("shift", bool()),
      field("alt", bool()),
      field("meta", bool()),
    ]);

  // MouseEvent: (x=Float, y=Float, button=Int, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool,
  //              occurred=Float, handled=Float)
  // button: 0=left, 1=middle, 2=right
  //
  // occurred: the browser's own stamp for when the event happened.
  // handled:  when Hazel got round to dispatching it.
  // Both are performance.now()-relative milliseconds, so their
  // DIFFERENCE is how long the event waited on a busy editor -- the one
  // number a program can use to measure Hazel's responsiveness from the
  // inside. Appended last so anything reading the older fields by
  // position is undisturbed.
  let mouse: Typ.t =
    prod([
      field("x", float()),
      field("y", float()),
      field("button", int()),
      field("ctrl", bool()),
      field("shift", bool()),
      field("alt", bool()),
      field("meta", bool()),
      field("occurred", float()),
      field("handled", float()),
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
        /* Splice(r): place the splice `r` names here -- a hole holding
           the client's own code, edited in place inside the widget and
           typed in the client's scope. The ref comes from the model:
           a field marked with parens has type (ref=SpliceRef, value=t). */
        ("Splice", Some(var("SpliceRef"))),
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
      // === Event handlers (Elm-style: handlers produce messages) ===
      // Simple events: handler IS the msg value (Unknown)
      ("OnClick", Some(unknown(Internal))),
      ("OnDoubleClick", Some(unknown(Internal))),
      ("OnMouseEnter", Some(unknown(Internal))),
      ("OnMouseLeave", Some(unknown(Internal))),
      ("OnFocus", Some(unknown(Internal))),
      ("OnBlur", Some(unknown(Internal))),
      ("OnSubmit", Some(unknown(Internal))),
      // Events with mouse data: MouseEvent -> msg
      ("OnMouseDown", Some(arrow(var("MouseEvent"), unknown(Internal)))),
      ("OnMouseUp", Some(arrow(var("MouseEvent"), unknown(Internal)))),
      ("OnMouseMove", Some(arrow(var("MouseEvent"), unknown(Internal)))),
      // Mouse events with element-relative position: (x, y) in px -> msg
      ("OnClickAt", Some(arrow(prod([int(), int()]), unknown(Internal)))),
      (
        "OnMouseDownAt",
        Some(arrow(prod([int(), int()]), unknown(Internal))),
      ),
      (
        "OnMouseMoveAt",
        Some(arrow(prod([int(), int()]), unknown(Internal))),
      ),
      (
        "OnMouseUpAt",
        Some(arrow(prod([int(), int()]), unknown(Internal))),
      ),
      // Wheel with element-relative position: (x, y, dx, dy) -> msg
      (
        "OnWheelAt",
        Some(
          arrow(prod([int(), int(), float(), float()]), unknown(Internal)),
        ),
      ),
      // Events with key data: KeyEvent -> msg
      ("OnKeyDown", Some(arrow(var("KeyEvent"), unknown(Internal)))),
      ("OnKeyUp", Some(arrow(var("KeyEvent"), unknown(Internal)))),
      ("OnKeyPress", Some(arrow(var("KeyEvent"), unknown(Internal)))),
      // Events with string data: String -> msg
      ("OnInput", Some(arrow(string(), unknown(Internal)))),
      ("OnChange", Some(arrow(string(), unknown(Internal)))),
      // === Generic attribute escape hatches ===
      ("Create", Some(prod([string(), string()]))), // generic attr(name, value)
      ("BoolAttr", Some(prod([string(), bool()]))) // generic bool attr
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
        ("CmdBatch", Some(list(var("Cmd")))),
        // === DOM manipulation ===
        ("Focus", Some(string())), // element id
        ("Blur", Some(string())), // element id
        ("ScrollIntoView", Some(string())), // element id
        ("ScrollTo", Some(prod([string(), float(), float()]))), // id, x, y
        // === Clipboard ===
        ("CopyToClipboard", Some(string())),
        // === Time-delayed message dispatch ===
        ("Delay", Some(prod([float(), unknown(Internal)]))), // ms, msg
        // === Sound ===
        ("PlayTone", Some(prod([float(), float()]))), // freq Hz, duration ms
        // === Speech ===
        ("Say", Some(string())),
        // === Randomness (Elm-style: drawn at the boundary, not in eval) ===
        ("Random", Some(arrow(float(), unknown(Internal)))), // [0,1) -> msg
        // === Debugging ===
        ("Log", Some(string())),
      ]),
    );
};

// Subscription type for event sources (continuous events)
module Sub = {
  let t: Typ.t =
    IdTagged.FreshGrammar.Typ.rec_(
      IdTagged.FreshGrammar.TPat.var("Sub"),
      sum_type([
        // === No-op ===
        ("SubNone", None),
        // === Batch multiple subscriptions ===
        ("SubBatch", Some(list(var("Sub")))),
        // === Window events ===
        // OnResize: (Int, Int) -> msg
        (
          "OnResize",
          Some(arrow(prod([int(), int()]), unknown(Internal))),
        ),
        // OnVisibilityChange: Bool -> msg
        ("OnVisibilityChange", Some(arrow(bool(), unknown(Internal)))),
        // === Global keyboard (document level) ===
        // OnDocumentKeyDown: KeyEvent -> msg
        (
          "OnDocumentKeyDown",
          Some(arrow(var("KeyEvent"), unknown(Internal))),
        ),
        // OnDocumentKeyUp: KeyEvent -> msg
        (
          "OnDocumentKeyUp",
          Some(arrow(var("KeyEvent"), unknown(Internal))),
        ),
        // === Time-based ===
        // Every: (interval ms, Float -> msg)
        (
          "Every",
          Some(prod([float(), arrow(float(), unknown(Internal))])),
        ),
        // AnimationFrame: Float -> msg
        ("AnimationFrame", Some(arrow(float(), unknown(Internal)))),
      ]),
    );
};

// App type for full applications with Elm-style MVU architecture
// App = (init_model, update, view, subs) where
//   update: (msg, model) -> model, or (msg, model) -> (model, Cmd)
//   view: model -> HTML
//   subs: model -> Sub
module App = {
  let t: Typ.t =
    prod([
      unknown(Internal), // init_model
      arrow(
        prod([unknown(Internal), unknown(Internal)]), // update: (msg, model) ->
        unknown(Internal),
      ), //   model (or (model, Cmd))
      arrow(unknown(Internal), var("HTML")), // view: model -> HTML
      arrow(unknown(Internal), var("Sub")) // subs: model -> Sub
    ]);
};

module JSON = {
  /* Self-reference for the recursive type */
  let self: Typ.t = var("JSON");

  /* type JSON =
     + Assoc([(String, JSON)])
     + Bool(Bool)
     + Float(Float)
     + Int(Int)
     + List([JSON])
     + String(String)
     + Null */
  let t: Typ.t =
    rec_(
      Fresh.TPat.var("JSON"),
      sum_type([
        ("Assoc", Some(list(prod([string(), self])))),
        ("Bool", Some(bool())),
        ("Float", Some(float())),
        ("Int", Some(int())),
        ("List", Some(list(self))),
        ("String", Some(string())),
        ("Null", None),
      ]),
    );
};

// Text footprint of a livelit's GUI: the livelit `shape` member.
// Inline(w) is one line wide w columns; Block(w, h) and Tab(w, h) are
// h lines tall — code flows below a Block but continues on the TOP
// line beside a Tab.
module LivelitShape = {
  let t: Typ.t =
    sum_type([
      ("Inline", Some(int())),
      ("Block", Some(prod([int(), int()]))),
      ("Tab", Some(prod([int(), int()]))),
    ]);
};

/* Keyboard shortcuts, used as the analyzed type of the Shortcuts config
 * slide (see ShortcutConfiguration / ConfigurationMode).
 *
 * `Meta` is deliberately abstract rather than a literal "cmd"/"ctrl"
 * string: it resolves to cmd on macOS and ctrl elsewhere at the moment a
 * binding is applied, so one config program means the same thing on every
 * machine. `Ctrl` is the literal control key, for bindings that should NOT
 * follow the platform.
 *
 * `Unbound` is how an action says it has no shortcut — the reason the type
 * is a sum rather than a bare String. */
module Shortcut = {
  /* ---- The Hazel types, registered in the builtin context ---- */

  /* type KeyMod = Meta + Ctrl + Shift + Alt */
  let key_mod_typ: Typ.t =
    sum_type([
      ("Meta", None),
      ("Ctrl", None),
      ("Shift", None),
      ("Alt", None),
    ]);

  /* The chord: which modifiers are held, and the key itself. */
  let chord_typ: Typ.t = prod([list(var("KeyMod")), string()]);

  /* type Shortcut = Unbound + Bound(([KeyMod], String)) */
  let typ: Typ.t =
    sum_type([("Unbound", None), ("Bound", Some(chord_typ))]);

  /* ---- The OCaml mirror, plus the encoding between the two ----

     Kept here beside the Hazel types so there is exactly one definition of
     what a shortcut is. Both the config slide (ShortcutConfiguration, in
     web) and the keybinding projector (KeybindingProj, in haz3lcore) read
     and write shortcut syntax through these. */

  /* list converters for the derivings below; a bare `open Util` here would
     shadow this module's own Option. */
  open Sexplib.Std;
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type key_mod =
    | Meta
    | Ctrl
    | Shift
    | Alt;

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type binding =
    | Unbound
    | Bound(list(key_mod), string);

  let all_key_mods = [Meta, Ctrl, Shift, Alt];

  let name_of_key_mod = (m: key_mod): string =>
    switch (m) {
    | Meta => "Meta"
    | Ctrl => "Ctrl"
    | Shift => "Shift"
    | Alt => "Alt"
    };

  /* Built fresh per occurrence, never hoisted to a module-level value:
     FreshGrammar mints the id when the combinator is CALLED, so a shared
     value would hand every occurrence the same id — statics still passes,
     but the editor collapses them into one tile with N shards and
     Highlight.of_tile fails at render. Unannotated, exactly as a
     constructor the user typed would parse. */
  let ctr = (name: string): Exp.t =>
    IdTagged.FreshGrammar.Exp.constructor(name, None);

  let exp_of_key_mod = (m: key_mod): Exp.t => ctr(name_of_key_mod(m));

  let exp_of_binding = (b: binding): Exp.t => {
    IdTagged.FreshGrammar.Exp.(
      switch (b) {
      | Unbound => ctr("Unbound")
      | Bound(mods, key) =>
        ap(
          Forward,
          ctr("Bound"),
          tuple([list_lit(List.map(exp_of_key_mod, mods)), string(key)]),
        )
      }
    );
  };

  let key_mod_of_exp = (v: Exp.t): option(key_mod) =>
    List.find_map(
      m =>
        switch (Unboxing.unbox(SumNoArg(name_of_key_mod(m)), v)) {
        | Matches () => Some(m)
        | _ => None
        },
      all_key_mods,
    );

  let binding_of_exp = (v: Exp.t): option(binding) =>
    switch (Unboxing.unbox(SumNoArg("Unbound"), v)) {
    | Matches () => Some(Unbound)
    | _ =>
      switch (Unboxing.unbox(SumWithArg("Bound"), v)) {
      | Matches(arg) =>
        switch (Unboxing.unbox(Tuple(2), arg)) {
        | Matches([mods, key]) =>
          switch (
            Unboxing.unbox(ListLit, mods),
            Unboxing.unbox(Atom(String), key),
          ) {
          | (Matches(ms), Matches(k)) =>
            Some(Bound(List.filter_map(key_mod_of_exp, ms), k))
          | _ => None
          }
        | _ => None
        }
      | _ => None
      }
    };

  /* ---- Resolution: the ONLY place the platform is consulted ---- */

  let string_of_key_mod = (m: key_mod): string =>
    switch (m) {
    | Meta => Util.Os.is_mac^ ? "cmd" : "ctrl"
    | Ctrl => "ctrl"
    | Shift => "shift"
    | Alt => "alt"
    };

  /* Canonical modifier order so a rendered chord is stable; hotkeys-js
     compares sorted key codes, so the order is display only. */
  let string_of_chord = (mods: list(key_mod), key: string): string => {
    let ordered =
      List.filter(m => List.mem(m, mods), [Meta, Ctrl, Alt, Shift]);
    String.concat("+", List.map(string_of_key_mod, ordered) @ [key]);
  };

  let string_of_binding = (b: binding): option(string) =>
    switch (b) {
    | Unbound => None
    | Bound(mods, key) => Some(string_of_chord(mods, key))
    };
};

/* Colours, used as the analyzed type of the Colors config slide.

   * Only DATA constructors live here — the arithmetic (lighten, mix, …) is a
   * set of ordinary builtin functions in BuiltinsColor.re. That split is
   * deliberate: because the maths is functions, every role in the config
   * EVALUATES down to a canonical `Oklch(l, c, h)`, which is the form both the
   * CSS applier and a future colour-picker projector want to read and write. If
   * mixing were a constructor instead, a role's value would be an unevaluated
   * tree and neither could do anything useful with it.
   *
   * `Transparent` and `Rgb` occupy fairly common constructor names. A user
   * program that declares its own shadows these lexically, as usual. */
module Color = {
  /* Self-reference: Fade wraps another colour. */
  let self: Typ.t = var("ColorValue");

  /* type ColorValue =
     + Oklch((Float, Float, Float))   /* l 0..100, chroma, hue degrees */
     + Fade((ColorValue, Float))      /* alpha 0..100 */
     + Rgb((Int, Int, Int))          /* sRGB bytes, 0..255 */
     + Transparent */
  let typ: Typ.t =
    rec_(
      Fresh.TPat.var("ColorValue"),
      sum_type([
        ("Oklch", Some(prod([float(), float(), float()]))),
        ("Fade", Some(prod([self, float()]))),
        ("Rgb", Some(prod([int(), int(), int()]))),
        ("Transparent", None),
      ]),
    );

  /* ---- OCaml mirror, plus the encoding between the two ---- */

  /* float/string converters for the derivings below; a bare `open Util` here
     would shadow this module's own Option. */
  open Sexplib.Std;
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t =
    | Oklch(float, float, float)
    | Fade(t, float)
    | Rgb(int, int, int)
    | Transparent;

  /* Fresh per occurrence, never a module-level value — FreshGrammar mints the
     id at call time, and a shared constructor collapses every occurrence into
     one tile, crashing Highlight.of_tile. */
  let ctr = (name: string): Exp.t =>
    IdTagged.FreshGrammar.Exp.constructor(name, None);

  let rec exp_of: t => Exp.t =
    fun
    | Transparent => ctr("Transparent")
    | Oklch(l, c, h) =>
      IdTagged.FreshGrammar.Exp.(
        ap(Forward, ctr("Oklch"), tuple([float(l), float(c), float(h)]))
      )
    | Rgb(r, g, b) =>
      IdTagged.FreshGrammar.Exp.(
        ap(Forward, ctr("Rgb"), tuple([int(r), int(g), int(b)]))
      )
    | Fade(inner, a) =>
      IdTagged.FreshGrammar.Exp.(
        ap(Forward, ctr("Fade"), tuple([exp_of(inner), float(a)]))
      );

  /* Strip ascriptions first: a builtin's result carries an ascription to its
     declared return type, so `color_with_lightness(...)` decodes as
     `Asc(Oklch(...), ColorValue)` and the unboxing below would miss it. */
  let rec of_exp = (v: Exp.t): option(t) => {
    let v = DHExp.strip_ascriptions(v);
    switch (Unboxing.unbox(SumNoArg("Transparent"), v)) {
    | Matches () => Some(Transparent)
    | _ =>
      switch (Unboxing.unbox(SumWithArg("Rgb"), v)) {
      | Matches(arg) =>
        switch (Unboxing.unbox(Tuple(3), arg)) {
        | Matches([r, g, b]) =>
          switch (
            Unboxing.unbox(Atom(Int), r),
            Unboxing.unbox(Atom(Int), g),
            Unboxing.unbox(Atom(Int), b),
          ) {
          /* Hazel Ints are arbitrary-precision, so a byte has to come back
             through `Bigint.to_int`; anything outside 0..255 is clamped
             rather than rejected, since a picker drag can overshoot. */
          | (Matches(r), Matches(g), Matches(b)) =>
            let byte = v =>
              switch (Bigint.to_int(v)) {
              | Some(i) => Some(min(255, max(0, i)))
              | None => None
              };
            switch (byte(r), byte(g), byte(b)) {
            | (Some(r), Some(g), Some(b)) => Some(Rgb(r, g, b))
            | _ => None
            };
          | _ => None
          }
        | _ => None
        }
      | _ =>
        switch (Unboxing.unbox(SumWithArg("Oklch"), v)) {
        | Matches(arg) =>
          switch (Unboxing.unbox(Tuple(3), arg)) {
          | Matches([l, c, h]) =>
            switch (
              Unboxing.unbox(Atom(Float), l),
              Unboxing.unbox(Atom(Float), c),
              Unboxing.unbox(Atom(Float), h),
            ) {
            | (Matches(l), Matches(c), Matches(h)) => Some(Oklch(l, c, h))
            | _ => None
            }
          | _ => None
          }
        | _ =>
          switch (Unboxing.unbox(SumWithArg("Fade"), v)) {
          | Matches(arg) =>
            switch (Unboxing.unbox(Tuple(2), arg)) {
            | Matches([inner, a]) =>
              switch (of_exp(inner), Unboxing.unbox(Atom(Float), a)) {
              | (Some(inner), Matches(a)) => Some(Fade(inner, a))
              | _ => None
              }
            | _ => None
            }
          | _ => None
          }
        }
      }
    };
  };

  /* ---- Rendering to CSS ---- */

  /* Fixed-point with the tail trimmed, NOT %g and never string_of_float.
     %g emits scientific notation for small or large magnitudes (0.00001
     becomes "1e-05", 1234567. becomes "1.23457e+06") and string_of_float
     leaves a trailing dot ("90."). All three are invalid inside oklch(), and
     an invalid value makes setProperty a SILENT no-op — the variable simply
     keeps its old value with nothing logged. Non-finite components come from
     a hole or a divide-by-zero in the config and are pinned to 0 for the
     same reason. */
  let num = (f: float): string =>
    if (!Float.is_finite(f)) {
      "0";
    } else {
      let s = Printf.sprintf("%.5f", f);
      let last = ref(String.length(s));
      while (last^ > 1 && s.[last^ - 1] == '0') {
        decr(last);
      };
      let s = String.sub(s, 0, last^);
      let n = String.length(s);
      n > 0 && s.[n - 1] == '.' ? String.sub(s, 0, n - 1) : s;
    };

  /* Alpha goes through color-mix so it composes with any inner colour rather
     than only with the oklch() slash form. */
  let rec to_css: t => string =
    fun
    /* Not the `transparent` keyword: a keyword behaves differently as the
       origin of a relative colour, and variables.css uses `oklch(from …)`
       against palette entries. */
    | Transparent => "oklch(0 0 0 / 0)"
    /* The author's own numbers, kept as written. Arithmetic resolves Rgb into
       the OKLCH working space, so a value only stays Rgb when nothing touched
       it -- and then echoing it back is the least surprising thing. */
    | Rgb(r, g, b) => Printf.sprintf("rgb(%d, %d, %d)", r, g, b)
    | Oklch(l, c, h) =>
      "oklch(" ++ num(l) ++ "% " ++ num(c) ++ " " ++ num(h) ++ ")"
    | Fade(inner, a) =>
      "color-mix(in oklch, "
      ++ to_css(inner)
      ++ " "
      ++ num(a)
      ++ "%, transparent)";

  /* ---- sRGB conversion ----

     OKLCH is the palette's working space, but people arrive with hex codes
     and the picker offers an RGB mode, so both directions are needed. These
     are the Ottosson matrices; `l` is 0..100 here, 0..1 in the maths. */

  let srgb_of_linear = (c: float): float =>
    c <= 0.0031308 ? 12.92 *. c : 1.055 *. c ** (1.0 /. 2.4) -. 0.055;
  let linear_of_srgb = (c: float): float =>
    c <= 0.04045 ? c /. 12.92 : ((c +. 0.055) /. 1.055) ** 2.4;

  /* Clamped to the sRGB cube: OKLCH describes colours no display can show,
     and every consumer here wants a drawable byte. */
  let rgb_of_oklch = ((l, c, h): (float, float, float)): (int, int, int) => {
    let hr = h *. Float.pi /. 180.;
    let (a, b) = (c *. cos(hr), c *. sin(hr));
    let l' = l /. 100.;
    let (l_, m_, s_) = (
      l' +. 0.3963377774 *. a +. 0.2158037573 *. b,
      l' -. 0.1055613458 *. a -. 0.0638541728 *. b,
      l' -. 0.0894841775 *. a -. 1.2914855480 *. b,
    );
    let (lc, mc, sc) = (l_ *. l_ *. l_, m_ *. m_ *. m_, s_ *. s_ *. s_);
    let byte = v =>
      int_of_float(
        Float.round(
          Float.min(1., Float.max(0., srgb_of_linear(v))) *. 255.,
        ),
      );
    (
      byte(4.0767416621 *. lc -. 3.3077115913 *. mc +. 0.2309699292 *. sc),
      byte((-1.2684380046) *. lc +. 2.6097574011 *. mc -. 0.3413193965 *. sc),
      byte((-0.0041960863) *. lc -. 0.7034186147 *. mc +. 1.7076147010 *. sc),
    );
  };

  let oklch_of_rgb = ((r, g, b): (int, int, int)): (float, float, float) => {
    let lin = v => linear_of_srgb(float_of_int(v) /. 255.);
    let (r, g, b) = (lin(r), lin(g), lin(b));
    let cbrt = x => x < 0. ? -. (-. x ** (1. /. 3.)) : x ** (1. /. 3.);
    let (l_, m_, s_) = (
      cbrt(0.4122214708 *. r +. 0.5363325363 *. g +. 0.0514459929 *. b),
      cbrt(0.2119034982 *. r +. 0.6806995451 *. g +. 0.1073969566 *. b),
      cbrt(0.0883024619 *. r +. 0.2817188376 *. g +. 0.6299787005 *. b),
    );
    let ll = 0.2104542553 *. l_ +. 0.7936177850 *. m_ -. 0.0040720468 *. s_;
    let a = 1.9779984951 *. l_ -. 2.4285922050 *. m_ +. 0.4505937099 *. s_;
    let bb = 0.0259040371 *. l_ +. 0.7827717662 *. m_ -. 0.8086757660 *. s_;
    let h = atan2(bb, a) *. 180. /. Float.pi;
    (ll *. 100., sqrt(a *. a +. bb *. bb), h < 0. ? h +. 360. : h);
  };

  /* --- sRGB <-> HSV ---------------------------------------------------

     Not a space the language knows about; it exists because a saturation x
     value square under a hue strip is how people expect to pick an sRGB
     colour, and neither stored representation lays out that way. Here rather
     than in the picker so it is tested beside the conversions it resembles.

     h 0..360 wrapping, s and v 0..1. Every triple is inside the sRGB cube --
     the whole difference from OKLCH, which has a region to clamp. */

  let rgb_of_hsv = ((h, s, v): (float, float, float)): (int, int, int) => {
    let h = Float.rem(Float.rem(h, 360.) +. 360., 360.);
    let s = Float.min(1., Float.max(0., s));
    let v = Float.min(1., Float.max(0., v));
    let sector = h /. 60.;
    let i = int_of_float(Float.floor(sector));
    let f = sector -. Float.floor(sector);
    let (p, q, t) = (
      v *. (1. -. s),
      v *. (1. -. s *. f),
      v *. (1. -. s *. (1. -. f)),
    );
    let (r, g, b) =
      switch (i mod 6) {
      | 0 => (v, t, p)
      | 1 => (q, v, p)
      | 2 => (p, v, t)
      | 3 => (p, q, v)
      | 4 => (t, p, v)
      | _ => (v, p, q)
      };
    let byte = x => int_of_float(Float.round(x *. 255.));
    (byte(r), byte(g), byte(b));
  };

  /* Grey has no hue and black has no saturation either, so a bare conversion
     invents them -- and inventing zero is what makes a picker's hue jump home
     when value hits the bottom. `~like` is handed back in those cases. */
  let hsv_of_rgb =
      (
        ~like as (h0, s0, _): (float, float, float),
        (r, g, b): (int, int, int),
      )
      : (float, float, float) => {
    let (rf, gf, bf) = (
      float_of_int(r) /. 255.,
      float_of_int(g) /. 255.,
      float_of_int(b) /. 255.,
    );
    let mx = Float.max(rf, Float.max(gf, bf));
    let mn = Float.min(rf, Float.min(gf, bf));
    let d = mx -. mn;
    let h =
      if (d == 0.) {
        h0;
      } else {
        let h =
          if (mx == rf) {
            60. *. Float.rem((gf -. bf) /. d, 6.);
          } else if (mx == gf) {
            60. *. ((bf -. rf) /. d +. 2.);
          } else {
            60. *. ((rf -. gf) /. d +. 4.);
          };
        h < 0. ? h +. 360. : h;
      };
    (h, mx == 0. ? s0 : d /. mx, mx);
  };

  let hex_of_oklch = (t: (float, float, float)): string => {
    let (r, g, b) = rgb_of_oklch(t);
    Printf.sprintf("#%02x%02x%02x", r, g, b);
  };

  /* Accepts #rgb, #rrggbb, and rgb(r, g, b), with or without the hash. Yields
     the bytes as written, so pasting a hex into an `Rgb` literal lands exactly
     rather than detouring through OKLCH and returning a step off. */
  let rgb_of_css = (s: string): option((int, int, int)) => {
    let s = String.trim(String.lowercase_ascii(s));
    let hex_digit = c =>
      switch (c) {
      | '0' .. '9' => Some(Char.code(c) - 48)
      | 'a' .. 'f' => Some(Char.code(c) - 87)
      | _ => None
      };
    /* The length check has to gate the indexing, not sit beside it in a
       tuple: every branch of a tuple is evaluated, so a malformed string
       raised Invalid_argument instead of returning None. */
    let of_hex = h => {
      let n = String.length(h);
      if (n != 3 && n != 6) {
        None;
      } else {
        let w = n == 3 ? 1 : 2;
        let pair = i =>
          switch (hex_digit(h.[i]), hex_digit(h.[i + w - 1])) {
          | (Some(a), Some(b)) => Some(w == 1 ? a * 17 : a * 16 + b)
          | _ => None
          };
        switch (pair(0), pair(w), pair(2 * w)) {
        | (Some(r), Some(g), Some(b)) => Some((r, g, b))
        | _ => None
        };
      };
    };
    let strip = (p, s) => {
      let n = String.length(p);
      String.length(s) > n && String.sub(s, 0, n) == p
        ? Some(String.sub(s, n, String.length(s) - n)) : None;
    };
    switch (strip("#", s)) {
    | Some(h) => of_hex(h)
    | None =>
      switch (strip("rgb(", s)) {
      | Some(rest) when String.length(rest) > 0 =>
        let rest = String.sub(rest, 0, String.length(rest) - 1);
        switch (
          String.split_on_char(',', rest)
          |> List.map(x => int_of_string_opt(String.trim(x)))
        ) {
        | [Some(r), Some(g), Some(b)] => Some((r, g, b))
        | _ => None
        };
      | _ => of_hex(s)
      }
    };
  };

  let oklch_of_css = (s: string): option((float, float, float)) =>
    switch (rgb_of_css(s)) {
    | Some(rgb) => Some(oklch_of_rgb(rgb))
    | None => None
    };
};

// List of type aliases to add to the context
// Some are sum types (with constructors), others are product types (no constructors)

/* ===== Opt-in HTML modules (prototype) =====
   Html, Attr, Cmd and Sub are builtin MODULE VALUES rather than ~120 global
   constructor names and seven global type aliases. A program writes
   `Html.div([Attr.style(...)], [Html.text("hi")])`, annotates with `Html.T`,
   `Attr.T`, `Cmd.T`, `Sub.T`, `Html.App`, `Attr.KeyEvent`, and no user name
   can collide with the substrate (a module called `App` used to lose its
   type members to the builtin alias `App`). Each value member is the
   constructor itself, as a function; the constructor NAMES are unchanged, so
   the renderer and the MVU runtime need no change. */
module HtmlModules = {
  let path = (m: string, t: string): Typ.t =>
    Typ.fresh(ProdProjection(Typ.fresh(Var(m)), Typ.fresh(Label(t))));

  /* Where the old global aliases now live. */
  let homes = [
    ("HTML", ("Html", "T")),
    ("Attr", ("Attr", "T")),
    ("Cmd", ("Cmd", "T")),
    ("Sub", ("Sub", "T")),
    ("App", ("Html", "App")),
    ("KeyEvent", ("Attr", "KeyEvent")),
    ("MouseEvent", ("Attr", "MouseEvent")),
  ];

  /* Free references to the old global aliases become paths into the
     modules; a reference to the module's own alias becomes its member T.
     Rec-bound occurrences (HTML inside Rec(HTML, ...)) are left alone by
     Typ.subst. */
  let qualify = (~self: string, ty: Typ.t): Typ.t =>
    List.fold_left(
      (ty, (alias, (m, t))) => {
        let target = alias == self ? Typ.fresh(Var("T")) : path(m, t);
        Typ.subst(target, Fresh.TPat.var(alias), ty);
      },
      ty,
      homes,
    );

  /* CmdNone -> none, OnClickAt -> on_click_at, H1 -> h1, Type -> type_ */
  let member_name = (ctr: string): string =>
    switch (ctr) {
    | "CmdNone"
    | "SubNone" => "none"
    | "CmdBatch"
    | "SubBatch" => "batch"
    | "Type" => "type_"
    | _ =>
      let b = Buffer.create(16);
      String.iteri(
        (i, c) => {
          if (i > 0
              && Char.uppercase_ascii(c) == c
              && Char.lowercase_ascii(c) != c) {
            Buffer.add_char(b, '_');
          };
          Buffer.add_char(b, Char.lowercase_ascii(c));
        },
        ctr,
      );
      Buffer.contents(b);
    };

  let variants = (ty: Typ.t): list((string, option(Typ.t))) => {
    let of_sum = sm =>
      List.filter_map(
        fun
        | ConstructorMap.Variant(c, _, t) => Some((c, t))
        | _ => None,
        sm,
      );
    switch (Typ.term_of(ty)) {
    | Sum(sm) => of_sum(sm)
    | Rec(_, body) =>
      switch (Typ.term_of(body)) {
      | Sum(sm) => of_sum(sm)
      | _ => []
      }
    | _ => []
    };
  };

  /* A module named [name] whose type members are [types]; the member T is
     the sum whose constructors become the value members. [self] is the old
     global alias this module replaces. */
  let mk =
      (~name: string, ~self: string, ~types: list((string, Typ.t)))
      : BuiltinsUtil.const => {
    let ctors = List.assoc("T", types);
    let q = qualify(~self);
    let self_t = Typ.fresh(Var("T"));
    let members =
      variants(ctors)
      |> List.map(((ctr, arg)) => {
           let member_ty =
             switch (arg) {
             | None => self_t
             | Some(a) => arrow(q(a), self_t)
             };
           /* The annotation names the type by its path, `Html.T`, the way
              the global alias used to stay `Var("HTML")`: compact, resolved
              lazily by Ascriptions through the builtin context. */
           let ctor_ty =
             switch (arg) {
             | None => path(name, "T")
             | Some(a) => arrow(q(a), path(name, "T"))
             };
           (
             member_name(ctr),
             member_ty,
             Exp.fresh(Constructor(ctr, Some(Some(ctor_ty)))),
           );
         });
    let sig_items =
      List.map(
        ((n, ty)) => Sig.item_of_member(Sig.TypeManifest(n, q(ty))),
        types,
      )
      @ List.map(
          ((n, ty, _)) => Sig.item_of_member(Sig.Val(n, ty)),
          members,
        );
    let mod_items =
      List.map(((n, _, v)) => Mod.fresh(ModVal(n, v)), members);
    {
      name,
      typ: Sig(sig_items),
      imp: Exp.fresh(Module(mod_items)),
    };
  };
};

let module_builtins: list(BuiltinsUtil.const) = [
  HtmlModules.mk(
    ~name="Html",
    ~self="HTML",
    ~types=[("T", HTML.t), ("App", App.t)],
  ),
  HtmlModules.mk(
    ~name="Attr",
    ~self="Attr",
    ~types=[
      ("T", HTML.attr),
      ("KeyEvent", Event.key),
      ("MouseEvent", Event.mouse),
    ],
  ),
  HtmlModules.mk(~name="Cmd", ~self="Cmd", ~types=[("T", Cmd.t)]),
  HtmlModules.mk(~name="Sub", ~self="Sub", ~types=[("T", Sub.t)]),
];

/* The term a builtin module member denotes (a constructor), so statics
   can elaborate `Html.div` to `Div` directly: the runtime then never holds
   or substitutes the module value itself, and results print as before. */
let builtin_module_member = (m: string, x: string): option(Exp.t) =>
  /* (the local module Option above shadows Stdlib.Option) */
  switch (
    List.find_opt((c: BuiltinsUtil.const) => c.name == m, module_builtins)
  ) {
  | Some({imp, _}) =>
    switch (Exp.term_of(imp)) {
    | Module(items) =>
      List.fold_left(
        (acc, item: Mod.t) =>
          switch (item.term) {
          | ModVal(y, v) when y == x => Some(v)
          | _ => acc
          },
        None,
        items,
      )
    | _ => None
    }
  | None => None
  };

/* THE LIVELIT TYPE. The signature every user-defined livelit definition is
   checked against, at the definition site (UserLivelit.detect). Writing it
   once, here, is what lets the check be a signature check rather than four
   hand-rolled member comparisons.

   Model, Action and Expansion are ABSTRACT: each livelit chooses them, and
   the signature only says that the four members agree about them. The check
   realizes each abstract member by the definition's own manifest type
   (Typ.sig_sub), so `expand` is checked at that sum with that
   livelit's actual types -- which is the obligation the paper discharges
   per use, moved to the definition.

   Nothing SEALS a livelit with this signature: sealing would hide
   Expansion, and a use of ^name must keep synthesizing it concretely for
   clients to reason about. `shape` and helper members are deliberately
   absent -- they are optional, and extra members are allowed by width
   subtyping. */
/* ---- Figure 3's two command monads --------------------------------- */

/* Hazel's type language has Poly and Rec but NO application: `typ_term`
   carries no Ap. That is why Option above is monomorphic with a hole
   rather than Option(a), and it is why the paper's UpdateCmd(t) and
   ViewCmd(t) are BUILT here rather than spelled -- the same way
   livelit_expand_typ builds expand's type out of Model and Expansion.

   A livelit definition never writes these types. Analysis against the
   realized signature supplies them, which is the route that already lets
   Functional and Macro resolve without a `type Expand` member. */

/* A type, as a value. Enough to NAME the type a splice holds, which is
   all new_splice asks of it.

   Deliberately NOT the quotation of Sec. 3.2.5. Naming Int is a choice
   from a closed set; quoting an expression is not, and the two want
   different machinery. Keeping them apart is what lets a splice be
   created before Exp is inhabited. */
let typ_typ: Typ.t =
  sum_type([
    ("IntT", None),
    ("FloatT", None),
    ("BoolT", None),
    ("StringT", None),
  ]);

/* A splice editor's size (Sec. 3.2.3). The paper's Dim "currently
   supports only a fixed character width, with overflow causing
   scrolling", so a character count is the whole of it. */
let dim_typ: Typ.t = sum_type([("Chars", Some(int()))]);

/* What eval_splice answers with. Sec. 3.2.3 leaves it to each provider
   whether indeterminate results are supported -- "this behavior is
   highly domain-specific" -- so both arms are visible and a view decides
   what to do with Indet. */
let result_typ: Typ.t =
  sum_type([("Val", Some(unknown(Internal))), ("Indet", None)]);

/* The two monads, as command trees.

   Each is a Rec whose arms are Pure and one per command, and every
   command carries a CONTINUATION from its own answer. That is what makes
   sequencing expressible without do-notation, which Hazel does not have:
   `bind` builds a tree and the system interprets it, rather than the
   livelit running anything itself.

   Sec. 3.2.4 is explicit that the difference between them is the point:
   "The UpdateCmd monad does not itself have the ability to request
   evaluation (eval_splice), because the model should not depend directly
   on which closure the user has selected." Two capability sets, not one
   monad used twice -- so eval_splice, editor and result_view appear in
   ViewCmd only, and new_splice and set_splice in UpdateCmd only.

   Not built on the Cmd type above, though the two look alike. Cmd is
   fire-and-forget -- CmdNone, CmdBatch of a list, no Pure and no
   continuation -- so it can neither return a value nor let one command's
   answer decide the next. Both are exactly what these need: new_splice
   hands back a ref that the rest of the sequence uses. Cmd's shape is
   still the precedent for how a recursive effect type is declared here,
   which is why these are built the same way. */

let update_cmd = (t: Typ.t): Typ.t => {
  let self = var("$UpdateCmd");
  rec_(
    Fresh.TPat.var("$UpdateCmd"),
    sum_type([
      ("Pure", Some(t)),
      /* A bind is a node, not a step: `do p <- c in body` IS a command
         tree rather than something that reduces to one, so the evaluator
         leaves it alone and the interpreter walks it. The paper is clear
         that these commands are the system's to run.

         The bound command's payload type cannot be written here. It is
         existential -- `c` is a command of SOME a, and the continuation
         consumes that same a -- and the type language has neither
         existentials nor application. Unknown is the honest spelling.
         The precision is not lost, only moved: the Bind FORM's typing
         rule checks c against M(a) and the pattern against a. */
      (
        "Bind",
        Some(prod([unknown(Internal), arrow(unknown(Internal), self)])),
      ),
      /* new_splice : (Typ, Maybe(Exp)) -> UpdateCmd(SpliceRef) */
      (
        "NewSplice",
        Some(
          prod([
            prod([var("Typ"), var("Option")]),
            arrow(var("SpliceRef"), self),
          ]),
        ),
      ),
      /* set_splice : (SpliceRef, Exp) -> UpdateCmd(()) */
      (
        "SetSplice",
        Some(
          prod([
            prod([var("SpliceRef"), var("Exp")]),
            arrow(prod([]), self),
          ]),
        ),
      ),
    ]),
  );
};

let view_cmd = (t: Typ.t): Typ.t => {
  let self = var("$ViewCmd");
  rec_(
    Fresh.TPat.var("$ViewCmd"),
    sum_type([
      ("Pure", Some(t)),
      /* A bind is a node, not a step: `do p <- c in body` IS a command
         tree rather than something that reduces to one, so the evaluator
         leaves it alone and the interpreter walks it. The paper is clear
         that these commands are the system's to run.

         The bound command's payload type cannot be written here. It is
         existential -- `c` is a command of SOME a, and the continuation
         consumes that same a -- and the type language has neither
         existentials nor application. Unknown is the honest spelling.
         The precision is not lost, only moved: the Bind FORM's typing
         rule checks c against M(a) and the pattern against a. */
      (
        "Bind",
        Some(prod([unknown(Internal), arrow(unknown(Internal), self)])),
      ),
      /* eval_splice : SpliceRef -> ViewCmd(Maybe(Result)) */
      (
        "EvalSplice",
        Some(prod([var("SpliceRef"), arrow(var("Option"), self)])),
      ),
      /* editor : (SpliceRef, Dim) -> ViewCmd(Html(a)) */
      (
        "Editor",
        Some(
          prod([
            prod([var("SpliceRef"), var("Dim")]),
            arrow(HtmlModules.path("Html", "T"), self),
          ]),
        ),
      ),
      /* result_view : (SpliceRef, Dim) -> ViewCmd(Maybe(Html(a))) */
      (
        "ResultView",
        Some(
          prod([
            prod([var("SpliceRef"), var("Dim")]),
            arrow(var("Option"), self),
          ]),
        ),
      ),
    ]),
  );
};

/* Which of the two monads a type is, and what it is a command OF.

   The knowledge of the encoding lives here, next to the builders, so a
   reader never has to reconstruct it from a pattern match elsewhere. The
   tag is the Rec's binder name, which is why the builders chose names no
   user can write: `$UpdateCmd` is not a type variable anyone can bind.

   The payload is read off the Pure arm, because Pure is the arm that
   holds the monad's own answer -- update_cmd(t) puts t there and nowhere
   else. */
type cmd_monad =
  | UpdateMonad
  | ViewMonad;

let show_cmd_monad = (m: cmd_monad): string =>
  switch (m) {
  | UpdateMonad => "UpdateCmd"
  | ViewMonad => "ViewCmd"
  };

let monad_of_typ = (ty: Typ.t): option((cmd_monad, Typ.t)) =>
  switch (Typ.term_of(ty)) {
  | Rec(tp, body) =>
    let named =
      switch (TPat.tyvar_of_utpat(tp)) {
      | Some("$UpdateCmd") => Some(UpdateMonad)
      | Some("$ViewCmd") => Some(ViewMonad)
      | _ => None
      };
    switch (named, Typ.term_of(body)) {
    | (Some(which), Sum(variants)) =>
      variants
      |> List.find_map((v: ConstructorMap.variant(Typ.t)) =>
           switch (v) {
           | Variant("Pure", _, Some(payload)) => Some((which, payload))
           | Variant(_, _, _)
           | BadEntry(_) => None
           }
         )
    | (_, _) => None
    };
  | _ => None
  };

/* The members every livelit has. */
let livelit_common = (model, action) => [
  Sig.item_of_member(Sig.TypeAbstract("Model")),
  Sig.item_of_member(Sig.TypeAbstract("Action")),
  Sig.item_of_member(Sig.TypeAbstract("Expansion")),
  /* `init : Model`, not the paper's `UpdateCmd(Model)`.
     Not an oversight and not yet fixable: init is SYNTAX, not a value --
     UserLivelit keeps its expression as model_default and Triggers pastes
     that text at the caret when the livelit's name is typed. There are no
     statics and no evaluator at that moment, so a command there would
     have nothing to perform it. Sec. 3.2.1's init runs new_splice; ours
     cannot until init becomes a value. */
  Sig.item_of_member(Sig.Val("init", model)),
  /* Figure 3, curried as the paper curries it:
       update : Model -> Action -> UpdateCmd(Model)
       view   : Model -> ViewCmd(Html(Action))
     Both were plain functions returning plain values. They are commands
     now because that is the only way a splice can be written or read:
     set_splice lives in UpdateCmd and eval_splice in ViewCmd, and neither
     is reachable from a function that merely returns. */
  Sig.item_of_member(
    Sig.Val("update", arrow(model, arrow(action, update_cmd(model)))),
  ),
  Sig.item_of_member(
    Sig.Val("view", arrow(model, view_cmd(HtmlModules.path("Html", "T")))),
  ),
];

/* ONE signature, and `expand` is a SUM. A livelit's use denotes either a
   VALUE, which `Functional` computes, or a PROGRAM, which `Macro` writes
   while handing back the splices it refers to (Figure 3 of the livelits
   paper, Omar et al., PLDI 2021).

   This replaces an earlier design with two signatures, LivelitFun and
   LivelitMac, carrying members `expand_fun` and `expand_mac`. The reason
   given for splitting them was that a signature can only say a member is
   REQUIRED -- optional members are expressed by leaving them out, since
   extra members are allowed by width subtyping -- so "exactly one of
   expand_fun / expand_mac" could not be said inside a single signature.

   A sum says exactly that, and says it in the member's own type rather
   than in the module system: `expand` is required, and its value commits
   to one arm. Which kind a livelit is stops being a question about which
   signature it answers to and becomes a question about how it inhabits
   one type, which is the question it always was.

   The Macro arm's expansion is a FUNCTION of its splices, and that shape
   does two jobs at once: a splice passed as an argument is evaluated
   outside the expansion, so a binder inside cannot capture it, AND the
   expansion can be checked once against the splices' declared types
   without knowing their contents. Capture avoidance and compositional
   typing are the same decision.

   The Macro arm is not yet usable: `Exp` below is an uninhabited
   placeholder, so a Macro expansion can be written but never returns a
   value. That is the honest state of it -- there is no quoted-code type,
   no quotation syntax, and no splice_new. The arm is here so the target
   is legible and so the two kinds have names.

   Full words until someone picks something shorter. */
/* Built here rather than inline, because the use site needs the SAME sum
   with the livelit's concrete Model and Expansion substituted in
   (UserLivelit.member_ty). Two copies would drift. */
let livelit_expand_typ = (~model: Typ.t, ~expansion: Typ.t): Typ.t =>
  sum_type([
    ("Functional", Some(arrow(model, expansion))),
    (
      "Macro",
      Some(arrow(model, prod([var("Exp"), list(var("SpliceRef"))]))),
    ),
  ]);

/* The constructors have to be IN SCOPE at the definition site, or
   `let expand = Functional(...)` fails with "Constructor is not defined"
   -- measured, and while it was unresolved it silently disabled BOTH the
   definition-site and the use-site expansion checks, which is the §3.2.5
   obligation this whole file exists to discharge.

   A livelit therefore declares the sum itself, as a `type Expand` member.
   That is one repeated line per definition, and it is the price of
   keeping the check: a GLOBAL alias would cost the author nothing and
   put the constructors in scope the way `Ord` puts `Lt` there, but its
   arms would have to be unknown -- Model and Expansion are per-livelit
   while the alias is global -- so `Functional(f)` would synthesize at the
   alias's type, f's real type would be widened away, and sig_sub would
   have nothing left to compare. Measured: a livelit declaring
   `Expansion = String` whose Functional returns Int then reports no
   errors at all.

   The durable fix is probably to make the definition-site member check
   ANALYTIC -- analyze `expand` against the realized signature type rather
   than synthesize and compare afterwards, which is what the declared and
   annotated forms do implicitly and why they still catch the mismatch.
   Open question for Cyrus, since the sum was his suggestion. */

let livelit: Typ.t = {
  let model = var("Model");
  let action = var("Action");
  let expansion = var("Expansion");
  sig_(
    livelit_common(model, action)
    @ [
      Sig.item_of_member(
        Sig.Val("expand", livelit_expand_typ(~model, ~expansion)),
      ),
    ],
  );
};

/* `Exp` is quoted code, and is still a placeholder: an empty sum has no
   values, so it names what Figure 3 needs without pretending to provide
   it. It becomes real when quotation does. */
let exp_typ: Typ.t = sum_type([]);

/* A SpliceRef is a handle to a hole holding the client's own code. It
   carries the splice's id, which is what the projector resolves when a
   view says `Html.splice(r)`.

   The constructor is visible, so a client can in principle forge one.
   `Html.splice` of a forged or stale ref renders as an error rather than
   anything dangerous, and making it genuinely abstract wants a module
   with an abstract type member -- worth doing, not worth blocking on. */
let splice_ref_typ: Typ.t = sum_type([("SpliceRef", Some(string()))]);

let type_aliases: list((string, Typ.t)) = [
  ("Ord", Ord.t),
  ("Option", Option.t),
  ("Either", Either.t),
  ("JSON", JSON.t),
  ("KeyMod", Shortcut.key_mod_typ),
  ("Shortcut", Shortcut.typ),
  ("ColorValue", Color.typ),
  ("$Meta", meta_type),
  ("LivelitShape", LivelitShape.t),
  ("Exp", exp_typ),
  ("Typ", typ_typ),
  ("Dim", dim_typ),
  ("Result", result_typ),
  ("SpliceRef", splice_ref_typ),
  ("Livelit", livelit),
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
      | Sum(cons_map) => Ctx.add_ctrs(ctx, name, cons_map)
      | Rec(_, tbody) =>
        switch (Typ.term_of(tbody)) {
        | Sum(cons_map) => Ctx.add_ctrs(ctx, name, cons_map)
        | _ => ctx
        }
      | _ => ctx // Product types have no constructors to add
      }
    },
    Ctx.empty,
    type_aliases,
  );
};

/* ---- bind and return, with real types ------------------------------ */

/* These carry honest System-F types rather than the holes every other
   builtin here uses for genericity:

     return : forall a. a -> M(a)
     bind   : forall a. forall b. (M(a), a -> M(b)) -> M(b)

   Expressible because the application to `a` happens in OCaml, when the
   type is built -- the object language never applies a type constructor,
   which it cannot do.

   There are FOUR of these and not two because Hazel's kinds are
   `Singleton | Abstract` with no arrow: a type variable stands for a
   type, never for a type constructor, so `forall M. a -> M(a)` cannot be
   written and each monad needs its own pair. Nobody writes these names --
   `do` elaborates to them and supplies the instantiations -- so the
   duplication costs a reader nothing.

   They are `const` builtins because `hazel_fn` forces `arrow(arg, ret)`
   and a forall is not an arrow. */

let monad_ops: list(const) = {
  let a = () => var("a");
  let b = () => var("b");
  let tp = n => Fresh.TPat.var(n);
  let ret_op = (~name: string, ~cmd: Typ.t => Typ.t): const => {
    name,
    typ: Typ.term_of(poly(tp("a"), arrow(a(), cmd(a())))),
    imp:
      Fresh.(
        Exp.(
          typ_fun(
            tp("a"),
            fn(
              Pat.var("x"),
              ap(
                Forward,
                constructor("Pure", Some(Some(cmd(a())))),
                var("x"),
              ),
              None,
              None,
            ),
            None,
          )
        )
      ),
  };
  let bind_op = (~name: string, ~cmd: Typ.t => Typ.t): const => {
    name,
    typ:
      Typ.term_of(
        poly(
          tp("a"),
          poly(
            tp("b"),
            arrow(prod([cmd(a()), arrow(a(), cmd(b()))]), cmd(b())),
          ),
        ),
      ),
    imp:
      Fresh.(
        Exp.(
          typ_fun(
            tp("a"),
            typ_fun(
              tp("b"),
              fn(
                Pat.var("ck"),
                ap(
                  Forward,
                  constructor("Bind", Some(Some(cmd(b())))),
                  var("ck"),
                ),
                None,
                None,
              ),
              None,
            ),
            None,
          )
        )
      ),
  };
  [
    ret_op(~name="update_return", ~cmd=update_cmd),
    bind_op(~name="update_bind", ~cmd=update_cmd),
    ret_op(~name="view_return", ~cmd=view_cmd),
    bind_op(~name="view_bind", ~cmd=view_cmd),
  ];
};

/* ---- Figure 3's splice commands ------------------------------------ */

/* Each is a ONE-NODE tree: the constructor holding its arguments and a
   continuation that stops immediately at Pure. Nothing here performs
   anything -- `new_splice(t, e)` does not create a splice, it describes
   creating one. The `do` form grafts a longer sequence on, and the
   livelit machinery is what finally walks the tree and acts.

   That indirection is the point rather than an artifact: creating a
   splice changes the editor, and evaluation cannot change the editor. */
let cmd_ctor = (~ctor: string, ~cmd_ty: Typ.t): Exp.t =>
  Fresh.(
    Exp.(
      fn(
        Pat.var("args"),
        ap(
          Forward,
          constructor(ctor, Some(Some(cmd_ty))),
          tuple([
            var("args"),
            fn(
              Pat.var("x"),
              ap(
                Forward,
                constructor("Pure", Some(Some(cmd_ty))),
                var("x"),
              ),
              None,
              None,
            ),
          ]),
        ),
        None,
        Some(ctor),
      )
    )
  );

/* The paper spells these new_splice / set_splice / eval_splice (Sec.
   3.2.1, 3.2.4, 3.2.3). Those names are kept: this is the interface the
   paper describes, and a livelit author reading Figure 3 should find the
   same words here. */
let splice_builtins: list(hazel_fn) = [
  {
    /* new_splice : (Typ, Maybe(Exp)) -> UpdateCmd(SpliceRef) */
    str: "fun args -> NewSplice((args, fun r -> Pure(r)))",
    name: "new_splice",
    arg: Prod([var("Typ"), var("Option")]),
    ret: Typ.term_of(update_cmd(var("SpliceRef"))),
    imp: cmd_ctor(~ctor="NewSplice", ~cmd_ty=update_cmd(var("SpliceRef"))),
  },
  {
    /* set_splice : (SpliceRef, Exp) -> UpdateCmd(()) */
    str: "fun args -> SetSplice((args, fun u -> Pure(u)))",
    name: "set_splice",
    arg: Prod([var("SpliceRef"), var("Exp")]),
    ret: Typ.term_of(update_cmd(prod([]))),
    imp: cmd_ctor(~ctor="SetSplice", ~cmd_ty=update_cmd(prod([]))),
  },
  {
    /* eval_splice : SpliceRef -> ViewCmd(Maybe(Result)) */
    str: "fun args -> EvalSplice((args, fun r -> Pure(r)))",
    name: "eval_splice",
    arg: Typ.term_of(var("SpliceRef")),
    ret: Typ.term_of(view_cmd(var("Option"))),
    imp: cmd_ctor(~ctor="EvalSplice", ~cmd_ty=view_cmd(var("Option"))),
  },
  {
    /* editor : (SpliceRef, Dim) -> ViewCmd(Html(a)) */
    str: "fun args -> Editor((args, fun h -> Pure(h)))",
    name: "editor",
    arg: Prod([var("SpliceRef"), var("Dim")]),
    ret: Typ.term_of(view_cmd(HtmlModules.path("Html", "T"))),
    imp:
      cmd_ctor(
        ~ctor="Editor",
        ~cmd_ty=view_cmd(HtmlModules.path("Html", "T")),
      ),
  },
  {
    /* result_view : (SpliceRef, Dim) -> ViewCmd(Maybe(Html(a))) */
    str: "fun args -> ResultView((args, fun h -> Pure(h)))",
    name: "result_view",
    arg: Prod([var("SpliceRef"), var("Dim")]),
    ret: Typ.term_of(view_cmd(var("Option"))),
    imp: cmd_ctor(~ctor="ResultView", ~cmd_ty=view_cmd(var("Option"))),
  },
];

let builtins = Option.builtins @ splice_builtins;
let constructor_entries = constructors.entries @ types;

/* Build an Ord-returning compare builtin from an Atom.compare_entry, the
 * same way of_atom_builtin handles atom-to-atom conversions. */
let of_atom_compare =
    ((name, Atom.Cmp(kind, cmp)): (string, Atom.compare_entry))
    : BuiltinsUtil.fn => {
  let ty = Typ.fresh_atom(Atom.cls_of_kind(kind));
  BuiltinsUtil.{
    name,
    arg: Prod([ty, ty]),
    ret: Ord.t.term,
    imp:
      binary((d1, d2) => {
        let-unbox n1 = (Atom(kind), d1);
        let-unbox n2 = (Atom(kind), d2);
        Some(
          switch (cmp(n1, n2)) {
          | 0 => Ord.eq
          | n when n < 0 => Ord.lt
          | _ => Ord.gt
          },
        );
      }),
    custom_statics: None,
  };
};

/* Flip Lt ↔ Gt, leave Eq alone. Lets a descending sort reuse an ascending
 * comparator without a second pass to reverse the list. */
let invert_ord: BuiltinsUtil.fn =
  BuiltinsUtil.{
    name: "invert_ord",
    arg: Ord.t.term,
    ret: Ord.t.term,
    imp: d =>
      switch (DHExp.term_of(d)) {
      | Constructor("Lt", _) => Some(Ord.gt)
      | Constructor("Gt", _) => Some(Ord.lt)
      | Constructor("Eq", _) => Some(Ord.eq)
      | _ => None
      },
    custom_statics: None,
  };

let ord_builtins: list(BuiltinsUtil.fn) =
  [invert_ord] @ List.map(of_atom_compare, Atom.compare_builtins);
