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
   * `Transparent` and `Hex` occupy fairly common constructor names. A user
   * program that declares its own shadows these lexically, as usual. */
module Color = {
  /* Self-reference: Fade wraps another colour. */
  let self: Typ.t = var("ColorValue");

  /* type ColorValue =
     + Oklch((Float, Float, Float))   /* l 0..100, chroma, hue degrees */
     + Fade((ColorValue, Float))      /* alpha 0..100 */
     + Hex(String)
     + Transparent */
  let typ: Typ.t =
    rec_(
      Fresh.TPat.var("ColorValue"),
      sum_type([
        ("Oklch", Some(prod([float(), float(), float()]))),
        ("Fade", Some(prod([self, float()]))),
        ("Hex", Some(string())),
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
    | Hex(string)
    | Transparent;

  /* Fresh per occurrence, never a module-level value — FreshGrammar mints the
     id at call time, and a shared constructor collapses every occurrence into
     one tile, crashing Highlight.of_tile. */
  let ctr = (name: string): Exp.t =>
    IdTagged.FreshGrammar.Exp.constructor(name, None);

  let rec exp_of: t => Exp.t =
    fun
    | Transparent => ctr("Transparent")
    | Hex(s) =>
      IdTagged.FreshGrammar.Exp.(ap(Forward, ctr("Hex"), string(s)))
    | Oklch(l, c, h) =>
      IdTagged.FreshGrammar.Exp.(
        ap(Forward, ctr("Oklch"), tuple([float(l), float(c), float(h)]))
      )
    | Fade(inner, a) =>
      IdTagged.FreshGrammar.Exp.(
        ap(Forward, ctr("Fade"), tuple([exp_of(inner), float(a)]))
      );

  let rec of_exp = (v: Exp.t): option(t) =>
    switch (Unboxing.unbox(SumNoArg("Transparent"), v)) {
    | Matches () => Some(Transparent)
    | _ =>
      switch (Unboxing.unbox(SumWithArg("Hex"), v)) {
      | Matches(arg) =>
        switch (Unboxing.unbox(Atom(String), arg)) {
        | Matches(s) => Some(Hex(s))
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

  /* Alpha goes through color-mix so it composes with any inner colour,
     including Hex, rather than only with the oklch() slash form. */
  let rec to_css: t => string =
    fun
    /* Not the `transparent` keyword: a keyword behaves differently as the
       origin of a relative colour, and variables.css uses `oklch(from …)`
       against palette entries. */
    | Transparent => "oklch(0 0 0 / 0)"
    | Hex(s) => s
    | Oklch(l, c, h) =>
      "oklch(" ++ num(l) ++ "% " ++ num(c) ++ " " ++ num(h) ++ ")"
    | Fade(inner, a) =>
      "color-mix(in oklch, "
      ++ to_css(inner)
      ++ " "
      ++ num(a)
      ++ "%, transparent)";
};

// List of type aliases to add to the context
let type_aliases: list((string, Typ.t)) = [
  ("Ord", Ord.t),
  ("Option", Option.t),
  ("Either", Either.t),
  ("JSON", JSON.t),
  ("KeyMod", Shortcut.key_mod_typ),
  ("Shortcut", Shortcut.typ),
  ("ColorValue", Color.typ),
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

// Add constructors for type aliases to the context
let constructors: Ctx.t = {
  List.fold_left(
    (ctx, (name, typ)) => {
      let cons_map =
        switch (Typ.term_of(typ)) {
        | Sum(cons_map) => cons_map
        | Rec(_, tbody) =>
          switch (Typ.term_of(tbody)) {
          | Sum(cons_map) => cons_map
          | _ => failwith("Type alias must be a sum type")
          }
        | _ => failwith("Type alias must be a sum type")
        };
      Ctx.add_ctrs(ctx, name, cons_map);
    },
    Ctx.empty,
    type_aliases,
  );
};

let builtins = Option.builtins;
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
