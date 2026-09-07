open Util;
open BuiltinsUtil;

/* The standard library, grouped into modules.
   ------------------------------------------

   Every function here already exists as a flat builtin, and keeps its flat
   name: this file adds a second, qualified way to reach the same
   implementation, so `String.length` and `string_length` are the same
   function and no existing program changes meaning. The flat names stay
   because they are what every program, slide and exercise in the repository
   calls today; the modules are the front door from here on.

   A module builtin is an ordinary `Const` whose type is a signature and
   whose value is a module. Both sides are derived from the same builtin
   records, so a member's type cannot drift from the function it names, and
   the members are the very terms the flat names are bound to rather than
   copies of them.

   The value's items are `ModVal`, already-evaluated bindings, because the
   environment holds values: a module with pending items would be handed to
   the program unevaluated and its projections would not reduce. */

module Fresh = IdTagged.FreshGrammar;

let typ_of_builtin: builtin => Typ.t =
  fun
  | Const({typ, _}) => Typ.fresh(typ)
  | Fn({arg, ret, _})
  | HazelFn({arg, ret, _}) =>
    Fresh.Typ.arrow(Typ.fresh(arg), Typ.fresh(ret));

/* A member of a module: the name it takes inside the module, and the builtin
   it stands for. */
type member = {
  member: Var.t,
  builtin,
};

let sig_item_of_member = ({member, builtin}: member): Sig.t =>
  Fresh.Sig.sig_let(
    Fresh.Pat.asc(Fresh.Pat.var(member), typ_of_builtin(builtin)),
  );

let mod_item_of_member = ({member, builtin}: member): Mod.t =>
  IdTagged.fresh(ModVal(member, snd(imp_of_builtin(builtin))): Mod.term);

/* [~types] are manifest type members, declared in the signature and dropped
   from the value the way evaluation drops any type item. */
let module_builtin =
    (~name: Var.t, ~types: list((string, Typ.t))=[], members: list(member))
    : builtin =>
  Const({
    name,
    typ:
      Sig(
        List.map(
          ((n, ty)) => Fresh.Sig.sig_type(Fresh.TPat.var(n), ty),
          types,
        )
        @ List.map(sig_item_of_member, members),
      ),
    imp: Fresh.Exp.module_(List.map(mod_item_of_member, members)),
  });

/* Members named by dropping [prefix] from each builtin's name; a builtin
   without the prefix is left out. */
let by_prefix = (~prefix: string, builtins: list(builtin)): list(member) =>
  builtins
  |> List.filter_map(b => {
       let name = name_of_builtin(b);
       String.starts_with(~prefix, name)
       && String.length(name) > String.length(prefix)
         ? Some({
             member:
               String.sub(
                 name,
                 String.length(prefix),
                 String.length(name) - String.length(prefix),
               ),
             builtin: b,
           })
         : None;
     });

/* Members that keep their names. */
let same_names = (builtins: list(builtin)): list(member) =>
  List.map(
    b =>
      {
        member: name_of_builtin(b),
        builtin: b,
      },
    builtins,
  );

/* Members chosen and renamed by hand, given a pool to look them up in. */
let picked =
    (pool: list(builtin), pairs: list((Var.t, Var.t))): list(member) =>
  pairs
  |> List.filter_map(((member, name)) =>
       pool
       |> List.find_opt(b => name_of_builtin(b) == name)
       |> Option.map(builtin =>
            {
              member,
              builtin,
            }
          )
     );

/* ===== the pools ===== */

let base_fns =
  List.map(fn_builtin, BuiltinsBase.string_fns)
  @ List.map(fn_builtin, BuiltinsBase.numeric_fns)
  @ List.map(fn_builtin, BuiltinsBase.pair_fns)
  @ List.map(fn_builtin, BuiltinsBase.misc_fns);
let base_consts = List.map(const_builtin, BuiltinsBase.numeric_constants);
let converters = List.map(of_atom_builtin, Atom.converter_builtins);
let compares = List.map(fn_builtin, BuiltinsADT.ord_builtins);
let list_fns = List.map(hazel_fn_builtin, BuiltinsList.builtins);
let option_fns = List.map(hazel_fn_builtin, BuiltinsADT.builtins);

let pool =
  base_fns @ base_consts @ converters @ compares @ list_fns @ option_fns;

/* ===== the modules ===== */

/* `List.map`, `List.fold_left`, … The list functions are the ones whose flat
   names crowd the global namespace worst: `map`, `filter`, `length`, `find`
   and `head` are all top-level today. */
let list_module = module_builtin(~name="List", same_names(list_fns));

/* `String.length`, `String.split`, … plus the conversions that produce a
   string and the comparison, which read better as members than as
   `string_of_int` and `string_compare`. */
let string_module =
  module_builtin(
    ~name="String",
    by_prefix(~prefix="string_", base_fns)
    @ picked(
        converters,
        [
          ("of_int", "string_of_int"),
          ("of_float", "string_of_float"),
          ("of_bool", "string_of_bool"),
        ],
      )
    @ picked(compares, [("compare", "string_compare")]),
  );

let int_module =
  module_builtin(
    ~name="Int",
    picked(base_fns, [("abs", "abs"), ("mod", "int_mod")])
    @ picked(
        converters,
        [
          ("of_string", "int_of_string"),
          ("of_float", "int_of_float"),
          ("to_string", "string_of_int"),
          ("to_float", "float_of_int"),
        ],
      )
    @ picked(compares, [("compare", "int_compare")]),
  );

let float_module =
  module_builtin(
    ~name="Float",
    picked(
      base_fns,
      [
        ("abs", "abs_float"),
        ("mod", "float_mod"),
        ("ceil", "ceil"),
        ("floor", "floor"),
        ("sqrt", "sqrt"),
        ("exp", "exp"),
        ("log", "log"),
        ("log10", "log10"),
        ("sin", "sin"),
        ("cos", "cos"),
        ("tan", "tan"),
        ("asin", "asin"),
        ("acos", "acos"),
        ("atan", "atan"),
        ("is_finite", "is_finite"),
        ("is_infinite", "is_infinite"),
        ("is_nan", "is_nan"),
      ],
    )
    @ picked(
        base_consts,
        [
          ("pi", "pi"),
          ("infinity", "infinity"),
          ("neg_infinity", "neg_infinity"),
          ("nan", "nan"),
          ("epsilon", "epsilon_float"),
        ],
      )
    @ picked(
        converters,
        [("of_int", "float_of_int"), ("of_string", "float_of_string")],
      )
    @ picked(compares, [("compare", "float_compare")]),
  );

/* `Option.map`, `Option.bind`, `Option.to_list`. The type member says what
   the module is about; `Option` is also a type alias, and the two live in
   different namespaces. */
let option_module =
  module_builtin(
    ~name="Option",
    ~types=[("T", BuiltinsADT.Option.t)],
    by_prefix(~prefix="option_", option_fns),
  );

let pair_module =
  module_builtin(
    ~name="Pair",
    same_names(List.map(fn_builtin, BuiltinsBase.pair_fns)),
  );

/* ===== implicit instances =====

   The implicits feature (see docs/modules.md) resolves a module parameter
   from the instances in scope. Without instances for the base types every
   program has to write `ShowInt` and its friends by hand before it can use
   an implicit at all, so the standard library ships them: three signatures
   as type aliases, and one instance per base type per signature.

   An instance's signature carries a MANIFEST type member (`type T = Int`),
   not an abstract one: resolution selects an instance by that member, so a
   sealed instance could never be chosen. That is the caveat the docs give
   for hand-written instances, and it is why these are spelled out here
   rather than ascribed to the signature alias. */

let show_sig: Typ.t =
  Fresh.Typ.sig_([
    Fresh.Sig.sig_type_abstract(Fresh.TPat.var("T")),
    Fresh.Sig.sig_let(
      Fresh.Pat.asc(
        Fresh.Pat.var("show"),
        Fresh.Typ.arrow(Fresh.Typ.var("T"), Fresh.Typ.string()),
      ),
    ),
  ]);

let ord_sig: Typ.t =
  Fresh.Typ.sig_([
    Fresh.Sig.sig_type_abstract(Fresh.TPat.var("T")),
    Fresh.Sig.sig_let(
      Fresh.Pat.asc(
        Fresh.Pat.var("compare"),
        Fresh.Typ.arrow(
          Fresh.Typ.prod([Fresh.Typ.var("T"), Fresh.Typ.var("T")]),
          BuiltinsADT.Ord.t,
        ),
      ),
    ),
  ]);

/* No EQ: polymorphic `==` already compares any two values of the same type,
   so an equality signature would earn its keep only once a type could
   choose its own equality, which needs the sharing constraint the modules
   documentation calls for. */
let signature_aliases: list((string, Typ.t)) = [
  ("SHOW", show_sig),
  ("ORD", ord_sig),
];

/* A member written here rather than taken from the pool. Builtin functions
   are fixpoints by convention, even when nothing recurses, so that their
   environments are handled the same way. */
let hazel_member =
    (~name: Var.t, ~arg: Typ.t, ~ret: Typ.t, ~str: string, imp): builtin =>
  HazelFn({
    name,
    arg: Typ.term_of(arg),
    ret: Typ.term_of(ret),
    str,
    imp,
  });

let string_show =
  hazel_member(
    ~name="show",
    ~arg=Fresh.Typ.string(),
    ~ret=Fresh.Typ.string(),
    ~str="fix show -> fun s -> s",
    Fresh.Exp.fix_f(
      Fresh.Pat.var("show"),
      Fresh.Exp.fn(Fresh.Pat.var("s"), Fresh.Exp.var("s"), None, None),
      None,
    ),
  );

/* An instance of one signature for one base type: `type T = <ty>` plus the
   single member, taken from the builtin that already implements it. */
let instance =
    (~name: Var.t, ~ty: Typ.t, ~member: Var.t, ~from: Var.t): option(builtin) =>
  pool
  |> List.find_opt(b => name_of_builtin(b) == from)
  |> Option.map(builtin =>
       module_builtin(
         ~name,
         ~types=[("T", ty)],
         [
           {
             member,
             builtin,
           },
         ],
       )
     );

let show_instances =
  [
    ("ShowInt", Fresh.Typ.int(), "string_of_int"),
    ("ShowFloat", Fresh.Typ.float(), "string_of_float"),
    ("ShowBool", Fresh.Typ.bool(), "string_of_bool"),
  ]
  |> List.filter_map(((name, ty, from)) =>
       instance(~name, ~ty, ~member="show", ~from)
     )
  |> (
    xs =>
      xs
      @ [
        /* A string shows as itself; there is no `string_of_string` to
           borrow. */
        module_builtin(
          ~name="ShowString",
          ~types=[("T", Fresh.Typ.string())],
          [
            {
              member: "show",
              builtin: string_show,
            },
          ],
        ),
      ]
  );

let ord_instances =
  [
    ("OrdInt", Fresh.Typ.int(), "int_compare"),
    ("OrdFloat", Fresh.Typ.float(), "float_compare"),
    ("OrdString", Fresh.Typ.string(), "string_compare"),
  ]
  |> List.filter_map(((name, ty, from)) =>
       instance(~name, ~ty, ~member="compare", ~from)
     );

let instances: list(builtin) = show_instances @ ord_instances;

/* The instances are NOT marked implicit, so resolution does not see them
   until a program asks for them:

     let implicit ShowInt = ShowInt in show(3)

   Making them ambient is a language decision, not a library one, and it
   needs a priority or coherence rule first: with no such rule, a program
   that defines its own instance for a base type under any other name makes
   every call ambiguous against the shipped one, and 12 of the implicits
   tests change meaning. What the library can settle is the vocabulary and
   the implementations, which is the laborious part; opting in is one line
   per instance, exactly what the Module Implicits slide already writes. */

let signature_entries: list(Ctx.entry) =
  signature_aliases
  |> List.map(((name, typ)) =>
       Ctx.TVarEntry({
         name,
         id: Id.invalid,
         kind: Ctx.Singleton(typ),
       })
     );

let builtins: list(builtin) =
  [
    list_module,
    string_module,
    int_module,
    float_module,
    option_module,
    pair_module,
  ]
  @ instances;
