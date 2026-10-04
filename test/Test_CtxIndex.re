open Alcotest;
open Haz3lcore;
open Language;

/* Ctx.lookup_tvar, lookup_var and lookup_ctr answer from an index once a
   scan reaches the builtin tail. They must answer exactly as the plain
   scan does: the same entry, not merely an equal one. */

let scan_tvar = (ctx: Ctx.t, name) =>
  List.find_map(
    fun
    | Ctx.TVarEntry(v) when v.name == name => Some(v.kind)
    | _ => None,
    ctx.entries,
  );

let scan_var = (ctx: Ctx.t, name) =>
  List.find_map(
    fun
    | Ctx.VarEntry(v) when v.name == name => Some(v)
    | _ => None,
    ctx.entries,
  );

let scan_ctr = (ctx: Ctx.t, name) =>
  List.find_map(
    fun
    | Ctx.ConstructorEntry(v) when v.name == name => Some(v)
    | _ => None,
    ctx.entries,
  );

let same = (a, b) =>
  switch (a, b) {
  | (None, None) => true
  | (Some(x), Some(y)) => x === y
  | _ => false
  };

let entry_name: Ctx.entry => string =
  fun
  | VarEntry({name, _})
  | ConstructorEntry({name, _})
  | TVarEntry({name, _})
  | LivelitEntry({name, _}) => name;

/* Every ctx statics records for a program. */
let ctxs_of = (name, text): list(Ctx.t) =>
  switch (PersistentZipper.parse_text(~source=name, ~root=Exp, text)) {
  | None => fail(name ++ ": failed to parse")
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let (info_map, _) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    Id.Map.bindings(info_map) |> List.map(((_, info)) => Info.ctx_of(info));
  };

let resolve = path =>
  List.find_opt(Sys.file_exists, [path, Filename.concat("../../..", path)])
  |> Option.value(~default=path);

let read_file = path => {
  let ic = open_in_bin(resolve(path));
  let s = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  s;
};

/* Names asked about: every name in the ctx, and some in no ctx. */
let check_ctxs = (what, ctxs) => {
  let hits = Ctx.tail_index_hits^;
  let checked = ref(0);
  List.iteri(
    (i, ctx: Ctx.t) => {
      let names =
        ["NoSuchName", "no_such_name", "T", "Html", "HTML", "Int"]
        @ List.map(entry_name, ctx.entries)
        |> List.sort_uniq(String.compare);
      List.iter(
        name => {
          incr(checked);
          let ok =
            same(Ctx.lookup_tvar(ctx, name), scan_tvar(ctx, name))
            && same(Ctx.lookup_var(ctx, name), scan_var(ctx, name))
            && same(Ctx.lookup_ctr(ctx, name), scan_ctr(ctx, name));
          if (!ok) {
            fail(Printf.sprintf("%s: ctx %d, name %s", what, i, name));
          };
        },
        names,
      );
    },
    ctxs,
  );
  check(bool, what ++ ": some lookups", true, checked^ > 0);
  /* The control: the index answered some of them, so the comparison
     covers it and not only the scan. */
  check(bool, what ++ ": index used", true, Ctx.tail_index_hits^ > hits);
};

/* ~40 ctxs per program, spread across it: every name in each is looked
   up three ways, so all of a slide's thousands would be slow. */
let sample = ctxs => {
  let n = List.length(ctxs);
  let step = max(1, n / 40);
  List.filteri((i, _) => i mod step == 0, ctxs);
};

let tests = (
  "Ctx.TailIndex",
  [
    test_case("Color (Figure 3): index is the scan", `Quick, () =>
      check_ctxs(
        "color-fig3",
        sample(
          ctxs_of(
            "color-fig3",
            read_file("hazel-programs/docs/livelits/color-fig3.hz"),
          ),
        ),
      )
    ),
    test_case("Dynamic Row or Column: index is the scan", `Quick, () =>
      check_ctxs(
        "splice-row",
        sample(
          ctxs_of(
            "splice-row",
            read_file("hazel-programs/docs/livelits/splice-row.hz"),
          ),
        ),
      )
    ),
    /* User bindings that shadow builtin names, of each kind: the prefix
       must win over the index. */
    test_case("shadowed builtin names", `Quick, () =>
      check_ctxs(
        "shadowing",
        ctxs_of(
          "shadowing",
          "type HTML = Int in let Html = 1 in type Option = +Some(Int) +None in module Attr = { type T = Int } in let concat = 2 in Some(concat)",
        ),
      )
    ),
  ],
);
