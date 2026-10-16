/* Bakes the Hazel-source standard library (plans/stdlib.md §2): parses,
   checks and elaborates the concatenated sources once, at build time.
   usage: bake <blob> <segment> <file.hz>...: base64 Marshal of
   Library.baked, and of the parsed source (for the editor) */
open Haz3lcore;
open Language;

let fail = msg => {
  prerr_endline("bake: " ++ msg);
  exit(1);
};

let read = path => In_channel.with_open_bin(path, In_channel.input_all);

let write_base64 = (path, v) =>
  Out_channel.with_open_bin(path, oc =>
    Out_channel.output_string(
      oc,
      Js_of_ocaml.Js.to_string(
        Js_of_ocaml.Js.Unsafe.global##btoa(
          Js_of_ocaml.Js.bytestring(Marshal.to_string(v, [])),
        ),
      ),
    )
  );

/* every annotation one shared id (Marshal stores it once) and no
   secondary: install re-ids the elab, as evaluation results are */
let compact: Exp.t => Exp.t = {
  let shared = Id.mk();
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) =>
      {
        ...x,
        annotation: {
          ...IdTagged.IdTag.mk_internal([shared]),
          lexeme: x.annotation.lexeme,
        },
      }
      |> continue;
  e =>
    Exp.map_term(
      ~f_exp=f,
      ~f_pat=f,
      ~f_typ=f,
      ~f_tpat=f,
      ~f_rul=f,
      ~f_mod=f,
      ~f_sig=f,
      ~f_mpat=f,
      e,
    );
};

/* names bound by top-level items, last binding last and once */
let rec top_names = (items: list(Mod.t)): list(Var.t) =>
  switch (items) {
  | [] => []
  | [item, ...rest] =>
    let later = top_names(rest);
    let here =
      switch (item.term) {
      | ModLet(p, _) => Pat.bound_vars(p)
      | ModuleMod(mp, _) => ModuleHelpers.mpat_names(mp)
      | _ => []
      };
    List.filter(x => !List.mem(x, later), here) @ later;
  };

/* the items as the let/module/type chain a slide would write, ending in
   [tail] */
let rec chain = (items: list(Mod.t), tail: Exp.t): Exp.t =>
  switch (items) {
  | [] => tail
  | [item, ...rest] =>
    let body = chain(rest, tail);
    switch (item.term) {
    | ModLet(p, def) => Exp.fresh(Let(p, def, body))
    | ModType(tp, ty) => Exp.fresh(TyAlias(tp, ty, body))
    | ModuleMod(mp, def) => Exp.fresh(ModuleExp(mp, def, body))
    | _ => fail("the library's top level may only hold definitions")
    };
  };

let () = {
  let (out, segment_out, files) =
    switch (Array.to_list(Sys.argv)) {
    | [_, out, segment_out, ...files] when files != [] => (
        out,
        segment_out,
        files,
      )
    | _ => fail("usage: bake <blob> <segment> <file.hz>...")
    };
  let text =
    files |> List.map(read) |> List.map(String.trim) |> String.concat(";\n");
  let segment =
    switch (FastParse.parsed_of_text(~root=Mod, text)) {
    | Ok(p) => p.segment
    | Error(note) => fail("the library is off the fast parser: " ++ note)
    };
  let items =
    switch (MakeTerm.Incr.term_of_mod(segment).term) {
    | Module(items) => items
    | _ => fail("the library did not parse as module items")
    };
  let names = top_names(items);
  if (List.length(names) < 2) {
    fail("the library must bind at least two names");
  };
  let tail = Exp.fresh(Tuple(List.map(x => Exp.fresh(Var(x)), names)));
  let base = Builtins.ctx_init(Some(Operators.default_mode));
  let (map, elab) = Statics.mk(CoreSettings.on, base, chain(items, tail));
  switch (Statics.Map.error_ids(map)) {
  | [] => ()
  | ids =>
    fail(
      Printf.sprintf("%d static errors in the library", List.length(ids)),
    )
  };
  let entries =
    switch (Statics.Map.lookup(Exp.rep_id(tail), map)) {
    | Some(info) =>
      switch (Ctx.subtract_prefix(Info.ctx_of(info), base)) {
      | Some(added) => added.entries
      | None => fail("the library's context does not extend the builtins'")
      }
    | None => fail("no statics for the library's tail")
    };
  let elab = compact(elab);
  let baked: Library.baked = {
    hash: Digest.to_hex(Digest.string(text)),
    entries,
    names,
    elab,
  };
  let kb = v =>
    Printf.sprintf("%dKB", String.length(Marshal.to_string(v, [])) / 1024);
  prerr_endline(
    Printf.sprintf(
      "bake: %d names; entries %s, elab %s; segment %s",
      List.length(names),
      kb(entries),
      kb(elab),
      kb(segment),
    ),
  );
  write_base64(out, baked);
  write_base64(segment_out, segment);
};
