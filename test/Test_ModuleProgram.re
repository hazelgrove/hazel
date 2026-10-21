/* ModuleProgram: a program of named modules, each module's body read as an
   editor rooted at Mod reads it, and the program's errors and holes told
   per module. The texts are Dress Up's Scripting panel's as it ships them
   (kids-choice-games, dress-up/js/25-scripting.js). */
open Alcotest;
open Haz3lcore;
open Language;

let preamble = "type Cmd = + Done + Flip(String, Cmd) + Say(String, Cmd) in
type Event = + Pull(String) in
type Globals = (lever_L = Bool, lever_R = Bool, wiring = String) in";

let common = "# Sub-routines every room can use, as Common.flip2 and so on. #
let flip2 = fun (a, b) : (String, String) -> Flip(a, Flip(b, Done))";

let trap = "# A red lever pulled: flip it, and the pits wired to it follow, #
# as they always have. Try: Pull(\"L\") => Common.flip2(\"lever_L\", \"lever_R\") #
let on_event = fun (g, e) : (Globals, Event) ->
  case e
  | Pull(side) => Flip(\"lever_\" ++ side, Done)
  end";

let tail = "(trap_room = TrapRoom.on_event, rooms = 1)";

let ascribed_tail = "(trap_room = (TrapRoom.on_event : (Globals, Event) -> Cmd), rooms = 1)";

/* the rules the game's check (tools/checks/scripting482.mjs) puts in the
   trap room */
let rule = body =>
  "let on_event = fun (g, e) : (Globals, Event) -> case e | Pull(side) => "
  ++ body
  ++ " end";
let flip2_rule = rule("Common.flip2(\"lever_L\", \"lever_R\")");
let say_rule = rule("Say(\"hello from Hazel\", Done)");
let error_rule = rule("Flip(1, Done)");

let program = (~common=common, ~trap=trap, ~tail=tail, ()) =>
  ModuleProgram.of_texts(
    ~preamble,
    ~modules=[("Common", common), ("TrapRoom", trap)],
    ~tail,
  );

let ctx = () => Builtins.ctx_init(Some(Operators.default_mode));

let errors = (p: ModuleProgram.t): list(ModuleProgram.error) => {
  let (map, _) = Statics.mk(CoreSettings.on, ctx(), p.term);
  ModuleProgram.errors(p, map);
};

let modules_of = (es: list(ModuleProgram.error)): list(string) =>
  List.map(
    (e: ModuleProgram.error) =>
      Option.value(e.module_, ~default="(program)"),
    es,
  );

let zipper = (~root, text) =>
  switch (ModuleProgram.zipper_of_text(~root, text)) {
  | Some(z) => z
  | None => fail("does not parse: " ++ text)
  };

/* the kinds of a Module term's items */
let items = (e: Exp.t): list(string) =>
  switch (e.term) {
  | Module(items) =>
    List.map(
      (m: Mod.t) =>
        switch (m.term) {
        | ModLet(_) => "ModLet"
        | ModType(_) => "ModType"
        | ModExp(_) => "ModExp"
        | ModuleMod(_) => "ModuleMod"
        | EmptyHole => "EmptyHole"
        | MultiHole(_) => "MultiHole"
        | Invalid(_) => "Invalid"
        },
      items,
    )
  | _ => ["not a module: " ++ Exp.show(e)]
  };

/* the rules above, with a closer left out or one too many */
let common_missing_paren = "let flip2 = fun (a, b) : (String, String) -> Flip(a, Flip(b, Done)";
let common_stray_brace = common ++ " }";
let trap_missing_end = "let on_event = fun (g, e) : (Globals, Event) -> case e | Pull(side) => Common.flip2(\"lever_L\", \"lever_R\")";
let wrong_return_rule = rule("\"hi\"");

/* each module's definition in a program's term, by name */
let rec module_defs = (e: Exp.t): list((string, Exp.t)) =>
  switch (e.term) {
  | Let(_, _, rest)
  | TyAlias(_, _, rest) => module_defs(rest)
  | ModuleExp(mp, def, rest) =>
    switch (ModuleProgram.name_of_module_pat(mp)) {
    | Some(name) => [(name, def), ...module_defs(rest)]
    | None => module_defs(rest)
    }
  | _ => []
  };

let tests = (
  "ModuleProgram",
  [
    test_case(
      "typing `let` at a Mod root gives a module let",
      `Quick,
      () => {
        let typed = text =>
          switch (Parser.to_zipper(~root=Sort.Mod, text)) {
          | Some(z) => MakeTerm.from_zip_for_sem(z, ~root=Sort.Mod).term
          | None => fail("typing failed: " ++ text)
          };
        check(
          list(string),
          "let x = 1",
          ["ModLet"],
          items(typed("let x = 1")),
        );
        check(
          list(string),
          "two members",
          ["ModLet", "ModLet"],
          items(typed("let a = 1; let b = a")),
        );
        check(
          list(string),
          "a member being typed: a hole, not a broken module",
          ["ModLet"],
          items(typed("let x = ")),
        );
      },
    ),
    test_case("the shipped Common and TrapRoom texts round-trip", `Quick, () =>
      List.iter(
        ((name, text, kinds)) => {
          let z = zipper(~root=Sort.Mod, text);
          let printed = PersistentZipper.to_string(z);
          check(string, name ++ ": text, zipper, text", text, printed);
          let term = ModuleProgram.term_of_zipper(~root=Sort.Mod, z);
          check(list(string), name ++ ": its members", kinds, items(term));
          check(
            bool,
            name ++ ": the same term from the printed text",
            true,
            Exp.fast_equal(
              term,
              ModuleProgram.term_of_zipper(
                ~root=Sort.Mod,
                zipper(~root=Sort.Mod, printed),
              ),
            ),
          );
        },
        [
          ("Common", common, ["ModLet"]),
          ("TrapRoom", trap, ["ModLet"]),
          ("TrapRoom, the flip2 rule", flip2_rule, ["ModLet"]),
        ],
      )
    ),
    test_case(
      "the shipped program has no errors and no holes",
      `Quick,
      () => {
        let p = program();
        check(list(string), "errors", [], modules_of(errors(p)));
        check(
          list(pair(string, int)),
          "holes",
          [("Common", 0), ("TrapRoom", 0)],
          fst(ModuleProgram.holes(p)),
        );
        check(int, "outside the modules", 0, snd(ModuleProgram.holes(p)));
      },
    ),
    test_case(
      "Common.flip2 is in scope in TrapRoom",
      `Quick,
      () => {
        check(
          list(string),
          "with Common",
          [],
          modules_of(errors(program(~trap=flip2_rule, ()))),
        );
        check(
          bool,
          "without it, TrapRoom has an error",
          true,
          List.mem(
            "TrapRoom",
            modules_of(errors(program(~common="", ~trap=flip2_rule, ()))),
          ),
        );
      },
    ),
    test_case(
      "an error in TrapRoom is attributed to TrapRoom",
      `Quick,
      () => {
        let es = errors(program(~trap=error_rule, ()));
        check(bool, "there is one", true, es != []);
        check(
          list(string),
          "each in TrapRoom",
          List.map(_ => "TrapRoom", es),
          modules_of(es),
        );
      },
    ),
    test_case(
      "an error in the tail is the program's, unless it is on a module's member",
      `Quick,
      () => {
        check(
          list(string),
          "1 + true",
          ["(program)"],
          modules_of(
            errors(
              program(
                ~tail="(trap_room = TrapRoom.on_event, rooms = 1 + true)",
                (),
              ),
            ),
          ),
        );
        check(
          list(string),
          "TrapRoom.nope: TrapRoom has no nope",
          ["TrapRoom"],
          modules_of(
            errors(
              program(~tail="(trap_room = TrapRoom.nope, rooms = 1)", ()),
            ),
          ),
        );
      },
    ),
    test_case(
      "what a room's rule lacks shows in the tail, and is the room's",
      `Quick,
      () => {
        List.iter(
          trap =>
            check(
              list(string),
              "TrapRoom [" ++ String.escaped(trap) ++ "] has no on_event",
              ["TrapRoom"],
              modules_of(errors(program(~trap, ()))),
            ),
          ["", "?", "1 + 2"],
        );
        check(
          list(string),
          "a rule that gives a String where the tail asks for a Cmd",
          ["TrapRoom"],
          modules_of(
            errors(
              program(~trap=wrong_return_rule, ~tail=ascribed_tail, ()),
            ),
          ),
        );
      },
    ),
    test_case(
      "a bracket or keyword left open, or closed with no opener, is incomplete in its module",
      `Quick,
      () => {
        let incomplete = (~common=common, ~trap=flip2_rule, ()) =>
          ModuleProgram.incomplete(program(~common, ~trap, ()));
        check(
          pair(list(pair(string, int)), int),
          "the shipped program",
          ([("Common", 0), ("TrapRoom", 0)], 0),
          incomplete(),
        );
        List.iter(
          ((what, common, trap, expected)) =>
            check(
              pair(list(pair(string, int)), int),
              what,
              (expected, 0),
              incomplete(~common, ~trap, ()),
            ),
          [
            (
              "a stray } in Common",
              common_stray_brace,
              flip2_rule,
              [("Common", 1), ("TrapRoom", 0)],
            ),
            (
              "Common missing its last )",
              common_missing_paren,
              flip2_rule,
              [("Common", 1), ("TrapRoom", 0)],
            ),
            (
              "TrapRoom missing its end",
              common,
              trap_missing_end,
              [("Common", 0), ("TrapRoom", 1)],
            ),
          ],
        );
        check(
          pair(list(pair(string, int)), int),
          "the tail missing its )",
          ([("Common", 0), ("TrapRoom", 0)], 1),
          ModuleProgram.incomplete(
            program(~tail="(trap_room = TrapRoom.on_event, rooms = 1", ()),
          ),
        );
      },
    ),
    test_case(
      "bodies only the recovering parser reads print back, as modules",
      `Quick,
      () =>
      List.iter(
        ((text, printed)) => {
          check(
            bool,
            text ++ ": not read by the fast parser",
            true,
            FastParse.of_text(~root=Sort.Mod, text) == None,
          );
          let z = zipper(~root=Sort.Mod, text);
          check(
            string,
            "text, zipper, text",
            printed,
            PersistentZipper.to_string(z),
          );
          check(
            bool,
            text ++ ": a module",
            true,
            switch (MakeTerm.from_zip_for_sem(z, ~root=Sort.Mod).term.term) {
            | Module(_) => true
            | _ => false
            },
          );
        },
        [
          (common_stray_brace, common_stray_brace),
          (common_missing_paren, common_missing_paren),
          (trap_missing_end, trap_missing_end),
          /* the editor closes a comment it opens */
          ("let a = 1 # todo", "let a = 1 # todo#"),
        ],
      )
    ),
    test_case(
      "a cache reads again only the parts whose text changed",
      `Quick,
      () => {
        let cache = ModuleProgram.cache();
        let read = (~common=common, ~trap=trap, ()) =>
          module_defs(
            ModuleProgram.of_texts_cached(
              cache,
              ~preamble,
              ~modules=[("Common", common), ("TrapRoom", trap)],
              ~tail,
            ).
              term,
          );
        let first = read();
        let second = read(~trap=flip2_rule, ());
        check(
          bool,
          "Common, unchanged, is the term read before",
          true,
          List.assoc("Common", first) === List.assoc("Common", second),
        );
        check(
          bool,
          "TrapRoom, changed, is read again",
          false,
          List.assoc("TrapRoom", first) === List.assoc("TrapRoom", second),
        );
        check(
          bool,
          "a cached program has the same errors",
          true,
          modules_of(
            errors(
              ModuleProgram.of_texts_cached(
                cache,
                ~preamble,
                ~modules=[("Common", common), ("TrapRoom", error_rule)],
                ~tail,
              ),
            ),
          )
          == modules_of(errors(program(~trap=error_rule, ()))),
        );
        let _ =
          ModuleProgram.of_texts_cached(
            cache,
            ~preamble,
            ~modules=[("Common", common)],
            ~tail,
          );
        check(
          int,
          "a module gone from the program is gone from the cache",
          3,
          Hashtbl.length(cache),
        );
      },
    ),
    test_case("the tail can ascribe a room's rule its type", `Quick, () =>
      check(
        list(string),
        "errors",
        [],
        modules_of(errors(program(~tail=ascribed_tail, ()))),
      )
    ),
    test_case(
      "holes are counted per module",
      `Quick,
      () => {
        let p =
          program(
            ~trap="let on_event = fun (g, e) : (Globals, Event) -> ?",
            (),
          );
        check(
          list(pair(string, int)),
          "a ? in TrapRoom",
          [("Common", 0), ("TrapRoom", 1)],
          fst(ModuleProgram.holes(p)),
        );
        check(list(string), "and no errors", [], modules_of(errors(p)));
        let (holes, _) =
          ModuleProgram.holes(program(~common="let flip2 = ", ()));
        check(
          bool,
          "a member being written in Common",
          true,
          List.assoc("Common", holes) > 0
          && List.assoc("TrapRoom", holes) == 0,
        );
      },
    ),
    test_case(
      "a name given twice is a problem",
      `Quick,
      () => {
        let p =
          ModuleProgram.of_texts(
            ~preamble,
            ~modules=[("Common", common), ("Common", "let x = 1")],
            ~tail="()",
          );
        check(
          list(string),
          "problems",
          ["Common"],
          modules_of(p.problems),
        );
      },
    ),
    test_case(
      "a name that is not a module's is a problem",
      `Quick,
      () => {
        let p =
          ModuleProgram.of_texts(
            ~preamble,
            ~modules=[("", "let x = 1"), ("Trap Room", "let y = 2")],
            ~tail="()",
          );
        check(
          list(string),
          "problems",
          ["", "Trap Room"],
          modules_of(p.problems),
        );
      },
    ),
    test_case(
      "a Mod-rooted editor's statics see the whole program, on its own ids",
      `Quick,
      () => {
        /* the editor pattern: the room's own zipper, its term stitched
           into the program before statics */
        let z = zipper(~root=Sort.Mod, flip2_rule);
        let common_body =
          switch (ModuleProgram.body_of_text(common)) {
          | Some(b) => b
          | None => fail("Common does not parse")
          };
        let preamble =
          switch (ModuleProgram.preamble_of_text(preamble)) {
          | Some(p) => p
          | None => fail("the preamble does not parse")
          };
        let tail =
          switch (ModuleProgram.exp_of_text(tail)) {
          | Some(t) => t
          | None => fail("the tail does not parse")
          };
        let stitched = ref(None);
        let statics =
          CachedStatics.init(
            ~settings=CoreSettings.on,
            ~is_dynamic_term=false,
            ~stitch=
              body => {
                let p =
                  ModuleProgram.stitch(
                    ~preamble,
                    ~modules=[("Common", common_body), ("TrapRoom", body)],
                    ~tail,
                  );
                stitched := Some(p);
                p.term;
              },
            ~ctx=ctx(),
            ~root=Sort.Mod,
            z,
          );
        let p =
          switch (stitched^) {
          | Some(p) => p
          | None => fail("not stitched")
          };
        check(
          list(string),
          "no errors",
          [],
          modules_of(ModuleProgram.errors(p, statics.info_map)),
        );
        let tile_ids =
          Zipper.zip(z)
          |> List.filter_map((piece: Piece.t) =>
               switch (piece) {
               | Tile(t) => Some(t.id)
               | _ => None
               }
             );
        check(bool, "the editor has tiles", true, tile_ids != []);
        check(
          bool,
          "each has its statics, and is TrapRoom's",
          true,
          List.for_all(
            id =>
              Id.Map.mem(id, statics.info_map)
              && ModuleProgram.module_of(p, id) == Some("TrapRoom"),
            tile_ids,
          ),
        );
      },
    ),
    test_case(
      "a body with nothing written is the empty module, not a hole",
      `Quick,
      () => {
        List.iter(
          text => {
            let p = program(~common=text, ());
            check(
              list(pair(string, int)),
              "holes in [" ++ String.escaped(text) ++ "]",
              [("Common", 0), ("TrapRoom", 0)],
              fst(ModuleProgram.holes(p)),
            );
            check(list(string), "errors", [], modules_of(errors(p)));
          },
          ["", "  \n", "# nothing yet #"],
        );
        check(
          list(string),
          "an empty editor",
          [],
          items(ModuleProgram.body_of_zipper(Zipper.init())),
        );
      },
    ),
    test_case(
      "an editor with nothing written is the empty module to its statics too",
      `Quick,
      () => {
        let preamble =
          switch (ModuleProgram.preamble_of_text(preamble)) {
          | Some(p) => p
          | None => fail("the preamble does not parse")
          };
        let tail =
          switch (ModuleProgram.exp_of_text("(rooms = 1, other = 2)")) {
          | Some(t) => t
          | None => fail("the tail does not parse")
          };
        List.iter(
          ((what, z)) => {
            let stitched = ref(None);
            let _ =
              CachedStatics.init(
                ~settings=CoreSettings.on,
                ~is_dynamic_term=false,
                ~stitch=
                  body => {
                    let p =
                      ModuleProgram.stitch(
                        ~preamble,
                        ~modules=[("TrapRoom", body)],
                        ~tail,
                      );
                    stitched := Some(p);
                    p.term;
                  },
                ~ctx=ctx(),
                ~root=Sort.Mod,
                z,
              );
            switch (stitched^) {
            | Some(p) =>
              check(
                list(pair(string, int)),
                what,
                [("TrapRoom", 0)],
                fst(ModuleProgram.holes(p)),
              )
            | None => fail("not stitched")
            };
          },
          [
            ("an empty editor", Zipper.init()),
            ("comments only", zipper(~root=Sort.Mod, "# nothing yet #")),
          ],
        );
      },
    ),
    test_case(
      "a comment ends on its line, so text after an unclosed one is written",
      `Quick,
      () => {
        List.iter(
          ((text, expected)) =>
            check(
              bool,
              String.escaped(text),
              expected,
              ModuleProgram.nothing_written(text),
            ),
          [
            ("", true),
            ("  \n", true),
            ("# nothing yet #", true),
            ("# a #\n  # b #\n", true),
            ("# todo", false),
            ("# todo\nflip #", false),
            ("# a # x", false),
          ],
        );
        check(
          bool,
          "laid out inside its braces",
          true,
          switch (
            Str.search_forward(
              Str.regexp_string("module Common = {\n# todo\nflip #\n} in"),
              ModuleProgram.text_of_parts(
                ~preamble,
                ~modules=[("Common", "# todo\nflip #")],
                ~tail,
              ),
              0,
            )
          ) {
          | _ => true
          | exception Not_found => false
          },
        );
      },
    ),
    test_case(
      "a member after the last `;` is missing, so it is a hole", `Quick, () =>
      check(
        list(pair(string, int)),
        "let a = 1;",
        [("Common", 1), ("TrapRoom", 0)],
        fst(ModuleProgram.holes(program(~common="let a = 1;", ()))),
      )
    ),
  ],
);
