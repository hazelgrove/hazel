open Alcotest;
open Haz3lcore;
open Web;

let settings = {
  ...Language.CoreSettings.off,
  statics: true,
};
let zipper = Test_TabDispatch.padding_zipper;
let checked = z =>
  CachedStatics.init(
    ~settings,
    ~is_dynamic_term=true,
    ~stitch=Fun.id,
    ~root=Exp,
    z,
  );

let expected_type = info =>
  switch (info) {
  | Some(Language.Info.InfoExp({ana, _}))
  | Some(Language.Info.InfoPat({ana, _})) =>
    Printer.of_segment(
      ~holes="?",
      TypToSegment.typ_to_segment(
        ~settings=ExpToSegment.Settings.of_core(~inline=true, settings),
        ana,
      ),
    )
  | _ => "none"
  };

/* per-item statics, as Scratch and documentation slides run them */
let checked_compositional = z =>
  CachedStatics.init_compositional(~settings, ~stitch=Fun.id, ~root=Exp, z);

/* as the editor runs them: on its syntax's segment and item parses */
let checked_shared = z => {
  let syntax = CachedSyntax.init(~root=Exp, z);
  let source = CachedSyntax.semantic_source(z, syntax);
  check(bool, "the syntax's segment is the source", true, source != None);
  CachedStatics.init_compositional(
    ~settings,
    ~stitch=Fun.id,
    ~root=Exp,
    ~shared=syntax.t_cache,
    ~source?,
    z,
  );
};

let example = (~compositional=false, ~shared=false, input, expected) =>
  test_case(
    (shared ? "shared: " : compositional ? "compositional: " : "")
    ++ String.escaped(input),
    `Quick,
    () => {
      let z = zipper(input);
      let statics =
        shared
          ? checked_shared(z)
          : compositional ? checked_compositional(z) : checked(z);
      check(
        string,
        "expected type of implied hole",
        expected,
        ImpliedHole.at_caret(~statics, z) |> expected_type,
      );
    },
  );

let let_input = "let x : Int = ¦\nlet y = 2 in y";

let tests = [
  (
    "ImpliedHole",
    [
      example(let_input, "Int"),
      example(~compositional=true, let_input, "Int"),
      example(
        ~compositional=true,
        "let f(x : Int) : Bool = ¦\nlet y = 2 in y",
        "Bool",
      ),
      example(
        ~compositional=true,
        "let x : Int = 1¦\nlet y = 2 in y",
        "none",
      ),
      example(~shared=true, let_input, "Int"),
      example(
        ~shared=true,
        "let f(x : Int) : Bool = ¦\nlet y = 2 in y",
        "Bool",
      ),
      test_case(
        "a moved caret or an unparsed buffer reads the zipper",
        `Quick,
        () => {
          let z = zipper("let x : Int = 1¦ \nlet y = 2 in y");
          let syntax = CachedSyntax.init(~root=Exp, z);
          let moved =
            Test_Editing.perform(z, [Move(Local(Right, ByChar))]);
          check(
            bool,
            "moved",
            true,
            CachedSyntax.semantic_source(moved, syntax) == None,
          );
          let buffered = {
            ...z,
            selection: Selection.mk_buffer(Unparsed, []),
          };
          check(
            bool,
            "unparsed buffer",
            true,
            CachedSyntax.semantic_source(buffered, syntax) == None,
          );
        },
      ),
      example("let x = ¦\nlet y = 2 in y", "?"),
      example("let f(x : Int) : Bool = ¦\nlet y = 2 in y", "Bool"),
      example("let f : Int -> Bool = fun x ->¦", "Bool"),
      example("let x : Int = if true then¦", "Int"),
      example("if¦\nlet y = 2 in y", "Bool"),
      example("let x : Int = 1¦\nlet y = 2 in y", "none"),
      example("let x : Int = ¿ ¦\nlet y = 2 in y", "none"),
      example("let x : Int = ? ¦\nlet y = 2 in y", "none"),
      example("let x : Int =\nlet ¦y = 2 in y", "none"),
      example("let x : I¦nt =\nlet y = 2 in y", "none"),
      test_case(
        "stale annotation with unchanged tile IDs",
        `Quick,
        () => {
          let z = zipper(let_input);
          let statics = checked(z);
          let changed =
            ZipperBase.MapPiece.go(
              fun
              | Tile({form: Tok("Int"), _} as t) => [
                  Tile({
                    ...t,
                    form: Tok("Bool"),
                  }),
                ]
              | p => [p],
              z,
            );
          check(
            string,
            "stale type is suppressed",
            "none",
            ImpliedHole.at_caret(~statics, changed) |> expected_type,
          );
          check(
            string,
            "fresh type becomes visible",
            "Bool",
            ImpliedHole.at_caret(~statics=checked(changed), changed)
            |> expected_type,
          );
        },
      ),
      test_case(
        "caret movement keeps the checked snapshot usable",
        `Quick,
        () => {
          let z = zipper("let x : Int = ¦ \nlet y = 2 in y");
          let statics = checked(z);
          let moved =
            Test_Editing.perform(z, [Move(Local(Right, ByChar))]);
          check(
            string,
            "same hole after moving across space",
            "Int",
            ImpliedHole.at_caret(~statics, moved) |> expected_type,
          );
        },
      ),
      test_case(
        "selection uses ordinary inspection",
        `Quick,
        () => {
          let z = zipper(let_input);
          let statics = checked(z);
          let selected =
            Test_Editing.perform(
              z,
              [Select(Resize(Local(Left, ByChar)))],
            );
          check(
            bool,
            "selection fixture",
            false,
            Selection.is_empty(selected.selection),
          );
          check(
            string,
            "no implied hole for selection",
            "none",
            ImpliedHole.at_caret(~statics, selected) |> expected_type,
          );
        },
      ),
      test_case(
        "missing snapshot or info falls back",
        `Quick,
        () => {
          let z = zipper(let_input);
          let statics = checked(z);
          List.iter(
            statics =>
              check(
                string,
                "no speculative information",
                "none",
                ImpliedHole.at_caret(~statics, z) |> expected_type,
              ),
            [
              {
                ...statics,
                completion: None,
              },
              {
                ...statics,
                info_map: Util.Id.Map.empty,
              },
            ],
          );
        },
      ),
      test_case(
        "deferred analysis invalidates even an unchanged source",
        `Quick,
        () => {
          let z = zipper(let_input);
          let model =
            CodeWithStatics.Model.mk(
              ~statics=checked(z),
              Editor.Model.mk(~root=Exp, z),
            );
          let calculate = (~is_edited, ~statics_mode, model) =>
            CodeWithStatics.Update.calculate(
              ~settings,
              ~is_edited,
              ~statics_mode,
              ~stitch=Fun.id,
              ~dynamics=Language.Dynamics.Map.empty,
              ~is_dynamic_term=true,
              model,
            );
          let deferred =
            calculate(~is_edited=true, ~statics_mode=Defer, model);
          check(
            string,
            "pending analysis is not used",
            "none",
            ImpliedHole.at_caret(~statics=deferred.statics, z)
            |> expected_type,
          );
          let refreshed =
            calculate(~is_edited=false, ~statics_mode=Force, deferred);
          check(
            string,
            "feedback returns after refresh",
            "Int",
            ImpliedHole.at_caret(~statics=refreshed.statics, z)
            |> expected_type,
          );
        },
      ),
      test_case(
        "inspector override leaves ordinary cursor information intact",
        `Quick,
        () => {
          let z = zipper(let_input);
          let statics = checked(z);
          let normal = Indicated.ci_of(z, statics.info_map);
          let cursor: Cursor.cursor(unit) = {
            ...Cursor.empty,
            info: normal,
            implied_hole:
              Lazy.from_fun(() => ImpliedHole.at_caret(~statics, z)),
            editor_read_only: false,
          };
          check(
            string,
            "bottom inspector",
            "Int",
            CursorInspector.info_for_view(~quiver=true, cursor)
            |> expected_type,
          );
          check(
            bool,
            "ordinary indication stays unchanged",
            true,
            cursor.info === normal,
          );
          check(
            bool,
            "quiver off",
            true,
            CursorInspector.info_for_view(~quiver=false, cursor) === normal,
          );
          check(
            bool,
            "read-only editor",
            true,
            CursorInspector.info_for_view(
              ~quiver=true,
              {
                ...cursor,
                editor_read_only: true,
              },
            )
            === normal,
          );
        },
      ),
    ],
  ),
];
