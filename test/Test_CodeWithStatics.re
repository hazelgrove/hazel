open Alcotest;
open Web;
open Haz3lcore;

/* `CodeWithStatics.Update.calculate` decides, once per frame, whether to redo
 * statics or reuse the previous `CachedStatics`. That gate is pure
 * incrementality logic and it is load-bearing: elaboration, the evaluation
 * request and probe targets are all read off whichever `statics` it returns. It
 * is also invisible when it is wrong, because a stale `info_map` still renders.
 *
 * The gate is
 *
 *   statics_mode == StaticsForce || is_edited && statics_mode != StaticsDefer
 *
 * so the way to observe it is to hand `calculate` a model whose DOCUMENT has
 * moved on while its `statics` still describes the previous one -- exactly the
 * state an edit produces -- and ask which document the resulting statics
 * describes. `clean` has no errors and `broken` has one, so `error_ids` answers
 * that without depending on allocation identity (`CachedStatics.init` memoizes
 * below this layer, so physical equality does not distinguish the two paths). */

let settings = Settings.Model.init.core;

let calculate =
    (
      ~is_edited,
      ~statics_mode=CodeWithStatics.StaticsNormal,
      model: CodeWithStatics.Model.t,
    ) =>
  CodeWithStatics.Update.calculate(
    ~settings,
    ~is_edited,
    ~statics_mode,
    ~stitch=x => x,
    ~dynamics=model.dynamics,
    ~is_dynamic_term=false,
    model,
  );

let editor_of_text = (text: string): Editor.Model.t =>
  switch (Parser.to_zipper(~root=Sort.Exp, text)) {
  | None => failwith("could not parse: " ++ text)
  | Some(z) => Editor.Model.mk(z, ~root=Sort.Exp)
  };

let of_text = (text: string): CodeWithStatics.Model.t =>
  text
  |> editor_of_text
  |> CodeWithStatics.Model.mk
  |> calculate(~is_edited=true);

/* Swap the document without touching `statics`, the way an edit leaves the
   model just before `calculate` runs. */
let with_document = (text, model: CodeWithStatics.Model.t) => {
  ...model,
  editor: editor_of_text(text),
};

let clean = "1 + 1";
let broken = "1 + true";

let describes_broken = (model: CodeWithStatics.Model.t) =>
  model.statics.error_ids != [];

/* Guard the fixtures themselves: if `broken` ever stops being a static error
   the tests below would all pass for the wrong reason. */
let fixtures = () => {
  check(
    bool,
    "clean has no errors",
    false,
    describes_broken(of_text(clean)),
  );
  check(
    bool,
    "broken has an error",
    true,
    describes_broken(of_text(broken)),
  );
};

let staged = (~is_edited, ~statics_mode=CodeWithStatics.StaticsNormal, ()) =>
  of_text(clean)
  |> with_document(broken)
  |> calculate(~is_edited, ~statics_mode)
  |> describes_broken;

let tests = (
  "CodeWithStatics",
  [
    test_case("fixtures differ in error_ids", `Quick, fixtures),
    test_case("an edited frame recomputes statics", `Quick, () =>
      check(
        bool,
        "statics describes the new document",
        true,
        staged(~is_edited=true, ()),
      )
    ),
    test_case("an unedited frame reuses statics", `Quick, () =>
      check(
        bool,
        "statics still describes the old document",
        false,
        staged(~is_edited=false, ()),
      )
    ),
    /* The debounce: while the user is still typing, ScratchMode passes
       StaticsDefer and schedules a RefreshStatics for later. An edited frame
       must NOT recompute under it, or the debounce does nothing at all. */
    test_case(
      "StaticsDefer skips the recompute on an edited frame", `Quick, () =>
      check(
        bool,
        "statics still describes the old document",
        false,
        staged(
          ~is_edited=true,
          ~statics_mode=CodeWithStatics.StaticsDefer,
          (),
        ),
      )
    ),
    /* The other side of the debounce: the deferred refresh arrives as an
       unedited frame, so StaticsForce has to override the is_edited gate or the
       recompute never happens. */
    test_case("StaticsForce recomputes on an unedited frame", `Quick, () =>
      check(
        bool,
        "statics describes the new document",
        true,
        staged(
          ~is_edited=false,
          ~statics_mode=CodeWithStatics.StaticsForce,
          (),
        ),
      )
    ),
    /* Elaboration is read off the same `statics`, so a reused frame has to hold
       the old elaborated term too -- not just the old info_map. */
    test_case(
      "a reused frame carries the old elaborated term",
      `Quick,
      () => {
        let before = of_text(clean);
        let after =
          before |> with_document(broken) |> calculate(~is_edited=false);
        check(
          bool,
          "elaborated is unchanged",
          true,
          before.statics.elaborated === after.statics.elaborated,
        );
      },
    ),
  ],
);
