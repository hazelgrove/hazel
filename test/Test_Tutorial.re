open Alcotest;
open Web;

/* Rules that must hold for whatever lessons are authored in
   hazel-programs/tutorial/, not facts about the current set: their titles are
   SlidePaths, and the folder navigation in TutorialsMode plus the breadcrumb
   in EditorModeView both depend on how those are shaped. */

let lessons = TutorialSettings.lessons;
let paths = List.map(~f=Tutorial.path_of, lessons);
let strings = list(string);

let folder_of = (p: SlidePath.t): string =>
  SlidePath.folder(p) |> Option.value(~default="<no folder>");

/* Folders in first-appearance order. */
let folders =
  paths
  |> List.map(~f=folder_of)
  |> List.fold_left(
       ~f=
         (acc, f) =>
           List.mem(acc, f, ~equal=String.equal) ? acc : acc @ [f],
       ~init=[],
     );

let tests = [
  (
    "Tutorial lesson paths",
    [
      test_case("every lesson names exactly one folder", `Quick, () =>
        List.iter(
          ~f=
            (p: SlidePath.t) =>
              check(
                bool,
                "one non-empty folder segment: " ++ SlidePath.to_string(p),
                true,
                switch (SlidePath.folders(p)) {
                | [f] => !String.equal(f, "")
                | _ => false
                },
              ),
          paths,
        )
      ),
      test_case("each folder's lessons are contiguous", `Quick, ()
        /* Grouping tolerates gaps, but the dropdown's option order and the
           reading order of Slides.re both assume contiguity. */
        =>
          check(
            strings,
            "no folder is revisited",
            folders,
            paths
            |> List.map(~f=folder_of)
            |> List.fold_left(
                 ~f=
                   (acc, f) =>
                     switch (List.rev(acc)) {
                     | [last, ..._] when String.equal(last, f) => acc
                     | _ => acc @ [f]
                     },
                 ~init=[],
               ),
          )
        ),
      test_case(
        "lesson ids are unique",
        `Quick,
        () => {
          /* A lesson's identity is its id, not its title -- that is what makes
             retitling and recategorizing safe, and it is why the per-lesson
             store key is the id (TutorialsMode.Store.save_exercise). Two
             lessons sharing an id would share saved work. */
          let ids =
            List.map(
              ~f=spec => Tutorial.id_of(spec) |> Haz3lcore.Id.to_string,
              lessons,
            );
          check(
            int,
            "distinct ids",
            List.length(lessons),
            ids |> List.dedup_and_sort(~compare=String.compare) |> List.length,
          );
        },
      ),
      test_case(
        "no title is a proper prefix of another",
        `Quick,
        () => {
          /* Such a pair makes the shorter lesson unreachable from the deeper
             breadcrumb dropdown. */
          let segs = List.map(~f=SlidePath.segments, paths);
          List.iter(
            ~f=
              a =>
                List.iter(
                  ~f=
                    b =>
                      check(
                        bool,
                        "not a proper prefix: "
                        ++ String.concat(~sep=" / ", a)
                        ++ " vs "
                        ++ String.concat(~sep=" / ", b),
                        false,
                        List.length(a) < List.length(b)
                        && List.equal(
                             String.equal,
                             Util.ListUtil.take(List.length(a), b),
                             a,
                           ),
                      ),
                  segs,
                ),
            segs,
          );
        },
      ),
    ],
  ),
];
