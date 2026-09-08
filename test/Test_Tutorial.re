open Alcotest;
open Web;

/* A ratchet over the authored .hzt lessons: their titles are SlidePaths, and
   the folder navigation in TutorialsMode plus the breadcrumb in
   EditorModeView both depend on how they are shaped. */

let lessons = TutorialSettings.lessons;
let paths = List.map(Tutorial.path_of, lessons);
let strings = list(string);
let ints = list(int);

/* Folders in first-appearance order. */
let folder_of = (p: SlidePath.t): string =>
  SlidePath.folder(p) |> Option.value(~default="<no folder>");

let folders =
  paths
  |> List.map(folder_of)
  |> List.fold_left((acc, f) => List.mem(f, acc) ? acc : acc @ [f], []);

/* The categorization: the intro track runs through Labeled Tuple Projection,
   then the remaining labeled-tuple structural features get a folder, and
   Tables holds the table lessons, the rich probe built for tables, and the
   three tasks ported from the tables-study branch. */
let expected_folders = [
  "Basics x19",
  "Tuple Structural Operations x3",
  "Tables x6",
];

let tests = [
  (
    "Tutorial lesson paths",
    [
      test_case("every lesson names a folder", `Quick, () =>
        List.iter(
          (p: SlidePath.t) =>
            check(
              bool,
              "folder is a single non-empty segment: "
              ++ SlidePath.to_string(p),
              true,
              switch (SlidePath.folders(p)) {
              | [f] => f != ""
              | _ => false
              },
            ),
          paths,
        )
      ),
      test_case("the folders are the four expected tracks", `Quick, ()
        /* Sizes included so an accidental recategorization is caught, not
           just a renamed or missing folder. */
        =>
          check(
            strings,
            "folders in order, with lesson counts",
            expected_folders,
            folders
            |> List.map(f =>
                 Printf.sprintf(
                   "%s x%d",
                   f,
                   paths |> List.filter(p => folder_of(p) == f) |> List.length,
                 )
               ),
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
            |> List.map(folder_of)
            |> List.fold_left(
                 (acc, f) =>
                   switch (List.rev(acc)) {
                   | [last, ..._] when last == f => acc
                   | _ => acc @ [f]
                   },
                 [],
               ),
          )
        ),
      test_case("the default lesson is Basics / Holes", `Quick, ()
        /* TutorialsMode.StoreTutorialKey.default opens lesson 0. */
        =>
          check(
            string,
            "lesson 0",
            "Basics / Holes",
            SlidePath.to_string(List.nth(paths, 0)),
          )
        ),
      test_case(
        "next walks exactly one folder, then stops",
        `Quick,
        () => {
          /* Drives the real navigation the arrows use: from a folder's first
             lesson, repeated "next" must visit that folder's lessons in order
             and then clamp -- clamping is what makes the view show the
             completion message instead of a next arrow. */
          let rec walk = (acc, i) => {
            let next = SlidePath.step_in_folder(~current=i, ~by=1, paths);
            next == i ? List.rev([i, ...acc]) : walk([i, ...acc], next);
          };
          let rec back = (acc, i) => {
            let prev = SlidePath.step_in_folder(~current=i, ~by=-1, paths);
            prev == i ? [i, ...acc] : back([i, ...acc], prev);
          };
          List.iter(
            folder => {
              let expected =
                paths
                |> List.mapi((i, p) => (i, p))
                |> List.filter_map(((i, p)) =>
                     folder_of(p) == folder ? Some(i) : None
                   );
              check(
                ints,
                "forward through " ++ folder,
                expected,
                walk([], List.hd(expected)),
              );
              check(
                ints,
                "backward through " ++ folder,
                expected,
                back([], List.nth(expected, List.length(expected) - 1)),
              );
            },
            folders,
          );
        },
      ),
      test_case("folder edges are where the arrows stop", `Quick, () =>
        List.iteri(
          (i, _) => {
            let (pos, size) = SlidePath.folder_position(~current=i, paths);
            let at = string_of_int(i);
            check(
              bool,
              "is_first agrees with a clamped prev at " ++ at,
              pos == 0,
              SlidePath.step_in_folder(~current=i, ~by=-1, paths) == i,
            );
            check(
              bool,
              "is_last agrees with a clamped next at " ++ at,
              pos == size - 1,
              SlidePath.step_in_folder(~current=i, ~by=1, paths) == i,
            );
          },
          paths,
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
              spec => Tutorial.id_of(spec) |> Haz3lcore.Id.to_string,
              lessons,
            );
          check(
            int,
            "distinct ids",
            List.length(lessons),
            ids |> List.sort_uniq(String.compare) |> List.length,
          );
        },
      ),
      test_case(
        "no title is a proper prefix of another",
        `Quick,
        () => {
          /* Such a pair makes the shorter lesson unreachable from the deeper
             breadcrumb dropdown. */
          let segs = List.map(SlidePath.segments, paths);
          List.iter(
            a =>
              List.iter(
                b =>
                  check(
                    bool,
                    "not a proper prefix: "
                    ++ String.concat(" / ", a)
                    ++ " vs "
                    ++ String.concat(" / ", b),
                    false,
                    List.length(a) < List.length(b)
                    && Util.ListUtil.take(List.length(a), b) == a,
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
