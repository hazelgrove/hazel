/* SegmentValidator.re - Validation predicates for sync invariants.

   This module provides development-time checks to detect invariant violations
   after sync operations. These checks are O(n) in segment size and should be
   disabled in production builds.

   See docs/automerge-granular-sync.md "Invariants and Validation Framework". */

open Util;

/* Check shards/children relationship for all tiles.
   Invariant: length(shards) == length(children) + 1 */
let validate_shards_children = (seg: Segment.t): list(string) => {
  let errors = ref([]);
  let rec check = (seg: Segment.t, path: string) => {
    seg
    |> List.iteri((i, p) =>
         switch (p) {
         | Piece.Tile(t) =>
           let expected = List.length(t.children) + 1;
           let actual = List.length(t.shards);
           if (actual != expected && actual > 0) {
             errors :=
               [
                 path
                 ++ "/tile"
                 ++ string_of_int(i)
                 ++ ": shards="
                 ++ string_of_int(actual)
                 ++ " but children="
                 ++ string_of_int(List.length(t.children)),
                 ...errors^,
               ];
           };
           t.children
           |> List.iteri((j, child) =>
                check(
                  child,
                  path
                  ++ "/tile"
                  ++ string_of_int(i)
                  ++ "/child"
                  ++ string_of_int(j),
                )
              );
         | _ => ()
         }
       );
  };
  check(seg, "root");
  errors^;
};

/* Check segment shape consistency via skeleton generation.
   Invariant: Segment.skel(seg) doesn't throw Skel.Nonconvex_segment */
let validate_shape = (seg: Segment.t): list(string) => {
  let errors = ref([]);
  let rec check = (seg: Segment.t, path: string) => {
    switch (Segment.skel(seg)) {
    | exception Skel.Nonconvex_segment =>
      errors := [path ++ ": Nonconvex segment (shape conflict)", ...errors^]
    | exception Skel.Input_contains_secondary => () /* Shouldn't happen - skel filters secondary */
    | _ => ()
    };
    /* Recursively check tile children */
    seg
    |> List.iter(p =>
         switch (p) {
         | Piece.Tile(t) =>
           t.children
           |> List.iteri((j, child) =>
                check(child, path ++ "/child" ++ string_of_int(j))
              )
         | _ => ()
         }
       );
  };
  check(seg, "root");
  errors^;
};

/* Check UUID uniqueness across segment.
   Invariant: All piece IDs are unique across the entire edit state */
let validate_unique_ids = (seg: Segment.t): list(string) => {
  let ids = Segment.ids(seg);
  let unique_ids = List.sort_uniq(Id.compare, ids);
  if (List.length(ids) != List.length(unique_ids)) {
    ["Duplicate piece IDs detected in segment"];
  } else {
    [];
  };
};

/* Run all validations, log any errors.
   Call this after sync operations to detect invariant violations.
   Disable in production for performance. */
let validate_all = (seg: Segment.t): unit => {
  let errors =
    validate_shards_children(seg)
    @ validate_shape(seg)
    @ validate_unique_ids(seg);
  errors
  |> List.iter(e =>
       Js_of_ocaml.Firebug.console##warn(
         Js_of_ocaml.Js.string("[SYNC VALIDATION] " ++ e),
       )
     );
};

/* Validate and return whether any errors were found */
let has_errors = (seg: Segment.t): bool => {
  let errors =
    validate_shards_children(seg)
    @ validate_shape(seg)
    @ validate_unique_ids(seg);
  List.length(errors) > 0;
};
