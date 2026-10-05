open Alcotest;
open Web;

/* Persist.reconcile_names splices newly shipped slides into a browser's
 * saved name list. The saved list used to win outright, which froze the
 * Documentation dropdown at whatever it held on the first visit. */

module P = ScratchMode.Persist;

let meta = (~current=0, ~known=[], names): P.slide_meta => {
  current,
  names,
  known_defaults: known,
};

let names = (m: P.slide_meta) => m.names;

let defaults = ["a", "b", "c", "d"];

let check_names = (msg, expected, actual) =>
  check(list(string), msg, expected, actual);

let tests = (
  "SlideReconcile",
  [
    test_case(
      "a new default lands in default order, not at the end",
      `Quick,
      () => {
        /* "b" is new; the saved list has never been offered it */
        let m = meta(~known=["a", "c", "d"], ["a", "c", "d"]);
        check_names(
          "b goes between a and c",
          ["a", "b", "c", "d"],
          P.reconcile_names(~default_names=defaults, m) |> names,
        );
      },
    ),
    test_case(
      "several new defaults keep their relative order",
      `Quick,
      () => {
        let m = meta(~known=["d"], ["d"]);
        check_names(
          "a, b, c all arrive before d",
          ["a", "b", "c", "d"],
          P.reconcile_names(~default_names=defaults, m) |> names,
        );
      },
    ),
    test_case(
      "a default the user deleted stays deleted",
      `Quick,
      () => {
        /* "b" is in known_defaults, so its absence is a deletion */
        let m = meta(~known=defaults, ["a", "c", "d"]);
        let r = P.reconcile_names(~default_names=defaults, m);
        check_names("b does not come back", ["a", "c", "d"], names(r));
        check(bool, "nothing to save", true, r == m);
      },
    ),
    test_case(
      "user slides and reordering survive",
      `Quick,
      () => {
        let m = meta(~known=["a", "c", "d"], ["mine", "d", "a", "c"]);
        check_names(
          "saved order kept, b splices in before c",
          ["mine", "d", "a", "b", "c"],
          P.reconcile_names(~default_names=defaults, m) |> names,
        );
      },
    ),
    test_case(
      "current follows its slide by name",
      `Quick,
      () => {
        let m = meta(~current=1, ~known=["a", "c", "d"], ["a", "c", "d"]);
        let r = P.reconcile_names(~default_names=defaults, m);
        /* was index 1 = "c"; "b" pushed it to index 2 */
        check(int, "still pointing at c", 2, r.current);
        check(
          string,
          "and that slide is c",
          "c",
          List.nth(names(r), r.current),
        );
      },
    ),
    test_case(
      "a meta from before this existed seeds from what it still has",
      `Quick,
      () => {
        /* known_defaults empty = unrecorded; "a"/"c"/"d" are present so
           they count as offered, and only the genuinely missing "b"
           splices in */
        let m = meta(["a", "c", "d"]);
        let r = P.reconcile_names(~default_names=defaults, m);
        check_names("b arrives once", ["a", "b", "c", "d"], names(r));
        check(
          list(string),
          "and the full default list is now recorded",
          defaults,
          r.known_defaults,
        );
      },
    ),
    test_case(
      "reconciling twice changes nothing",
      `Quick,
      () => {
        let m = meta(["a", "c", "d"]);
        let r1 = P.reconcile_names(~default_names=defaults, m);
        let r2 = P.reconcile_names(~default_names=defaults, r1);
        check(bool, "second pass is a fixpoint", true, r1 == r2);
      },
    ),
    test_case(
      "the real deck picks up a slide added later",
      `Quick,
      () => {
        let shipped =
          List.map(fst, snd(Lazy.force(Init.startup).documentation));
        /* This branch's newest slide, renamed after browsers had saved
           the list under its old name. */
        let stale =
          List.filter(n => n != "Livelits / Dynamic Row or Column", shipped);
        check(
          bool,
          "the stale list really is missing it",
          true,
          List.length(stale) == List.length(shipped) - 1,
        );
        let r =
          P.reconcile_names(~default_names=shipped, meta(~known=[], stale));
        check_names("restored in shipped order", shipped, names(r));
      },
    ),
    /* A shared deck's slides are the space's (merge_shared_names). */
    test_case(
      "slides others added join the shared deck, after its last slide",
      `Quick,
      () => {
        let shared = n =>
          String.length(n) > 2 && String.sub(n, 0, 2) == "S/";
        let (current, ns) =
          P.merge_shared_names(
            ~shared,
            ~in_space=["S/mine", "S/theirs"],
            ~default_names=["a", "S/about"],
            (2, ["a", "S/about", "S/mine", "z"]),
          );
        check_names(
          "theirs after mine, before z",
          ["a", "S/about", "S/mine", "S/theirs", "z"],
          ns,
        );
        check(int, "current still on S/mine", 2, current);
      },
    ),
    test_case(
      "a shared slide the space no longer has is gone, unless shipped",
      `Quick,
      () => {
        let shared = n =>
          String.length(n) > 2 && String.sub(n, 0, 2) == "S/";
        let (current, ns) =
          P.merge_shared_names(
            ~shared,
            ~in_space=[],
            ~default_names=["a", "S/about"],
            (2, ["a", "S/about", "S/deleted", "z"]),
          );
        check_names("deleted dropped", ["a", "S/about", "z"], ns);
        check(int, "current clamped", 2, current);
      },
    ),
    test_case(
      "with no shared slide listed yet, others' go at the end",
      `Quick,
      () => {
        let shared = n =>
          String.length(n) > 2 && String.sub(n, 0, 2) == "S/";
        let (_, ns) =
          P.merge_shared_names(
            ~shared,
            ~in_space=["S/x"],
            ~default_names=["a"],
            (0, ["a", "b"]),
          );
        check_names("appended", ["a", "b", "S/x"], ns);
      },
    ),
  ],
);
