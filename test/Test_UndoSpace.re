/* (probe, ^ticket(665); not for merging) What the undo history keeps as the
   program grows: the live heap after a forced collection, with 0, 1, 10 and
   100 undo entries, at each size. Twice: as the history keeps them on this
   branch (History.compact_head: every entry compacted but the newest), and
   with each step's whole page kept as well, as a history without the
   compaction would keep them. The edits are a "1" typed again and again in
   the middle definition of a chain of n lets, through the app's loop
   (Test_UndoCaches.step). Needs node --expose-gc; prints, asserts nothing. */
module Cells = Test_Undo.Cells;
module OM = Test_OpenMember;
module UC = Test_UndoCaches;

let src = (n: int): string => {
  let b = Stdlib.Buffer.create(n * 24);
  Stdlib.Buffer.add_string(b, "let v0 = 0 in\n");
  for (i in 1 to n - 1) {
    Stdlib.Buffer.add_string(
      b,
      Printf.sprintf("let v%d = v%d + %d in\n", i, i - 1, i),
    );
  };
  Stdlib.Buffer.add_string(b, Printf.sprintf("v%d", n - 1));
  Stdlib.Buffer.contents(b);
};

/* the live heap in MB, after two forced collections; None without gc */
let heap = (): option(float) => {
  open Js_of_ocaml;
  let g = Js.Unsafe.global;
  let gc: Js.Unsafe.any = Js.Unsafe.get(g, "gc");
  if (Js.Unsafe.equals(gc, Js.undefined)) {
    None;
  } else {
    ignore(Js.Unsafe.fun_call(gc, [||]));
    ignore(Js.Unsafe.fun_call(gc, [||]));
    let mu =
      Js.Unsafe.meth_call(Js.Unsafe.get(g, "process"), "memoryUsage", [||]);
    Some(Js.float_of_number(Js.Unsafe.get(mu, "heapUsed")) /. 1e6);
  };
};

/* the page ready for edits: built, analyzed, the caret in the middle
   definition, after `v(mid-1) + mid` */
let ready = (n: int): Web.History.Model.t => {
  let text = src(n);
  let mid = n / 2;
  let (m, _) = UC.step(OM.mk(~root=Exp, text), Refresh);
  let m = OM.refresh(m);
  OM.refresh(
    Cells.run(
      m,
      Globals(
        ActiveEditor(
          Move(
            Point(
              OM.after(text, Printf.sprintf("v%d + %d", mid - 1, mid)),
              None,
            ),
          ),
        ),
      ),
    ),
  );
};

/* the heap with 0, 1, 10 and 100 entries; [whole]: each step's page kept
   too */
let run = (~whole: bool, n: int) => {
  let m = ready(n);
  let kept = ref([]);
  let base = heap();
  let rec go = (m, k, marks) =>
    k > 100
      ? (m, List.rev(marks))
      : {
        let (m, _) = UC.step(m, Test_Undo.insert("1"));
        if (whole) {
          kept := [Web.History.Model.(m.current), ...kept^];
        };
        let marks =
          List.mem(k, [1, 10, 100]) ? [(k, heap()), ...marks] : marks;
        go(m, k + 1, marks);
      };
  let (m, marks) = go(m, 1, []);
  let depth = List.length(Web.History.Model.(m.undo_stack));
  let mb = (x: option(float)) =>
    switch (x, base) {
    | (Some(x), Some(b)) => Printf.sprintf("%+.1f", x -. b)
    | _ => "?"
    };
  let per =
    switch (List.assoc_opt(10, marks), List.assoc_opt(100, marks)) {
    | (Some(Some(a)), Some(Some(b))) =>
      Printf.sprintf("%.3f", (b -. a) /. 90.)
    | _ => "?"
    };
  Printf.printf(
    "UNDOSPACE n=%d %s: base %s MB | after 1 edit %s | 10 %s | 100 %s MB (%s MB an entry, from the 10th to the 100th) | %d entries held, %d kept whole by the probe\n%!",
    n,
    whole ? "whole" : "as kept",
    switch (base) {
    | Some(b) => Printf.sprintf("%.1f", b)
    | None => "? (no gc: run node with --expose-gc)"
    },
    mb(List.assoc(1, marks)),
    mb(List.assoc(10, marks)),
    mb(List.assoc(100, marks)),
    per,
    depth,
    List.length(kept^),
  );
  /* (kept until here, so its pages are alive at every mark) */
  ignore(Sys.opaque_identity(kept^));
};

/* without the runner's checks, which the app doesn't run (as
   Test_UndoScaling on probe/665-undo-vs-redo): monolithic statics after
   every calc, and the global remold after every edit */
let unchecked = (f: unit => unit) => {
  let (statics, normalize) = (
    Haz3lcore.DefStatics.after_calc^,
    Haz3lcore.Zipper.normalize_check^,
  );
  Haz3lcore.DefStatics.after_calc := None;
  Haz3lcore.Zipper.normalize_check := None;
  Fun.protect(
    ~finally=
      () => {
        Haz3lcore.DefStatics.after_calc := statics;
        Haz3lcore.Zipper.normalize_check := normalize;
      },
    f,
  );
};

let sizes = () =>
  switch (Sys.getenv_opt("UNDO_SIZES")) {
  | Some(s) => List.map(int_of_string, String.split_on_char(',', s))
  | None => [400, 1600, 6400]
  };

let tests = (
  "UndoSpace",
  [
    Alcotest.test_case(
      "the heap with 0, 1, 10 and 100 undo entries, against program size",
      `Slow,
      () =>
      unchecked(() =>
        List.iter(
          n => {
            run(~whole=false, n);
            run(~whole=true, n);
          },
          sizes(),
        )
      )
    ),
  ],
);
