open Alcotest;
open Haz3lcore;

/* TEMPORARY measurement harness (not a gate): where does the 2.5s
   whole-zipper sexp autosave cost live, and what would PER-ITEM
   serialized persistence cost per edit? Prints timings; always
   passes. Run: bash test/run_node.sh test 'SerBench' */

let now = (): float =>
  Js_of_ocaml.Js.Unsafe.meth_call(
    Js_of_ocaml.Js.Unsafe.get(Js_of_ocaml.Js.Unsafe.global, "Date"),
    "now",
    [||],
  );

let time = (label: string, f: unit => 'a): 'a => {
  let t0 = now();
  let x = f();
  Printf.printf("  %-42s %8.1fms\n", label, now() -. t0);
  x;
};

let time3 = (label, f) => {
  /* median-ish of 3: first run includes warmup, print all */
  let t0 = now();
  let _ = f();
  let t1 = now();
  let _ = f();
  let t2 = now();
  let x = f();
  let t3 = now();
  Printf.printf(
    "  %-42s %8.1f / %6.1f / %6.1fms\n",
    label,
    t1 -. t0,
    t2 -. t1,
    t3 -. t2,
  );
  x;
};

let bench_zipper = (name: string, z: Zipper.t) => {
  Printf.printf("== %s ==\n", name);
  let text = time("print text (MarkerParse.to_text)", () =>
    MarkerParse.to_text(z)
  );
  Printf.printf("  text chars: %d\n", String.length(text));
  let sexp = time3("sexp_of_t (tree construction)", () =>
    Zipper.sexp_of_t(z)
  );
  let str = time3("Sexp.to_string (string build)", () =>
    Sexplib.Sexp.to_string(sexp)
  );
  Printf.printf("  sexp chars: %d\n", String.length(str));
  let _ = time3("restore: of_string + t_of_sexp", () =>
    Sexplib.Sexp.of_string(str) |> Zipper.t_of_sexp
  );
  /* per-item proxy: top-level piece slices of increasing size */
  let seg = Zipper.unselect_and_zip(z);
  let n = List.length(seg);
  Printf.printf("  top-level pieces: %d\n", n);
  List.iter(
    k =>
      if (k <= n) {
        let slice = Util.ListUtil.sublist((0, k), seg);
        let label = Printf.sprintf("segment sexp+string, %d pieces", k);
        let s =
          time3(label, () =>
            Sexplib.Sexp.to_string(Segment.sexp_of_t(slice))
          );
        Printf.printf("    (%d sexp chars)\n", String.length(s));
      },
    [10, 50, 200],
  );
  /* whole-segment sexp (zipper minus caret bookkeeping) */
  let _ =
    time3("segment sexp+string, ALL pieces", () =>
      Sexplib.Sexp.to_string(Segment.sexp_of_t(seg))
    );
  ();
};

let load = (name: string): option(Zipper.t) =>
  switch (CorpusUtil.mega_src(name)) {
  | None => None
  | Some(text) =>
    PersistentZipper.parse_text(~source="serbench", ~root=Sort.Exp, text)
  };

let cases = [
  test_case("serialization cost accounting", `Quick, () => {
    switch (load("mega-1k.hz")) {
    | None => Alcotest.skip()
    | Some(z) => bench_zipper("mega-1k (27k chars)", z)
    };
    switch (load("mega-4k.hz")) {
    | None => ()
    | Some(z) => bench_zipper("mega-4k (123k chars)", z)
    };
  }),
];

let tests = [("SerBench", cases)];
