/* (^report(47), ^ticket(718)) Experiment 1: the same first-order value sent
   each way. Matthew, 2026-11-08: "a special case of Value ... that's
   especially efficient to serialize for first-order data", and: experiment
   with Fumola's more efficient design, then submit a plan to the group.

   A prototype of the data form (no source ids, no holes, no closures, no
   sharing), made from a value term by [of_term]. Then the Witch's picture,
   and every probe sample on the slide that converts, timed each way:
   - the term by Marshal, as the worker sends it today;
   - as data by Marshal, and by Marshal with no sharing table;
   - as data made plain JS values, copied by the browser's structuredClone
     (what postMessage does), and as JSON.
   globalThis.hazelDataExperiment(), called by the Firefox check after an
   edit, reads the last result the worker sent while the page kept its
   timeline ([keep], from WorkerClient) and returns JSON. Nothing here runs
   unless the timeline is on or it is called. */
open Js_of_ocaml;

module Data = {
  type t =
    | Atom(Language.Atom.t)
    | List(list(t))
    | Tuple(list(field))
    | Ctor(string, option(t))
  and field = {
    label: option(string),
    value: t,
  };

  let all = (f, xs) =>
    List.fold_right(
      (x, acc) =>
        switch (acc, f(x)) {
        | (Some(l), Some(v)) => Some([v, ...l])
        | _ => None
        },
      xs,
      Some([]),
    );

  /* the node under its ascriptions: only this node's, not a rewrite of the
     whole subterm, as DHExp.strip_ascriptions is (called at every node, that
     made the conversion quadratic: 110 ms for the Witch's picture) */
  let rec peel = (e: Language.Exp.t): Language.Exp.t =>
    switch (e.term) {
    | Asc(d, _) => peel(d)
    | _ => e
    };

  /* None at the first function, closure, hole or open term */
  let rec of_term = (e: Language.Exp.t): option(t) => {
    let e = peel(e);
    switch (e.term) {
    | Atom(a) => Some(Atom(a))
    | ListLit(xs) => Option.map(xs => List(xs), all(of_term, xs))
    | Tuple(xs) => Option.map(fs => Tuple(fs), all(field, xs))
    | TupLabel(_) => Option.map(f => Tuple([f]), field(e))
    | Constructor(name, _) => Some(Ctor(name, None))
    | Ap(_, {term: Constructor(name, _), _}, arg) =>
      Option.map(a => Ctor(name, Some(a)), of_term(arg))
    | Parens(e) => of_term(e)
    | _ => None
    };
  }
  and field = (e: Language.Exp.t): option(field) =>
    switch (peel(e).term) {
    | TupLabel({term: Label(l), _}, v) =>
      Option.map(
        value =>
          {
            label: Some(l),
            value,
          },
        of_term(v),
      )
    | _ =>
      Option.map(
        value =>
          {
            label: None,
            value,
          },
        of_term(e),
      )
    };

  let rec count = (d: t): int =>
    switch (d) {
    | Atom(_)
    | Ctor(_, None) => 1
    | Ctor(_, Some(d)) => 1 + count(d)
    | List(xs) => List.fold_left((n, x) => n + count(x), 1, xs)
    | Tuple(fs) => List.fold_left((n, f) => n + count(f.value), 1, fs)
    };

  /* plain JS: a number, string or boolean; an array; an object for a
     tuple whose fields are all labeled; {c, v} for a constructor */
  let rec to_js = (d: t): Js.Unsafe.any =>
    switch (d) {
    | Atom(Int(b))
    | Atom(Nat(b)) => Js.Unsafe.inject(Js.string(Bigint.to_string(b)))
    | Atom(SInt(i)) =>
      Js.Unsafe.inject(Js.number_of_float(float_of_int(i)))
    | Atom(Float(f)) => Js.Unsafe.inject(Js.number_of_float(f))
    | Atom(Bool(b)) => Js.Unsafe.inject(Js.bool(b))
    | Atom(String(s)) => Js.Unsafe.inject(Js.string(s))
    | List(xs) =>
      Js.Unsafe.inject(Js.array(Array.of_list(List.map(to_js, xs))))
    | Tuple(fs) when fs != [] && List.for_all(f => f.label != None, fs) =>
      Js.Unsafe.obj(
        Array.of_list(
          List.map(
            f => (Option.value(f.label, ~default=""), to_js(f.value)),
            fs,
          ),
        ),
      )
    | Tuple(fs) =>
      Js.Unsafe.inject(
        Js.array(Array.of_list(List.map(f => to_js(f.value), fs))),
      )
    | Ctor(c, None) =>
      Js.Unsafe.obj([|("c", Js.Unsafe.inject(Js.string(c)))|])
    | Ctor(c, Some(v)) =>
      Js.Unsafe.obj([|
        ("c", Js.Unsafe.inject(Js.string(c))),
        ("v", to_js(v)),
      |])
    };
};

/* (^report(47), experiment 4) data printed as Hazel writes it: a list in
   brackets, a tuple in parens with its labels, a constructor and its
   argument; numbers and strings as literals */
let rec data_to_string = (d: Data.t): string =>
  switch (d) {
  | Atom(Int(b))
  | Atom(Nat(b)) => Bigint.to_string(b)
  | Atom(SInt(i)) => string_of_int(i)
  | Atom(Float(f)) => Printf.sprintf("%g", f)
  | Atom(Bool(b)) => b ? "true" : "false"
  | Atom(String(s)) => "\"" ++ s ++ "\""
  | List(xs) =>
    "[" ++ String.concat(", ", List.map(data_to_string, xs)) ++ "]"
  | Tuple(fs) =>
    "("
    ++ String.concat(
         ", ",
         List.map(
           (f: Data.field) =>
             switch (f.label) {
             | Some(l) => l ++ " = " ++ data_to_string(f.value)
             | None => data_to_string(f.value)
             },
           fs,
         ),
       )
    ++ ")"
  | Ctor(c, None) => c
  | Ctor(c, Some(v)) => c ++ "(" ++ data_to_string(v) ++ ")"
  };

/* a term printed as the CLI prints a result (src/CLI/Print.re) */
let term_settings: Haz3lcore.ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: false,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  use_literal_lexemes: false,
  hole_tiles: false,
  project_tables: false,
};
let term_to_string = (e: Language.Exp.t): string =>
  Haz3lcore.Printer.of_segment(
    ~holes="?",
    Haz3lcore.ExpToSegment.exp_to_segment(~settings=term_settings, e),
  );

/* ms per call: batches of at least 25 ms (Firefox's clock ticks in whole
   ms), the median of five */
let per_op = (f: unit => 'a): float => {
  let batch = () => {
    let t0 = Util.BenchMarks.now();
    let n = ref(0);
    while (Util.BenchMarks.now() -. t0 < 25. || n^ < 3) {
      ignore(f());
      incr(n);
    };
    (Util.BenchMarks.now() -. t0) /. float_of_int(n^);
  };
  let xs = List.sort(compare, List.init(5, _ => batch()));
  List.nth(xs, 2);
};

let structured_clone = (v: Js.Unsafe.any): Js.Unsafe.any =>
  Js.Unsafe.fun_call(
    Js.Unsafe.get(Js.Unsafe.global, "structuredClone"),
    [|v|],
  );
let json = Js.Unsafe.get(Js.Unsafe.global, "JSON");
let stringify = (v: Js.Unsafe.any): Js.t(Js.js_string) =>
  Js.Unsafe.meth_call(json, "stringify", [|v|]);
let parse = (s: Js.t(Js.js_string)): Js.Unsafe.any =>
  Js.Unsafe.meth_call(json, "parse", [|Js.Unsafe.inject(s)|]);

/* one value, every way; [term] is what the worker sends today, [convert]
   makes [data] from it */
let measure = (~convert: unit => 'c, term: 'term, data: Data.t): Yojson.Safe.t => {
  let t_str = Marshal.to_string(term, []);
  let d_str = Marshal.to_string(data, []);
  let n_str =
    switch (Marshal.to_string(data, [Marshal.No_sharing])) {
    | s => Some(s)
    | exception _ => None
    };
  let js = Data.to_js(data);
  let j_str = stringify(js);
  let num = f => `Float(Float.round(f *. 1000.) /. 1000.);
  `Assoc([
    ("nodes", `Int(Data.count(data))),
    ("term_bytes", `Int(String.length(t_str))),
    ("data_bytes", `Int(String.length(d_str))),
    (
      "data_noshare_bytes",
      switch (n_str) {
      | Some(s) => `Int(String.length(s))
      | None => `Null
      },
    ),
    ("json_bytes", `Int(j_str##.length)),
    (
      "ms",
      `Assoc([
        ("term_encode", num(per_op(() => Marshal.to_string(term, [])))),
        (
          "term_decode",
          num(per_op(() => ignore(Marshal.from_string(t_str, 0)))),
        ),
        ("of_term", num(per_op(convert))),
        ("data_encode", num(per_op(() => Marshal.to_string(data, [])))),
        (
          "data_decode",
          num(per_op(() => ignore(Marshal.from_string(d_str, 0)))),
        ),
        (
          "data_noshare_encode",
          switch (n_str) {
          | Some(_) =>
            num(per_op(() => Marshal.to_string(data, [Marshal.No_sharing])))
          | None => `Null
          },
        ),
        ("to_js", num(per_op(() => Data.to_js(data)))),
        ("structured_clone", num(per_op(() => structured_clone(js)))),
        ("json_encode", num(per_op(() => stringify(js)))),
        ("json_decode", num(per_op(() => parse(j_str)))),
      ]),
    ),
  ]);
};

/* (Matthew, 2026-11-08: treemaps of the space, before and after the data
   form) a value's parts, each part's Marshal bytes split four ways, each a
   difference of exact Marshal sizes, so they add up:
   - ids and tags: the term, against the same term with every id tag one
     shared tag (written once, then referred to);
   - term structure: that, against the part as data;
   - data structure: the data, against its atoms alone;
   - data: the atoms themselves (each one's Marshal, less its header). */
let marshal_header = String.length(Marshal.to_string(0, [])) - 1;
let mbytes = (v: 'a): int =>
  String.length(Marshal.to_string(v, [])) - marshal_header;

let one_tag: Lazy.t(Language.IdTagged.IdTag.t) =
  lazy(Language.IdTagged.IdTag.fresh());
let shared_tags = (e: Language.Exp.t): Language.Exp.t => {
  let tag = Lazy.force(one_tag);
  Language.Exp.map_term(
    ~f_exp=
      (continue, e) => {
        let e: Language.Exp.t = continue(e);
        {
          ...e,
          annotation: tag,
        };
      },
    ~f_pat=
      (continue, p) => {
        let p: Language.Pat.t = continue(p);
        {
          ...p,
          annotation: tag,
        };
      },
    ~f_typ=
      (continue, t) => {
        let t: Language.Typ.t = continue(t);
        {
          ...t,
          annotation: tag,
        };
      },
    e,
  );
};

let rec atoms = (d: Data.t): list(Language.Atom.t) =>
  switch (d) {
  | Atom(a) => [a]
  | Ctor(_, None) => []
  | Ctor(_, Some(d)) => atoms(d)
  | List(xs) => List.concat_map(atoms, xs)
  | Tuple(fs) => List.concat_map((f: Data.field) => atoms(f.value), fs)
  };

let split = (term: Language.Exp.t, data: Data.t): Yojson.Safe.t => {
  let t = mbytes(term);
  let s = mbytes(shared_tags(term));
  let d = mbytes(data);
  let p = List.fold_left((n, a) => n + mbytes(a), 0, atoms(data));
  `Assoc([
    ("term", `Int(t)),
    ("shared_tags", `Int(s)),
    ("data", `Int(d)),
    ("atoms", `Int(p)),
  ]);
};

/* a part's name: its "name" field, if it is a record that has one, or a
   constructor around one (a shape: Polygon(p), Path(p)) */
let rec name_of = (d: Data.t): option(string) =>
  switch (d) {
  | Ctor(_, Some(d)) => name_of(d)
  | Tuple(fs) =>
    List.find_map(
      (f: Data.field) =>
        switch (f.label, f.value) {
        | (Some("name"), Atom(String(s))) => Some(s)
        | _ => None
        },
      fs,
    )
  | _ => None
  };

/* a tuple field's value, under its label */
let field_term = (t: Language.Exp.t): Language.Exp.t =>
  switch (Data.peel(t).term) {
  | TupLabel(_, v) => v
  | _ => t
  };

/* the parts: the elements of the first list met going down, term and data
   side by side, through parens, constructors and records (into the field
   with the most nodes): the picture's shapes, under the livelit's model
   (picture = SpliceRef(id, shapes)) */
let rec parts =
        (term: Language.Exp.t, data: Data.t): list((Language.Exp.t, Data.t)) => {
  let term = Data.peel(term);
  switch (term.term, data) {
  | (Parens(e), _) => parts(e, data)
  | (ListLit(ts), List(ds)) when List.length(ts) == List.length(ds) =>
    List.combine(ts, ds)
  | (Ap(_, {term: Constructor(_), _}, arg), Ctor(_, Some(d))) =>
    parts(arg, d)
  | (TupLabel(_, v), Tuple([f])) => parts(v, f.value)
  | (Tuple(ts), Tuple(fs)) when List.length(ts) == List.length(fs) =>
    switch (
      List.fold_left(
        (best, (t, f: Data.field)) =>
          switch (best) {
          | Some((_, bd)) when Data.count(bd) >= Data.count(f.value) => best
          | _ => Some((field_term(t), f.value))
          },
        None,
        List.combine(ts, fs),
      )
    ) {
    | Some((t, d)) => parts(t, d)
    | None => []
    }
  | _ => []
  };
};

/* each part's bytes as it adds them: the Marshal size of the first k parts,
   less that of the first k - 1, so the parts add up to the whole, and what
   the parts share (the types every constructor carries) is counted once, on
   the first part that uses it */
let marginal = (f: list('a) => int, xs: list('a)): list(int) => {
  let prev = ref(f([]));
  let acc = ref([]);
  List.iteri(
    (i, _) => {
      let s = f(Util.ListUtil.take(i + 1, xs));
      acc := [s - prev^, ...acc^];
      prev := s;
    },
    xs,
  );
  List.rev(acc^);
};

let tree = (term: Language.Exp.t, data: Data.t): Yojson.Safe.t => {
  let ps = parts(term, data);
  let ts = List.map(fst, ps);
  let ds = List.map(snd, ps);
  let t = marginal(xs => mbytes(xs), ts);
  let s = marginal(xs => mbytes(List.map(shared_tags, xs)), ts);
  let d = marginal(xs => mbytes(xs), ds);
  let a = marginal(xs => mbytes(List.concat_map(atoms, xs)), ds);
  `Assoc([
    ("whole", split(term, data)),
    ("method", `String("marginal")),
    (
      "parts",
      `List(
        List.mapi(
          (i, (_, dd)) =>
            `Assoc([
              (
                "name",
                `String(
                  Option.value(name_of(dd), ~default=string_of_int(i)),
                ),
              ),
              (
                "split",
                `Assoc([
                  ("term", `Int(List.nth(t, i))),
                  ("shared_tags", `Int(List.nth(s, i))),
                  ("data", `Int(List.nth(d, i))),
                  ("atoms", `Int(List.nth(a, i))),
                ]),
              ),
            ]),
          ps,
        ),
      ),
    ),
  ]);
};

/* the last few large messages from the worker, as they came */
let recent: ref(list(WorkerServer.Active.response)) = ref([]);

/* the samples of the last result among them */
let samples = (): list(Language.Sample.t) =>
  switch (
    List.find_map(
      raw =>
        switch (WorkerServer.Active.decode_response(raw)) {
        | WorkerServer.ServerMessage.Result(r) => Some(r)
        | _ => None
        },
      recent^,
    )
  ) {
  | None => []
  | Some(r) =>
    List.concat_map(
      ((_, v): (string, WorkerServer.Response.value)) =>
        switch (v) {
        | Ok((_, st)) =>
          Util.Id.Map.fold(
            (_, ss, acc) => ss @ acc,
            st.Language.EvaluatorState.probes,
            [],
          )
        | Error(_) => []
        },
      r.response,
    )
  };

/* (^report(47)) experiments 3, 4 and 5 on one value: what an id costs to
   mint here; printing it as a term (as results are shown) and as data;
   carrying it into JS, from the term as hazel-jit does and from data */
let more = (term: Language.Exp.t, data: Data.t): Yojson.Safe.t => {
  let num = f => `Float(Float.round(f *. 10000.) /. 10000.);
  let ts = term_to_string(term);
  let ds = data_to_string(data);
  `Assoc([
    (
      "id_ns",
      num(
        per_op(() =>
          for (_ in 1 to 1000) {
            ignore(Util.Id.mk());
          }
        )
        *. 1000.,
      ),
    ),
    ("print_term_chars", `Int(String.length(ts))),
    ("print_data_chars", `Int(String.length(ds))),
    ("print_term_ms", num(per_op(() => term_to_string(term)))),
    ("print_data_ms", num(per_op(() => data_to_string(data)))),
    (
      "into_js_term_ms",
      switch (Language.HazelJitValue.to_js(term)) {
      | _ => num(per_op(() => Language.HazelJitValue.to_js(term)))
      | exception exn => `String(Printexc.to_string(exn))
      },
    ),
    ("into_js_data_ms", num(per_op(() => Data.to_js(data)))),
  ]);
};

let run = (): Yojson.Safe.t => {
  let ss = samples();
  let converted =
    List.filter_map(
      (s: Language.Sample.t) =>
        Option.map(d => (s.value, d), Data.of_term(s.value)),
      ss,
    );
  /* the picture: the converted sample with the most nodes */
  let picture =
    List.fold_left(
      (best, (t, d)) =>
        switch (best) {
        | Some((_, bd)) when Data.count(bd) >= Data.count(d) => best
        | _ => Some((t, d))
        },
      None,
      converted,
    );
  `Assoc([
    ("samples", `Int(List.length(ss))),
    ("converted", `Int(List.length(converted))),
    (
      "picture",
      switch (picture) {
      | Some((t, d)) => measure(~convert=() => Data.of_term(t), t, d)
      | None => `Null
      },
    ),
    (
      "more",
      switch (picture) {
      | Some((t, d)) => more(t, d)
      | None => `Null
      },
    ),
    (
      "picture_tree",
      switch (picture) {
      | Some((t, d)) => tree(t, d)
      | None => `Null
      },
    ),
    (
      "all_converted",
      converted == []
        ? `Null
        : measure(
            ~convert=
              () => List.map(((t, _)) => Data.of_term(t), converted),
            List.map(fst, converted),
            Data.List(List.map(snd, converted)),
          ),
    ),
  ]);
};

let registered = ref(false);
let register = () =>
  if (! registered^) {
    registered := true;
    Js.Unsafe.set(
      Js.Unsafe.global,
      "hazelDataExperiment",
      Js.wrap_callback(() =>
        Js.string(
          Yojson.Safe.to_string(
            switch (run()) {
            | j => j
            | exception exn =>
              `Assoc([("error", `String(Printexc.to_string(exn)))])
            },
          ),
        )
      ),
    );
  };

/* a message from the worker, as it came: the large ones kept while the
   page keeps its timeline */
let keep = (raw: WorkerServer.Active.response): unit =>
  if (Util.BenchMarks.on()) {
    register();
    if (Core.Byte_units.bytes_float(WorkerServer.Active.size_response(raw))
        >= 100_000.) {
      recent := Util.ListUtil.take(4, [raw, ...recent^]);
    };
  };

/* (^report(47), experiment 3) the page's own id count, for the check to read
   before and after an edit */
let () =
  Js.Unsafe.set(
    Js.Unsafe.global,
    "hazelIdCount",
    Js.wrap_callback(() => WorkerServer.id_count()),
  );
