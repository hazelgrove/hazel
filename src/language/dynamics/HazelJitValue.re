/* hazel-jit's boundary (^ticket(694), slice 3): a Hazel value carried into the JS that compiled code works on, a JS value
   carried back as a Hazel value, and a livelit's view run that way.

   Into JS (to_js): numbers, booleans and strings as JS's; a list as cons cells ({h, t}, null for []); a tuple as an array,
   each labeled component also a property by its label; a constructor as {c} or {c, v}. A value of another form (a
   function in a model, say) is not carried: Unsupported, and the caller runs the interpreter as before.

   Back from JS (of_js): the same, the other way; and a JS number, which does not say whether it was an Int or a Float,
   is read by the type expected where it sits. The types come from the compiled term (HazelJit.compiled): each
   constructor's type as elaboration gave it, so a constructor's argument is read at its argument's type, and the term's
   type aliases, so a type that names another is seen through. Where no type is known (Unknown, a type variable), a whole
   number is an Int and any other a Float. A JS function in the answer (an event handler that takes a payload, say, or a
   ViewCmd's continuation) comes back as a Hazel closure when the compiled code made it: each compiled function carries
   the index of its Hazel function and the values it captured ($R.fn), and comes back as that function closed over those
   values. A recursive one, or one hazel-jit did not make, is Unsupported, and the view runs on the interpreter. */

/* The setting (Settings: Compiled views): on, a livelit's view runs as JS compiled from its Hazel */
let enabled = ref(true);

/* (^ticket(716)) What ran how, counted in the page (globalThis.hazelJit: counts by what, and the reasons a view or an
   update went to the interpreter or through Hazel), for a check in a browser to read; nothing else reads it */
let note: Lazy.t(Js_of_ocaml.Js.Unsafe.any) =
  lazy(
    Js_of_ocaml.Js.Unsafe.eval_string(
      {|(function (what, why) {
  var s = globalThis.hazelJit || (globalThis.hazelJit = { counts: {}, why: {} });
  s.counts[what] = (s.counts[what] || 0) + 1;
  if (why) s.why[what + ": " + why] = (s.why[what + ": " + why] || 0) + 1;
})|},
    )
  );
let count = (~why="", what: string): unit =>
  Js_of_ocaml.Js.Unsafe.fun_call(
    Lazy.force(note),
    [|
      Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(what)),
      Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(why)),
    |],
  );

open Js_of_ocaml;

/* The ids the way back gives the terms it makes: FreshGrammar's, each its own. (^ticket(694), 2026-11-07, for measuring
   only) While shared_ids is set, every term it makes gets one shared, invalid id instead, as CallMemo's keys do, to see
   what making ids costs it. */
let shared_ids = ref(false);
let shared_tag = IdTagged.IdTag.mk_internal([Id.invalid]);
module G =
  Grammar.Factory({
    type t = IdTagged.IdTag.t;
    let default_value = () =>
      shared_ids^ ? shared_tag : IdTagged.fresh_annotation();
  });

let unsupported = HazelJit.unsupported;

let inject = Js.Unsafe.inject;
let number = (f: float): Js.Unsafe.any => inject(Js.number_of_float(f));
let jstr = (s: string): Js.Unsafe.any => inject(Js.string(s));
let typeof = (v: Js.Unsafe.any): string =>
  Js.to_string(Js.typeof(Js.Unsafe.coerce(v)));
let is_null = (v: Js.Unsafe.any): bool =>
  Js.Unsafe.equals(v, Js.null) || Js.Unsafe.equals(v, Js.undefined);
let is_array = (v: Js.Unsafe.any): bool =>
  Js.to_bool(
    Js.Unsafe.meth_call(
      Js.Unsafe.get(Js.Unsafe.global, "Array"),
      "isArray",
      [|v|],
    ),
  );
let has = (v: Js.Unsafe.any, k: string): bool =>
  Js.to_bool(
    Js.Unsafe.meth_call(
      Js.Unsafe.get(Js.Unsafe.global, "Object"),
      "hasOwn",
      [|v, jstr(k)|],
    ),
  );
let get = (v: Js.Unsafe.any, k: string): Js.Unsafe.any =>
  Js.Unsafe.get(v, k);

/* ---- into JS */

let cons = (h: Js.Unsafe.any, t: Js.Unsafe.any): Js.Unsafe.any =>
  Js.Unsafe.obj([|("h", h), ("t", t)|]);

let rec to_js = (v: Exp.t): Js.Unsafe.any =>
  switch (Exp.term_of(v)) {
  | Parens(v)
  | Asc(v, _) => to_js(v)
  | Atom(Int(n))
  | Atom(Nat(n)) => number(Bigint.to_float(n))
  | Atom(SInt(n)) => number(float_of_int(n))
  | Atom(Float(f)) => number(f)
  /* (^ticket(716)) a negative number as the syntax has it, a minus before it: a model read from the program's text, before
     its run has a value (current = -1, found by the browser check on the Polygons slide) */
  | UnOp(Int(Minus) | Float(Minus), e) =>
    let v = to_js(e);
    typeof(v) == "number"
      ? number(-. Js.float_of_number(Js.Unsafe.coerce(v)))
      : unsupported(
          "a value hazel-jit does not carry into JS: a minus before a non-number",
        );
  | Atom(Bool(b)) => inject(Js.bool(b))
  | Atom(String(s)) => jstr(s)
  | ListLit(vs) =>
    List.fold_right((v, t) => cons(to_js(v), t), vs, inject(Js.null))
  | Cons(h, t) => cons(to_js(h), to_js(t))
  | Tuple(vs) => tuple(vs)
  | TupLabel(_) => tuple([v])
  | Constructor(name, _) => Js.Unsafe.obj([|("c", jstr(name))|])
  | Ap(Forward, {term: Constructor(name, _), _}, arg) =>
    Js.Unsafe.obj([|("c", jstr(name)), ("v", to_js(arg))|])
  | _ =>
    let s = Exp.show(v);
    unsupported(
      "a value hazel-jit does not carry into JS: "
      ++ (String.length(s) > 80 ? String.sub(s, 0, 80) ++ "..." : s),
    );
  }
and tuple = (vs: list(Exp.t)): Js.Unsafe.any => {
  let parts =
    List.map(
      (v: Exp.t) =>
        switch (Exp.match_tup_label(v)) {
        | Some((l, v)) => (Some(l), to_js(v))
        | None => (None, to_js(v))
        },
      vs,
    );
  let arr = Js.array(Array.of_list(List.map(snd, parts)));
  List.iter(
    ((l, js)) =>
      switch (l) {
      | Some(l) => Js.Unsafe.set(arr, Js.string(l), js)
      | None => ()
      },
    parts,
  );
  inject(arr);
};

/* ---- back from JS */

type types = {
  ctors: Hashtbl.t(string, option(Typ.t)),
  aliases: Hashtbl.t(string, Typ.t),
  funs: Hashtbl.t(int, (Exp.t, list(string))),
};

let types_of = (c: HazelJit.compiled): types => {
  ctors: c.ctors,
  aliases: c.aliases,
  funs: c.funs,
};

/* A type seen through parens, aliases (a few deep) and rec; None when it says nothing about a value's shape */
let rec shape = (ts: types, depth: int, t: Typ.t): option(Typ.t) =>
  switch (t.term) {
  | Parens(t)
  | Rec(_, t) => shape(ts, depth, t)
  | Var(x) when depth < 8 =>
    switch (Hashtbl.find_opt(ts.aliases, x)) {
    | Some(t) => shape(ts, depth + 1, t)
    | None => None
    }
  | Var(_)
  | Unknown(_) => None
  | _ => Some(t)
  };

/* (2026-11-07) a whole number that fits an int (31 bits under js_of_ocaml) directly; only a bigger one by its digits,
   through Printf and Bigint.of_string, which cost every number about 2 us */
let whole = (f: float): Bigint.t =>
  Float.abs(f) < 1073741824.0
    ? Bigint.of_int(int_of_float(f))
    : Bigint.of_string(Printf.sprintf("%.0f", f));

/* ~seen: the functions being carried back, outermost first: one met again inside itself is a recursive function (or
   one that captures itself through another), which a Hazel closure cannot hold as a plain value */
let rec of_js =
        (~seen=[], ts: types, ty: option(Typ.t), v: Js.Unsafe.any): Exp.t => {
  let ty = Option.bind(ty, shape(ts, 0));
  switch (typeof(v)) {
  | "number" =>
    let f = Js.float_of_number(Js.Unsafe.coerce(v));
    switch (Option.map((t: Typ.t) => t.term, ty)) {
    | Some(Atom(Float)) => G.Exp.float(f)
    | Some(Atom(SInt)) => G.Exp.sint(int_of_float(f))
    | Some(Atom(Nat)) => G.Exp.nat(whole(f))
    | Some(Atom(Int)) => G.Exp.big_int(whole(f))
    | _ => Float.is_integer(f) ? G.Exp.big_int(whole(f)) : G.Exp.float(f)
    };
  | "string" => G.Exp.string(Js.to_string(Js.Unsafe.coerce(v)))
  | "boolean" => G.Exp.bool(Js.to_bool(Js.Unsafe.coerce(v)))
  | "function" when has(v, "$k") =>
    /* a function the compiled code made: its Hazel function, closed over the values it captured, each carried back
       (with no type to go by) */
    if (List.exists(w => Js.Unsafe.equals(w, v), seen)) {
      unsupported("a recursive function in the answer");
    };
    let k =
      int_of_float(Js.float_of_number(Js.Unsafe.coerce(get(v, "$k"))));
    switch (Hashtbl.find_opt(ts.funs, k)) {
    | None => unsupported("a function the compiled code did not make")
    | Some((term, names)) =>
      let caps = get(v, "$e");
      let values =
        is_null(caps)
          ? []
          : List.mapi(
              (i, x) =>
                (
                  x,
                  of_js(
                    ~seen=[v, ...seen],
                    ts,
                    None,
                    Js.Unsafe.get(caps, i),
                  ),
                ),
              names,
            );
      G.Exp.closure(Environment.of_bindings(values), term);
    };
  | "function" =>
    unsupported(
      "a function in the answer that hazel-jit did not make (a builtin, say)",
    )
  | "object" when is_null(v) => G.Exp.list_lit([])
  | "object" when is_array(v) => of_tuple(~seen, ts, ty, v)
  | "object" when has(v, "c") =>
    let name = Js.to_string(Js.Unsafe.coerce(get(v, "c")));
    let t = Option.join(Hashtbl.find_opt(ts.ctors, name));
    let ann = Option.map(t => Some(t), t);
    if (has(v, "v")) {
      let arg_ty =
        switch (Option.bind(t, shape(ts, 0))) {
        | Some({term: Arrow(a, _), _}) => Some(a)
        | _ => None
        };
      G.Exp.ap(
        Forward,
        G.Exp.constructor(name, ann),
        of_js(~seen, ts, arg_ty, get(v, "v")),
      );
    } else {
      G.Exp.constructor(name, ann);
    };
  | "object" when has(v, "h") =>
    let elt =
      switch (Option.map((t: Typ.t) => t.term, ty)) {
      | Some(List(t)) => Some(t)
      | _ => None
      };
    let rec walk = (v, acc) =>
      is_null(v)
        ? List.rev(acc)
        : walk(get(v, "t"), [of_js(~seen, ts, elt, get(v, "h")), ...acc]);
    G.Exp.list_lit(walk(v, []));
  | t => unsupported("a JS " ++ t ++ " in the answer")
  };
}
/* A JS array as a tuple: by the expected product type, its labels and each component's type; with none, the labels the
   array carries (a property holding the very value at an index is that index's label) */
and of_tuple = (~seen, ts: types, ty: option(Typ.t), v: Js.Unsafe.any): Exp.t => {
  let n =
    int_of_float(Js.float_of_number(Js.Unsafe.coerce(get(v, "length"))));
  let at = i => Js.Unsafe.get(v, i);
  let fields: list((option(string), option(Typ.t))) =
    switch (Option.map((t: Typ.t) => t.term, ty)) {
    | Some(Prod(tys)) when List.length(tys) == n =>
      List.map(
        (t: Typ.t) =>
          switch (Typ.match_tup_label(t)) {
          | Some((l, t)) => (Some(l), Some(t))
          | None => (None, Some(t))
          },
        tys,
      )
    | _ =>
      let keys =
        Js.to_array(
          Js.Unsafe.meth_call(
            Js.Unsafe.get(Js.Unsafe.global, "Object"),
            "keys",
            [|v|],
          ),
        )
        |> Array.to_list
        |> List.map(k => Js.to_string(k))
        /* (2026-11-07) a label is a key that is not an index; int_of_string_opt said so by raising and catching an
           exception for every label, about 4 us each under js_of_ocaml */
        |> List.filter(k =>
             k == "" || !String.for_all(c => c >= '0' && c <= '9', k)
           );
      let used = ref([]);
      List.init(n, i =>
        switch (
          List.find_opt(
            k => !List.mem(k, used^) && Js.Unsafe.equals(get(v, k), at(i)),
            keys,
          )
        ) {
        | Some(k) =>
          used := [k, ...used^];
          (Some(k), None);
        | None => (None, None)
        }
      );
    };
  let comps =
    List.mapi(
      (i, (l, t)) => {
        let e = of_js(~seen, ts, t, at(i));
        switch (l) {
        | Some(l) => G.Exp.tup_label(G.Exp.label(l), e)
        | None => e
        };
      },
      fields,
    );
  G.Exp.tuple(comps);
};

/* ---- back as a diff (^ticket(709), a sub-experiment of ^ticket(694); Matthew, 2026-11-07: "the answer back (of_js) as a
   diff based on the last time we ran it")

   Carrying a whole answer back costs time in its size. When the last run's answer is at hand, in JS and as the Hazel
   value it was carried back as, only what changed needs carrying: diff, in JS, compares the two answers, and patch
   applies what it says to the last Hazel value, keeping every part that did not change as it was (the same OCaml
   values, shared). What diff compares first is identity: the compiled code builds a new answer functionally, so a part
   it did not touch (a list's tail after the element it replaced, a field it did not set) is the same JS object, and
   diff stops there without looking inside.

   The diff language, as JS values:
     null             the same (nothing to carry)
     {n: v}           new: v, carried back whole (of_js), at the type expected there
     {c: d}           the same constructor, its argument changed by d
     {t: [[i, d]..]}  a tuple of the same length, component i changed by d (the others the same)
     {l: [op..]}      a list, edited in order from its head; after the last op, the rest is the old list's own tail:
                        {k: n} keep the next n elements, {e: d} change the next by d, {i: v} insert v here,
                        {x: 1} drop the next, {a: list} the old list ended: append list's elements, new,
                        {cut: 1} the new list ends here
   A list is walked from its head, so a change at position k costs time in k, not in the list's length; and keeping
   runs costs nothing per element on the Hazel side but walking the OCaml list. */
let diff_js: Lazy.t(Js.Unsafe.any) =
  lazy(
    Js.Unsafe.eval_string(
      {|(function diff(a, b) {
  if (a === b) return null;
  if (a === null || b === null || typeof a !== "object" || typeof b !== "object") return {n: b};
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || a.length !== b.length) return {n: b};
    var ch = [];
    for (var i = 0; i < a.length; i++) { var d = diff(a[i], b[i]); if (d !== null) ch.push([i, d]); }
    return ch.length ? {t: ch} : null;
  }
  if (Object.hasOwn(a, "c")) {
    if (!Object.hasOwn(b, "c") || a.c !== b.c || Object.hasOwn(a, "v") !== Object.hasOwn(b, "v")) return {n: b};
    if (!Object.hasOwn(a, "v")) return null;
    var dc = diff(a.v, b.v);
    return dc === null ? null : {c: dc};
  }
  if (Object.hasOwn(a, "h")) {
    if (!Object.hasOwn(b, "h")) return {n: b};
    var ops = [], keep = 0;
    for (;;) {
      if (a === b) break;
      if (a === null) { if (keep) ops.push({k: keep}); keep = 0; ops.push({a: b}); break; }
      if (b === null) { if (keep) ops.push({k: keep}); keep = 0; ops.push({cut: 1}); break; }
      if (b.t === a) { if (keep) ops.push({k: keep}); keep = 0; ops.push({i: b.h}); b = b.t; continue; }
      if (a.t === b) { if (keep) ops.push({k: keep}); keep = 0; ops.push({x: 1}); a = a.t; continue; }
      var de = diff(a.h, b.h);
      if (de === null) keep++;
      else { if (keep) ops.push({k: keep}); keep = 0; ops.push({e: de}); }
      a = a.t; b = b.t;
    }
    return ops.length ? {l: ops} : null;
  }
  return {n: b};
})|},
    )
  );
let diff = (a: Js.Unsafe.any, b: Js.Unsafe.any): Js.Unsafe.any =>
  Js.Unsafe.fun_call(Lazy.force(diff_js), [|a, b|]);

let js_list = (v: Js.Unsafe.any): list(Js.Unsafe.any) => {
  let n =
    int_of_float(Js.float_of_number(Js.Unsafe.coerce(get(v, "length"))));
  List.init(n, i => Js.Unsafe.get(v, i));
};
let int_of = (v: Js.Unsafe.any): int =>
  int_of_float(Js.float_of_number(Js.Unsafe.coerce(v)));

/* A diff applied to the last answer's Hazel value. A diff that does not fit the value (it was not made from this
   value's answer) is Unsupported. */
let rec patch =
        (ts: types, ty: option(Typ.t), old: Exp.t, d: Js.Unsafe.any): Exp.t =>
  if (is_null(d)) {
    old;
  } else {
    let ty = Option.bind(ty, shape(ts, 0));
    if (has(d, "n")) {
      let v = get(d, "n");
      /* (^ticket(715)) a number with no type to read it by, where a number was: read as that one was, so a whole Float
         stays a Float */
      switch (ty, typeof(v), Exp.term_of(old)) {
      | (None, "number", Atom(Float(_))) =>
        G.Exp.float(Js.float_of_number(Js.Unsafe.coerce(v)))
      | (None, "number", Atom(SInt(_))) =>
        G.Exp.sint(int_of_float(Js.float_of_number(Js.Unsafe.coerce(v))))
      | _ => of_js(ts, ty, v)
      };
    } else {
      switch (Exp.term_of(old)) {
      | Parens(e)
      | Asc(e, _) => patch(ts, ty, e, d)
      | Ap(Forward, {term: Constructor(name, _), _} as c, arg)
          when has(d, "c") =>
        let arg_ty =
          switch (
            Option.bind(
              Option.join(Hashtbl.find_opt(ts.ctors, name)),
              shape(ts, 0),
            )
          ) {
          | Some({term: Arrow(a, _), _}) => Some(a)
          | _ => None
          };
        G.Exp.ap(Forward, c, patch(ts, arg_ty, arg, get(d, "c")));
      | Tuple(items) when has(d, "t") =>
        let changes =
          js_list(get(d, "t"))
          |> List.map(p =>
               (int_of(Js.Unsafe.get(p, 0)), Js.Unsafe.get(p, 1))
             );
        let tys =
          switch (Option.map((t: Typ.t) => t.term, ty)) {
          | Some(Prod(tys)) when List.length(tys) == List.length(items) =>
            List.map(
              (t: Typ.t) =>
                switch (Typ.match_tup_label(t)) {
                | Some((_, t)) => Some(t)
                | None => Some(t)
                },
              tys,
            )
          | _ => List.map(_ => None, items)
          };
        G.Exp.tuple(
          List.mapi(
            (i, (e: Exp.t, t)) =>
              switch (List.assoc_opt(i, changes)) {
              | None => e
              | Some(di) =>
                switch (Exp.term_of(e)) {
                | TupLabel(l, inner) =>
                  G.Exp.tup_label(l, patch(ts, t, inner, di))
                | _ => patch(ts, t, e, di)
                }
              },
            List.combine(items, tys),
          ),
        );
      | ListLit(xs) when has(d, "l") =>
        let elt =
          switch (Option.map((t: Typ.t) => t.term, ty)) {
          | Some(List(t)) => Some(t)
          | _ => None
          };
        let rec walk_js = (v, acc) =>
          is_null(v)
            ? acc
            : walk_js(get(v, "t"), [of_js(ts, elt, get(v, "h")), ...acc]);
        /* acc: the new list's elements so far, reversed; xs: what is left of the old list */
        let rec go = (ops, xs, acc) =>
          switch (ops) {
          | [] => List.rev_append(acc, xs)
          | [op, ...ops] =>
            if (has(op, "k")) {
              let rec keep = (n, xs, acc) =>
                n == 0
                  ? (xs, acc)
                  : (
                    switch (xs) {
                    | [x, ...xs] => keep(n - 1, xs, [x, ...acc])
                    | [] => unsupported("patch: kept past the list's end")
                    }
                  );
              let (xs, acc) = keep(int_of(get(op, "k")), xs, acc);
              go(ops, xs, acc);
            } else if (has(op, "e")) {
              switch (xs) {
              | [x, ...xs] =>
                go(ops, xs, [patch(ts, elt, x, get(op, "e")), ...acc])
              | [] => unsupported("patch: changed past the list's end")
              };
            } else if (has(op, "i")) {
              go(ops, xs, [of_js(ts, elt, get(op, "i")), ...acc]);
            } else if (has(op, "x")) {
              switch (xs) {
              | [_, ...xs] => go(ops, xs, acc)
              | [] => unsupported("patch: dropped past the list's end")
              };
            } else if (has(op, "a")) {
              List.rev(walk_js(get(op, "a"), acc));
            } else if (has(op, "cut")) {
              List.rev(acc);
            } else {
              unsupported("patch: an edit it does not know");
            }
          };
        G.Exp.list_lit(go(js_list(get(d, "l")), xs, []));
      | _ => unsupported("patch: the diff does not fit the value")
      };
    };
  };

/* ---- running compiled code */

/* The runtime ($R), made once */
let runtime: Lazy.t(Js.Unsafe.any) =
  lazy(Js.Unsafe.eval_string("(" ++ HazelJit.Runtime.source ++ ")"));

/* A compiled term's value: its source run as new Function("$R", src)(runtime) */
let instantiate = (src: string): Js.Unsafe.any =>
  Js.Unsafe.fun_call(
    Js.Unsafe.pure_js_expr(
      "(function (src, R) { return new Function('$R', src)(R); })",
    ),
    [|jstr(src), Lazy.force(runtime)|],
  );

/* What a JS exception says */
let message = (exn: exn): string =>
  switch (Js_error.of_exn(exn)) {
  | Some(e) => Js_error.message(e)
  | None => Printexc.to_string(exn)
  };

/* A livelit's view, compiled: the definition (its elaborated module) compiled once and run, its view member kept, with
   the types the way back reads by */
type view = {
  fn: Js.Unsafe.any,
  /* the definition's module, compiled: its other members (update, ^ticket(715)) */
  members: Js.Unsafe.any,
  types,
  compile_ms: float,
};

let now = (): float =>
  Js.float_of_number(
    Js.Unsafe.meth_call(
      Js.Unsafe.get(Js.Unsafe.global, "performance"),
      "now",
      [||],
    ),
  );

let compile_view =
    (~env: Environment.t(Exp.t), def: Exp.t): result(view, string) => {
  let t0 = now();
  switch (HazelJit.compile_full(~env, def)) {
  | exception (HazelJit.Unsupported(why)) => Error(why)
  | c =>
    switch (instantiate(c.src)) {
    | exception exn =>
      Error("the compiled definition did not run: " ++ message(exn))
    | m =>
      let fn = get(m, "view");
      if (typeof(fn) != "function") {
        Error("the definition has no view member");
      } else if (has(fn, "$missing")) {
        Error(
          "its view did not compile: "
          ++ Js.to_string(Js.Unsafe.coerce(get(fn, "$missing"))),
        );
      } else {
        Ok({
          fn,
          members: m,
          types: types_of(c),
          compile_ms: now() -. t0,
        });
      };
    }
  };
};

/* Each definition compiled once (eval_view's): the last few definitions, by their term, each with its compiled view or
   why it has none. A view whose answer once failed to come back is not tried again. */
let cache: ref(list((Exp.t, result(view, string)))) = ref([]);
let same_def = (a: Exp.t, b: Exp.t): bool => a === b || Exp.fast_equal(a, b);
let view_of_def =
    (~env: Environment.t(Exp.t), def: Exp.t): result(view, string) =>
  switch (List.find_opt(((d, _)) => same_def(d, def), cache^)) {
  | Some((_, r)) => r
  | None =>
    let r = compile_view(~env, def);
    cache := [(def, r), ...List.filteri((i, _) => i < 15, cache^)];
    r;
  };
let give_up = (def: Exp.t, why: string): unit =>
  cache :=
    List.map(
      ((d, r)) => same_def(d, def) ? (d, Error(why)) : (d, r),
      cache^,
    );

/* (^ticket(714)) The two halves of run_view, apart: the compiled view called on the model, its answer left in JS (to draw
   straight from: HazelJitDOM); and that answer carried back as a Hazel value, when something asks for one */
let call_view = (v: view, model: Exp.t): result(Js.Unsafe.any, string) =>
  switch (Js.Unsafe.fun_call(v.fn, [|to_js(model)|])) {
  | js => Ok(js)
  | exception (HazelJit.Unsupported(why)) => Error(why)
  | exception exn => Error(message(exn))
  };
let answer_of = (v: view, js: Js.Unsafe.any): result(Exp.t, string) =>
  switch (of_js(v.types, None, js)) {
  | e => Ok(e)
  | exception (HazelJit.Unsupported(why)) => Error(why)
  | exception exn => Error(message(exn))
  };

/* (^ticket(715), ^report(45)'s next step 2) The definition's update, compiled, on a model and an action: when it answers
   Pure(new model), the new model carried back as a diff against the model it was given (patch: every part it did not
   change is that model's own, the same Hazel value), read by the definition's Model type when it names one. Any other
   command, or a diff that does not fit, is Error: the caller runs the update on the interpreter, as before. */
let call_update =
    (v: view, model: Exp.t, action: Exp.t): result(Exp.t, string) => {
  let update = get(v.members, "update");
  if (typeof(update) != "function") {
    Error("the definition has no update member");
  } else if (has(update, "$missing")) {
    Error(
      "its update did not compile: "
      ++ Js.to_string(Js.Unsafe.coerce(get(update, "$missing"))),
    );
  } else {
    switch (
      {
        let before = to_js(model);
        let cmd =
          Js.Unsafe.fun_call(
            Js.Unsafe.fun_call(update, [|before|]),
            [|to_js(action)|],
          );
        if (typeof(cmd) == "object"
            && !is_null(cmd)
            && has(cmd, "c")
            && Js.to_string(Js.Unsafe.coerce(get(cmd, "c"))) == "Pure"
            && has(cmd, "v")) {
          let ty =
            Option.map(
              (t: Typ.t) => t,
              Hashtbl.find_opt(v.types.aliases, "Model"),
            );
          patch(v.types, ty, model, diff(before, get(cmd, "v")));
        } else {
          unsupported("an update command other than Pure");
        };
      }
    ) {
    | m => Ok(m)
    | exception (HazelJit.Unsupported(why)) => Error(why)
    | exception exn => Error(message(exn))
    };
  };
};

/* The view of a model: the model into JS, the view called, its answer back */
let run_view = (v: view, model: Exp.t): result(Exp.t, string) =>
  switch (of_js(v.types, None, Js.Unsafe.fun_call(v.fn, [|to_js(model)|]))) {
  | e => Ok(e)
  | exception (HazelJit.Unsupported(why)) => Error(why)
  | exception exn => Error(message(exn))
  };
