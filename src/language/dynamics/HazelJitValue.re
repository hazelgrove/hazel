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

open Js_of_ocaml;
module G = IdTagged.FreshGrammar;

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

let whole = (f: float): Bigint.t =>
  Bigint.of_string(Printf.sprintf("%.0f", f));

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
        |> List.filter(k => int_of_string_opt(k) == None);
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

/* The view of a model: the model into JS, the view called, its answer back */
let run_view = (v: view, model: Exp.t): result(Exp.t, string) =>
  switch (of_js(v.types, None, Js.Unsafe.fun_call(v.fn, [|to_js(model)|]))) {
  | e => Ok(e)
  | exception (HazelJit.Unsupported(why)) => Error(why)
  | exception exn => Error(message(exn))
  };
