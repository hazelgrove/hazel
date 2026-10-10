/* Remembering a pure function's answers by the IDENTITY of its argument,
   in a JS WeakMap: for functions of immutable values that are asked again
   about the very same object, as a redraw asks about the same cached value
   as the redraw before. An entry lives only as long as its key. Arguments
   that are not objects (numbers, strings) are not remembered: the function
   just runs. */
open Js_of_ocaml;

let impl: Lazy.t(Js.Unsafe.any) =
  lazy(
    Js.Unsafe.js_expr(
      {js|(function (f) {
  const memo = new WeakMap();
  return x => {
    if (typeof x !== "object" || x === null) return f(x);
    const hit = memo.get(x);
    if (hit !== undefined) return hit[0];
    const r = f(x);
    memo.set(x, [r]);
    return r;
  };
})|js},
    )
  );

/* f, answering again from memory for an argument it has seen. f must be
   pure, and its argument immutable. */
let memo = (f: 'a => 'b): ('a => 'b) => {
  let g: Js.Unsafe.any =
    Js.Unsafe.fun_call(
      Lazy.force(impl),
      [|Js.Unsafe.inject(Js.wrap_callback(f))|],
    );
  x => Js.Unsafe.fun_call(g, [|Js.Unsafe.inject(x)|]);
};
