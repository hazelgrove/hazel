/* A table keyed by the IDENTITY of an object, in a JS WeakMap: an entry
   lives only as long as its key. For remembering things about immutable
   values that are met again as the very same object. Keys must be
   objects (boxed OCaml values), not ints or other immediates. */
open Js_of_ocaml;

type t('k, 'v) = Js.Unsafe.any;

let create = (): t('k, 'v) =>
  Js.Unsafe.new_obj(Js.Unsafe.global##._WeakMap, [||]);

let find_opt = (table: t('k, 'v), key: 'k): option('v) => {
  let r: Js.Optdef.t('v) =
    Js.Unsafe.meth_call(table, "get", [|Js.Unsafe.inject(key)|]);
  Js.Optdef.to_option(r);
};

let replace = (table: t('k, 'v), key: 'k, value: 'v): unit =>
  Js.Unsafe.meth_call(
    table,
    "set",
    [|Js.Unsafe.inject(key), Js.Unsafe.inject(value)|],
  )
  |> ignore;
