open Js_of_ocaml;

/* Thin bindings to the D3 modules exposed on `window.d3` (see
 * src/web/www/prebundle.js). D3 is used purely as a math library: scales and
 * tick generation return plain numbers, which ChartProj turns into
 * virtual-dom SVG. No D3 code ever touches the real DOM.
 *
 * Only `d3-scale` and `d3-array` are required (scaleLinear, scaleBand,
 * ticks). */

let inj = Js.Unsafe.inject;
let meth = Js.Unsafe.meth_call;
let d3 = () => Js.Unsafe.global##.d3;

let floats = (xs: list(float)) => inj(Js.array(Array.of_list(xs)));

/* A continuous linear scale mapping `domain` onto `range`. */
let scale_linear =
    (~domain: (float, float), ~range: (float, float)): (float => float) => {
  let (d0, d1) = domain;
  let (r0, r1) = range;
  let s = meth(d3(), "scaleLinear", [||]);
  let s = meth(s, "domain", [|floats([d0, d1])|]);
  let s = meth(s, "range", [|floats([r0, r1])|]);
  (x) => (Js.Unsafe.fun_call(s, [|inj(x)|]): float);
};

/* "Nice", human-readable tick values across [lo, hi] (d3-array's ticks). */
let ticks = (~lo: float, ~hi: float, ~count: int): list(float) => {
  let arr = meth(d3(), "ticks", [|inj(lo), inj(hi), inj(count)|]);
  Array.to_list(Js.to_array(arr));
};

/* A band scale over `count` evenly spaced categories, keyed by index. Returns
 * the left-edge position function and the band width. */
let scale_band =
    (~count: int, ~range: (float, float), ~padding: float)
    : (int => float, float) => {
  let (r0, r1) = range;
  let domain = inj(Js.array(Array.init(count, i => i)));
  let s = meth(d3(), "scaleBand", [||]);
  let s = meth(s, "domain", [|domain|]);
  let s = meth(s, "range", [|floats([r0, r1])|]);
  let s = meth(s, "padding", [|inj(padding)|]);
  let pos = (i: int): float => Js.Unsafe.fun_call(s, [|inj(i)|]);
  let bandwidth: float = meth(s, "bandwidth", [||]);
  (pos, bandwidth);
};
