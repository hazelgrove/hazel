open Virtual_dom.Vdom.Attr;

let fstr = f => Printf.sprintf("%f", f);

let cx = f => create("cx", fstr(f));
let cy = f => create("cy", fstr(f));
let rx = f => create("rx", fstr(f));
let ry = f => create("ry", fstr(f));

let x = f => create("x", fstr(f));
let y = f => create("y", fstr(f));
let width = f => create("width", fstr(f));
let height = f => create("height", fstr(f));

let stroke_width = f => create("stroke-width", fstr(f));
let vector_effect = s => create("vector-effect", s);
let filter = s => create("filter", s);

let offset = f => create("offset", Printf.sprintf("%f%%", 100. *. f));
let stop_color = s => create("stop-color", s);
let stop_opacity = f => create("stop-opacity", Printf.sprintf("%f", f));
