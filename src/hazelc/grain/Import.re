[@deriving sexp]
type t = {
  name: Ident.t,
  path: ImportPath.t,
};

let mk = (name, path) => {name, path};

let name = ({name, _}) => name;
let path = ({path, _}) => path;

module H = {
  let ident = (imp, x) => Ident.join(name(imp), x);
  let var = (imp, x) => x |> ident(imp) |> Expr.var;
  let pvar = (imp, x) => x |> ident(imp) |> Pat.var;

  let ap = (imp, fn, xs) => Expr.ap(var(imp, fn), xs);
  let ap1 = (imp, fn, x) => ap(imp, fn, [x]);
  let ap2 = (imp, fn, x1, x2) => ap(imp, fn, [x1, x2]);
  let ap3 = (imp, fn, x1, x2, x3) => ap(imp, fn, [x1, x2, x3]);
  let ap4 = (imp, fn, x1, x2, x3, x4) => ap(imp, fn, [x1, x2, x3, x4]);
  let ap5 = (imp, fn, x1, x2, x3, x4, x5) =>
    ap(imp, fn, [x1, x2, x3, x4, x5]);

  let ctor = (imp, name, xs) => Expr.ctor(ident(imp, name), xs);
  let ctor0 = (imp, name) => var(imp, name);
  let ctor1 = (imp, name, x) => ctor(imp, name, [x]);
  let ctor2 = (imp, name, x1, x2) => ctor(imp, name, [x1, x2]);
  let ctor3 = (imp, name, x1, x2, x3) => ctor(imp, name, [x1, x2, x3]);
  let ctor4 = (imp, name, x1, x2, x3, x4) =>
    ctor(imp, name, [x1, x2, x3, x4]);
  let ctor5 = (imp, name, x1, x2, x3, x4, x5) =>
    ctor(imp, name, [x1, x2, x3, x4, x5]);

  let pctor = (imp, name, xs) => Pat.ctor(ident(imp, name), xs);
  let pctor0 = (imp, name) => pvar(imp, name);
  let pctor1 = (imp, name, x) => pctor(imp, name, [x]);
  let pctor2 = (imp, name, x1, x2) => pctor(imp, name, [x1, x2]);
  let pctor3 = (imp, name, x1, x2, x3) => pctor(imp, name, [x1, x2, x3]);
  let pctor4 = (imp, name, x1, x2, x3, x4) =>
    pctor(imp, name, [x1, x2, x3, x4]);
  let pctor5 = (imp, name, x1, x2, x3, x4, x5) =>
    pctor(imp, name, [x1, x2, x3, x4, x5]);
};
