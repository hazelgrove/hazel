/* FIXME: Rename to GStd or GrainStd to avoid shadowing actual stdlib. */

module type I = {
  let name: Ident.t;
  let from: Path.t;
};

module type USE_META = {let imp: Import.t;};
module type META = {let fmodl: FileModule.stub;};

/**
  "int32" module.
 */
module Int32: {
  include META;

  module Use:
    (I) =>
     {
      include USE_META;

      let add: (Expr.t, Expr.t) => Expr.t;
      let sub: (Expr.t, Expr.t) => Expr.t;
      let mul: (Expr.t, Expr.t) => Expr.t;
      let div: (Expr.t, Expr.t) => Expr.t;
      let lt: (Expr.t, Expr.t) => Expr.t;
      let gt: (Expr.t, Expr.t) => Expr.t;
      let lte: (Expr.t, Expr.t) => Expr.t;
      let gte: (Expr.t, Expr.t) => Expr.t;
      let incr: Expr.t => Expr.t;
      let decr: Expr.t => Expr.t;
      let div_u: (Expr.t, Expr.t) => Expr.t;
      let eq: (Expr.t, Expr.t) => Expr.t;
    };
};

/**
  "int64" module.
 */
module Int64: {
  include META;

  module Use:
    (I) =>
     {
      include USE_META;

      let add: (Expr.t, Expr.t) => Expr.t;
      let sub: (Expr.t, Expr.t) => Expr.t;
      let mul: (Expr.t, Expr.t) => Expr.t;
      let div: (Expr.t, Expr.t) => Expr.t;
      let lt: (Expr.t, Expr.t) => Expr.t;
      let gt: (Expr.t, Expr.t) => Expr.t;
      let lte: (Expr.t, Expr.t) => Expr.t;
      let gte: (Expr.t, Expr.t) => Expr.t;
      let incr: Expr.t => Expr.t;
      let decr: Expr.t => Expr.t;
      let div_u: (Expr.t, Expr.t) => Expr.t;
      let eq: (Expr.t, Expr.t) => Expr.t;
    };
};

/**
  "float32" module.
 */
module Float32: {
  include META;

  module Use:
    (I) =>
     {
      include USE_META;

      let add: (Expr.t, Expr.t) => Expr.t;
      let sub: (Expr.t, Expr.t) => Expr.t;
      let mul: (Expr.t, Expr.t) => Expr.t;
      let div: (Expr.t, Expr.t) => Expr.t;
      let lt: (Expr.t, Expr.t) => Expr.t;
      let gt: (Expr.t, Expr.t) => Expr.t;
      let lte: (Expr.t, Expr.t) => Expr.t;
      let gte: (Expr.t, Expr.t) => Expr.t;
      let eq: (Expr.t, Expr.t) => Expr.t;
    };
};

/**
  "float64" module.
 */
module Float64: {
  include META;

  module Use:
    (I) =>
     {
      include USE_META;

      let add: (Expr.t, Expr.t) => Expr.t;
      let sub: (Expr.t, Expr.t) => Expr.t;
      let mul: (Expr.t, Expr.t) => Expr.t;
      let div: (Expr.t, Expr.t) => Expr.t;
      let lt: (Expr.t, Expr.t) => Expr.t;
      let gt: (Expr.t, Expr.t) => Expr.t;
      let lte: (Expr.t, Expr.t) => Expr.t;
      let gte: (Expr.t, Expr.t) => Expr.t;
      let eq: (Expr.t, Expr.t) => Expr.t;
    };
};

/**
  "map" module.
 */
module Map: {
  include META;

  module Use:
    (I) =>
     {
      include USE_META;

      let from_list: Expr.t => Expr.t;
    };
};

let print: Expr.t => Expr.t;

let and_: (Expr.t, Expr.t) => Expr.t;
let or_: (Expr.t, Expr.t) => Expr.t;

let eq: (Expr.t, Expr.t) => Expr.t;
let neq: (Expr.t, Expr.t) => Expr.t;
