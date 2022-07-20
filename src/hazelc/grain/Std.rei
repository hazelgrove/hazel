open Expr;

/**
  "int32" module.
 */
module Int32: {
  include Lib.META;

  let add: (expr, expr) => expr;
  let sub: (expr, expr) => expr;
  let mul: (expr, expr) => expr;
  let div: (expr, expr) => expr;
  let lt: (expr, expr) => expr;
  let gt: (expr, expr) => expr;
  let lte: (expr, expr) => expr;
  let gte: (expr, expr) => expr;
  let incr: expr => expr;
  let decr: expr => expr;
  let div_u: (expr, expr) => expr;
  let eq: (expr, expr) => expr;
};

/**
  "int64" module.
 */
module Int64: {
  include Lib.META;

  let add: (expr, expr) => expr;
  let sub: (expr, expr) => expr;
  let mul: (expr, expr) => expr;
  let div: (expr, expr) => expr;
  let lt: (expr, expr) => expr;
  let gt: (expr, expr) => expr;
  let lte: (expr, expr) => expr;
  let gte: (expr, expr) => expr;
  let incr: expr => expr;
  let decr: expr => expr;
  let div_u: (expr, expr) => expr;
  let eq: (expr, expr) => expr;
};

/**
  "float32" module.
 */
module Float32: {
  include Lib.META;

  let add: (expr, expr) => expr;
  let sub: (expr, expr) => expr;
  let mul: (expr, expr) => expr;
  let div: (expr, expr) => expr;
  let lt: (expr, expr) => expr;
  let gt: (expr, expr) => expr;
  let lte: (expr, expr) => expr;
  let gte: (expr, expr) => expr;
  let eq: (expr, expr) => expr;
};

/**
  "float64" module.
 */
module Float64: {
  include Lib.META;

  let add: (expr, expr) => expr;
  let sub: (expr, expr) => expr;
  let mul: (expr, expr) => expr;
  let div: (expr, expr) => expr;
  let lt: (expr, expr) => expr;
  let gt: (expr, expr) => expr;
  let lte: (expr, expr) => expr;
  let gte: (expr, expr) => expr;
  let eq: (expr, expr) => expr;
};

/**
  "map" module.
 */
module Map: {
  include Lib.META;

  /**
    [fromList].
   */
  let from_list: expr => expr;
};
