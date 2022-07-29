module Full: {
  [@deriving sexp]
  type t;

  let mk: (Path.t, Module.t) => t;

  /**
    [path fmodl] is the path of the module.
   */
  let path: t => Path.t;

  /**
    [modl fmodl] is the body of the module.
   */
  let modl: t => Module.t;

  /**
    [import fmodl ~name ~from] is the import at [from] for [fmodl] using the name
    [name].
   */
  let import: (t, ~name: Ident.t, ~from: Path.t) => Import.t;
};

module Stub: {
  [@deriving sexp]
  type t;

  let mk: Path.t => t;

  /**
    [path fmodl] is the path of the module.
   */
  let path: t => Path.t;

  /**
    [import fmodl ~name ~from] is the import at [from] for [fmodl] using the name
    [name].
   */
  let import: (t, ~name: Ident.t, ~from: Path.t) => Import.t;
};

[@deriving sexp]
type full = Full.t;

[@deriving sexp]
type stub = Stub.t;

/**
  The type for a module within a file.
 */
[@deriving sexp]
type t;

let of_full: Full.t => t;
let of_stub: Stub.t => t;

let to_full: t => option(Full.t);
let to_stub: t => option(Stub.t);

/**
  [mk path modl] is a module with the given path and body.
 */
let mk: (Path.t, Module.t) => t;

/**
  [mk' path] is a stub module with the given path.
 */
let mk': Path.t => t;

/**
  [path fmodl] is the path of the module.
 */
let path: t => Path.t;

/**
  [modl fmodl] is the body of the module; [None] if it is a stub.
 */
let modl: t => option(Module.t);

/**
  [import fmodl ~name ~from] is the import at [from] for [fmodl] using the name
  [name].
 */
let import: (t, ~name: Ident.t, ~from: Path.t) => Import.t;
