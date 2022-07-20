open Module;

[@deriving sexp]
type t = {
  path: import_path,
  prog,
};

/**
  [mk path prog] is a module with the given path and body.
 */
let mk: (import_path, prog) => t;

/**
  [mk_lib path] is an existing module with the given path.
 */
let mk_lib: import_path => t;

/**
  [path fmd] is the path of the module.
 */
let path: t => import_path;

/**
  [prog fmd] is the body of the module.
 */
let prog: t => prog;

/**
  [to_module fmd] is the module given by [fmd].
 */
let to_module: t => prog;
