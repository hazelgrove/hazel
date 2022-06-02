[@deriving sexp]
type command;

module Opts: {
  [@deriving sexp]
  type arg('a) = 'a;
  [@deriving sexp]
  type opt('a) = option('a);
  [@deriving sexp]
  type flag = option(bool);

  [@deriving sexp]
  type t;

  let make: (~grain: string) => t;
};

module Compile: {
  /* See `grain --help`. */
  [@deriving sexp]
  type opts = {
    source: Opts.arg(string),
    output: Opts.opt(string),
    graceful: Opts.flag,
    includes: Opts.opt(list(string)),
    stdlib: Opts.opt(string),
    initial_memory_pages: Opts.opt(int),
    maximum_memory_pages: Opts.opt(int),
    compilation_mode: Opts.opt(string),
    elide_type_info: Opts.flag,
    experimental_wasm_tail_call: Opts.flag,
    debug: Opts.flag,
    wat: Opts.flag,
    hide_locs: Opts.flag,
    no_gc: Opts.flag,
    no_bulk_memory: Opts.flag,
    wasi_polyfill: Opts.opt(string),
    use_start_section: Opts.flag,
    no_link: Opts.flag,
    no_pervasives: Opts.flag,
    optimize: Opts.opt(int),
    parser_debug_level: Opts.opt(int),
    memory_base: Opts.opt(string),
    source_map: Opts.flag,
    strict_sequence: Opts.flag,
    verbose: Opts.flag,
  };

  [@deriving sexp]
  type t;

  let make: (~source: Opts.arg(string), Opts.t) => t;

  let with_source: (string, t) => t;
  let with_output: (string, t) => t;
  let with_graceful: (bool, t) => t;
  let with_includes: (list(string), t) => t;
  let with_stdlib: (string, t) => t;
  let with_initial_memory_pages: (int, t) => t;
  let with_maximum_memory_pages: (int, t) => t;
  let with_compilation_mode: (string, t) => t;
  let with_elide_type_info: (bool, t) => t;
  let with_experimental_wasm_tail_call: (bool, t) => t;
  let with_debug: (bool, t) => t;
  let with_wat: (bool, t) => t;
  let with_hide_locs: (bool, t) => t;
  let with_no_gc: (bool, t) => t;
  let with_no_bulk_memory: (bool, t) => t;
  let with_wasi_polyfill: (string, t) => t;
  let with_use_start_section: (bool, t) => t;
  let with_no_link: (bool, t) => t;
  let with_no_pervasives: (bool, t) => t;
  let with_optimize: (int, t) => t;
  let with_parser_debug_level: (int, t) => t;
  let with_memory_base: (string, t) => t;
  let with_source_map: (bool, t) => t;
  let with_strict_sequence: (bool, t) => t;
  let with_verbose: (bool, t) => t;

  let to_command: t => command;
};

module Run: {
  [@deriving sexp]
  type t;

  let make: (~modl: Opts.arg(string), Opts.t) => t;

  let to_command: t => command;
};

module Format: {
  [@deriving sexp]
  type t;

  let make: (~source: Opts.arg(string), Opts.t) => t;

  let with_in_place: (bool, t) => t;

  let to_command: t => command;
};

[@deriving sexp]
type opts = Opts.t;

let make: (~grain: string) => opts;
let string_of_command: command => string;

type outcome = {
  stdout: string,
  status: result(unit, int),
};

let execute: (~capture_stdout: bool, command) => outcome;
