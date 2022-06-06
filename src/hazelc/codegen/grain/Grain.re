open Sexplib.Std;

[@deriving sexp]
type command = string;

module Opts = {
  [@deriving sexp]
  type arg('a) = 'a;
  [@deriving sexp]
  type opt('a) = option('a);
  [@deriving sexp]
  type flag = option(bool);

  [@deriving sexp]
  type t = {grain: string};

  let make = (~grain: string): t => {grain: grain};

  let identity = cmd => cmd;

  let use_grain = opts => opts.grain;
  let use_subcmd = (opts, subcmd) =>
    Printf.sprintf("%s %s", use_grain(opts), subcmd);

  let use_arg = (arg, cmd) => Printf.sprintf("%s %s", cmd, arg);
  let use_opt = (flag, arg, cmd) => cmd |> use_arg(flag) |> use_arg(arg);
  let use_flag = (flag, cmd) => cmd |> use_arg(flag);
  let use_flag' = (flag, toggle) =>
    switch (toggle) {
    | Some(true) => use_flag(flag)
    | _ => identity
    };

  let fold_uses = (uses, cmd) =>
    List.fold_left((cmd, use) => use(cmd), cmd, uses);
};

module Compile = {
  [@deriving sexp]
  type opts = {
    source: Opts.arg(string),
    output: Opts.opt(string),
    includes: Opts.opt(list(string)),
    stdlib: Opts.opt(string),
    initial_memory_pages: Opts.opt(int),
    maximum_memory_pages: Opts.opt(int),
    compilation_mode: Opts.opt(string),
    release: Opts.flag,
    elide_type_info: Opts.flag,
    experimental_wasm_tail_call: Opts.flag,
    debug: Opts.flag,
    wat: Opts.flag,
    hide_locs: Opts.flag,
    no_color: Opts.flag,
    no_gc: Opts.flag,
    no_bulk_memory: Opts.flag,
    wasi_polyfill: Opts.opt(string),
    use_start_section: Opts.flag,
    no_link: Opts.flag,
    no_pervasives: Opts.flag,
    parser_debug_level: Opts.opt(int),
    memory_base: Opts.opt(string),
    source_map: Opts.flag,
    strict_sequence: Opts.flag,
    verbose: Opts.flag,
  };

  [@deriving sexp]
  type t = (Opts.t, opts);

  let make = (~source, opts): t => (
    opts,
    {
      source,
      output: None,
      includes: None,
      stdlib: None,
      initial_memory_pages: None,
      maximum_memory_pages: None,
      compilation_mode: None,
      release: None,
      elide_type_info: None,
      experimental_wasm_tail_call: None,
      debug: None,
      wat: None,
      hide_locs: None,
      no_color: None,
      no_gc: None,
      no_bulk_memory: None,
      wasi_polyfill: None,
      use_start_section: None,
      no_link: None,
      no_pervasives: None,
      parser_debug_level: None,
      memory_base: None,
      source_map: None,
      strict_sequence: None,
      verbose: None,
    },
  );

  let with' = (f: opts => opts, (opts, copts): t): t => (opts, f(copts));

  let with_source = source => with'(copts => {...copts, source});
  let with_output = output =>
    with'(copts => {...copts, output: Some(output)});
  let with_includes = includes =>
    with'(copts => {...copts, includes: Some(includes)});
  let with_stdlib = stdlib =>
    with'(copts => {...copts, stdlib: Some(stdlib)});
  let with_initial_memory_pages = initial_memory_pages =>
    with'(copts =>
      {...copts, initial_memory_pages: Some(initial_memory_pages)}
    );
  let with_maximum_memory_pages = maximum_memory_pages =>
    with'(copts =>
      {...copts, maximum_memory_pages: Some(maximum_memory_pages)}
    );
  let with_compilation_mode = compilation_mode =>
    with'(copts => {...copts, compilation_mode: Some(compilation_mode)});
  let with_release = release =>
    with'(copts => {...copts, release: Some(release)});
  let with_elide_type_info = elide_type_info =>
    with'(copts => {...copts, elide_type_info: Some(elide_type_info)});
  let with_experimental_wasm_tail_call = experimental_wasm_tail_call =>
    with'(copts =>
      {
        ...copts,
        experimental_wasm_tail_call: Some(experimental_wasm_tail_call),
      }
    );
  let with_debug = debug => with'(copts => {...copts, debug: Some(debug)});
  let with_wat = wat => with'(copts => {...copts, wat: Some(wat)});
  let with_hide_locs = hide_locs =>
    with'(copts => {...copts, hide_locs: Some(hide_locs)});
  let with_no_color = no_color =>
    with'(copts => {...copts, no_color: Some(no_color)});
  let with_no_gc = no_gc => with'(copts => {...copts, no_gc: Some(no_gc)});
  let with_no_bulk_memory = no_bulk_memory =>
    with'(copts => {...copts, no_bulk_memory: Some(no_bulk_memory)});
  let with_wasi_polyfill = wasi_polyfill =>
    with'(copts => {...copts, wasi_polyfill: Some(wasi_polyfill)});
  let with_use_start_section = use_start_section =>
    with'(copts => {...copts, use_start_section: Some(use_start_section)});
  let with_no_link = no_link =>
    with'(copts => {...copts, no_link: Some(no_link)});
  let with_no_pervasives = no_pervasives =>
    with'(copts => {...copts, no_pervasives: Some(no_pervasives)});
  let with_parser_debug_level = parser_debug_level =>
    with'(copts => {...copts, parser_debug_level: Some(parser_debug_level)});
  let with_memory_base = memory_base =>
    with'(copts => {...copts, memory_base: Some(memory_base)});
  let with_source_map = source_map =>
    with'(copts => {...copts, source_map: Some(source_map)});
  let with_strict_sequence = strict_sequence =>
    with'(copts => {...copts, strict_sequence: Some(strict_sequence)});
  let with_verbose = verbose =>
    with'(copts => {...copts, verbose: Some(verbose)});

  let to_command = ((opts, copts): t): command => {
    let use_source = copts => Opts.use_arg(copts.source);
    let use_output = copts =>
      switch (copts.output) {
      | Some(output) => Opts.use_opt("-o", output)
      | None => Opts.identity
      };

    let use_includes = copts =>
      switch (copts.includes) {
      | Some(includes) =>
        Opts.fold_uses(List.map(Opts.use_opt("-I"), includes))
      | None => Opts.identity
      };
    let use_stdlib = copts =>
      switch (copts.stdlib) {
      | Some(output) => Opts.use_opt("--stdlib", output)
      | None => Opts.identity
      };
    let use_initial_memory_pages = copts =>
      switch (copts.initial_memory_pages) {
      | Some(pages) =>
        Opts.use_opt("--initial-memory-pages", string_of_int(pages))
      | None => Opts.identity
      };
    let use_maximum_memory_pages = copts =>
      switch (copts.maximum_memory_pages) {
      | Some(pages) =>
        Opts.use_opt("--maximum-memory-pages", string_of_int(pages))
      | None => Opts.identity
      };
    let use_compilation_mode = copts =>
      switch (copts.compilation_mode) {
      | Some(mode) => Opts.use_opt("--compilation-mode", mode)
      | None => Opts.identity
      };
    let use_release = copts => Opts.use_flag'("--release", copts.release);
    let use_elide_type_info = copts =>
      Opts.use_flag'("--elide-type-info", copts.elide_type_info);
    let use_experimental_wasm_tail_call = copts =>
      Opts.use_flag'(
        "--experimental-wasm-tail-call",
        copts.experimental_wasm_tail_call,
      );
    let use_debug = copts => Opts.use_flag'("--debug", copts.debug);
    let use_wat = copts => Opts.use_flag'("--wat", copts.wat);
    let use_hide_locs = copts =>
      Opts.use_flag'("--hide-locs", copts.hide_locs);
    let use_no_color = copts => Opts.use_flag'("--no_color", copts.no_color);
    let use_no_gc = copts => Opts.use_flag'("--no-gc", copts.no_gc);
    let use_no_bulk_memory = copts =>
      Opts.use_flag'("--no-bulk-memory", copts.no_bulk_memory);
    let use_wasi_polyfill = copts =>
      switch (copts.wasi_polyfill) {
      | Some(path) => Opts.use_opt("--wasi-polyfill", path)
      | _ => Opts.identity
      };
    let use_use_start_section = copts =>
      Opts.use_flag'("--use-start-section", copts.use_start_section);
    let use_no_link = copts => Opts.use_flag'("--no-link", copts.no_link);
    let use_no_pervasives = copts =>
      Opts.use_flag'("--no-pervasives", copts.no_pervasives);
    let use_parser_debug_level = copts =>
      switch (copts.parser_debug_level) {
      | Some(level) =>
        Opts.use_opt("--parser-debug-level", string_of_int(level))
      | None => Opts.identity
      };
    let use_memory_base = copts =>
      switch (copts.memory_base) {
      | Some(addr) => Opts.use_opt("--memory-base", addr)
      | None => Opts.identity
      };
    let use_source_map = copts =>
      Opts.use_flag'("--source-map", copts.source_map);
    let use_strict_sequence = copts =>
      Opts.use_flag'("--strict-sequence", copts.strict_sequence);
    let use_verbose = copts => Opts.use_flag'("--verbose", copts.verbose);

    let uses = [
      use_source,
      use_output,
      use_includes,
      use_stdlib,
      use_initial_memory_pages,
      use_maximum_memory_pages,
      use_compilation_mode,
      use_release,
      use_elide_type_info,
      use_experimental_wasm_tail_call,
      use_debug,
      use_wat,
      use_hide_locs,
      use_no_color,
      use_no_gc,
      use_no_bulk_memory,
      use_wasi_polyfill,
      use_use_start_section,
      use_no_link,
      use_no_pervasives,
      use_parser_debug_level,
      use_memory_base,
      use_source_map,
      use_strict_sequence,
      use_verbose,
    ];

    Opts.use_subcmd(opts, "compile")
    |> Opts.fold_uses(List.map(use => use(copts), uses));
  };
};

module Run = {
  [@deriving sexp]
  type opts = {modl: Opts.arg(string)};

  [@deriving sexp]
  type t = (Opts.t, opts);

  let make = (~modl, opts): t => (opts, {modl: modl});

  let to_command = ((opts, ropts): t) => {
    let use_source = ropts => Opts.use_arg(ropts.modl);
    let uses = [use_source];

    Opts.use_subcmd(opts, "run")
    |> Opts.fold_uses(List.map(use => use(ropts), uses));
  };
};

module Format = {
  [@deriving sexp]
  type opts = {
    source: Opts.arg(string),
    in_place: Opts.flag,
  };

  [@deriving sexp]
  type t = (Opts.t, opts);

  let make = (~source, opts): t => (opts, {source, in_place: None});

  let with' = (f: opts => opts, (opts, copts): t): t => (opts, f(copts));
  let with_in_place = in_place =>
    with'(fopts => {...fopts, in_place: Some(in_place)});

  let to_command = ((opts, fopts): t) => {
    let use_source = ropts => Opts.use_arg(ropts.source);
    let use_in_place = ropts =>
      switch (ropts.in_place) {
      | Some(true) => Opts.use_flag("--in-place")
      | _ => Opts.identity
      };

    let uses = [use_source, use_in_place];

    Opts.use_subcmd(opts, "format")
    |> Opts.fold_uses(List.map(use => use(fopts), uses));
  };
};

[@deriving sexp]
type opts = Opts.t;
let make = Opts.make;

let string_of_command = (cmd: command) => cmd;

type outcome = {
  stdout: string,
  status: result(unit, int),
};

let execute = (~capture_stdout, cmd: command): outcome => {
  let input_all = t => {
    let chunk_size = 65536;
    let buffer = Buffer.create(chunk_size);
    let rec loop = () => {
      Caml.Buffer.add_channel(buffer, t, chunk_size);
      loop();
    };
    try(loop()) {
    | End_of_file => Buffer.contents(buffer)
    };
  };

  if (capture_stdout) {
    let stdout = Unix.open_process_in(cmd);
    let stdout_out = input_all(stdout);
    let status =
      switch (Unix.close_process_in(stdout)) {
      | WEXITED(0) => Ok()
      | WEXITED(c)
      | WSIGNALED(c)
      | WSTOPPED(c) => Error(c)
      };

    {stdout: stdout_out, status};
  } else {
    let status =
      switch (Sys.command(cmd)) {
      | 0 => Ok()
      | c => Error(c)
      };
    {stdout: "", status};
  };
};
