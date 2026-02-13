/**
 * Performance logging for Patchwork sync operations
 *
 * Usage:
 *   let log = PerfLog.start("operation_name");
 *   // ... do work ...
 *   PerfLog.end(log);
 *
 * Logs will appear in console with format:
 *   [PERF] operation_name: 123.45ms
 */
open Js_of_ocaml;

type t = {
  name: string,
  start_time: float,
  context: option(string),
};

let enabled = ref(false);

let enable = () => enabled := true;
let disable = () => enabled := false;

let log = (message: string): unit =>
  if (enabled^) {
    Firebug.console##log(Js.string("[PERF] " ++ message));
  };

let now = (): float => {
  // Use performance.now() for high-resolution timing
  Js.Unsafe.fun_call(Js.Unsafe.js_expr("performance.now"), [||])
  |> Js.float_of_number;
};

let start = (~context: option(string)=None, name: string): t => {
  {
    name,
    start_time: now(),
    context,
  };
};

let end_ = (log: t): unit =>
  if (enabled^) {
    let elapsed = now() -. log.start_time;
    let context_str =
      switch (log.context) {
      | Some(c) => " (" ++ c ++ ")"
      | None => ""
      };
    let elapsed_str =
      Js.number_of_float(elapsed)##toFixed(2) |> Js.to_string;
    let message =
      "[PERF] " ++ log.name ++ context_str ++ ": " ++ elapsed_str ++ "ms";
    Firebug.console##log(Js.string(message));

    // Also log as structured data for easier parsing
    let data = [|
      ("type", Js.Unsafe.inject(Js.string("perf"))),
      ("name", Js.Unsafe.inject(Js.string(log.name))),
      ("elapsed_ms", Js.Unsafe.inject(Js.number_of_float(elapsed))),
      ("timestamp", Js.Unsafe.inject(Js.number_of_float(now()))),
    |];
    switch (log.context) {
    | Some(c) =>
      let data_with_context =
        Array.append(
          data,
          [|("context", Js.Unsafe.inject(Js.string(c)))|],
        );
      Firebug.console##log(Js.Unsafe.obj(data_with_context));
    | None => Firebug.console##log(Js.Unsafe.obj(data))
    };
  };

let measure = (name: string, f: unit => 'a): 'a => {
  let log = start(name);
  let result = f();
  end_(log);
  result;
};

let measure_with_context = (name: string, context: string, f: unit => 'a): 'a => {
  let log = start(~context=Some(context), name);
  let result = f();
  end_(log);
  result;
};

// Count nodes in various data structures for context
module Count = {
  let pieces_in_segment = (seg: Base.segment): int => {
    List.length(seg);
  };

  let rec pieces_in_segment_deep = (seg: Base.segment): int => {
    List.fold_left(
      (acc, piece) => {
        acc
        + 1
        + (
          switch (piece) {
          | Base.Tile({children, _}) =>
            List.fold_left(
              (acc2, child_seg) => acc2 + pieces_in_segment_deep(child_seg),
              0,
              children,
            )
          | _ => 0
          }
        )
      },
      0,
      seg,
    );
  };
};
