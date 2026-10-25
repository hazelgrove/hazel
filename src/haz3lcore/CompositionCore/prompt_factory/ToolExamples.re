open Util;

/* The edit and view tools' examples, as data: the system prompt and the
   tool descriptions print them, and Test_AgentReply runs each one on its
   program and checks its results as written */

type call = {
  tool: string,
  args: list((string, API.Json.t)),
};

type example = {
  /* what it shows */
  intent: string,
  /* the program it runs on, Exp-rooted */
  program: string,
  /* one reply's calls */
  calls: list(call),
  /* what each call's result says, as the tool reports it (its start);
     None for the view tools, whose results say only that they ran */
  results: option(list(string)),
  /* its calls also go in their tools' descriptions */
  in_tool: bool,
};

let call = (tool, args) => {
  tool,
  args: List.map(((k, v)) => (k, `String(v)), args),
};

/* a call as the prompt shows it: its other arguments inline, then its
   code in a fence, line breaks and quotes as written: shown as a JSON
   string, its escapes get copied into calls */
let call_text = (c: call): string => {
  let (code, rest) =
    List.partition(
      ((k, v)) =>
        switch (v) {
        | `String(_) => k == "code"
        | _ => false
        },
      c.args,
    );
  let inline =
    c.tool
    ++ " {"
    ++ String.concat(
         ", ",
         List.map(((k, v)) => k ++ ": " ++ Yojson.Safe.to_string(v), rest),
       )
    ++ "}";
  switch (code) {
  | [(_, `String(code))] => inline ++ ", code:\n```\n" ++ code ++ "\n```"
  | _ => inline
  };
};

let shapes = {|type Shape = + Circle(Int) + Rect(Int, Int) in
let area(s: Shape): Int =
  case s
  | Circle(r) => 3 * r * r
  | Rect(w, h) => w * h
  end
in
hint "rect area" test area(Rect(2, 3)) == 6 end;
let perimeter(s: Shape): Int =
  let double(n: Int): Int = 2 * n in
  case s
  | Circle(r) => double(3 * r)
  | Rect(w, h) => double(w + h)
  end
in
module Geo = {
  let scale(k: Int, s: Shape): Shape =
    case s
    | Circle(r) => Circle(k * r)
    | Rect(w, h) => Rect(k * w, k * h)
    end;
  let unit = Rect(1, 1)
} in
let total(xs: [Shape]): Int =
  case xs
  | [] => 0
  | x :: rest => area(x) + total(rest)
  end
in
hint "total of two" test total([Circle(1), Geo.unit]) == 4 end;
total([Geo.scale(2, Geo.unit)])|};

let on_shapes = (~in_tool=false, intent, calls, results) => {
  intent,
  program: shapes,
  calls,
  results,
  in_tool,
};

let all: list(example) = [
  on_shapes(
    ~in_tool=true,
    "Add a definition after another, its doc comment inside it, after its header",
    [
      call(
        "insert",
        [
          ("after", "area"),
          (
            "code",
            "let is_round(s: Shape): Bool =\n  # whether s is a circle #\n  case s\n  | Circle(_) => true\n  | Rect(_, _) => false\n  end",
          ),
        ],
      ),
    ],
    Some(["Added is_round."]),
  ),
  on_shapes(
    "Add a unit test right after its function's last test; a short hint names it",
    [
      call(
        "insert",
        [
          ("after", "\"rect area\""),
          ("code", "hint \"circle area\" test area(Circle(1)) == 3 end"),
        ],
      ),
    ],
    Some(["Added \"circle area\"."]),
  ),
  on_shapes(
    "Add an integration test at the end, above the result",
    [
      call(
        "insert",
        [
          ("last_in", "/"),
          (
            "code",
            "hint \"scaled total\" test total([Geo.scale(2, Geo.unit)]) == 4 end",
          ),
        ],
      ),
    ],
    Some(["Added \"scaled total\"."]),
  ),
  on_shapes(
    "Add a module member, first in the module",
    [
      call(
        "insert",
        [("first_in", "Geo"), ("code", "let origin = Circle(0)")],
      ),
    ],
    Some(["Added Geo/origin."]),
  ),
  on_shapes(
    ~in_tool=true,
    "Replace a function's body: for `let f(x) = …`, its definition",
    [
      call(
        "replace",
        [
          ("target", "area:definition"),
          (
            "code",
            "case s\n| Circle(r) => 314 * r * r / 100\n| Rect(w, h) => w * h\nend",
          ),
        ],
      ),
    ],
    Some(["Replaced area:definition.\n- | Circle(r) => 3 * r * r"]),
  ),
  on_shapes(
    ~in_tool=true,
    "Replace a function's result, past its local definitions, which stay",
    [
      call(
        "replace",
        [
          ("target", "perimeter/=>"),
          (
            "code",
            "case s\n| Circle(r) => double(314 * r / 100)\n| Rect(w, h) => double(w + h)\nend",
          ),
        ],
      ),
    ],
    Some(["Replaced perimeter/=>.\n- | Circle(r) => double(3 * r)"]),
  ),
  on_shapes(
    "Replace a definition inside a definition",
    [
      call(
        "replace",
        [("target", "perimeter/double:definition"), ("code", "n + n")],
      ),
    ],
    Some(["Replaced perimeter/double:definition."]),
  ),
  on_shapes(
    "Replace a whole entry, here to annotate a member",
    [
      call(
        "replace",
        [("target", "Geo/unit"), ("code", "let unit: Shape = Rect(1, 1)")],
      ),
    ],
    Some(["Replaced Geo/unit."]),
  ),
  on_shapes(
    "Fix a test addressed by its hint",
    [
      call(
        "replace",
        [
          ("target", "\"total of two\""),
          (
            "code",
            "hint \"total of two\" test total([Circle(1), Rect(1, 2)]) == 5 end",
          ),
        ],
      ),
    ],
    Some(["Replaced \"total of two\"."]),
  ),
  on_shapes(
    "Replace the program's result, `=>`",
    [
      call(
        "replace",
        [("target", "=>"), ("code", "total([Circle(1), Geo.unit])")],
      ),
    ],
    Some(["Replaced =>."]),
  ),
  {
    intent: "Pick one of several same-named entries with #k, in program order",
    program: "let n = 1 in\nlet n = n + 1 in\nn",
    calls: [
      call("replace", [("target", "n#2:definition"), ("code", "n * 10")]),
    ],
    results: Some(["Replaced n#2:definition."]),
    in_tool: false,
  },
  on_shapes(
    ~in_tool=true,
    "Delete a test by its hint",
    [call("delete", [("target", "\"rect area\"")])],
    Some(["Deleted \"rect area\"."]),
  ),
  on_shapes(
    "A delete that would leave uses unbound is refused, naming them",
    [call("delete", [("target", "area")])],
    Some([
      "Not deleted: it would leave uses dangling. Delete or change them first, in the same reply:\n- area, used at \"rect area\", total",
    ]),
  ),
  on_shapes(
    ~in_tool=true,
    "Move a definition into a module",
    [call("move", [("target", "perimeter"), ("last_in", "Geo")])],
    Some(["Moved perimeter last in Geo (now Geo/perimeter)."]),
  ),
  on_shapes(
    ~in_tool=true,
    "Rename a module member used elsewhere: its uses follow",
    [call("rename", [("target", "Geo/scale"), ("to", "scaled")])],
    Some(["Renamed Geo/scale to Geo/scaled, and its 1 use."]),
  ),
  on_shapes(
    ~in_tool=true,
    "Rename a function's parameter, with name: its uses follow",
    [
      call("rename", [("target", "area"), ("name", "s"), ("to", "shape")]),
    ],
    Some(["Renamed s to shape in area, and its 1 use."]),
  ),
  on_shapes(
    ~in_tool=true,
    "Open entries to read them in full",
    [
      {
        tool: "open",
        args: [
          ("targets", `List([`String("total"), `String("Geo/scale")])),
        ],
      },
    ],
    None,
  ),
  on_shapes(
    ~in_tool=true,
    "Close every open entry",
    [
      {
        tool: "close",
        args: [("all", `Bool(true))],
      },
    ],
    None,
  ),
];

let for_tool = (tool: string): list(call) =>
  List.concat_map(
    e => e.in_tool ? List.filter(c => c.tool == tool, e.calls) : [],
    all,
  );

/* the examples, for the system prompt: the program, then each with its
   calls and results */
let prompt_text: string = {
  let example = (e: example) =>
    "- "
    ++ e.intent
    ++ ":\n"
    ++ (
      e.program == shapes
        ? ""
        : "on `"
          ++ String.concat(" ", String.split_on_char('\n', e.program))
          ++ "`\n"
    )
    ++ String.concat(
         "",
         List.mapi(
           (i, c) =>
             call_text(c)
             ++ "\n"
             ++ (
               switch (e.results) {
               | Some(rs) =>
                 "→ "
                 ++ String.concat(
                      "\n  ",
                      String.split_on_char('\n', List.nth(rs, i)),
                    )
                 ++ "\n"
               | None => ""
               }
             ),
           e.calls,
         ),
       );
  "On this program:\n```\n"
  ++ shapes
  ++ "\n```\n"
  ++ String.concat("", List.map(example, all));
};
