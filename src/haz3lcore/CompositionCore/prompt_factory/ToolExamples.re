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
  /* what each call's result says, as the tool reports it; None for the
     view tools, whose results say only that they ran */
  results: option(list(string)),
  /* its calls also go in their tools' descriptions */
  in_tool: bool,
};

let call = (tool, args) => {
  tool,
  args: List.map(((k, v)) => (k, `String(v)), args),
};

let call_line = (c: call): string =>
  c.tool ++ " " ++ Yojson.Safe.to_string(`Assoc(c.args));

let shapes = {|type Shape = + Circle(Int) + Rect(Int, Int) in
let area(s: Shape): Int =
  case s
  | Circle(r) => 3 * r * r
  | Rect(w, h) => w * h
  end
in
test area(Rect(2, 3)) == 6 end;
let perimeter(s: Shape): Int =
  case s
  | Circle(r) => 6 * r
  | Rect(w, h) => 2 * (w + h)
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
    "Add a unit test right after its function's last test, here test#1",
    [
      call(
        "insert",
        [("after", "test#1"), ("code", "test area(Circle(1)) == 3 end")],
      ),
    ],
    Some(["Added test#2."]),
  ),
  on_shapes(
    "Add an integration test at the end, above the result; a hint names it",
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
    "Replace a function's body, past its parameters",
    [
      call(
        "replace",
        [
          ("target", "area:body"),
          (
            "code",
            "case s\n| Circle(r) => 314 * r * r / 100\n| Rect(w, h) => w * h\nend",
          ),
        ],
      ),
    ],
    Some(["Replaced area:body."]),
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
    ~in_tool=true,
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
    "Delete an unhinted test by test#k",
    [call("delete", [("target", "test#1")])],
    Some(["Deleted test#1."]),
  ),
  on_shapes(
    "A delete that would leave uses unbound is refused, naming them",
    [call("delete", [("target", "area")])],
    Some([
      "Not deleted: it would leave uses dangling. Delete or change them first, in the same reply:\n- area, used at test#1, total",
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
        : "  on `"
          ++ String.concat(" ", String.split_on_char('\n', e.program))
          ++ "`\n"
    )
    ++ String.concat(
         "",
         List.mapi(
           (i, c) =>
             "  "
             ++ call_line(c)
             ++ "\n"
             ++ (
               switch (e.results) {
               | Some(rs) =>
                 "  → "
                 ++ String.concat(
                      "\n    ",
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
