open Util;

/* The edit verbs. Each takes paths (EntryPath); the system prompt teaches
   paths, parts and places, with examples (ToolExamples) */

let tool =
    (
      ~name: string,
      ~description: string,
      ~properties: list((string, API.Json.t)),
      ~required: list(string),
    )
    : API.Json.t =>
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String(name)),
        ("description", `String(String.trim(description))),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            ("properties", `Assoc(properties)),
            ("required", `List(List.map(r => `String(r), required))),
          ]),
        ),
      ]),
    ),
  ]);

let string_param = (desc: string): API.Json.t =>
  `Assoc([("type", `String("string")), ("description", `String(desc))]);

let paths_param = (desc: string): API.Json.t =>
  `Assoc([
    ("type", `String("array")),
    ("description", `String(desc)),
    ("items", `Assoc([("type", `String("string"))])),
  ]);

/* string parameters, by name and description */
let strings = List.map(((key, desc)) => (key, string_param(desc)));

let place_properties = [
  ("before", "Path of the entry it goes before."),
  ("after", "Path of the entry it goes after."),
  (
    "first_in",
    "Path of the module or definition it goes first in; \"/\" for the top level.",
  ),
  (
    "last_in",
    "Path of the module or definition it goes last in; \"/\" for the top level (above the program's result).",
  ),
];

let example = (name: string): string =>
  switch (ToolExamples.for_tool(name)) {
  | [] => ""
  | calls =>
    "\n\nExample"
    ++ (List.length(calls) > 1 ? "s" : "")
    ++ ":\n"
    ++ String.concat("\n", List.map(ToolExamples.call_text, calls))
  };

let insert: API.Json.t =
  tool(
    ~name="insert",
    ~description=
      {|
Adds entries (definitions, types, modules, tests, statements), written without a trailing `in` or `;`. Give exactly one place: `before` or `after` an entry, or `first_in` / `last_in` a module's members or a definition's body.|}
      ++ example("insert"),
    ~properties=
      strings([("code", "The entries' code."), ...place_properties]),
    ~required=["code"],
  );

let replace: API.Json.t =
  tool(
    ~name="replace",
    ~description=
      {|
Replaces the entry at `target` with `code`, or one part of it: `target:definition` (between `=` and its `in` or `;`, which you never write; for `let f(x) = …`, f's body; for `let f = fun …`, from `fun`), `target:pattern` (between `let` and `=`; to rename, use rename). `target/=>` is the result of a definition's body, past its local definitions. A whole entry is written without its trailing `in` or `;`. Refused if a use outside the new code would then refer to another binder.|}
      ++ example("replace"),
    ~properties=
      strings([
        (
          "target",
          "Path of the entry, optionally with :definition, :pattern or :head.",
        ),
        ("code", "The new code."),
      ]),
    ~required=["target", "code"],
  );

let delete: API.Json.t =
  tool(
    ~name="delete",
    ~description=
      {|
Removes the entry at `target`. Refused while its names are still used: the result lists the uses; change or delete them in the same reply.|}
      ++ example("delete"),
    ~properties=strings([("target", "Path of the entry.")]),
    ~required=["target"],
  );

let move: API.Json.t =
  tool(
    ~name="move",
    ~description=
      {|
Moves the entry at `target` to a place, as for insert, keeping its code and probes. Moving between a module and a definition's body converts `;` and `in`; uses that no longer reach it are not rewritten.|}
      ++ example("move"),
    ~properties=
      strings([
        ("target", "Path of the entry to move."),
        ...place_properties,
      ]),
    ~required=["target"],
  );

let rename: API.Json.t =
  tool(
    ~name="rename",
    ~description=
      {|
Renames the definition, type or module at `target`, and every use of it, `M.member` uses included. With `name`, renames that variable bound in the entry's pattern instead: a function's parameter, a destructured name. Refused if a use would then refer to something else.|}
      ++ example("rename"),
    ~properties=
      strings([
        (
          "target",
          "Path of the entry; for a pattern binding several names, the name's path.",
        ),
        (
          "name",
          "Optional: a variable bound in the entry's pattern, such as a parameter, to rename instead of the entry's own name.",
        ),
        ("to", "The new name."),
      ]),
    ~required=["target", "to"],
  );
