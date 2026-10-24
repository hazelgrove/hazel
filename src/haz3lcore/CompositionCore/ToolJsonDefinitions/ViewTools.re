open Util;

let expand_description = {|
Opens entries: each shows in full below the map, under `# open: path #`, with its errors marked at their lines and probe values inline. It stays open, following renames, until you collapse it.

Parameters:
paths: list(string) — entry paths as the map's annotations and errors name them (e.g. "total", "Geo/scale", "M/\"parses ints\"", "f/test#1")
|};

let paths_param = (what: string) =>
  `Assoc([
    ("type", `String("array")),
    ("description", `String(what)),
    ("items", `Assoc([("type", `String("string"))])),
  ]);

let tool = (~name, ~description, ~properties, ~required) =>
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String(name)),
        ("description", `String(description)),
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

let expand: API.Json.t =
  tool(
    ~name="expand",
    ~description=expand_description,
    ~properties=[
      ("paths", paths_param("The paths of the entries to open.")),
    ],
    ~required=["paths"],
  );

let collapse_description = {|
Closes open entries: they no longer show below the map. `all: true` closes every open entry.

Parameters:
paths: list(string) — paths of open entries
all: bool — close all of them (paths may then be omitted)
|};

let collapse: API.Json.t =
  tool(
    ~name="collapse",
    ~description=collapse_description,
    ~properties=[
      ("paths", paths_param("The paths of the entries to close.")),
      (
        "all",
        `Assoc([
          ("type", `String("boolean")),
          ("description", `String("Close every open entry.")),
        ]),
      ),
    ],
    ~required=[],
  );
