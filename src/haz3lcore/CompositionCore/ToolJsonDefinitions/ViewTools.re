open Util;

let targets_param = (what: string): (string, API.Json.t) => (
  "targets",
  `Assoc([
    ("type", `String("array")),
    ("description", `String(what)),
    ("items", `Assoc([("type", `String("string"))])),
  ]),
);

let open_: API.Json.t =
  EditTools.tool(
    ~name="open",
    ~description=
      {|
Opens entries: each shows in full below the map, under `#› open: path #`, with its errors marked at their lines and probe values at line ends. It stays open, following renames, until closed.|}
      ++ EditTools.example("open"),
    ~properties=[targets_param("Paths of the entries to open.")],
    ~required=["targets"],
  );

let close: API.Json.t =
  EditTools.tool(
    ~name="close",
    ~description=
      {|
Closes open entries, or with `all: true` every open entry.|}
      ++ EditTools.example("close"),
    ~properties=[
      targets_param("Paths of the open entries to close."),
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
