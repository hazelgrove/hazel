open Util;

let read_docs_description =
  {|
Fetch a detailed how-to guide for a Hazel capability. Available topics:

|}
  ++ DocPacks.topic_lines
  ++ {|

Read the relevant guide BEFORE building the kind of thing it covers; the
guide is returned as this tool's result and stays available for the rest of
the session. Costs one tool call and nothing else.
|};

let read_docs: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("read_docs")),
        ("description", `String(read_docs_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "topic",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "enum",
                      `List(
                        List.map(s => `String(s), DocPacks.topic_names),
                      ),
                    ),
                    ("description", `String("Which guide to fetch")),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("topic")])),
          ]),
        ),
      ]),
    ),
  ]);
