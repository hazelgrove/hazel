open Util;

/* The editable catalog includes the original constellation and ARIA demos. */
[@deriving yojson]
type t = {
  id: string,
  group: string,
  title: string,
  prompt: string,
};

let all: list(t) =
  [%blob "../../demo-prompts.json"]
  |> Yojson.Safe.from_string
  |> Yojson.Safe.Util.to_list
  |> List.map(t_of_yojson);

let groups = ["Quick starts", "Constellation", "ARIA demos", "Follow-ups"];
let find = id => List.find_opt((p: t) => p.id == id, all);
