/* On-demand documentation packs, served by the `read_docs` tool. The
   always-on system prompt and the tool description carry only the one-line
   blurbs below (both generated from this registry, so they cannot drift),
   and a full guide costs context only when the agent pulls it. Fenced code
   in pack bodies is validated by Test_PromptFactory. */

type pack = {
  name: string,
  blurb: string, /* one line: what it teaches and when to read it */
  body: string,
};

/* No packs yet on this branch: the existing guides (mvu, livelits,
   creative) document features that are not on dev. The registry, the
   read_docs tool, and the prompt splice all key off this list, so packs
   added here become available everywhere at once. */
let all: list(pack) = [];

let lookup = (name: string): option(pack) =>
  List.find(all, ~f=p => String.equal(p.name, String.strip(name)));

let topic_lines: string =
  all
  |> List.map(~f=p => "- `" ++ p.name ++ "` — " ++ p.blurb)
  |> String.concat(~sep="\n");

let topic_names: list(string) = List.map(all, ~f=p => p.name);
