open Util;

let place_probe_description =
  {|
Places probes on the bindings at the given paths to observe their runtime values.
When a probe is active, the agent view shows evaluated values at line ends (e.g., `expr  #› ≡ value #`).
This is a powerful debugging and verification tool — use it to check whether code behaves as expected.
Placing a probe opens its entry, so the results show.
Values come from the evaluator; the next context refresh waits for evaluation to finish, so probe values are normally present immediately. A `∅` means the probed expression was never reached during evaluation (e.g. a branch not taken or a function never called) — that itself is useful evidence.

Parameters:
paths: list(string) — paths to the bindings to probe

Example:
Given the program:
```
let double(x) = x * 2 in
let result = double(5) in
result
```
Calling place_probe(paths=["result"]) shows:
```
#› map · no errors #
let double(x) = ⋯ in  #› 1 use #
let result = ⋯ in  #› open below · 1 use #
result  #› ⇒ 10 #

#› open: result #
let result = ⟦double(5)⟧ in  #› ≡ 10 #
```
The ≡ 10 shows that `result`'s probed expression, in ⟦ ⟧, evaluates to 10.

|}
  ++ "\n\n"
  ++ ProjectorCatalog.probe_tools_crossref;

let place_probe: API.Json.t =
  EditTools.tool(
    ~name="place_probe",
    ~description=place_probe_description,
    ~properties=[
      (
        "paths",
        EditTools.paths_param(
          "The paths to the bindings to probe for runtime values.",
        ),
      ),
    ],
    ~required=["paths"],
  );

let remove_probe_description =
  {|
Removes probes from the bindings at the given paths.
Use this to clean up probes after verifying code behavior.

Parameters:
paths: list(string) — paths to the bindings to remove probes from

Example:
Calling remove_probe(paths=["result"]) removes the probe from "result",
and the view no longer shows evaluated values for that binding.

|}
  ++ "\n\n"
  ++ ProjectorCatalog.probe_tools_crossref;

let remove_probe: API.Json.t =
  EditTools.tool(
    ~name="remove_probe",
    ~description=remove_probe_description,
    ~properties=[
      (
        "paths",
        EditTools.paths_param(
          "The paths to the bindings to remove probes from.",
        ),
      ),
    ],
    ~required=["paths"],
  );

let toggle_probe_description =
  {|
Toggles probes on or off for the bindings at the given paths.
If a probe is already placed, it is removed. If no probe exists, one is placed.
Placing a probe opens its entry.

Parameters:
paths: list(string) — paths to the bindings to toggle probes on

Example:
If "result" has no probe, calling toggle_probe(paths=["result"]) places one.
Calling toggle_probe(paths=["result"]) again removes it.

|}
  ++ "\n\n"
  ++ ProjectorCatalog.probe_tools_crossref;

let toggle_probe: API.Json.t =
  EditTools.tool(
    ~name="toggle_probe",
    ~description=toggle_probe_description,
    ~properties=[
      (
        "paths",
        EditTools.paths_param(
          "The paths to the bindings to toggle probes on.",
        ),
      ),
    ],
    ~required=["paths"],
  );
