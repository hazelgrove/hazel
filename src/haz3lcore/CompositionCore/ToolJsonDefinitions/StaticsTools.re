open Util;

let place_statics_description = {|
Places the **statics** refractor on the bindings at the given paths: a GUI type-ascription overlay (offside type cell) for human-readable types on expressions.
This does not change program text. If a runtime probe is on the same binding, it is replaced by statics (same transition as the editor’s statics toggle).
Placing statics opens its entry.

Parameters:
paths: list(string) — paths to the bindings

See the system prompt **Projectors and the agent view** for how statics relate to probes and livelits.
|};

let place_statics: API.Json.t =
  EditTools.tool(
    ~name="place_statics",
    ~description=place_statics_description,
    ~properties=[
      (
        "paths",
        EditTools.paths_param(
          "Paths to bindings where the statics (type overlay) refractor should be shown.",
        ),
      ),
    ],
    ~required=["paths"],
  );

let remove_statics_description = {|
Removes **only** the statics refractor from the bindings at the given paths.
Runtime probes on the same binding are left unchanged (unlike remove_probe).

Parameters:
paths: list(string) — paths to the bindings to clear statics overlays from
|};

let remove_statics: API.Json.t =
  EditTools.tool(
    ~name="remove_statics",
    ~description=remove_statics_description,
    ~properties=[
      (
        "paths",
        EditTools.paths_param(
          "Paths to bindings to remove statics overlays from.",
        ),
      ),
    ],
    ~required=["paths"],
  );

let toggle_statics_description = {|
Toggles the statics refractor on or off for the bindings at the given paths (same behavior as the editor statics toggle).
Its entry opens when statics end up enabled.

Parameters:
paths: list(string) — paths to the bindings to toggle
|};

let toggle_statics: API.Json.t =
  EditTools.tool(
    ~name="toggle_statics",
    ~description=toggle_statics_description,
    ~properties=[
      (
        "paths",
        EditTools.paths_param(
          "Paths to bindings to toggle statics overlays on.",
        ),
      ),
    ],
    ~required=["paths"],
  );
