# Hazel as a JavaScript library (a draft)

`www/hazel-lib.js`, built beside the app's `hazel.js` (and so served by every branch build at `hazel.org/build/<branch>/hazel-lib.js`), defines `window.Hazel`, to analyze and run Hazel programs from any page.

```js
Hazel.version                    // "0.1 (draft, js-library)"
Hazel.build                      // the commit it was built from, "local" off GitHub
Hazel.analyze(text)              // { errors: [string] }
Hazel.run(text)                  // { errors: [string], value }
```

A program's errors do not stop it running: Hazel evaluates around its holes and marks, so `value` is there either way, and the page decides which errors it accepts.

## Values

| Hazel | JS |
|---|---|
| `Int`, `Float` (and the other numbers) | number |
| `Bool` | boolean |
| `String` | string |
| a list | an array |
| `()` | `null` |
| a labeled tuple `(x = 1, y = 2)` | an object `{x: 1, y: 2}` |
| another tuple `(1, true)` | an array `[1, true]` |
| a constructor `A`, applied `B(3)` | `{tag: "A"}`, `{tag: "B", value: 3}` |
| a function | a JS function |

The same conversion runs the other way for a function's arguments: one argument, or several as a tuple. So a page can analyze a program once and call its functions many times:

```js
const rule = Hazel.run(`
  type Cmd = + Done + Flip(Int, Cmd) in
  type Event = + Pull(Int) in
  fun e : Event -> case e | Pull(l) => Flip(l, Done) end`).value;
rule({tag: "Pull", value: 2})   // {tag: "Flip", value: [2, {tag: "Done"}]}
```

Anything else (a type, a hole) comes back as `{hazel: "<its printing>"}`.

A constructor from the page has no type of its own, so Hazel checks it where a parameter is annotated (`fun e : Event -> ...`). An applied one is checked whole, tag and payload: `{tag: "Pull", value: "x"}` against `+ Pull(Int)`, or a tag the type does not have, `{tag: "Bogus", value: 3}`, stays stuck at the parameter and the call returns `{hazel: ...}`. One with no value is not checked at all: `{tag: "Bogus"}` gets through, even a parameter annotated `Int`, and a `case` then gives it to a wildcard rule if it has one, or stays stuck. With no annotation, a `case` matches constructors by name.

## A program of modules

A page with an editor per module, each holding a module's body (its members, with no `in`), hands Hazel the texts and lets Hazel build the one program from them:

```js
Hazel.analyzeProgram(spec)       // { ok, errors, holes, programHoles, incomplete, programIncomplete, ms }
Hazel.runProgram(spec)           // the same, and value

spec = {
  preamble: "type Cmd = + Done + Flip(String, Cmd) in ...",  // definitions every module sees, ending in `in`
  modules: [{name: "Common", text: "let flip2 = ..."},
            {name: "TrapRoom", text: "let on_event = ..."}],
  tail: "(trap_room = TrapRoom.on_event, rooms = 1)",       // what the program gives; () if left out
}
```

The program is the preamble, then `module Common = { <its text> } in` for each module in order, then the tail, as one term (`ModuleProgram`, in haz3lcore). Each module sees the preamble and the modules before it, as `Common.flip2`. A body is read the way an editor rooted at the module sort reads it, so `let flip2 = ...` is a module member. A body with nothing written in it, or only comments, is the empty module.

- `errors`: `[{module, message, id}]`, `module` the name of the module the error is in, or `null` for the preamble and the tail. An error in the tail on a use of a module's member, as `TrapRoom.on_event`, is that module's: the member is missing (a blank room has none), or is not of the type the tail asks of it (`(TrapRoom.on_event : (Globals, Event) -> Cmd)` and a rule that gives a string). A name that is not a module's name, or two modules with one name, is an error too.
- `holes`: each module's holes, `{Common: 0, TrapRoom: 1}`; `programHoles`, the preamble's and the tail's. A member left after a last `;` is a hole, as the tile editor shows it (a page that wants the member before it to count can drop the `;` first).
- `incomplete`: each module's brackets and keywords left open (a `(` with no `)`, a `case` with no `end`), or closed with no opener (a stray `}`), the same way; `programIncomplete`, the preamble's and the tail's. Hazel reads such a text by completing it, so it may have no hole and no error, and the tile editor shows the shards it supplied as ghosts.
- `ok`: no errors, no holes and nothing incomplete. A rule still being written can have holes or open brackets and no errors, so a page acts on `value` only when `ok`.
- `value` converts as `run`'s does.

Hazel remembers the texts of the last program it read, part by part, so a page that calls again as one module is edited has only that module read again. A text reads fast, or slower when only Hazel's recovering parser can read it: a bracket or keyword left open, or a stray closer. Half-written last members (`let x =`, a last `;`) read fast. Measured once under node on a laptop (dev build, the panel's program with a trap room of about 300 and about 2000 characters, medians of 5 calls, each range clear of the others): well-formed, 21 and 81 ms; a room missing an `end`, read again on each call, 90 ms and 1.76 s (the slow parser's time grows with about the square of the module's length); the same broken room while another module is edited, 22 and 63 ms.

## Hazel in a frame (embed mode)

`hazel.org/build/<branch>/index.html#embed` is Hazel's own app in a mode for another page's `<iframe>`: one read-only cell of Hazel's tiles, with statics on and Hazel's own error marks, showing a program the page posts to it. None of the app's startup runs: no saved state (IndexedDB), saved settings or theme, autosave, worker, or focus grabs. The frame's root has no tabindex, so the frame does not take focus from the page by itself (a click in it focuses it, as it does any frame). Hazel does not run the program there, and the theme is Hazel's default.

```js
const HAZEL = "https://hazel.org";
const frame = document.createElement("iframe");
frame.style.border = "0";   // the posted height is the frame's content box
frame.src = HAZEL + "/build/js-library/index.html#embed";
window.addEventListener("message", e => {
  if (e.origin !== HAZEL || e.source !== frame.contentWindow) return;
  switch (e.data.type) {
    case "ready":  frame.contentWindow.postMessage({type: "set", ...spec}, HAZEL); break;
    case "size":   frame.style.height = e.data.height + "px"; break;
    case "status": show(e.data.ok, e.data.errors, e.data.holes); break;
    case "crash":  fallBack(e.data.message); break;
  }
});
```

Protocol 1. Page to frame:

- `{type: "set", text}`: one program, as text.
- `{type: "set", preamble, modules, tail}`: a program of modules, the spec `analyzeProgram` takes (`result` is taken for `tail`). The frame shows it as one text: the preamble, then `module Common = {` its text `} in` for each module, then the tail (`ModuleProgram.text_of_parts`). A module with nothing written in it shows as `module Common = {} in`. Each module's text is read on its own, inside its own braces (`ModuleProgram.zipper_of_parts`), so a bracket it leaves open stays in that module, and the frame shows the program `analyzeProgram` builds.
- `{type: "ping"}`: answered with `ready`, for a page that missed it; the frame then posts its `size` and `status` again.

A page can send a `set` on every change. The frame shows the latest one sent, set once per animation frame, so sets sent faster than they are read do not queue up, and a `set` of the program the frame already shows does nothing. A module whose text is the one last sent at its place is not read again. The frame drops the spaces that start each line, as Hazel does for its slides: Hazel lays out indentation itself.

Frame to page:

- `{type: "ready", protocol: 1, build}`, after the frame's first display. `build` is the commit the frame was built from, as `Hazel.build` gives it in that build's `hazel-lib.js`, so a page can tell whether the two came from one deploy (a branch build is cached for up to 10 minutes).
- `{type: "size", height}`: the height of the frame's content in CSS pixels, rounded up, for the page to size the frame to. The frame does not scroll itself (long lines scroll sideways inside it), so a frame given less shows less.
- `{type: "status", ok, errors, holes, programHoles, incomplete, programIncomplete}`: the program's errors, holes and incomplete tiles, the fields as `analyzeProgram` gives them, on the frame's own ids. For a `set` with `text`, `holes` and `incomplete` name the modules the text defines.
- `{type: "crash", message}`: an exception in the frame, each message once. A page can fall back to showing text.

`size` and `status` are posted only when they change. The frame looks after every display, which comes every animation frame.

Origins: the frame takes messages only from its parent window, and only from these origins: `https://matthewhammer.org` and `https://www.matthewhammer.org` (Messed Up Dress Up), `localhost` and `127.0.0.1` on any port over http or https (its dev servers), and the frame's own origin, so the frame opened on its own takes a program posted from its console. The list is `site_origins` and `dev_hosts` in `src/web/embed/EmbedProtocol.re`, built into the frame; to embed Hazel in another site, add its origin there. Once the frame has taken a message, it posts only to that message's origin. Before that it posts only `ready` and `crash`, which carry no program, and to any origin (`"*"`).

The frame is `src/web/embed/`: `EmbedModel` (the cell, its update and what to post, free of the DOM, so a page that mounts Hazel's editor itself can reuse them), `EmbedProtocol` (the messages), `EmbedLink` (what the frame takes from its page and posts to it, with the page's window and the posting given, so tests stand in for them) and `EmbedMain` (the frame). Tests: `test/Test_Embed.re`. No browser has loaded `#embed` yet; before this leaves draft, a page on one localhost port framing `index.html#embed` from another (two origins), plus a page on an origin not allowed, checks the parts only a browser shows: fonts, the posted height against the frame, and that the frame does not take focus.

## First use, and next

Messed Up Dress Up's Scripting panel: a room's rules are Hazel functions from a game event to the commands to run, which the game interprets. Next here: editable module cells in the frame, each module's editor seeing the whole program. Later, if a frame is too heavy, an editor to mount in another page directly (`Hazel.mountEditor(element, options)`), on the same model.

The working part is `src/web/HazelLibCore.re`; `src/web/HazelLib.re` puts it on the page. Tests: `test/Test_HazelLib.re`, and `test/Test_ModuleProgram.re` for the program of modules.
