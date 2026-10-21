# Hazel as a JavaScript library (a draft)

`www/hazel-lib.js`, built beside the app's `hazel.js` (and so served by every branch build at `hazel.org/build/<branch>/hazel-lib.js`), defines `window.Hazel`, to analyze and run Hazel programs from any page.

```js
Hazel.version                    // "0.1 (draft, js-library)"
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
Hazel.analyzeProgram(spec)       // { ok, errors, holes, programHoles, ms }
Hazel.runProgram(spec)           // the same, and value

spec = {
  preamble: "type Cmd = + Done + Flip(String, Cmd) in ...",  // definitions every module sees, ending in `in`
  modules: [{name: "Common", text: "let flip2 = ..."},
            {name: "TrapRoom", text: "let on_event = ..."}],
  tail: "(trap_room = TrapRoom.on_event, rooms = 1)",       // what the program gives; () if left out
}
```

The program is the preamble, then `module Common = { <its text> } in` for each module in order, then the tail, as one term (`ModuleProgram`, in haz3lcore). Each module sees the preamble and the modules before it, as `Common.flip2`. A body is read the way an editor rooted at the module sort reads it, so `let flip2 = ...` is a module member. A body with nothing written in it, or only comments, is the empty module.

- `errors`: `[{module, message, id}]`, `module` the name of the module the error is in, or `null` for the preamble and the tail. A name that is not a module's name, or two modules with one name, is an error too.
- `holes`: each module's holes, `{Common: 0, TrapRoom: 1}`; `programHoles`, the preamble's and the tail's. A member left after a last `;` is a hole.
- `ok`: no errors and no holes. A rule still being written can have holes and no errors, so a page acts on `value` only when `ok`.
- `value` converts as `run`'s does.

## First use, and next

Messed Up Dress Up's Scripting panel: a room's rules are Hazel functions from a game event to the commands to run, which the game interprets. Next here: an editor to mount in another page (`Hazel.mountEditor(element, options)`).

The working part is `src/web/HazelLibCore.re`; `src/web/HazelLib.re` puts it on the page. Tests: `test/Test_HazelLib.re`, and `test/Test_ModuleProgram.re` for the program of modules.
