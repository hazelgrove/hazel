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

## First use, and next

Messed Up Dress Up's Scripting panel: a room's rules are Hazel functions from a game event to the commands to run, which the game interprets. Next here: an editor to mount in another page (`Hazel.mountEditor(element, options)`).

The working part is `src/web/HazelLibCore.re`; `src/web/HazelLib.re` puts it on the page. Tests: `test/Test_HazelLib.re`.
