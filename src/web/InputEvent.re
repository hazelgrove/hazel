/* The `input`-event counterpart of Keyboard: turns a change in an editor's
 * hidden text input into editor actions.
 *
 * Virtual keyboards and IMEs do not type key by key. Android reports every
 * letter as an "Unidentified" keydown and delivers the text through `input`
 * events, and a composing IME rewrites the word in progress as it goes. So
 * the input accumulates whatever the browser put in it, and each `input`
 * event is replayed as the edits taking the editor from the text already
 * dispatched to the new value: delete the divergent tail, grapheme by
 * grapheme, then insert the new one. One Insert per grapheme is exactly what
 * a hardware key produces, so molding and auto-pairing behave the same. */
open Haz3lcore;
open Util;

let common_prefix_length = (xs: array(string), ys: array(string)): int => {
  let n = min(Array.length(xs), Array.length(ys));
  let rec go = i => i < n && String.equal(xs[i], ys[i]) ? go(i + 1) : i;
  go(0);
};

let action_of_grapheme = (g: string): option(Action.t) =>
  switch (g) {
  | "\t" => None
  | _ => Some(Insert(g))
  };

let actions_of_input = (~composed: string, value: string): list(Action.t) => {
  let old = Unicode.graphemes(composed);
  let new_ = Unicode.graphemes(value);
  let k = common_prefix_length(old, new_);
  let deletes =
    List.init(Array.length(old) - k, _ =>
      Action.Destruct(Local(Left, ByChar))
    );
  let inserts =
    Array.sub(new_, k, Array.length(new_) - k)
    |> Array.to_list
    |> List.filter_map(action_of_grapheme);
  deletes @ inserts;
};
