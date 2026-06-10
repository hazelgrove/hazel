/* Headless driver: feed a scripted byte string through the same
   input-parse/update path as the interactive loop, then return the final
   frame as plain text. Powers `hazel-tui --replay` and the golden tests —
   no pty needed. */

let default_size = (24, 80);

/* Backslash escapes for writing key scripts: \e ESC, \r, \n, \t, \\,
   and \xHH for arbitrary bytes (e.g. \x03 for Ctrl+C). */
let unescape = (s: string): string => {
  let buf = Buffer.create(String.length(s));
  let len = String.length(s);
  let hex = c =>
    switch (c) {
    | '0' .. '9' => Char.code(c) - Char.code('0')
    | 'a' .. 'f' => Char.code(c) - Char.code('a') + 10
    | 'A' .. 'F' => Char.code(c) - Char.code('A') + 10
    | _ => failwith("replay: bad hex digit")
    };
  let rec go = i =>
    if (i < len) {
      if (s.[i] == '\\' && i + 1 < len) {
        switch (s.[i + 1]) {
        | 'e' =>
          Buffer.add_char(buf, '\027');
          go(i + 2);
        | 'r' =>
          Buffer.add_char(buf, '\r');
          go(i + 2);
        | 'n' =>
          Buffer.add_char(buf, '\n');
          go(i + 2);
        | 't' =>
          Buffer.add_char(buf, '\t');
          go(i + 2);
        | '\\' =>
          Buffer.add_char(buf, '\\');
          go(i + 2);
        | 'x' when i + 3 < len =>
          Buffer.add_char(
            buf,
            Char.chr(hex(s.[i + 2]) * 16 + hex(s.[i + 3])),
          );
          go(i + 4);
        | c =>
          Buffer.add_char(buf, c);
          go(i + 2);
        };
      } else {
        Buffer.add_char(buf, s.[i]);
        go(i + 1);
      };
    };
  go(0);
  Buffer.contents(buf);
};

let final_model =
    (~size=default_size, ~file: option(string)=None, keys: string): App.model => {
  Util.Os.is_mac := false;
  let model = ref(App.init(file));
  let (st, events) = AnsiInput.parse(AnsiInput.init, unescape(keys));
  let (_, flushed) = AnsiInput.flush(st);
  /* deterministic fake clock, spaced > the click-streak window so
     scripted clicks don't accidentally read as double-clicks */
  let clock = ref(0.0);
  List.iter(
    ev =>
      switch (Keymap.handle(ev)) {
      | None => ()
      | Some(action) =>
        clock := clock^ +. 1.0;
        let page = App.editor_height(~size, model^);
        let (m, _quit) = App.apply(~now=clock^, ~page, model^, action);
        model := App.disarm(m, action);
      },
    events @ flushed,
  );
  App.run_eval(model^);
};

/* Final frame as plain text (no styling), trailing whitespace trimmed */
let run =
    (~size=default_size, ~file: option(string)=None, keys: string): string => {
  let model = final_model(~size, ~file, keys);
  let (frame, _) = App.render(~size, model);
  Frame.to_plain_text(frame)
  |> String.split_on_char('\n')
  |> List.map(Util.StringUtil.trim_trailing_whitespace)
  |> String.concat("\n");
};

/* Just the editor text (what would be saved), for round-trip checks */
let buffer_text = (~size=default_size, ~file=None, keys: string): string => {
  let model = final_model(~size, ~file, keys);
  FileIo.zipper_to_text(model.editor.state.zipper);
};
