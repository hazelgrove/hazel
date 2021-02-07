type t = string;

let sexp_of_t = s => Sexplib.Sexp.of_string(s);
let t_of_sexp = sexp => Sexplib.Sexp.to_string(sexp);

let to_string = x => x;

/** Helper function to parse out the error location from the
   *  error message that Scanf.unescaped generates.
   */
let char_num_regexp = Str.regexp("\\([0-9]+\\):");
let bad_input_char_from_exn_msg = exn_msg => {
  let _ = Str.search_forward(char_num_regexp, exn_msg, 0);
  int_of_string(Str.matched_group(1, exn_msg)) - 1;
};

/** Helper function to determine whether Scanf.unescaped raised
   *  an end of file error (which occurs due to trailing \)
   */
let end_of_file_err_regexp = Str.regexp(".*end of file.*");
let is_end_of_file_err = exn_msg =>
  Str.string_match(end_of_file_err_regexp, exn_msg, 0);

let unescaped_total = s =>
  switch (Scanf.unescaped(s)) {
  | us => us /* fast path when there are no invalid escape sequences */
  | exception (Scanf.Scan_failure(_)) =>
    /* simplest approach seems to be to just iteratively run unescaped,
       look at the error location reported in the exception it raises,
       and then unescape the characters before the error and continue
       iteratively like that until no error is raised

       NOTE: I did look at Scanf to see if there was a more principled
       way to do this, but its public interface doesn't offer anything
       that I could make sense of.

       We could fork a subset of Scanf, but that seems like a maintenance
       issue (and its pretty complex).

       Alternatively we could write our own hand-rolled unescaper, but
       that may be a lot of work given all of the escape sequences
       that OCaml supports. This way is hacky and inefficient in the rare
       cases where you have many invalid escape sequences, but it works
       so it is what it is for now. */
    let fragments: Buffer.t = Buffer.create(String.length(s));
    let remaining = ref(s);
    let finished = ref(false);
    while (! finished^) {
      let r = remaining^;
      switch (Scanf.unescaped(r)) {
      | s =>
        Buffer.add_string(fragments, s);
        finished := true;
      | exception (Scanf.Scan_failure(exn_msg)) =>
        if (is_end_of_file_err(exn_msg)) {
          /* this occurs when there is a trailing \\ in the string */
          let prefix = String.sub(r, 0, String.length(r) - 1);
          Buffer.add_string(fragments, Scanf.unescaped(prefix) ++ "\\");
          finished := true;
        } else {
          let bad_char_idx = bad_input_char_from_exn_msg(exn_msg);
          if (String.equal(String.sub(r, bad_char_idx - 1, 1), "\"")) {
            Buffer.add_string(
              fragments,
              Scanf.unescaped(Str.string_before(r, bad_char_idx - 1)) ++ "\"",
            );
            remaining := Str.string_after(r, bad_char_idx);
          } else {
            let prefix = Str.string_before(r, bad_char_idx);
            let c = String.sub(r, bad_char_idx, 1);
            let fragment = Scanf.unescaped(prefix ++ "\\" ++ c);
            Buffer.add_string(fragments, fragment);
            remaining := Str.string_after(r, bad_char_idx + 1);
          };
        }
      };
    };
    Buffer.contents(fragments);
  };

let equal = String.equal;
let cat = (s1, s2) => s1 ++ s2;
