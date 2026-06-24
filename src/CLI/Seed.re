/* Edit-time seed injection for the Hazel CLI, mirroring `^^csv`.

   A `.hz` source may reference a random seed with the form

       ^^seed(42)

   where the integer is a DEFAULT. Before the program is parsed, the CLI replaces
   each `^^seed(N)` with an integer literal: the source default N (under `--yes` or
   when reading from stdin, for reproducible non-interactive runs), or — when
   prompting — a value the caller types, or a fresh OS-random seed.

   The substituted value is an ordinary integer literal, so the language itself
   stays pure (a program is a pure function of the spliced constant). Choosing the
   seed is a tooling step — the same edit-time-injection idea as `^^csv` — which is
   precisely what lets a future run draw a genuinely *random* seed that a pure
   program could never produce on its own. The rewrite happens before parsing, so
   the parser never sees the `^^seed` marker. */

let is_ws = (c: char): bool =>
  c == ' ' || c == '\t' || c == '\n' || c == '\r';

let skip_ws = (s: string, i: int): int => {
  let n = String.length(s);
  let j = ref(i);
  while (j^ < n && is_ws(s.[j^])) {
    j := j^ + 1;
  };
  j^;
};

let is_ident_char = (c: char): bool =>
  c >= 'a'
  && c <= 'z'
  || c >= 'A'
  && c <= 'Z'
  || c >= '0'
  && c <= '9'
  || c == '_';

let marker = "^^seed";

/* Parse an optionally '-'-prefixed integer starting at index i. Returns the value
   and the index just past the last digit. */
let parse_int_lit = (s: string, i: int): option((int, int)) => {
  let n = String.length(s);
  let j = ref(i);
  if (j^ < n && s.[j^] == '-') {
    j := j^ + 1;
  };
  let digits_start = j^;
  while (j^ < n && s.[j^] >= '0' && s.[j^] <= '9') {
    j := j^ + 1;
  };
  if (j^ > digits_start) {
    switch (int_of_string_opt(String.sub(s, i, j^ - i))) {
    | Some(v) => Some((v, j^))
    | None => None
    };
  } else {
    None;
  };
};

/* If `^^seed ( N )` (whitespace-tolerant) starts at index i, return the default N
   and the index just past the closing `)`. The char after the marker must not
   continue an identifier, so `^^seeded(...)` is left alone. */
let match_seed_call = (s: string, i: int): option((int, int)) => {
  let n = String.length(s);
  let mlen = String.length(marker);
  if (i
      + mlen <= n
      && String.sub(s, i, mlen) == marker
      && (i + mlen >= n || !is_ident_char(s.[i + mlen]))) {
    let p1 = skip_ws(s, i + mlen);
    if (p1 < n && s.[p1] == '(') {
      let p2 = skip_ws(s, p1 + 1);
      switch (parse_int_lit(s, p2)) {
      | Some((v, p3)) =>
        let p4 = skip_ws(s, p3);
        p4 < n && s.[p4] == ')' ? Some((v, p4 + 1)) : None;
      | None => None
      };
    } else {
      None;
    };
  } else {
    None;
  };
};

/* Replace every `^^seed(N)` reference in `src` with `string_of_int(choose(N))`,
   where `choose` maps a source default to the seed actually used. */
let splice = (~choose: int => int, src: string): string => {
  let n = String.length(src);
  let buf = Buffer.create(n);
  let i = ref(0);
  while (i^ < n) {
    switch (src.[i^] == '^' ? match_seed_call(src, i^) : None) {
    | Some((default, next)) =>
      Buffer.add_string(buf, string_of_int(choose(default)));
      i := next;
    | None =>
      Buffer.add_char(buf, src.[i^]);
      i := i^ + 1;
    };
  };
  Buffer.contents(buf);
};
