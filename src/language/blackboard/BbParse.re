/* A small parser for Blackboard's concrete syntax, close to the paper's
   listings, for tests and fixtures until the tile-based forms exist.

     term    ::= '(' name+ ':' term ')' '->' term       dependent arrow
               | mem '->' term                          non-dependent arrow
               | mem
     mem     ::= app (':' app)?
     app     ::= atom+
     atom    ::= name | 'type' | '(' term ')'

   Documents are line-based: `assume` and `construct` start blocks, `by t`
   ends one with tactic t (the rest of the line), and every other logical
   line is `name : term`.  A line beginning with whitespace continues the
   previous line, so long types may be broken as in the paper.  Braces for
   implicit arguments and user-defined `syntax` are not supported: write
   the arguments explicitly, e.g. `eq A a1 a2`.  A multi-name binder
   `(a1 a2 : A)` abbreviates `(a1 : A) -> (a2 : A)`. */

open BbTerm;

type token =
  | Ident(string)
  | LParen
  | RParen
  | Colon
  | Arrow
  | KwType;

exception Parse_error(string);

let is_ident_start = c =>
  c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_';
let is_ident_char = c =>
  is_ident_start(c) || c >= '0' && c <= '9' || c == '\'' || c == '-';

/* Tokenize one logical line. */
let tokenize = (line: string): list(token) => {
  let n = String.length(line);
  let rec go = (i, acc) =>
    if (i >= n) {
      List.rev(acc);
    } else {
      let c = line.[i];
      if (c == ' ' || c == '\t' || c == '\r') {
        go(i + 1, acc);
      } else if (c == '(') {
        go(i + 1, [LParen, ...acc]);
      } else if (c == ')') {
        go(i + 1, [RParen, ...acc]);
      } else if (c == ':') {
        go(i + 1, [Colon, ...acc]);
      } else if (c == '-' && i + 1 < n && line.[i + 1] == '>') {
        go(i + 2, [Arrow, ...acc]);
      } else if (i
                 + 2 < n
                 && Char.code(c) == 0xE2
                 && Char.code(line.[i + 1]) == 0x86
                 && Char.code(line.[i + 2]) == 0x92) {
        /* the arrow as a UTF-8 character */
        go(i + 3, [Arrow, ...acc]);
      } else if (is_ident_start(c)) {
        let j = ref(i + 1);
        while (j^ < n
               && is_ident_char(line.[j^])
               && !(line.[j^] == '-' && j^ + 1 < n && line.[j^ + 1] == '>')) {
          incr(j);
        };
        let word = String.sub(line, i, j^ - i);
        let tok = word == "type" ? KwType : Ident(word);
        go(j^, [tok, ...acc]);
      } else {
        raise(
          Parse_error("unexpected character '" ++ String.make(1, c) ++ "'"),
        );
      };
    };
  go(0, []);
};

/* Term parser over an array of tokens. */
let parse_term_tokens = (toks: array(token)): BbTerm.t => {
  let n = Array.length(toks);
  let pos = ref(0);
  let peek = () => pos^ < n ? Some(toks[pos^]) : None;
  let advance = () => incr(pos);
  let expect = (t, what) =>
    switch (peek()) {
    | Some(t') when t' == t => advance()
    | _ => raise(Parse_error("expected " ++ what))
    };
  let starts_atom = () =>
    switch (peek()) {
    | Some(Ident(_) | KwType | LParen) => true
    | _ => false
    };
  /* At '(': is this `( name+ : ... ) ->`? */
  let looks_like_binder = () => {
    let i = ref(pos^ + 1);
    let names = ref(0);
    while (i^ < n
           && (
             switch (toks[i^]) {
             | Ident(_) => true
             | _ => false
             }
           )) {
      incr(names);
      incr(i);
    };
    if (names^ == 0 || i^ >= n || toks[i^] != Colon) {
      false;
    } else {
      /* find the matching ')' */
      let depth = ref(1);
      let j = ref(pos^ + 1);
      while (j^ < n && depth^ > 0) {
        switch (toks[j^]) {
        | LParen => incr(depth)
        | RParen => decr(depth)
        | _ => ()
        };
        incr(j);
      };
      depth^ == 0 && j^ < n && toks[j^] == Arrow;
    };
  };
  let rec term = () =>
    if (peek() == Some(LParen) && looks_like_binder()) {
      advance();
      let rec names = acc =>
        switch (peek()) {
        | Some(Ident(x)) =>
          advance();
          names([x, ...acc]);
        | _ => List.rev(acc)
        };
      let xs = names([]);
      expect(Colon, "':' in binder");
      let a = term();
      expect(RParen, "')' closing binder");
      expect(Arrow, "'->' after binder");
      let b = term();
      List.fold_right((x, body) => Pi(x, a, body), xs, b);
    } else {
      let lhs = mem();
      switch (peek()) {
      | Some(Arrow) =>
        advance();
        Pi("_", lhs, term());
      | _ => lhs
      };
    }
  and mem = () => {
    let a = app();
    switch (peek()) {
    | Some(Colon) =>
      advance();
      Mem(a, app());
    | _ => a
    };
  }
  and app = () => {
    let head = atom();
    let rec args = f => starts_atom() ? args(App(f, atom())) : f;
    args(head);
  }
  and atom = () =>
    switch (peek()) {
    | Some(Ident(x)) =>
      advance();
      Var(x);
    | Some(KwType) =>
      advance();
      Type;
    | Some(LParen) =>
      advance();
      let t = term();
      expect(RParen, "')'");
      t;
    | Some(_) => raise(Parse_error("unexpected token"))
    | None => raise(Parse_error("unexpected end of input"))
    };
  let t = term();
  if (pos^ < n) {
    raise(Parse_error("trailing tokens"));
  };
  t;
};

let term = (s: string): result(BbTerm.t, string) =>
  switch (parse_term_tokens(Array.of_list(tokenize(s)))) {
  | t => Ok(t)
  | exception (Parse_error(msg)) => Error(msg ++ " in: " ++ s)
  };

/* Logical lines: a line beginning with whitespace continues the previous
   one; blank lines are skipped. */
let logical_lines = (s: string): list(string) => {
  let lines = String.split_on_char('\n', s);
  let is_blank = l => String.trim(l) == "";
  let continues = l =>
    String.length(l) > 0 && (l.[0] == ' ' || l.[0] == '\t');
  List.fold_left(
    (acc, l) =>
      if (is_blank(l)) {
        acc;
      } else {
        switch (acc) {
        | [prev, ...rest] when continues(l) => [
            prev ++ " " ++ String.trim(l),
            ...rest,
          ]
        | _ => [String.trim(l), ...acc]
        };
      },
    [],
    lines,
  )
  |> List.rev;
};

let first_word = (l: string): (string, string) =>
  switch (String.index_opt(l, ' ')) {
  | None => (l, "")
  | Some(i) => (
      String.sub(l, 0, i),
      String.trim(String.sub(l, i, String.length(l) - i)),
    )
  };

let entry_of_line = (l: string): entry => {
  switch (String.index_opt(l, ':')) {
  | None => raise(Parse_error("expected `name : type` in: " ++ l))
  | Some(i) =>
    let name = String.trim(String.sub(l, 0, i));
    if (name == ""
        || !
             List.for_all(
               is_ident_char,
               List.init(String.length(name), String.get(name)),
             )) {
      raise(Parse_error("bad entry name in: " ++ l));
    };
    let rest = String.sub(l, i + 1, String.length(l) - i - 1);
    let ty = parse_term_tokens(Array.of_list(tokenize(rest)));
    {
      name,
      ty,
    };
  };
};

type open_block =
  | OAssume(list(entry))
  | OConstruct(list(entry));

let doc = (s: string): result(doc, string) => {
  let close = (b, tactic) =>
    switch (b) {
    | OAssume(es) => Assume(List.rev(es), tactic)
    | OConstruct(es) =>
      switch (tactic) {
      | Some(t) => Construct(List.rev(es), t)
      | None => raise(Parse_error("construct block without `by`"))
      }
    };
  let step = ((blocks, cur), line) => {
    let (w, rest) = first_word(line);
    switch (w, cur) {
    | ("assume", None) => (blocks, Some(OAssume([])))
    | ("construct", None) => (blocks, Some(OConstruct([])))
    | ("assume" | "construct", Some(b)) =>
      /* a block without `by` ends when the next one starts */
      let blocks = [close(b, None), ...blocks];
      (blocks, Some(w == "assume" ? OAssume([]) : OConstruct([])));
    | ("by", Some(b)) => ([close(b, Some(rest)), ...blocks], None)
    | ("by", None) => raise(Parse_error("`by` outside a block"))
    | (_, None) =>
      raise(Parse_error("declaration outside a block: " ++ line))
    | (_, Some(OAssume(es))) => (
        blocks,
        Some(OAssume([entry_of_line(line), ...es])),
      )
    | (_, Some(OConstruct(es))) => (
        blocks,
        Some(OConstruct([entry_of_line(line), ...es])),
      )
    };
  };
  switch (List.fold_left(step, ([], None), logical_lines(s))) {
  | (blocks, None) => Ok(List.rev(blocks))
  | (blocks, Some(b)) => Ok(List.rev([close(b, None), ...blocks]))
  | exception (Parse_error(msg)) => Error(msg)
  };
};
