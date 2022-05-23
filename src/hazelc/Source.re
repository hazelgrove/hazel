open Sexplib.Std;

type t =
  | SourceString(string)
  | SourceLexbuf(Lexing.lexbuf)
  | SourceChannel(in_channel);

let sexp_of_t = source => {
  Sexplib.Sexp.(
    switch (source) {
    | SourceString(s) => List([Atom("string"), sexp_of_string(s)])
    | SourceLexbuf(_) => List([Atom("lexbuf"), Atom("<Lexing.lexbuf>")])
    | SourceChannel(_) => List([Atom("channel"), Atom("<in_channel>")])
    }
  );
};

let t_of_sexp = sexp => {
  Sexplib0.(
    Sexplib.Sexp.(
      {
        let of_sexp_error = (what, sexp) =>
          raise(Sexp.Of_sexp_error(Failure(what), sexp));

        switch (sexp) {
        | List([Atom("string"), el]) => SourceString(string_of_sexp(el))
        | List([Atom("lexbuf"), _]) =>
          of_sexp_error("source_of_sexp: cannot deseralize for lexbuf", sexp)
        | List([Atom("channel"), _]) =>
          of_sexp_error(
            "source_of_sexp: cannot deseralize for in_channel",
            sexp,
          )
        | List(_) =>
          of_sexp_error("source_of_sexp: list must be (string el)", sexp)
        | Atom(_) => of_sexp_error("source_of_sexp: list needed", sexp)
        };
      }
    )
  );
};

let to_lexbuf = (source: t) => {
  switch (source) {
  | SourceString(s) => s |> Lexing.from_string
  | SourceLexbuf(lexbuf) => lexbuf
  | SourceChannel(channel) => channel |> Lexing.from_channel
  };
};
