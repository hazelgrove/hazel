/* What each Fumola form is, for the cursor inspector.

   Fumola's surface is Motoko's. The parser is Motoko's grammar in lalrpop --
   `async`, `await`, `actor`, `label`, `try`/`catch`, `to_candid`, `?e`, `e!`,
   `#tag` are all still in it -- with an adapton core grafted in: `thunk`,
   `force`, `@`, and the `do` navigations. A reader who knows Motoko is
   therefore owed one particular fact about every form they meet, and it is
   not the same fact for every form:

   - most forms are Motoko's, unchanged, and the useful thing to say is that
     nothing is different;
   - a few wear Motoko's spelling over a different meaning, and those are the
     ones that will quietly mislead a Motoko reader -- `:=` above all;
   - a few have no Motoko counterpart at all, and those are where the adapton
     store actually shows up in the syntax.

   So every entry carries an `origin` as well as prose, and `entry_of` is a
   total function over `FumolaCls.t` with no catch-all. That is the point of
   writing it this way: a form added to the class list stops compiling here
   until someone says which of the four it is, which is a cheaper way to keep
   this honest than remembering to come back. */

open Language;
open FumolaCls;

/* Where a form comes from, from the point of view of someone who reads
   Motoko. `Integration` is neither language's: it exists because Fumola is
   being edited inside Hazel, and would not exist in a Fumola source file. */
type origin =
  | Motoko
  | Overloaded(string)
  | Fumola
  | Integration;

/* One form, as the inspector shows it: the concrete syntax that builds it,
   what it does, and where it came from. */
type entry = {
  syntax: string,
  meaning: string,
  origin,
};

let origin_line =
  fun
  | Motoko => "*From Motoko, and it means there what it means here.*"
  | Overloaded(what) =>
    "*Motoko's spelling over a different meaning: " ++ what ++ "*"
  | Fumola => "*Fumola's own. Motoko has no counterpart.*"
  | Integration => "*Neither language's. It exists at the Hazel boundary.*";

/* Every class, and no catch-all: this match is exhaustive, so a class added
   to `FumolaCls.t` stops the build here until someone says what it is. That
   is the whole mechanism -- there is no default entry to fall into, which is
   what "no documentation available" used to be. */
let entry_of = (cls: FumolaCls.t): entry =>
  switch (cls) {
  /* -- holes and the boundary -------------------------------------------- */
  | EmptyHole => {
      syntax: "( nothing yet )",
      meaning: "A Fumola term that has not been written. It is a real node rather than an absence, which is why the program around it still has a shape, still type-checks as far as it can, and still runs: a hole stops the evaluator only when the evaluator reaches it.\n\nThis is Hazel's idea, applied to Fumola's syntax. Fumola's own parser has no such thing -- a half-written Fumola program is a parse error in the CLI and a term with a hole in it here.",
      origin: Integration,
    }
  | MultiHole => {
      syntax: "( several terms, not yet one term )",
      meaning: "Fragments that are each well-formed but do not yet compose into one term -- what an edit leaves behind on the way from one shape to another. The inspector names it so that the state reads as unfinished rather than as broken.",
      origin: Integration,
    }
  | Invalid => {
      syntax: "( text the tile grammar could not read )",
      meaning: "Characters standing where a Fumola term should be, kept verbatim rather than discarded. Keeping them is the point: the text is what the writer typed, and losing it on the way to reporting an error would cost them the thing they were editing.",
      origin: Integration,
    }
  | Hazel => {
      syntax: "hazel <Hazel expression> end",
      meaning: "A Hazel expression standing where a Fumola term does. It is a real tile subtree, edited as Hazel, and it can appear anywhere a Fumola term can, as many times as the program wants.\n\nIt carries a *value* -- and a variable the surrounding Hazel program bound is one, since a Fumola program runs during evaluation, so by the time it is printed its escapes have been reduced. `Fumola (Tiles) / Self-inspection` turns that into a switch: `hazel growTheGraph end` reads a Hazel boolean, and flipping it changes the shape of the graph Fumola records.\n\nWhat an escape cannot carry is a value with no written Fumola form -- a function, a hole. The program says so when it runs.",
      origin: Integration,
    }
  | InstanceName => {
      syntax: "fumola $graphical as <name> in ... end",
      meaning: "The name of the Fumola VM instance this program runs against. It reads like a variable and is not one: nothing in the Hazel program around it binds this name, and nothing in the Fumola program inside it does either. It is a key, written into the program text, that the runtime looks up -- so the same name in two programs means one instance and one adapton store, and changing the name means a different store.\n\nThat is why the name is written rather than derived: the store survives every edit that leaves the name alone.",
      origin: Integration,
    }
  /* -- atoms ------------------------------------------------------------- */
  | Var => {
      syntax: "x",
      meaning: "A name used where a value is wanted, standing for whatever the nearest enclosing binding gave it. Ordinary lexical scope: the binding is found by reading outwards through the blocks the name sits in.\n\nWorth separating from the two Fumola things that also look like names. A cell is named by a symbol and read with `@`, not by a variable; and an instance name is a key the runtime looks up. A variable is none of that -- it is resolved before anything touches the store.",
      origin: Motoko,
    }
  | Int => {
      syntax: "42    1_000_000    0x2a",
      meaning: "A whole number. Underscores may be used as digit separators and are not part of the value, and a leading `0x` reads the digits as hexadecimal.\n\nNumbers double as symbols in Fumola, so the same `1` can name a cell as well as count something. Which it is depends on the position it stands in, not on how it is written.",
      origin: Motoko,
    }
  | Float => {
      syntax: "1.5    1e3    1.5e-3",
      meaning: "A number with a fractional part, written with a decimal point, an exponent, or both.",
      origin: Motoko,
    }
  | Char => {
      syntax: "'a'",
      meaning: "A single character, in single quotes, with the usual backslash escapes.",
      origin: Motoko,
    }
  | Text => {
      syntax: "\"hello\"",
      meaning: "A string of text, in double quotes, with the usual backslash escapes.",
      origin: Motoko,
    }
  | Bool => {
      syntax: "true    false",
      meaning: "A boolean. The two literals, and the only two values `and`, `or`, `not` and the comparisons produce.",
      origin: Motoko,
    }
  | Null => {
      syntax: "null",
      meaning: "The absent option. `?e` builds a present one, `e!` takes a present one apart, and `null` is what is left when there is nothing to carry.",
      origin: Motoko,
    }
  | Unit => {
      syntax: "()",
      meaning: "The empty tuple, and the value of anything run for its effect rather than its answer -- a `put`, an `ignore`, a block that ends in a declaration.",
      origin: Motoko,
    }
  | QuotedId => {
      syntax: "`name    `1    `group(`member)    ``(1 - `element)",
      meaning: "A symbol: Fumola's notion of a name. Symbols name cells, thunks, spaces and times, and they are first-order data -- a program can build one, pass it around, and compare it, which is what makes naming something a program can compute rather than a thing the language does behind it.\n\nThey nest, and the nesting is structure rather than decoration: `` `group(`member) `` is an application of one symbol to another and is not the same symbol as the text \"group(member)\". A doubled backtick quotes a whole term as a name, so ``` ``(1 - `element) ``` names *that expression* and is deliberately a different symbol from the one `` 1 - `element `` evaluates to.\n\nNumbers used as symbols are ordered; names are not, and that is on purpose -- unordered names express independence, which is what lets two parts of a computation be incomparable rather than merely unequal.",
      origin: Fumola,
    }
  | Prim => {
      syntax: "prim \"adaptonNow\"    prim \"adaptonPeekHistory\"",
      meaning: "A primitive, named by its text. This is the floor of the language: the operations the runtime implements itself, reached by name rather than by syntax, and the layer the `Adapton` module in `fumola/system/adapton.fumola` is written on top of.\n\nMost programs should call that module instead. Reaching for `prim` directly is for the operations that have no wrapper yet, and for the panel's own questions -- `adaptonPeekHistory` is how this editor reads the store it is showing you.",
      origin:
        Overloaded(
          "Motoko has a `prim` escape too, for the same purpose of naming runtime operations, but the set of primitives behind it is entirely Fumola's -- most of them are about the adapton store, which Motoko has nothing of.",
        ),
    }
  /* -- structure --------------------------------------------------------- */
  | Parens => {
      syntax: "( e )",
      meaning: "Grouping. It changes what binds to what and nothing else; the term inside is the term that runs.\n\nOne place it is not optional: a navigation takes a nullary expression, so the time in `do goto time (`hazel(1)) { ... }` needs its parentheses. Without them the parser stops.",
      origin: Motoko,
    }
  | Tuple => {
      syntax: "(a, b)    (a, b, c)",
      meaning: "Several values as one, positionally. Read back out with `.0`, `.1`, and so on.",
      origin: Motoko,
    }
  | Block => {
      syntax: "{ let a = 1 := 42; force t }",
      meaning: "A sequence of declarations, run in order, standing as one term. Its value is the value of the last thing in it, and a block whose last item is a declaration has the value `()`.\n\nThis is the body every other form asks for when it wants more than one step: a thunk's body, a navigation's scope, a function's body.",
      origin: Motoko,
    }
  | Array => {
      syntax: "[ a, b, c ]    [var a, b]",
      meaning: "A sequence of values of one type, indexed with `e[i]` from zero. `[var ...]` makes one whose elements can be assigned.",
      origin: Motoko,
    }
  /* -- application and access -------------------------------------------- */
  | Ap => {
      syntax: "f x    f<T> x    Adapton.now()",
      meaning: "Applying a function to an argument. Juxtaposition is the operator -- there is no separate call syntax -- so `f (1, 2)` passes one tuple, which is how a function of several arguments is written.",
      origin: Motoko,
    }
  | Proj => {
      syntax: "e.0    e.name",
      meaning: "Taking one component out of a tuple by position, or one field out of a record by name.",
      origin: Motoko,
    }
  | Index => {
      syntax: "e[i]",
      meaning: "Taking one element out of an array, counting from zero.",
      origin: Motoko,
    }
  | Bang => {
      syntax: "e!",
      meaning: "Unwraps a present option and gives its contents. If the option is `null` it does not fail here: it abandons the enclosing `do ? { ... }` block, which is then `null` as a whole. That makes a run of `!`s read as a sequence of steps any one of which may stop, without a test after each.",
      origin: Motoko,
    }
  /* -- operators --------------------------------------------------------- */
  | Variant => {
      syntax: "#simple    #graphical    #ok(42)",
      meaning: "A tagged value: a tag on its own, or a tag carrying one term. Variants are structural rather than declared -- writing `#graphical` does not require a type to have announced it first -- which is why the runtime can answer with `#simple` or `#graphical` and a caller can read it without a shared declaration.\n\nThese are the words `adaptonReset` takes and `adaptonMode` answers with.",
      origin: Motoko,
    }
  | Opt => {
      syntax: "?e",
      meaning: "The present option carrying `e`. `null` is the absent one, `e!` takes this apart, and `do ? { ... }` is where a failed `!` lands.",
      origin: Motoko,
    }
  | UnOp => {
      syntax: "-e    +e    ^e",
      meaning: "A prefix operator on one term: arithmetic negation, and bitwise complement.",
      origin: Motoko,
    }
  | Not => {
      syntax: "not e",
      meaning: "Boolean negation, written as a word rather than a symbol.",
      origin: Motoko,
    }
  | Unquote => {
      syntax: "~e    ~x",
      meaning: "Splices a value into a quoted term. Inside a quotation the surrounding text is being *built* rather than run, and `~` is the hole in it: the term under the tilde is evaluated now, and its value becomes part of the term being made.\n\nThis is what makes symbols computable rather than literal -- a name assembled from a loop variable is a quotation with an unquote in it.",
      origin: Fumola,
    }
  | BinOp => {
      syntax: "a + b    a - b    a * b    a / b    a % b    a # b    a | b    a & b",
      meaning: "Arithmetic and bitwise operators, and `#` for concatenation. The `%`-suffixed forms -- `+%`, `-%`, `*%` -- wrap on overflow instead of trapping.\n\nAn operator the runtime did not reduce stays in the value as structure: `` 1 - `element `` is a perfectly good symbol, and most of the names the level-tree and library examples mint have exactly that shape.",
      origin: Motoko,
    }
  | RelOp => {
      syntax: "a == b    a != b    a < b    a <= b    a > b    a >= b",
      meaning: "Comparison, giving a boolean.",
      origin: Motoko,
    }
  | And => {
      syntax: "a and b",
      meaning: "Conjunction, short-circuiting: `b` is not run when `a` is false. In a language with a store that is a statement about effects, not only about speed.",
      origin: Motoko,
    }
  | Or => {
      syntax: "a or b",
      meaning: "Disjunction, short-circuiting: `b` is not run when `a` is true.",
      origin: Motoko,
    }
  /* -- control ----------------------------------------------------------- */
  | If => {
      syntax: "if c { ... }    if c { ... } else { ... }",
      meaning: "A choice on a boolean. With no `else`, the missing branch is `()`, so the whole form is a statement rather than a value.",
      origin: Motoko,
    }
  | Switch => {
      syntax: "switch e { case #a x { ... }; case _ { ... } }",
      meaning: "Matching a value against patterns in order, taking the first that fits. This is how a variant is taken apart -- including the `#simple` / `#graphical` a mode query answers with.",
      origin: Motoko,
    }
  | Case => {
      syntax: "case <pattern> { ... }",
      meaning: "One arm of a switch: a pattern, and what to do when it matches. The pattern's variables are bound in the arm's body and nowhere else.",
      origin: Motoko,
    }
  | Assert => {
      syntax: "assert e",
      meaning: "Checks that `e` is true and fails the program if it is not. Its value is `()`, so it is written for its effect.",
      origin: Motoko,
    }
  | Ignore => {
      syntax: "ignore e",
      meaning: "Runs `e` for whatever it does and discards its value, giving `()`. It exists so that discarding is written down rather than inferred from context -- which matters most where the value being dropped came out of the store.",
      origin: Motoko,
    }
  | Return => {
      syntax: "return    return e",
      meaning: "Leaves the enclosing function with a value, or with `()` when no value is written.",
      origin: Motoko,
    }
  /* -- the adapton core -------------------------------------------------- */
  | Thunk => {
      syntax: "thunk { (@ a) + (@ b) }",
      meaning: "A suspended block, stored rather than run. The thunk becomes a node in the graph, and the reads it performs when it is eventually forced become that node's outgoing edges -- which is what lets the runtime know, later, exactly which thunks a change can reach.\n\nA thunk is not a closure with different spelling. A closure remembers values; a thunk is a *place in the graph* that remembers how it was computed, so that it can be repaired rather than recomputed when what it read changes.",
      origin: Fumola,
    }
  | Force => {
      syntax: "force t",
      meaning: "Demands a thunk's value. If it has never run, it runs, and its reads are recorded as it goes. If it has run and nothing it read has changed, the recorded answer is given back without running anything. If something it read has changed, it is repaired -- which may be cheaper than rerunning it, because repair follows only the parts the change actually reaches.\n\nThe choice between those three is the whole of incremental computation, and it is made here rather than by the programmer.",
      origin: Fumola,
    }
  | Put => {
      syntax: "`a := 42    1 := 1111",
      meaning: "Writes a value into the cell named by the symbol on the left, creating the cell if this is the first write. The cell is a node in the graph; writing to one that has been read is what starts a repair.\n\nThe left side is a *symbol*, not a variable. `` `a := 42 `` does not assign to anything lexically bound -- it names a place in the store, and the same symbol in another program, or after a reload, names the same place.",
      origin:
        Overloaded(
          "in Motoko `x := e` assigns to a mutable variable and the left side is a location the compiler knows. Here the left side is a name in the store, the write is visible to anything that read it, and it is what makes the graph move.",
        ),
    }
  | Get => {
      syntax: "@ a    @ `cell    (@ a) + (@ b)",
      meaning: "Reads the cell or thunk the symbol names. Inside a thunk this is what gets recorded: the edge from the thunk to what it read, which is how a later write knows whom to tell.\n\nSo `@` is never only a read. Outside a thunk it fetches a value; inside one it also declares a dependency, and that second half is the reason the graph exists.",
      origin: Fumola,
    }
  | DoPutForce => {
      syntax: "do @ `t { ... }",
      meaning: "Puts a thunk under the given symbol and forces it, in one step. The common shape -- name a computation, then demand it -- written once rather than as a `:=` followed by a `force` that has to repeat the name.",
      origin: Fumola,
    }
  | DoGoto => {
      syntax: "do goto time (`hazel(3)) { ... }    do goto space (`region) { ... }",
      meaning: "Runs the block at an *absolute* point in the store's coordinates, in one of its two dimensions.\n\nSpace and time are both part of a node's identity, so this is not a mode or a setting -- it changes which nodes the block's writes and reads are about. Time is ordered where the symbols naming it are ordered, and a read at one time answers the write at the greatest comparable earlier time, which is what makes a sequence of times a revision history rather than a set of separate stores. Symbols that are not comparable express independence, deliberately: two parts of a computation that cannot see each other.\n\nThe dimension word (`time`, `space`) is an ordinary identifier rather than a reserved word, so that `` `time `` remains available as a symbol.",
      origin: Fumola,
    }
  | DoWithin => {
      syntax: "do within time (`step) { ... }    do within space (`sub) { ... }",
      meaning: "The same, relative. `goto` replaces the current coordinate; `within` extends it, so the block runs at a point nested inside where it already was. Nesting is what lets a subcomputation have its own region of the store without having to know the whole path to it.",
      origin: Fumola,
    }
  /* -- declarations ------------------------------------------------------ */
  | Exp => {
      syntax: "<any term, as a step in a block>",
      meaning: "An expression standing where a declaration may: a step run for its effect, or, if it is the last one, for the block's value. This is the form that lets `force t` be the final line of a block without a `let` in front of it.",
      origin: Motoko,
    }
  | Let => {
      syntax: "let a = 1 := 42;    let (x, y) = pair;",
      meaning: "Binds the value of a term to a pattern, for the rest of the block. Immutable: the name goes on meaning that value.\n\nWatch what is bound in `let a = 1 := 42`. The `:=` writes to the cell named `1` and the `let` binds `a` to a pointer to that cell -- not to 42. Reading it back takes an `@`.",
      origin: Motoko,
    }
  | Var_ => {
      syntax: "var x = 0;",
      meaning: "Binds a name to a value and allows it to be reassigned later. Ordinary mutable state, private to the program that runs -- it is not a cell, nothing depends on it, and nothing is repaired when it changes.\n\nThe distinction is the one worth carrying away: a `var` is memory, a cell is a node in the graph.",
      origin: Motoko,
    }
  | Func => {
      syntax: "func f (x) { ... }    func f (x) = e",
      meaning: "Declares a function, with a block body or a single expression. Ordinary abstraction: calling one runs its body, and nothing about calling it is recorded in the graph.\n\nA function is not a thunk. If what you want is a computation the store can remember and repair, name it with `thunk` and demand it with `force`.",
      origin: Motoko,
    }
  | Import => {
      syntax: "import \"fumola/collections/levelTree\"",
      meaning: "Brings in a module by path, as text. The path names a packaged module that ships with the runtime, so a program can reach the collections and examples without anything on disk.",
      origin:
        Overloaded(
          "Motoko's `import` names a file relative to the package being compiled. Here it resolves against modules built into the runtime, which is why it works in a browser with no file system.",
        ),
    }
  /* -- patterns ---------------------------------------------------------- */
  | PatVar => {
      syntax: "x",
      meaning: "Matches anything and binds it to this name, for the scope the pattern governs.",
      origin: Motoko,
    }
  | PatWild => {
      syntax: "_",
      meaning: "Matches anything and binds nothing. Written where a value is deliberately not used, so that the omission is visible rather than inferred from an unused name.",
      origin: Motoko,
    }
  | PatLit => {
      syntax: "42    \"hello\"    true    null",
      meaning: "Matches exactly this value and binds nothing.",
      origin: Motoko,
    }
  | PatParens => {
      syntax: "( p )",
      meaning: "Grouping in a pattern. It changes what belongs to what and nothing else.",
      origin: Motoko,
    }
  | PatTuple => {
      syntax: "(x, y)",
      meaning: "Matches a tuple and takes it apart positionally, binding each component.",
      origin: Motoko,
    }
  | PatVariant => {
      syntax: "#simple    #ok(x)",
      meaning: "Matches a variant by tag, and matches its payload against the inner pattern when it carries one. This is the arm shape a `switch` over `#simple` / `#graphical` is written with.",
      origin: Motoko,
    }
  | PatOpt => {
      syntax: "?x",
      meaning: "Matches a present option and binds what it carries. `null` matches the absent one.",
      origin: Motoko,
    }
  };

/* The inspector's text: the syntax that builds the form, what it does, and
   where a Motoko reader should place it.

   The syntax goes in a fenced block rather than an inline span because a
   symbol starts with a backtick -- `` `a := 42 `` inside single backticks
   closes the span on its own first character, which rendered the put form as
   mangled text with a stray backtick trailing it. A fence has no such
   quarrel with its contents. */
let doc = (cls: FumolaCls.t): string => {
  let {syntax, meaning, origin} = entry_of(cls);
  "```\n" ++ syntax ++ "\n```\n\n" ++ meaning ++ "\n\n" ++ origin_line(origin);
};
