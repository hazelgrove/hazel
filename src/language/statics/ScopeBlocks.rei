/* Scope blocks: the regions of a program a variable is in scope over.

   Every let, function and case rule that binds a variable opens a scope,
   the term its binders are in scope over: a let's body, or the whole let
   when its definition is recursive, a function's body, a rule's right-hand
   side. Each scope is a block, and so are its binders and every use that
   resolves to one of them, painted like the scope so a use shows which
   scope its name comes from. The level is the number of scopes a block is
   inside; a use that is itself a scope's whole body is one deeper than
   that scope.

   Type variables, constructors and names from outside the program (builtins,
   modules) open no scope and paint no use. */

/* Every block of the program in ascending level, so painting them in order
   puts children over parents. */
let of_map: StaticsBase.Map.t => list(Blocks.t);
