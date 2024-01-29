%{
open QueryAst
%}

%token <int> DIGITS
%token <QueryAst.text_object_key> TEXT_OBJECT_KEY
%token <QueryAst.action_key> ACTION_KEY
%token <QueryAst.query_decoration> QUERY_DECORATION
%token <QueryAst.query_key> QUERY_KEY
%token TEXT_OBJECT_INNER
%token TEXT_OBJECT_QUERY
%token EOF

%start <QueryAst.command> main

%%

main:
  | action EOF { Action($1) }
  | query EOF { Query($1) }

action:
  | text_object ACTION_KEY { ($1, $2) }

text_object:
  | TEXT_OBJECT_INNER TEXT_OBJECT_KEY { Inner($2) }
  | TEXT_OBJECT_QUERY query { Queried($2) }

query:
  | text_object query_op { ($1, $2) }

query_op:
  | QUERY_KEY { (None, $1) }
  | QUERY_DECORATION QUERY_KEY { (Some($1), $2) }
