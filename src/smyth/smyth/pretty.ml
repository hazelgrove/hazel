open Lang

(* Parameters *)

let indent_size : int = 2

let max_inner : int = 30

(* Pretty printing types *)

type state = {indent: int; app_needs_parens: bool; fancy_needs_parens: bool}

type 'a printer = state -> 'a -> string

(* Helpers *)

let make_indent : int -> string = fun n -> String.make (indent_size * n) ' '

(* Collections *)

type paren_type = Round | Square | Angle

let left_paren : paren_type -> string = function
  | Round -> "("
  | Square -> "["
  | Angle -> "<"

let right_paren : paren_type -> string = function
  | Round -> ")"
  | Square -> "]"
  | Angle -> ">"

let collection : paren_type -> state -> 'a printer -> 'a list -> string =
 fun paren_type state print comps ->
  let comp_strings =
    List.map
      (print
         { indent= state.indent + 1
         ; app_needs_parens= false
         ; fancy_needs_parens= false })
      comps
  in
  let inside_len = comp_strings |> List.map String.length |> List2.sum in
  let contains_newline =
    comp_strings |> List.exists (fun s -> String.contains s '\n')
  in
  let left, sep, right =
    if contains_newline || inside_len > max_inner then
      let indent = make_indent state.indent in
      ( left_paren paren_type ^ " "
      , "\n" ^ indent ^ ", "
      , "\n" ^ indent ^ right_paren paren_type )
    else (left_paren paren_type, ", ", right_paren paren_type)
  in
  left ^ String.concat sep comp_strings ^ right

let wrapped_poly : string -> string = fun s -> "<" ^ s ^ ">"

(* Applications *)

let application :
    state -> 'head printer -> 'head -> 'arg printer -> 'arg -> string =
 fun state print_head head print_arg arg ->
  let head_string =
    print_head
      { indent= state.indent
      ; app_needs_parens= false
      ; fancy_needs_parens= true }
      head
  in
  let arg_string =
    print_arg
      { indent= state.indent + 1
      ; app_needs_parens= true
      ; fancy_needs_parens= true }
      arg
  in
  let sep =
    if String.contains arg_string '\n' then
      "\n" ^ make_indent (state.indent + 1)
    else " "
  in
  let inner = head_string ^ sep ^ arg_string in
  if state.app_needs_parens then "(" ^ inner ^ ")" else inner

(* Patterns *)

let rec pat' : pat printer =
 fun state -> function
  | PVar name -> name
  | PTuple comps -> collection Round state pat' comps
  | PWildcard -> "_"

let pat : pat -> string =
  pat' {indent= 0; app_needs_parens= false; fancy_needs_parens= false}

(* Types *)

let rec typ' : typ printer =
 fun state -> function
  | TArr (input, output) ->
      let inner =
        typ'
          { indent= state.indent
          ; app_needs_parens= false
          ; fancy_needs_parens= true }
          input
        ^ " -> "
        ^ typ'
            { indent= state.indent
            ; app_needs_parens= false
            ; fancy_needs_parens= false }
            output
      in
      if state.fancy_needs_parens then "(" ^ inner ^ ")" else inner
  | TTuple comps -> collection Round state typ' comps
  | TData (name, type_args) ->
      let type_args_string =
        if type_args = [] then ""
        else " " ^ String.concat " " (List.map (typ' state) type_args)
      in
      name ^ type_args_string
  | TForall (x, t) -> "forall " ^ x ^ ". " ^ typ' state t
  | TVar x -> x

let typ : typ -> string =
  typ' {indent= 0; app_needs_parens= false; fancy_needs_parens= false}

(* Expressions *)

let rec try_sugar : state -> exp -> string option =
 fun state exp ->
  match Sugar.nat exp with
  | Some n -> Some (string_of_int n)
  | None -> (
    match Sugar.listt exp with
    | Some (exp_list, []) -> Some (collection Square state exp' exp_list)
    | Some (exp_list, type_args) ->
        Some
          ( collection Square state exp' exp_list
          ^ collection Angle state typ' type_args )
    | None -> None )

and exp' : exp printer =
 fun state exp ->
  match try_sugar state exp with
  | Some sugar -> sugar
  | None -> (
    match exp with
    | EFix (rec_name_opt, PatParam param_pat, body) ->
        let lambda =
          "\\"
          ^ pat'
              { indent= state.indent
              ; app_needs_parens= true
              ; fancy_needs_parens= true }
              param_pat
          ^ " -> "
          ^ exp'
              { indent= state.indent
              ; app_needs_parens= false
              ; fancy_needs_parens= false }
              body
        in
        let inner =
          match rec_name_opt with
          | Some rec_name ->
              "let " ^ rec_name ^ " = " ^ lambda ^ " in " ^ rec_name
          | None -> lambda
        in
        if state.fancy_needs_parens then "(" ^ inner ^ ")" else inner
    | EFix (rec_name_opt, TypeParam x, body) ->
        exp' state
          (EFix (rec_name_opt, PatParam (PVar (wrapped_poly x)), body))
    | EApp (_, head, EAExp arg) -> application state exp' head exp' arg
    | EApp (_, head, EAType arg) ->
        application state exp' head
          (fun state s -> wrapped_poly (typ' state s))
          arg
    | EVar name -> name
    | ETuple comps -> collection Round state exp' comps
    | EProj (n, i, arg) ->
        application state
          (fun _ _ -> "#" ^ string_of_int n ^ "." ^ string_of_int i)
          () exp' arg
    | ECtor (ctor_name, type_args, arg) ->
        let type_args_string =
          if type_args = [] then ""
          else collection Angle state typ' type_args
        in
        let head_string = ctor_name ^ type_args_string in
        if Exp.syntactically_equal arg (ETuple []) then head_string
        else application state (fun _ _ -> head_string) () exp' arg
    | ECase (scrutinee, branches) ->
        let indent1 = make_indent (state.indent + 1) in
        let indent2 = make_indent 1 ^ indent1 in
        let print_branch (ctor_name, (param_pat, body)) =
          indent1 ^ ctor_name ^ " "
          ^ pat'
              { indent= state.indent + 1
              ; app_needs_parens= true
              ; fancy_needs_parens= true }
              param_pat
          ^ " -> \n" ^ indent2
          ^ exp'
              { indent= state.indent + 2
              ; app_needs_parens= false
              ; fancy_needs_parens= false }
              body
        in
        let inner =
          "case "
          ^ exp'
              { indent= state.indent
              ; app_needs_parens= false
              ; fancy_needs_parens= false }
              scrutinee
          ^ " of\n"
          ^ String.concat "\n\n" (List.map print_branch branches)
        in
        if state.fancy_needs_parens then "(" ^ inner ^ ")" else inner
    | EHole name -> "?" ^ string_of_int name
    | EAssert (_, _) -> "{ASSERTION}"
    | ETypeAnnotation (the_exp, the_typ) ->
        let inner =
          exp'
            { indent= state.indent
            ; app_needs_parens= true
            ; fancy_needs_parens= true }
            the_exp
          ^ " : "
          ^ typ'
              { indent= state.indent
              ; app_needs_parens= false
              ; fancy_needs_parens= false }
              the_typ
        in
        if state.fancy_needs_parens then "(" ^ inner ^ ")" else inner )

let exp : exp -> string =
  exp' {indent= 0; app_needs_parens= false; fancy_needs_parens= false}

(* Results *)

let rec res' : res printer =
 fun state res ->
  match res with
  | RFix (_env, rec_name_opt, PatParam param_pat, body) ->
      let lambda =
        "\\"
        ^ pat'
            { indent= state.indent
            ; app_needs_parens= true
            ; fancy_needs_parens= true }
            param_pat
        ^ " -> "
        ^ exp'
            { indent= state.indent
            ; app_needs_parens= false
            ; fancy_needs_parens= false }
            body
      in
      let inner =
        match rec_name_opt with
        | Some rec_name ->
            "let " ^ rec_name ^ " = " ^ lambda ^ " in " ^ rec_name
        | None -> lambda
      in
      if state.fancy_needs_parens then "(" ^ inner ^ ")" else inner
  | RFix (_env, rec_name_opt, TypeParam x, body) ->
      res' state
        (RFix (_env, rec_name_opt, PatParam (PVar (wrapped_poly x)), body))
  | RApp (head, RARes arg) -> application state res' head res' arg
  | RApp (head, RAType arg) ->
      application state res' head
        (fun state s -> wrapped_poly (typ' state s))
        arg
  | RTuple comps -> collection Round state res' comps
  | RProj (n, i, arg) ->
      application state
        (fun _ _ -> "#" ^ string_of_int n ^ "." ^ string_of_int i)
        () res' arg
  | RCtor (ctor_name, arg) ->
      application state (fun _ _ -> ctor_name) () res' arg
  | RCase (_env, scrutinee, branches) ->
      let indent1 = make_indent (state.indent + 1) in
      let indent2 = make_indent 1 ^ indent1 in
      let print_branch (ctor_name, (param_pat, body)) =
        indent1 ^ ctor_name ^ " "
        ^ pat'
            { indent= state.indent + 1
            ; app_needs_parens= true
            ; fancy_needs_parens= true }
            param_pat
        ^ " -> \n" ^ indent2
        ^ exp'
            { indent= state.indent + 2
            ; app_needs_parens= false
            ; fancy_needs_parens= false }
            body
      in
      let inner =
        "case "
        ^ res'
            { indent= state.indent
            ; app_needs_parens= false
            ; fancy_needs_parens= false }
            scrutinee
        ^ " of\n"
        ^ String.concat "\n\n" (List.map print_branch branches)
      in
      if state.fancy_needs_parens then "(" ^ inner ^ ")" else inner
  | RHole (_env, name) -> "?" ^ string_of_int name
  | RCtorInverse (ctor_name, arg) ->
      application state (fun _ _ -> ctor_name ^ "-1") () res' arg

let res : res -> string =
  res' {indent= 0; app_needs_parens= false; fancy_needs_parens= false}

(* Patterns *)

let rec value' : value printer =
 fun state -> function
  | VTuple comps -> collection Round state value' comps
  | VCtor (ctor_name, arg) ->
      application state (fun _ _ -> ctor_name) () value' arg

let value : value -> string =
  value' {indent= 0; app_needs_parens= false; fancy_needs_parens= false}
