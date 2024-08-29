[@deriving (show({with_path: false}), sexp, yojson)]
type op_un_bool =
  | Not;

[@deriving (show({with_path: false}), sexp, yojson)]
type op_un_meta =
  | Unquote;

[@deriving (show({with_path: false}), sexp, yojson)]
type op_un_int =
  | Minus;

[@deriving (show({with_path: false}), sexp, yojson)]
type op_bin_bool =
  | And
  | Or;

[@deriving (show({with_path: false}), sexp, yojson)]
type op_bin_int =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, yojson)]
type op_bin_float =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, yojson)]
type op_bin_string =
  | Concat
  | Equals;

[@deriving (show({with_path: false}), sexp, yojson)]
type op_bin_prop =
  | And
  | Or
  | Implies;

[@deriving (show({with_path: false}), sexp, yojson)]
type op_un =
  | Meta(op_un_meta)
  | Int(op_un_int)
  | Bool(op_un_bool);

[@deriving (show({with_path: false}), sexp, yojson)]
type op_bin =
  | Int(op_bin_int)
  | Float(op_bin_float)
  | Bool(op_bin_bool)
  | String(op_bin_string);

[@deriving (show({with_path: false}), sexp, yojson)]
type ap_direction =
  | Forward
  | Reverse;

// Are these show function necessary?
let show_op_un_meta: op_un_meta => string =
  fun
  | Unquote => "Un-quotation";

let show_op_un_bool: op_un_bool => string =
  fun
  | Not => "Boolean Negation";

let show_op_un_int: op_un_int => string =
  fun
  | Minus => "Integer Negation";

let show_unop: op_un => string =
  fun
  | Meta(op) => show_op_un_meta(op)
  | Bool(op) => show_op_un_bool(op)
  | Int(op) => show_op_un_int(op);

let show_op_bin_bool: op_bin_bool => string =
  fun
  | And => "Boolean Conjunction"
  | Or => "Boolean Disjunction";

let show_op_bin_int: op_bin_int => string =
  fun
  | Plus => "Integer Addition"
  | Minus => "Integer Subtraction"
  | Times => "Integer Multiplication"
  | Power => "Integer Exponentiation"
  | Divide => "Integer Division"
  | LessThan => "Integer Less Than"
  | LessThanOrEqual => "Integer Less Than or Equal"
  | GreaterThan => "Integer Greater Than"
  | GreaterThanOrEqual => "Integer Greater Than or Equal"
  | Equals => "Integer Equality"
  | NotEquals => "Integer Inequality";

let show_op_bin_float: op_bin_float => string =
  fun
  | Plus => "Float Addition"
  | Minus => "Float Subtraction"
  | Times => "Float Multiplication"
  | Power => "Float Exponentiation"
  | Divide => "Float Division"
  | LessThan => "Float Less Than"
  | LessThanOrEqual => "Float Less Than or Equal"
  | GreaterThan => "Float Greater Than"
  | GreaterThanOrEqual => "Float Greater Than or Equal"
  | Equals => "Float Equality"
  | NotEquals => "Float Inequality";

let show_op_bin_string: op_bin_string => string =
  fun
  | Concat => "String Concatenation"
  | Equals => "String Equality";

let show_binop: op_bin => string =
  fun
  | Int(op) => show_op_bin_int(op)
  | Float(op) => show_op_bin_float(op)
  | Bool(op) => show_op_bin_bool(op)
  | String(op) => show_op_bin_string(op);

let bool_op_to_string = (op: op_bin_bool): string => {
  switch (op) {
  | And => "&&"
  | Or => "||"
  };
};

let int_op_to_string = (op: op_bin_int): string => {
  switch (op) {
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Power => "**"
  | Divide => "/"
  | LessThan => "<"
  | LessThanOrEqual => "<="
  | GreaterThan => ">"
  | GreaterThanOrEqual => ">="
  | Equals => "=="
  | NotEquals => "!="
  };
};

let float_op_to_string = (op: op_bin_float): string => {
  switch (op) {
  | Plus => "+."
  | Minus => "-."
  | Times => "*."
  | Power => "**."
  | Divide => "/."
  | LessThan => "<."
  | LessThanOrEqual => "<=."
  | GreaterThan => ">."
  | GreaterThanOrEqual => ">=."
  | Equals => "==."
  | NotEquals => "!=."
  };
};

let string_op_to_string = (op: op_bin_string): string => {
  switch (op) {
  | Concat => "++"
  | Equals => "$=="
  };
};
