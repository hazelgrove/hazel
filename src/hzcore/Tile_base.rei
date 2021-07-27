type t('op, 'pre, 'post, 'bin) =
  | Op('op) // operand
  | Pre('pre) // unary prefix operator
  | Post('post) // unary postfix operator
  | Bin('bin); // binary infix operator

let get_op: t('op, _, _, _) => 'op;
let get_pre: t(_, 'pre, _, _) => 'pre;
let get_post: t(_, _, 'post, _) => 'post;
let get_bin: t(_, _, _, 'bin) => 'bin;
