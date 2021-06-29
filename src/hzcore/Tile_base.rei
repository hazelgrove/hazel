type t('op, 'pre, 'post, 'bin) =
  | Op('op) // _op_erand
  | Pre('pre) // unary _pre_fix operator
  | Post('post) // unary _post_fix operator
  | Bin('bin); // _bin_ary infix operator

