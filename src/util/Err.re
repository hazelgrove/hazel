let try_fail = (msg, a) =>
  try(a()) {
  | _ => failwith(msg)
  };
