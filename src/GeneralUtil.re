let force_opt opt =>
  switch opt {
  | Some x => x
  | _ => assert false
  };
