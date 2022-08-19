include
   (module type of DH.Environment) with
    type t_('env) = DH.Environment.t_('env) and
    type t = DH.Environment.t and
    type t' = DH.Environment.t';
