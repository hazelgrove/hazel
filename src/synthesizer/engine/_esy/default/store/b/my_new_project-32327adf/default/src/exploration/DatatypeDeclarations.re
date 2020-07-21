type identifier = string
and ert_exp = (ert, exp)
and exp = 
  | E_Bool(bool)
  | E_Variable(identifier)
  | E_Function(identifier, ert_exp)
  | E_Application(ert_exp, ert_exp)
  | E_Hole(mutable_ert)
and value =  
  | V_Bool(bool)
  | V_Function(ert, identifier, ert_exp)
  | V_Application(mutable_ert, value)
  | V_Hole(mutable_ert)/* Do I have to prefix this if I want it to be indistinguishable from E_Hole? :( */
  | V_Fail
and type_ =
  | T_Bool
  | T_Function(type_, type_)
  | T_Any
  | T_Fail
and environment = Tools.pairlist(identifier, value)
and example = (environment, value)
and examples = list(example)
and ert = (type_, examples)
and mutable_ert = MutablePair.t(type_, examples)