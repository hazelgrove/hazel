type t('a) = list(TermBase.StepperFilterKind.filter('a));
let extends = (flt, env) => [flt, ...env];
