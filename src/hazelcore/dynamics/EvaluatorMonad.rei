include StateMonad.S with type state = EvaluatorState.t;

let get_eig: t(EnvironmentIdGen.t);
let put_eig: EnvironmentIdGen.t => t(unit);
let with_eig: (EnvironmentIdGen.t => ('a, EnvironmentIdGen.t)) => t('a);

let take_step: t(unit);

let add_test: (KeywordID.t, TestMap.instance_report) => t(unit);
