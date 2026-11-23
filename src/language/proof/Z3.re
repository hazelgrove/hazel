/* Z3, the solver the checker verifies `algebra` steps with. `eval` runs
 * an SMT-LIB script and gives what Z3 printed, or None while Z3 isn't
 * loaded. Whatever JS context loads Z3 sets it (the page, the worker, the
 * test runner). */
let eval: ref(string => option(string)) = ref(_ => None);
