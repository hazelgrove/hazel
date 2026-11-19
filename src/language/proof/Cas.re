/* The computer algebra system the checker verifies `algebra` steps with:
 * Algebrite's `run`, which evaluates an Algebrite expression to its
 * printed result, or None while no Algebrite is loaded. Whatever JS
 * context loads Algebrite sets it (the page, the worker, the test
 * runner). */
let run: ref(string => option(string)) = ref(_ => None);
