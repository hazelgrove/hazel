/** Example helpers and "ground truth" example satisfaction. */;

open Lang;

/** "Upcasts" a simple value to an example. */

let from_value: value => example;

/** Example satisfaction as defined in {b Figure 5} of the ICFP 2020 paper. */

let res_satisfies: (hole_filling, res, example) => bool;

/** Example constraint satisfaction as defined in {b Figure 5} of the
    ICFP 2020 paper. */

let exp_satisfies: (hole_filling, exp, worlds) => bool;
