/** Result helpers. */;

open Lang;

/** Determines whether or not a result is {i final}, as defined in
    {b Figure 11} of the ICFP 2020 paper (Appendix A). */

let final: res => bool;

/** Determines whether or not a result is {i determinate}, as defined in
    {b Figure 11} of the ICFP 2020 paper (Appendix A). */

let determinate: res => bool;

/** Determines whether or not a result is {i indeterminate}, as defined in
    {b Figure 11} of the ICFP 2020 paper (Appendix A). */

let indeterminate: res => bool;

/** "Downcasts" a result to a simple value, as defined after
    {b Figure 11} of the ICFP 2020 paper (Appendix A). */

let to_value: res => option(value);

/** "Upcasts" a simple value to a result. */

let from_value: value => res;

/** [consistent r1 r2] returns the resumption assertions needed to ensure that
    [r1] and [r2] are consistent, as defined in {b Figure 7} of the ICFP 2020
    paper. */

let consistent: (res, res) => option(resumption_assertions);
