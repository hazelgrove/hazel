/* converts exp to string -- useful for debugging */
let string_of_exp: UHExp.t => string;

let string_of_zexp: ZExp.t => string /* parses the given string as an SExp encoding a UHExp   (does not do hole fixing) */;

let exp_of_string: string => UHExp.t /* exp_of_string followed by hole fixing */;

let fixed_exp_of_string: string => UHExp.t /* feel free to add other similar helper functions here */;
