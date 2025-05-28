open Util;

/* A syntax probe is inserted into syntax to capture
 * information during evaluation. The tag type below,
 * for the probe case, is used to collect binding ids
 * which are used to faciltate capturing intermediate
 * dynamic data such as environment variables and
 * callstack state. Probes are created during maketerm
 * and can be intercepted duting elaboration to add
 * static information to inform dynamic information
 * capture (such as as restricting environment variable
 * capture to variables actually referenced. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {refs: Binding.s};

/* A call at stack represented by function ap ids */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type call_stack = list(Id.t);

let empty: t = {refs: []};
