module HazelResult = Result;

let get_result_promise: Program.t => Lwt.t(HazelResult.t);
