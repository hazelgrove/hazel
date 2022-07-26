open Lwt.Syntax;
open Lwtutil;

/**
  [Lwt_timed] implementation for browser.
 */
module Lwt_timed = Lwt_timed.Make(Delay);
module Lwt_timed_pool = Lwt_timed_pool.Make(Lwt_timed);

module type S = {
  module Request: WebWorker.Serializable;
  module Response: WebWorker.Serializable;

  type t;

  let init: (~timeout: int, ~max: int) => t;
  let fill: (t, int) => Lwt.t(unit);
  let request: (t, Request.t) => Lwt.t(option(Response.t));
};

module Make = (W: WebWorker.S) => {
  open W;

  module Request = W.Request;
  module Response = W.Response;

  type t = {
    timeout: int,
    pool: Lwt_timed_pool.t(Client.t),
  };

  let create = () => Lwt.wrap(Client.init);

  let dispose = client => Lwt.wrap(() => Client.terminate(client));

  let validate = _client => Lwt.return_true;
  let check = _client => Lwt.return_true;

  let init = (~timeout, ~max) => {
    let pool =
      Lwt_timed_pool.init(~max, ~create, ~validate, ~check, ~dispose);
    {pool, timeout};
  };

  let fill = ({pool, _}, count) => {
    let rec fill =
      fun
      | 0 => Lwt.return_unit
      | n => {
          let* created = Lwt_timed_pool.add(pool);
          if (created) {
            fill(n - 1);
          } else {
            Lwt.return_unit;
          };
        };

    fill(count);
  };

  let request = ({pool, timeout}: t, req: Request.t) =>
    Lwt_timed_pool.use(pool, timeout, client =>
      req |> Client.request(client)
    );
};
