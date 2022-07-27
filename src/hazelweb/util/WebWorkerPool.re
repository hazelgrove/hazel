open Lwt.Syntax;

module type S = {
  module Request: WebWorker.Serializable;
  module Response: WebWorker.Serializable;

  type t;

  let init: (~timeout: int, ~max: int) => t;
  let fill: (t, int) => Lwt.t(unit);
  let request: (t, Request.t) => Lwt.t(option(Response.t));
};

module Make = (W: WebWorker.ClientS) => {
  module Request = W.Request;
  module Response = W.Response;

  type t = {
    timeout: int,
    pool: Lwt_timed_pool_js.t(W.t),
  };

  let create = () => Lwt.wrap(W.init);

  let dispose = client => Lwt.wrap(() => W.terminate(client));

  let validate = _client => Lwt.return_true;
  let check = _client => Lwt.return_true;

  let init = (~timeout, ~max) => {
    let pool =
      Lwt_timed_pool_js.init(~max, ~create, ~validate, ~check, ~dispose);
    {pool, timeout};
  };

  let fill = ({pool, _}, count) => {
    let rec fill =
      fun
      | 0 => Lwt.return_unit
      | n => {
          let* created = Lwt_timed_pool_js.add(pool);
          if (created) {
            fill(n - 1);
          } else {
            Lwt.return_unit;
          };
        };

    fill(count);
  };

  let request = ({pool, timeout}: t, req: Request.t) =>
    Lwt_timed_pool_js.use(pool, timeout, client => req |> W.request(client));
};
