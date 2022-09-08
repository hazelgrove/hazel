module type S = {
  module Request: WebWorker.Serializable;
  module Response: WebWorker.Serializable;

  type t;

  let init: (~timeout: int, ~max: int) => t;
  let add: t => Lwt.t(bool);
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

  let add = ({pool, _}: t) => Lwt_timed_pool_js.add(pool);

  let fill = ({pool, _}: t) => Lwt_timed_pool_js.fill(pool);

  let request = ({pool, timeout}: t, req: Request.t) =>
    Lwt_timed_pool_js.use(pool, timeout, client => req |> W.request(client));
};
