open Lwt;

module type S = {
  module Request: WebWorker.Serializable;
  module Response: WebWorker.Serializable;

  type t;

  let init: (~timeout: int, ~max: int) => t;
  let request: (t, Request.t) => Lwt.t(option(Response.t));
};

module Make = (W: WebWorker.S) => {
  open W;

  module Request = W.Request;
  module Response = W.Response;

  type t = {
    timeout: int,
    pool: TimedLwtPool.t(Client.t),
  };

  let create = () => Client.init() |> return;

  let dispose = client => Lwt.wrap(() => Client.terminate(client));

  let validate = _client => Lwt.return_true;
  let check = _client => Lwt.return_true;

  let init = (~timeout, ~max) => {
    pool: TimedLwtPool.init(~max, ~create, ~validate, ~check, ~dispose),
    timeout,
  };

  let request = ({pool, timeout}: t, req: Request.t) =>
    TimedLwtPool.use(
      pool,
      timeout,
      client => {
        let (client, res) = req |> Client.request(client);
        (res, client);
      },
    );
};
