open Lwt.Syntax;

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
    pool: TimedLwtPool.t(Client.t),
  };

  let create = () => Lwt.wrap(Client.init);

  let dispose = client => Lwt.wrap(() => Client.terminate(client));

  let validate = _client => Lwt.return_true;
  let check = _client => Lwt.return_true;

  let init = (~timeout, ~max) => {
    let pool = TimedLwtPool.init(~max, ~create, ~validate, ~check, ~dispose);
    {pool, timeout};
  };

  let fill = ({pool, _}, count) => {
    let rec fill =
      fun
      | 0 => Lwt.return_unit
      | n => {
          let* created = TimedLwtPool.add(pool);
          if (created) {
            fill(n - 1);
          } else {
            Lwt.return_unit;
          };
        };

    fill(count);
  };

  let request = ({pool, timeout}: t, req: Request.t) =>
    TimedLwtPool.use(pool, timeout, client => req |> Client.request(client));
};
