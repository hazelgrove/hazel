open Lang;

let empty: env = (([], []): env);

let all_res: env => list((string, res)) = (
  ((rs, _)) => rs: env => list((string, res))
);

let all_type: env => list((string, typ)) = (
  ((_, ts)) => ts: env => list((string, typ))
);

let concat: list(env) => env = (
  envs => {
    let (rss, tss) = List.split(envs);

    (List.concat(rss), List.concat(tss));
  }:
    list(env) => env
);

let add_res: ((string, res), env) => env = (
  (b, (rs, ts)) => ([b, ...rs], ts): ((string, res), env) => env
);

let concat_res: (list((string, res)), env) => env = (
  (b, (rs, ts)) => (b @ rs, ts): (list((string, res)), env) => env
);

let add_type: ((string, typ), env) => env = (
  (b, (rs, ts)) => (rs, [b, ...ts]): ((string, typ), env) => env
);

let concat_type: (list((string, typ)), env) => env = (
  (b, (rs, ts)) => (rs, b @ ts): (list((string, typ)), env) => env
);
