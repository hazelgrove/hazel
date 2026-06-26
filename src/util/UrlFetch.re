/* Raw-text URL GET, installed by each frontend at startup (the web uses
   XmlHttpRequest, node frontends use https; both via ApiHttp). The pure
   default keeps this library free of platform/DOM dependencies and simply
   reports that no transport is available. Mirrors TimeUtil.now_ms.

   [on_done] receives Ok(body) with the raw response text on a 2xx response,
   or Error(message) on a network/transport error or non-2xx status. */
let get:
  ref((~url: string, ~on_done: result(string, string) => unit) => unit) =
  ref((~url as _, ~on_done) =>
    on_done(Error("URL fetch unavailable in this environment"))
  );
