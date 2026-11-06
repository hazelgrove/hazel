/* The frame's side of its conversation with the page around it (embed
   mode), free of the DOM: the page's window, the frame's own origin and
   the function that posts to the page are given, so a test can stand in
   for all three (test/Test_Embed.re).

   A message is taken only from the parent window, and only from an
   origin EmbedProtocol.origin_allowed allows. Once one is taken, the
   frame posts only to that message's origin. Before that it posts ready
   and crash, which carry no program, to any origin ("*"). */
open Js_of_ocaml;

type t = {
  /* the window messages are taken from and posted to: the frame's
     parent */
  parent: Js.Unsafe.any,
  own_origin: string,
  /* post(data, target origin) to the parent */
  post: (Js.Unsafe.any, string) => unit,
  /* the origin of the last message taken from the page */
  mutable page_origin: option(string),
  /* the size and status the page has been told */
  mutable sent: EmbedModel.Outbox.sent,
  /* the crashes already posted, each posted once */
  mutable crashes: list(string),
  /* (ticket 578) a cell the page can edit, when it last asked: the frame
     takes it after the message (EmbedMain) */
  mutable editable: option(bool),
  /* and the module it asked to see alone (Some(None): the whole program) */
  mutable focus: option(option(string)),
  /* and the theme it asked for: (dark, high contrast) */
  mutable theme: option((bool, bool)),
  /* and whether it asked for line numbers */
  mutable lines: option(bool),
  /* and a name it asked to see */
  mutable reveal: option(string),
  /* and the font it asked for: (size in px, family) */
  mutable font: option((option(float), option(string))),
};

let make = (~parent, ~own_origin, ~post): t => {
  parent,
  own_origin,
  post,
  page_origin: None,
  sent: EmbedModel.Outbox.nothing_sent,
  crashes: [],
  editable: None,
  focus: None,
  theme: None,
  lines: None,
  reveal: None,
  font: None,
};

let send = (link: t, ~target: string, message: EmbedModel.Outbox.message) =>
  link.post(EmbedProtocol.encode(message), target);

/* to the page's origin once a message is taken from it, and before that
   to any */
let ready = (link: t): unit =>
  send(
    link,
    ~target=Option.value(link.page_origin, ~default="*"),
    Ready(BuildStamp.commit),
  );

let crash = (link: t, message: string): unit =>
  if (!List.mem(message, link.crashes)) {
    link.crashes = [message, ...link.crashes];
    try(
      send(
        link,
        ~target=Option.value(link.page_origin, ~default="*"),
        Crash(message),
      )
    ) {
    | _ => ()
    };
  };

/* A message event: the program it sets, when it is a set from the parent
   window and an allowed origin. A ping from there is answered with ready,
   and the size and status are posted again after the next display, for a
   page that missed them. Anything else is ignored. */
let receive = (link: t, event: Js.Unsafe.any): option(EmbedModel.Program.t) => {
  let origin = Js.Unsafe.get(event, "origin");
  if (Js.to_string(Js.typeof(origin)) == "string"
      && Js.Unsafe.equals(Js.Unsafe.get(event, "source"), link.parent)
      && EmbedProtocol.origin_allowed(
           ~own=link.own_origin,
           Js.to_string(origin),
         )) {
    let origin = Js.to_string(origin);
    switch (EmbedProtocol.decode(Js.Unsafe.get(event, "data"))) {
    | Some(Set(program)) =>
      link.page_origin = Some(origin);
      Some(program);
    | Some(Font(size, family)) =>
      link.page_origin = Some(origin);
      link.font = Some((size, family));
      None;
    | Some(Reveal(name)) =>
      link.page_origin = Some(origin);
      link.reveal = Some(name);
      None;
    | Some(Lines(on)) =>
      link.page_origin = Some(origin);
      link.lines = Some(on);
      None;
    | Some(Theme(dark, contrast)) =>
      link.page_origin = Some(origin);
      link.theme = Some((dark, contrast));
      None;
    | Some(Focus(name)) =>
      link.page_origin = Some(origin);
      link.focus = Some(name);
      None;
    | Some(Editable(on)) =>
      link.page_origin = Some(origin);
      link.editable = Some(on);
      None;
    | Some(Ping) =>
      link.page_origin = Some(origin);
      link.sent = EmbedModel.Outbox.nothing_sent;
      ready(link);
      None;
    | None => None
    };
  } else {
    None;
  };
};

/* After a display: the size (the height of the frame's content) and the
   status, each posted when it changed, once the page has sent a message */
let report =
    (link: t, ~caret: option(int)=?, ~height: int, model: EmbedModel.Model.t)
    : unit =>
  switch (link.page_origin) {
  | Some(target) =>
    let (messages, sent) =
      EmbedModel.Outbox.after_display(~caret?, ~height, model, link.sent);
    link.sent = sent;
    List.iter(send(link, ~target), messages);
  | None => ()
  };
