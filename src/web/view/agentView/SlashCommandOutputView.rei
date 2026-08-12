/* Renders a slash command's output payload. The per-command card builders
   are private. */

let view: Message.Model.slash_command_payload => Virtual_dom.Vdom.Node.t;
