/* Renders the output of a slash command in the agent chat, as a card
   specific to the command. */

let view: Message.Model.slash_command_payload => Virtual_dom.Vdom.Node.t;
