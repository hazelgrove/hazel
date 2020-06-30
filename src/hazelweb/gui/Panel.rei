/* Functions related to panels embedded in sidebars */

/**
 * For title bar that appears at the top of a panel.
 */
let view_of_main_title_bar: string => Virtual_dom.Vdom.Node.t;
/**
 * For title bars that appear mid-panel.
 */
let view_of_other_title_bar: string => Virtual_dom.Vdom.Node.t;
