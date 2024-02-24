let get_input_value = (): string => "";
let query_parser = (): QueryCommand.t => {
  let query = get_input_value();
  JsUtil.log(query);
  Select(Term);
};
