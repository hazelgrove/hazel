let to_string: float => string = (
  f => {
    let default = string_of_float(f);

    if (default.[String.length(default) - 1] == '.') {
      default ++ "0";
    } else {
      default;
    };
  }:
    float => string
);
