open Js_of_ocaml;
//open Virtual_dom.Vdom;

/*
 const data = null;

 const xhr = new XMLHttpRequest();
 xhr.withCredentials = true;

 xhr.addEventListener("readystatechange", function () {
 	if (this.readyState === this.DONE) {
 		console.log(this.responseText);
 	}
 });

 xhr.open("GET", "https://weatherapi-com.p.rapidapi.com/current.json?q=%3CREQUIRED%3E");
 xhr.setRequestHeader("X-RapidAPI-Key", "SIGN-UP-FOR-KEY");
 xhr.setRequestHeader("X-RapidAPI-Host", "weatherapi-com.p.rapidapi.com");

 xhr.send(data);
 */

let requestWeather = blag => {
  let xhr = Js_of_ocaml.XmlHttpRequest.create();
  xhr##.withCredentials := Js._true;

  xhr##.onreadystatechange := Js.wrap_callback(blag(xhr));
  //Js._true;

  xhr##_open(
    Js.string("GET"),
    Js.string(
      "https://weatherapi-com.p.rapidapi.com/current.json?q=Ann%20arbor",
    ),
    Js.bool(true),
  );
  // dunno what last bool arg is, picked true randomly
  xhr##setRequestHeader(
    Js.string("X-RapidAPI-Key"),
    Js.string("50adacef2cmsh8f5a12552751084p108b3fjsn4fb9ac1dab1d"),
  );
  xhr##setRequestHeader(
    Js.string("X-RapidAPI-Host"),
    Js.string("weatherapi-com.p.rapidapi.com"),
  );
  xhr##send(Js.null);
};

let processWeather = (xhr): option(string) =>
  if (xhr##.readyState == XmlHttpRequest.DONE) {
    Firebug.console##log(xhr##.responseText);
    let response_str =
      Js.Opt.case(
        xhr##.responseText,
        () => "NORESPONSE",
        x => Js.to_string(x),
      );
    let json = Yojson.Safe.from_string(response_str);
    let blah_str =
      switch (json) {
      | `Assoc([
          ("location", _),
          (
            "current",
            `Assoc([
              ("last_updated_epoch", _),
              ("last_updated", _),
              ("temp_c", _),
              ("temp_f", _),
              ("is_day", _),
              ("condition", `Assoc([("text", `String(condition)), ..._])),
              ..._,
            ]),
          ),
        ]) => condition
      | _ => "NOPE"
      };
    //print_endline("string to paste:");
    //print_endline(blah_str);
    Some("\"" ++ blah_str ++ "\"");
  } else {
    None;
  };
