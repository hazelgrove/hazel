open API;

let request = handler =>
  request(
    ~with_credentials=true,
    ~method=GET,
    ~url="https://weatherapi-com.p.rapidapi.com/current.json?q=Ann%20arbor",
    ~headers=[
      (
        "X-RapidAPI-Key",
        "50adacef2cmsh8f5a12552751084p108b3fjsn4fb9ac1dab1d",
      ),
      ("X-RapidAPI-Host", "weatherapi-com.p.rapidapi.com"),
    ],
    handler,
  );

let handle = (response: option(Json.t)): option(string) =>
  switch (response) {
  | Some(json) =>
    json
    |> Json.dot("current")
    |> opt(Json.dot("condition"))
    |> opt(Json.dot("text"))
    |> opt(Json.str)
  | _ => None
  };
