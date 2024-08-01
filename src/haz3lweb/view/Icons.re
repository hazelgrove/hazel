open Virtual_dom.Vdom;

let icon_size = 20.;

let simple_icon = (~transform="", ~view: string, ds: list(string)) =>
  /* takes a list of paths as strings, a viewport as a string,
     and an optional (string) transform to apply to each */
  Node.create_svg(
    "svg",
    ~attrs=
      Attr.[
        create("viewBox", view),
        create("width", Printf.sprintf("%fpx", icon_size)),
        create("height", Printf.sprintf("%fpx", icon_size)),
        create("preserveAspectRatio", "none"),
      ],
    List.map(
      d =>
        Node.create_svg(
          "path",
          ~attrs=
            [Attr.create("d", d)]
            @ (transform == "" ? [] : [Attr.create("transform", transform)]),
          [],
        ),
      ds,
    ),
  );

let gear =
  simple_icon(
    ~view="0 0 1200 1200",
    [
      "m1193.2 690.95c4.4883-29.664 6.8281-60.047 6.8281-90.961 0-30.91-2.3398-61.273-6.8281-90.938l-151.37-74.305c-3.8398-10.262-8.0156-20.363-12.562-30.266l54.469-159.52c-36.109-49.148-79.527-92.566-128.66-128.67l-159.53 54.469c-9.8984-4.5234-19.992-8.7109-30.266-12.551l-74.301-151.36c-29.664-4.5234-60.051-6.8516-90.961-6.8516s-61.285 2.3281-90.949 6.8516l-74.305 151.36c-10.262 3.8398-20.352 8.0273-30.266 12.551l-159.52-54.465c-49.129 36.109-92.543 79.535-128.66 128.66l54.457 159.52c-4.5117 9.8984-8.6875 20.004-12.539 30.266l-151.36 74.312c-4.5117 29.664-6.8516 60.023-6.8516 90.938 0 30.91 2.3398 61.297 6.8516 90.961l151.36 74.305c3.8516 10.262 8.0273 20.352 12.539 30.277l-54.453 159.51c36.121 49.129 79.535 92.543 128.66 128.68l159.52-54.457c9.9102 4.5117 20.004 8.6875 30.266 12.527l74.305 151.37c29.672 4.5039 60.047 6.8438 90.957 6.8438s61.297-2.3398 90.961-6.8398l74.293-151.37c10.273-3.8398 20.363-8.0156 30.289-12.527l159.5 54.457c49.129-36.133 92.543-79.547 128.65-128.68l-54.461-159.51c4.5469-9.9258 8.7227-20.016 12.562-30.277zm-593.17 48.516c-77.016 0-139.44-62.449-139.44-139.48 0-77.016 62.426-139.45 139.44-139.45s139.48 62.438 139.48 139.45c0 77.027-62.461 139.48-139.48 139.48z",
    ],
  );

let info =
  simple_icon(
    ~view="0 0 1200 1200",
    [
      "m1120.5 531.75c-17.062-130.24-82.332-249.36-182.92-333.83-100.59-84.477-229.19-128.18-360.42-122.48-131.23 5.7031-255.56 60.395-348.44 153.28s-147.57 217.21-153.28 348.44c-5.6992 131.23 38 259.83 122.48 360.42 84.477 100.59 203.59 165.86 333.83 182.92 106.75 13.984 215.22-5.1875 310.71-54.922 95.488-49.734 173.38-127.62 223.11-223.11 49.734-95.492 68.906-203.96 54.922-310.71zm-445.5 317.25c0 26.793-14.293 51.555-37.5 64.953-23.207 13.395-51.793 13.395-75 0-23.207-13.398-37.5-38.16-37.5-64.953v-225c0-26.793 14.293-51.555 37.5-64.953 23.207-13.395 51.793-13.395 75 0 23.207 13.398 37.5 38.16 37.5 64.953zm-75-423c-19.891 0-38.969-7.9023-53.031-21.969-14.066-14.062-21.969-33.141-21.969-53.031s7.9023-38.969 21.969-53.031c14.062-14.066 33.141-21.969 53.031-21.969s38.969 7.9023 53.031 21.969c14.066 14.062 21.969 33.141 21.969 53.031s-7.9023 38.969-21.969 53.031c-14.062 14.066-33.141 21.969-53.031 21.969z",
    ],
  );

let star =
  simple_icon(
    ~view="0 0 1200 1200",
    [
      "m1045.2 459.6-270-57.602-136.8-238.8c-16.801-28.801-58.801-28.801-75.602 0l-138 238.8-270 57.602c-32.398 7.1992-45.602 46.801-24 72l184.8 205.2-28.801 273.6c-3.6016 33.598 30 57.598 61.199 44.398l252-111.6 252 111.6c31.199 13.199 64.801-10.801 61.199-44.398l-28.801-273.6 184.8-205.2c21.602-25.203 9.6016-64.801-24-72z",
    ],
  );

let bomb =
  simple_icon(
    ~view="0 0 1200 1200",
    [
      "m700.88 364.88v-94.312c0-10.5-8.4375-18.75-18.75-18.75h-98.812c1.3125-22.5 9.5625-94.688 61.5-123.19 52.688-29.062 140.25-6.1875 253.31 66.375 8.625 5.625 20.25 3.1875 25.875-5.625 5.625-8.625 3-20.25-5.625-25.875-127.12-81.562-225.38-104.44-291.94-67.688-70.312 38.812-79.5 129.75-80.625 156h-99c-10.312 0-18.75 8.25-18.75 18.75v92.438c0 0.5625 0 1.3125 0.1875 1.875-153.19 55.688-262.69 202.69-262.69 374.81 0 219.94 178.88 398.81 398.81 398.81s399-178.88 399-398.81c0-171.94-109.5-318.75-262.5-374.81zm-82.688 90.75c-9.9375-3.1875-15.375-13.688-12.375-23.625 3.1875-9.75 13.688-15.375 23.438-12.188 2.8125 0.9375 279 90.75 237.75 385.31-1.3125 9.375-9.375 16.125-18.562 16.125-0.9375 0-1.6875 0-2.625-0.1875-10.312-1.3125-17.438-10.875-15.938-21 36.938-263.62-201.56-341.25-211.69-344.44z",
      "m893.25 244.88-30.562 29.25c-3.5625 3.5625-8.25 5.25-12.938 5.25-4.875 0-9.9375-1.875-13.5-5.8125-7.3125-7.5-6.9375-19.312 0.5625-26.438l30.562-29.25c7.5-7.3125 19.5-6.9375 26.625 0.5625s6.75 19.312-0.75 26.438z",
      "m958.31 83.25-6.75 41.625c-1.3125 9.375-9.375 15.75-18.375 15.75-0.9375 0-2.0625 0-3-0.1875-10.312-1.6875-17.25-11.25-15.562-21.375l6.75-41.812c1.6875-10.312 11.438-17.25 21.562-15.562s17.062 11.25 15.375 21.562z",
      "m1028.4 249.94c-3.5625 4.5-9 6.9375-14.625 6.9375-4.125 0-8.25-1.3125-11.625-4.125l-33-26.438c-8.0625-6.5625-9.375-18.375-2.8125-26.438 6.375-8.0625 18.188-9.375 26.25-2.8125l33 26.438c8.0625 6.5625 9.375 18.375 2.8125 26.438z",
      "m1023.6 155.06-38.438 17.812c-2.4375 1.125-5.25 1.6875-7.875 1.6875-6.9375 0-13.875-4.125-17.062-10.875-4.3125-9.375-0.1875-20.625 9.1875-24.938l38.438-17.812c9.375-4.3125 20.625-0.1875 24.938 9.1875s0.1875 20.625-9.1875 24.938z",
      "m956.62 284.06c0.75 10.312-7.125 19.312-17.438 19.875-0.5625 0.1875-0.9375 0.1875-1.3125 0.1875-9.75 0-18-7.6875-18.75-17.625l-2.8125-42.188c-0.5625-10.312 7.125-19.312 17.625-19.875 10.312-0.75 19.125 7.125 19.875 17.438z",
    ],
  );

let export =
  simple_icon(
    ~view="0 0 67.671 67.671",
    [
      "M52.946,23.348H42.834v6h10.112c3.007,0,5.34,1.536,5.34,2.858v26.606c0,1.322-2.333,2.858-5.34,2.858H14.724   c-3.007,0-5.34-1.536-5.34-2.858V32.207c0-1.322,2.333-2.858,5.34-2.858h10.11v-6h-10.11c-6.359,0-11.34,3.891-11.34,8.858v26.606   c0,4.968,4.981,8.858,11.34,8.858h38.223c6.358,0,11.34-3.891,11.34-8.858V32.207C64.286,27.239,59.305,23.348,52.946,23.348z",
      "M24.957,14.955c0.768,0,1.535-0.293,2.121-0.879l3.756-3.756v13.028v6v11.494c0,1.657,1.343,3,3,3s3-1.343,3-3V29.348v-6   V10.117l3.959,3.959c0.586,0.586,1.354,0.879,2.121,0.879s1.535-0.293,2.121-0.879c1.172-1.171,1.172-3.071,0-4.242l-8.957-8.957   C35.492,0.291,34.725,0,33.958,0c-0.008,0-0.015,0-0.023,0s-0.015,0-0.023,0c-0.767,0-1.534,0.291-2.12,0.877l-8.957,8.957   c-1.172,1.171-1.172,3.071,0,4.242C23.422,14.662,24.189,14.955,24.957,14.955z",
    ],
  );

let import =
  simple_icon(
    ~view="0 0 61 61",
    [
      "M49.561,16.464H39.45v6h10.111c3.008,0,5.341,1.535,5.341,2.857v26.607c0,1.321-2.333,2.858-5.341,2.858H11.34   c-3.007,0-5.34-1.537-5.34-2.858V25.324c0-1.322,2.333-2.858,5.34-2.858h10.11v-6H11.34C4.981,16.466,0,20.357,0,25.324v26.605   c0,4.968,4.981,8.857,11.34,8.857h38.223c6.357,0,11.34-3.891,11.34-8.857V25.324C60.902,20.355,55.921,16.464,49.561,16.464z",
      "M39.529,29.004c-0.768,0-1.535,0.294-2.121,0.88l-3.756,3.755V20.612v-6V3.117c0-1.656-1.343-3-3-3s-3,1.344-3,3v11.494v6   v13.23l-3.959-3.958c-0.586-0.586-1.354-0.88-2.121-0.88s-1.535,0.294-2.121,0.88c-1.172,1.17-1.172,3.07,0,4.241l8.957,8.957   c0.586,0.586,1.354,0.877,2.12,0.877c0.008,0,0.016,0,0.023,0s0.015,0,0.022,0c0.768,0,1.534-0.291,2.12-0.877l8.957-8.957   c1.172-1.171,1.172-3.071,0-4.241C41.064,29.298,40.298,29.004,39.529,29.004z",
    ],
  );

let undo =
  simple_icon(
    ~view="0 0 512 512",
    [
      "M129.7,46.4l37.2,37.7l-66.6,67.1h254.4c86.8,0,157.2,70.4,157.2,157.2s-70.4,157.2-157.2,157.2h-52.4v-52.4h52.4 c57.9,0,104.8-46.9,104.8-104.8s-46.9-104.8-104.8-104.8H100.4l66.6,65.8l-37.2,36.9L0,177.4L129.7,46.4z",
    ],
  );

let redo =
  simple_icon(
    ~view="0 0 512 512",
    [
      "M382.3,46.4l-37.2,37.7l66.6,67.1H157.2C70.4,151.2,0,221.6,0,308.4s70.4,157.2,157.2,157.2h52.4v-52.4h-52.4 c-57.9,0-104.8-46.9-104.8-104.8s46.9-104.8,104.8-104.8h254.4l-66.6,65.8l36.9,36.9l130-128.9L382.3,46.4z",
    ],
  );

let circle_question =
  simple_icon(
    ~view="-0.5 -0.5 25 25",
    [
      "M12 2c5.514 0 10 4.486 10 10s-4.486 10-10 10-10-4.486-10-10 4.486-10 10-10zm0-2c-6.627 0-12 5.373-12 12s5.373 12 12 12 12-5.373 12-12-5.373-12-12-12zm1.25 17c0 .69-.559 1.25-1.25 1.25-.689 0-1.25-.56-1.25-1.25s.561-1.25 1.25-1.25c.691 0 1.25.56 1.25 1.25zm1.393-9.998c-.608-.616-1.515-.955-2.551-.955-2.18 0-3.59 1.55-3.59 3.95h2.011c0-1.486.829-2.013 1.538-2.013.634 0 1.307.421 1.364 1.226.062.847-.39 1.277-.962 1.821-1.412 1.343-1.438 1.993-1.432 3.468h2.005c-.013-.664.03-1.203.935-2.178.677-.73 1.519-1.638 1.536-3.022.011-.924-.284-1.719-.854-2.297z",
    ],
  );

let github =
  simple_icon(
    ~view="-0.5 -0.5 25 25",
    [
      "M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z",
    ],
  );

let back =
  simple_icon(
    ~view="0 0 330 330",
    [
      "M250.606,154.389l-150-149.996c-5.857-5.858-15.355-5.858-21.213,0.001  c-5.857,5.858-5.857,15.355,0.001,21.213l139.393,139.39L79.393,304.394c-5.857,5.858-5.857,15.355,0.001,21.213  C82.322,328.536,86.161,330,90,330s7.678-1.464,10.607-4.394l149.999-150.004c2.814-2.813,4.394-6.628,4.394-10.606  C255,161.018,253.42,157.202,250.606,154.389z",
    ],
    ~transform="scale(-0.75, 0.75) translate(-330, 50)",
  );

let forward =
  simple_icon(
    ~view="0 0 330 330",
    [
      "M250.606,154.389l-150-149.996c-5.857-5.858-15.355-5.858-21.213,0.001  c-5.857,5.858-5.857,15.355,0.001,21.213l139.393,139.39L79.393,304.394c-5.857,5.858-5.857,15.355,0.001,21.213  C82.322,328.536,86.161,330,90,330s7.678-1.464,10.607-4.394l149.999-150.004c2.814-2.813,4.394-6.628,4.394-10.606  C255,161.018,253.42,157.202,250.606,154.389z",
    ],
    ~transform="scale(0.75, 0.75) translate(0, 50)",
  );

let eye =
  simple_icon(
    ~view="0 0 48 48",
    [
      "M24 9c-10 0-18.54 6.22-22 15 3.46 8.78 12 15 22 15 10.01 0 18.54-6.22 22-15-3.46-8.78-11.99-15-22-15zm0 25c-5.52 0-10-4.48-10-10s4.48-10 10-10 10 4.48 10 10-4.48 10-10 10zm0-16c-3.31 0-6 2.69-6 6s2.69 6 6 6 6-2.69 6-6-2.69-6-6-6z",
    ],
  );

let trash =
  simple_icon(
    ~view="0 0 24 24",
    [
      "M3 6v18h18v-18h-18zm5 14c0 .552-.448 1-1 1s-1-.448-1-1v-10c0-.552.448-1 1-1s1 .448 1 1v10zm5 0c0 .552-.448 1-1 1s-1-.448-1-1v-10c0-.552.448-1 1-1s1 .448 1 1v10zm5 0c0 .552-.448 1-1 1s-1-.448-1-1v-10c0-.552.448-1 1-1s1 .448 1 1v10zm4-18v2h-20v-2h5.711c.9 0 1.631-1.099 1.631-2h5.315c0 .901.73 2 1.631 2h5.712z",
    ],
  );

let hazelnut =
  simple_icon(
    ~view="100 0 500 500",
    [
      "m499.84 130.51c-93.363-93.363-207.59-129.06-300.65-36.051-95.07 95.113-116.07 213.15-62.562 350.83 3.1484 8.0508-0.875 28.438 9.5391 38.895 10.414 10.457 29.836 5.4688 36.707 8.0078 128.84 47.512 254.8 37.188 353.02-61.25 93.008-92.797 57.352-207.03-36.055-300.43zm-12.383 12.383c32.289 32.289 88.508 99.574 85.094 173.64-76.211-33.34-85.922-85.141-144.77-143.98-35.48-35.48-103.82-58.055-116.07-114.62 66.02-4.1992 129.24 38.324 175.74 84.961zm36.051 275.89c-87.5 87.5-199.98 106.62-334.55 56.875-9.1875-3.3672-27.344-1.0508-30.625-3.9375-3.8945-3.9375-1.5742-22.488-5.6016-32.855-51.539-132.48-32.375-241.11 58.625-332.11 25.418-25.375 53.113-40.949 82.773-46.68 13.434 66.012 87.195 90.777 121.5 124.86 58.449 58.672 69.082 114.23 154.88 149.71-5.0742 27.91-19.25 56.438-46.988 84.129z",
      "m274.01 374.72 108.11-108.11c1.7695-1.6133 2.8047-3.8789 2.8594-6.2734 0.054688-2.3945-0.875-4.707-2.5703-6.3984-1.6953-1.6914-4.0117-2.6133-6.4062-2.5508-2.3945 0.066406-4.6602 1.1055-6.2656 2.8828l-108.11 108.06c-3.418 3.4219-3.418 8.9648 0 12.383s8.9609 3.418 12.383 0z",
      "m445.42 399.61c-0.52344 0.39453-52.938 40.555-115.54 46.242-4.832 0.4375-8.3984 4.707-7.9609 9.5391 0.43359 4.832 4.7031 8.3984 9.5352 7.9609 67.855-6.168 122.5-48.125 124.69-49.832 1.9062-1.3945 3.168-3.5 3.4961-5.8398 0.32813-2.3398-0.30469-4.7109-1.75-6.5781-1.4492-1.8672-3.5898-3.0703-5.9375-3.3359-2.3438-0.26172-4.6992 0.43359-6.5273 1.9297z",
      "m344.93 410.64c2.2617 4.2617 7.5469 5.8828 11.812 3.6328 34.762-18.434 68.016-39.586 99.441-63.262 3.8086-2.9844 4.4727-8.4883 1.4883-12.297-2.9844-3.8047-8.4883-4.4688-12.293-1.4844-30.648 23.004-63.031 43.598-96.863 61.598-4.2422 2.2812-5.8477 7.5586-3.5859 11.812z",
      "m350 357.61c-2.8438-3.8984-8.3008-4.7578-12.207-1.9258-0.52344 0.35156-50.18 36.445-105.61 71.094-2.0391 1.1953-3.5078 3.1641-4.0742 5.4609-0.5625 2.2969-0.17578 4.7227 1.0781 6.7305 1.2539 2.0039 3.2656 3.4141 5.5781 3.9141 2.3125 0.49609 4.7266 0.039062 6.6953-1.2734 56.043-34.738 106-71.402 106.53-71.793 3.9258-2.8164 4.8242-8.2812 2.0117-12.207z",
    ],
  );

let magnify =
  simple_icon(
    ~view="0 0 24 24 ",
    [
      "M15.5 14h-.79l-.28-.27A6.471 6.471 0 0 0 16 9.5 6.5 6.5 0 1 0 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z",
    ],
  );

let chest =
  simple_icon(
    ~view="0 0 100 125",
    [
      "M56,54v-4c0-1.1,0.9-2,2-2s2,0.9,2,2v4c0,1.1-0.9,2-2,2S56,55.1,56,54z M94,28v12v36c0,3.3-2.7,6-6,6H12c-3.3,0-6-2.7-6-6  V40V28c0-5.5,4.5-10,10-10h68C89.5,18,94,22.5,94,28z M84,22H24c1.3,1.7,2,3.7,2,6v10h64V28C90,24.7,87.3,22,84,22z M10,38h12V28  c0-3.3-2.7-6-6-6s-6,2.7-6,6V38z M12,78h10V42H10v34C10,77.1,10.9,78,12,78z M90,42H26v36h62c1.1,0,2-0.9,2-2V42z",
    ],
  );

let sprout =
  simple_icon(
    ~view="0 0 100 125",
    [
      "M48.8,68.2c-2,0.8-1.6,2.2-2.1,2.5c-0.8,0.3-0.1-1-2.1-1.6c-4.5-1.4-4,4.3-6.5,4.2c-1.7-0.1-1.8-0.6-3.4-0.4  c-1.6,0.3-1.1,2.1-2.9,2.3c-1.1,0.1-0.7,1.2-2.4,1c-1.4-0.2-2,1.6-2.1,2.7c-0.1,0.8-0.3,1.6-1.6,2.3c-1.2,0.7-3.3,0.2-4.4,1.7  c-1.6,2-1.7,5.1-1.7,5.1h60.7c0,0-0.1-2.3-2.6-3.9c-0.7-0.4,0.1-0.4,0.4-2c0.1-0.6,0-1.5-0.7-2.6c-0.9-1.3-2.7-0.9-3.4-0.7  c0.2-0.9,0.2-2-1.2-2.9c-1-0.7-1.3-0.3-1.9-0.4c-0.7-0.1-0.3-0.8-1-1.2c-1.8-1.1-0.5-2.1-3.3-3.2c-1.7-0.7-2-0.6-5.6-0.6  c-1.4,0-1.7-2.3-3.6-1.8c-1.9,0.6-1.1-0.5-2.2-1.6c-0.3-0.3-0.9-0.5-1.5-0.5c-0.1-10.6-0.1-24.5,0.3-27.2c0.6-4.5,5.7-9.8,13.1-10.6  c-7,5.2-10.2,6.7-10.8,9.9c-0.6,3.2,5.1,2.5,9.6,0c4.5-2.5,12.1-14.6,12.4-19c-6.4-1.6-22,4.1-27.1,16.6C49.6,26.6,47.7,12,20.3,12  c-0.1,2.6,0.6,2.9,1.9,6.1c1.3,3.2,9.9,18.1,20.7,17.5c1.6-0.1-0.3-3.8-2.9-6.8c0,0-6.4-5.9-8.3-7.5c6.4,2.5,16.2,9.9,17,15.3  c0.5,3.5,0.4,20.2,0.2,31.7C49,68.2,48.9,68.2,48.8,68.2z",
    ],
  );

let x =
  simple_icon(
    ~view="2950 1000 450 450",
    [
      "M3382.84 784.3 3462.43 862.829 3540.96 783.238 3601.23 842.704 3522.7 922.295 3602.29 1000.82 3542.82 1061.09 3463.23 982.566 3384.7 1062.16 3324.43 1002.69 3402.96 923.1 3323.37 844.57Z",
    ],
  );

let backpack =
  simple_icon(
    ~view="0 0 1000 1000",
    [
      "m902.09 769.55c0.33594-35.148 0.20312-112.29-5.9141-188.52 0.53906 32.125 1.3789 61.523 2.4727 76.906 0.60938 8.8242 1.9531 55.023 3.4414 111.61z",
      "m608.14 108.45c1.043-24.695-0.28906-51.145 20.641-49.633 10.043 0.73047 27.348 3.2891 48.758 7.5703 23.734 4.7539 20.387 133.45 20.387 133.45l86.051 33.457c2.832-93.133-20.449-201.28-43.68-207.9-38.93-11.102-96.059-34.246-152.83-21.828-37.934 8.293-48.086 147.55-48.086 147.55l66.383 17.387c0.003906 0 0.69922-20.543 2.3789-60.059z",
      "m207.76 579.2c13.98 5.1719 37.02 12.77 64.297 21.324 20.387-56.461 54.961-99.527 54.961-99.527s7.9805-20.914 48.07-12.492c40.094 8.4102 41.965 26.711 41.965 26.711s-25.969 42.098-51.215 98.832c-1.5586 3.5039-3.6133 7.8594-5.1719 13.223 63.562 18.527 124.98 34.934 142.64 36.539 34.766 3.1797 122.09 6.9961 138.68-77.039 16.367-82.934 32.566-133.52 46.367-165.11 2.7617-6.8984 5.2422-12.613 7.0664-16.738 3.7578-8.4961 15.191-25.535 31.824-42.238v-0.023438c0.046875-0.12109 0.12109-0.17969 0.21484-0.20312 22.223-22.199 53.664-43.703 88.766-43.57 1.8477 0 4.1055 0.22656 6.6133 0.61328-20.652-22.969-44.184-42.527-69.238-52.188-5.6992-2.1953-212.62-83.977-274.44-75.227-1.7773 0.25391-3.4336 0.49219-5.0273 0.71875 3-0.43359 5.0273-0.71875 5.0273-0.71875-0.003907-0.003906-141.73 3.5117-235.45 97.156-48.961 48.91-91.945 136.66-118.93 220.97-0.34766 21.398 26.953 48.277 82.98 68.988z",
      "m484.35 726.64c-27.863-5.1133-79.621-19.297-134.79-35.605-8.3984 33.227-1.2617 90.301-1.2617 90.301s-10.848 25.895-53.605 16.43c-36.434-8.0898-35.496-34.379-35.496-34.379s-9.3125-53.352-1.1055-100.31c-52.512-16.535-97.129-31.379-113.54-37.68-19.754-7.5586-33.996-18.098-44.258-29.867-0.75781 4.668-1.4297 23.52-2.0625 28.008-37.219 265.1 40.035 379.27 45.508 403.38 7.3555 32.41 148.57 100.37 284.89 150.16 79.078 28.883 167.36 23.281 202.43 20.762l2.5664-0.14453c5.5781-102.5 20.773-369.32 28.223-532.61-11.805 59.953-110.79 73.777-177.5 61.562z",
      "m902.09 769.55c-1.4883-56.594-2.8203-102.77-3.4453-111.64-1.0938-15.359-1.9336-44.762-2.4727-76.895-5.4609-67.871-22.957-140.72-37.922-178.59-25.152-63.742-98.195 16.211-118.82 66.551-21.359 52.129-41.578 653.03-43.547 712.6 51-13.285 124.05-32.352 133.01-46.402 18.973-29.711 29.258-58.477 33.996-82.969 3.1445-16.309 45.324-31.559 45.324-31.559s-2.9883-129.6-5.6875-234.19l-0.74219 2.7617c0 0.019531 0.20312-7.1914 0.3125-19.672z",
      "m1089.1 508.25s-10.812-128.79-13.262-153.82c-5.9297-60.445-60.457-254.84-246.31-105.9l49.234 53.711s125.95-113.18 130.7 156.5c0.39453 22.824 1.9062 54.707 15.637 246.59 15.191 212.15-67.777 213.32-67.777 213.32l3.7422 90.215s30.816-8.0625 62.641-21.457c45.035-18.973 88.188-98.375 88.188-214.77-0.039062-46.051-22.793-264.41-22.793-264.41z",
      "m438.25 148.18 41.09-6.3125v-34.773l7.9062-28.441s-37.945 17.387-48.996 34.766c-11.062 17.387-15.816 26.867-15.816 34.766 0 7.9062 15.816-0.003907 15.816-0.003907z",
    ],
  );
