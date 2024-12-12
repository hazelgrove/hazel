open Util;
open Virtual_dom.Vdom;

type model_piece = {
  model: UExp.t,
  piece: Piece.tile,
};

let put: (string, Uuidm.t) => Piece.tile =
  (s, id) => {
    let piece = Piece.replace_id(id, Piece.mk_mono(Exp, s));
    switch (piece) {
    | Tile(t) => t
    | _ => failwith("put: not a tile")
    };
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  model_t: Typ.t,
  expansion_t: Typ.t,
  expansion_f: UExp.t => UExp.t,
  projector:
    (list(model_piece), Piece.tile => Ui_effect.t(unit)) =>
    Virtual_dom.Vdom.Node.t,
  size: int,
};

let slider: t = {
  name: "slider",
  expansion_t: Typ.temp(Int),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Int(n) => DHExp.fresh(Int(n))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(Int),
  projector: (model: list(model_piece), update) => {
    let {model, piece} = List.nth(model, 0);
    let n =
      switch (model.term) {
      | Int(n) => n
      | _ => failwith("Slider livelit: not given int")
      };

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        Util.Web.range(
          ~attrs=[Attr.on_input((_, v) => {update(put(v, piece.id))})],
          string_of_int(n),
        ),
      ],
    );
  },
  size: 20,
};

let double_slider: t = {
  name: "double_slider",
  expansion_t: Typ.temp(Prod([Typ.temp(Int), Typ.temp(Int)])),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Tuple([t1, t2]) => DHExp.fresh(Tuple([t1, t2]))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(Prod([Typ.temp(Int), Typ.temp(Int)])),
  projector: (models: list(model_piece), update) => {
    // Pattern match directly on models to extract the two elements
    let ((model_1, piece_1), (model_2, piece_2)) =
      switch (models) {
      | [{model: m1, piece: p1}, {model: m2, piece: p2}] => (
          (m1, p1),
          (m2, p2),
        )
      | _ => failwith("Expected exactly two model pieces")
      };

    // Pattern match on model terms to extract integers directly
    let (n1, n2) =
      switch (model_1.term, model_2.term) {
      | (Int(n1), Int(n2)) => (n1, n2)
      | _ => failwith("Double slider livelit: not given pair of ints")
      };

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        Util.Web.range(
          ~attrs=[Attr.on_input((_, v) => update(put(v, piece_1.id)))],
          string_of_int(n1),
        ),
        Util.Web.range(
          ~attrs=[Attr.on_input((_, v) => update(put(v, piece_2.id)))],
          string_of_int(n2),
        ),
      ],
    );
  },
  size: 20,
};

let script = (s: string) => {
  Node.create("script", [Node.text(s)]);
};

let js: t = {
  name: "js",
  expansion_t: Typ.temp(String),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | String(s) => DHExp.fresh(String(s))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(String),
  projector: (model: list(model_piece), _parent) => {
    let {model, piece} = List.nth(model, 0);
    let s =
      switch (model.term) {
      | String(s) => s
      | _ => failwith("JS livelit: not given string")
      };

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [script(s), Node.text("JS livelit")],
    );
  },
  size: 20,
};

/* General function to create a popup with communication */
let create_popup_with_communication = (~popup_content: string): string =>
  {|
(function() {
  const options = "width=800,height=600,scrollbars=yes,resizable=yes";
  const popup = window.open("", "_blank", options);
  var handlePopupMessage = null;

  if (popup) {
    popup.document.open();
    popup.document.write(`|}
  ++ popup_content
  ++ {|`);
    popup.document.close();

    // Function to update the popup
    function updatePopup(variable, newValue) {
      if (popup) {
        popup.postMessage({ variable: variable, value: newValue }, '*');
      }
    }

    // Set up listener for messages from the popup
    window.addEventListener('message', function(event) {
      if (event.source === popup && event.data) {
        // Handle message from popup
        if (handlePopupMessage) {
          handlePopupMessage(event.data);
        }
      }
    }, false);

    // Return an object with methods
    return {
      updatePopup: updatePopup,
      setHandlePopupMessage: function(handler) {
        handlePopupMessage = handler;
      }
    };
  } else {
    return null;
  }
})();
|};

let timestamp: t = {
  name: "timestamp",
  expansion_t: Typ.temp(Int),
  expansion_f: (_model: UExp.t) =>
    DHExp.fresh(Int(Float.to_int(JsUtil.timestamp()))),
  model_t: Typ.temp(Prod([])),
  projector: (_model: list(model_piece), _parent) =>
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("Timestamp livelit")],
    ),
  size: 20,
};

/* Emotion livelit definition */

let not_found: t = {
  name: "not_found",
  expansion_t: Typ.temp(Unknown(Internal)),
  expansion_f: (_model: UExp.t) => DHExp.fresh(String("No livelit found")),
  model_t: Typ.temp(Unknown(Internal)),
  projector: (_model: list(model_piece), _) =>
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("No livelit found")],
    ),
  size: 20,
};

/* Game livelit definition */
let game: t = {
  name: "game",
  expansion_t:
    Typ.temp(
      Prod([
        Typ.temp(Int), // Player X position
        Typ.temp(Int), // Player Y position
        Typ.temp(Int), // Energy
        Typ.temp(Int) // Random Seed
      ]),
    ),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Tuple([x, y, energy, seed]) =>
      DHExp.fresh(Tuple([x, y, energy, seed]))
    | _ => DHExp.fresh(Undefined)
    },
  model_t:
    Typ.temp(
      Prod([
        Typ.temp(Int), // Player X position
        Typ.temp(Int), // Player Y position
        Typ.temp(Int), // Energy
        Typ.temp(Int) // Random Seed
      ]),
    ),
  projector: (models: list(model_piece), update) => {
    // Extract the model pieces
    let (
      (model_x, piece_x),
      (model_y, piece_y),
      (model_energy, piece_energy),
      (model_seed, piece_seed),
    ) =
      switch (models) {
      | [
          {model: m_x, piece: p_x},
          {model: m_y, piece: p_y},
          {model: m_e, piece: p_e},
          {model: m_s, piece: p_s},
        ] => (
          (m_x, p_x),
          (m_y, p_y),
          (m_e, p_e),
          (m_s, p_s),
        )
      | _ => failwith("Expected exactly four model pieces")
      };

    // Extract the values
    let x =
      switch (model_x.term) {
      | Int(n) => n
      | _ => failwith("Game livelit: player X position is not an int")
      };
    let y =
      switch (model_y.term) {
      | Int(n) => n
      | _ => failwith("Game livelit: player Y position is not an int")
      };
    let energy =
      switch (model_energy.term) {
      | Int(n) => n
      | _ => failwith("Game livelit: energy is not an int")
      };
    let seed =
      switch (model_seed.term) {
      | Int(n) => n
      | _ => failwith("Game livelit: random seed is not an int")
      };

    /* Popup content with the game */
    let popup_content =
      {|
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Maze Game</title>
  <style>
    body { margin: 0; padding: 20px; font-family: Arial, sans-serif; }
    #gameCanvas { border: 1px solid black; }
  </style>
</head>
<body>
  <h2>Maze Game</h2>
  <canvas id="gameCanvas" width="400" height="400"></canvas>
  <p>Energy: <span id="energyDisplay"></span></p>
  <button id="getEnergyButton">Get More Energy</button>
  <script>
    // Game variables
    var canvas = document.getElementById('gameCanvas');
    var ctx = canvas.getContext('2d');
    var energyDisplay = document.getElementById('energyDisplay');
    var getEnergyButton = document.getElementById('getEnergyButton');

    // Initial game state
    var gameState = {
      x: |}
      ++ string_of_int(x)
      ++ {|,
      y: |}
      ++ string_of_int(y)
      ++ {|,
      energy: |}
      ++ string_of_int(energy)
      ++ {|,
      seed: |}
      ++ string_of_int(seed)
      ++ {|
    };

    // Goal position
    var width = 20;
    var height = 20;

    var goalX = width - 1;
    var goalY = height - 1;
    var goalImage = new Image();
    goalImage.src = 'https://hazel.org/imgs/hazel-logo.png'; // Replace with your goal image URL

    // Maze representation (20x20 grid)
    var maze = [];

    // Random maze generation using recursive backtracking
    function generateMaze(seed) {
      // Initialize the random number generator
      var rng_seed = seed;
      function random() {
        var x = Math.sin(rng_seed++) * 10000;
        return x - Math.floor(x);
      }

      // Initialize the maze grid
      maze = [];
      for (var y = 0; y < height; y++) {
        maze[y] = [];
        for (var x = 0; x < width; x++) {
          // Each cell has walls in all directions and is unvisited
          maze[y][x] = {
            x: x,
            y: y,
            walls: { north: true, south: true, east: true, west: true },
            visited: false
          };
        }
      }

      // Recursive backtracking algorithm
      function shuffle(array) {
        var currentIndex = array.length, temporaryValue, randomIndex;
        while (0 !== currentIndex) {
          randomIndex = Math.floor(random() * currentIndex);
          currentIndex -= 1;
          // Swap
          temporaryValue = array[currentIndex];
          array[currentIndex] = array[randomIndex];
          array[randomIndex] = temporaryValue;
        }
        return array;
      }

      function carvePassagesFrom(currentCell) {
        currentCell.visited = true;
        var directions = shuffle(['north', 'south', 'east', 'west']);
        for (var i = 0; i < directions.length; i++) {
          var dx = 0, dy = 0;
          var direction = directions[i];
          switch (direction) {
            case 'north': dy = -1; break;
            case 'south': dy = 1; break;
            case 'east': dx = 1; break;
            case 'west': dx = -1; break;
          }
          var nx = currentCell.x + dx;
          var ny = currentCell.y + dy;
          if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
            var neighbor = maze[ny][nx];
            if (!neighbor.visited) {
              currentCell.walls[direction] = false;
              var opposite = { north: 'south', south: 'north', east: 'west', west: 'east' };
              neighbor.walls[opposite[direction]] = false;
              carvePassagesFrom(neighbor);
            }
          }
        }
      }

      var startCell = maze[gameState.y][gameState.x];
      carvePassagesFrom(startCell);
    }

    generateMaze(gameState.seed);

    function drawMaze() {
      var cellSize = 20;
      ctx.strokeStyle = 'black';
      for (var y = 0; y < 20; y++) {
        for (var x = 0; x < 20; x++) {
          var cell = maze[y][x];
          var px = x * cellSize;
          var py = y * cellSize;

          if (cell.walls.north) {
            ctx.beginPath();
            ctx.moveTo(px, py);
            ctx.lineTo(px + cellSize, py);
            ctx.stroke();
          }
          if (cell.walls.east) {
            ctx.beginPath();
            ctx.moveTo(px + cellSize, py);
            ctx.lineTo(px + cellSize, py + cellSize);
            ctx.stroke();
          }
          if (cell.walls.south) {
            ctx.beginPath();
            ctx.moveTo(px + cellSize, py + cellSize);
            ctx.lineTo(px, py + cellSize);
            ctx.stroke();
          }
          if (cell.walls.west) {
            ctx.beginPath();
            ctx.moveTo(px, py + cellSize);
            ctx.lineTo(px, py);
            ctx.stroke();
          }
        }
      }
      if (gameState.energy < 1) {
        // draw text over canvas that says "Out of energy!"
        ctx.fillStyle = 'red';
        ctx.font = 'bold 24px Arial';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText('Out of energy!', canvas.width / 2, canvas.height / 2);
      }
    }

    function drawPlayer() {
      var cellSize = 20;
      ctx.fillStyle = 'blue';
      ctx.beginPath();
      ctx.arc(
        gameState.x * cellSize + cellSize / 2,
        gameState.y * cellSize + cellSize / 2,
        cellSize / 2 - 2,
        0,
        Math.PI * 2
      );
      ctx.fill();
    }

    function drawGoal() {
      var cellSize = 20;
      if (goalImage.complete) {
        ctx.drawImage(goalImage, goalX * cellSize, goalY * cellSize, cellSize, cellSize);
      } else {
        goalImage.onload = function() {
          ctx.drawImage(goalImage, goalX * cellSize, goalY * cellSize, cellSize, cellSize);
        };
      }
    }

    function updateEnergyDisplay() {
      energyDisplay.textContent = gameState.energy;
    }

    function drawGame() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      drawMaze();
      drawGoal();
      drawPlayer();
      updateEnergyDisplay();
    }

    function movePlayer(dx, dy) {
      var x = gameState.x;
      var y = gameState.y;

      var cell = maze[y][x];
      var direction = null;
      if (dx === 1 && dy === 0) direction = 'east';
      else if (dx === -1 && dy === 0) direction = 'west';
      else if (dx === 0 && dy === -1) direction = 'north';
      else if (dx === 0 && dy === 1) direction = 'south';

      if (direction && !cell.walls[direction] && gameState.energy > 0) {
        gameState.x += dx;
        gameState.y += dy;
        gameState.energy -= 1;
        drawGame();

        // Send individual updates
        window.opener.postMessage({ variable: 'x', value: gameState.x }, '*');
        window.opener.postMessage({ variable: 'y', value: gameState.y }, '*');
        window.opener.postMessage({ variable: 'energy', value: gameState.energy }, '*');

        // Check for goal
        if (gameState.x === goalX && gameState.y === goalY) {
          alert('You reached the goal!');
        }
      }
    }

    // Handle key presses for movement
    document.addEventListener('keydown', function(event) {
      switch (event.key) {
        case 'ArrowUp':
          movePlayer(0, -1);
          break;
        case 'ArrowDown':
          movePlayer(0, 1);
          break;
        case 'ArrowLeft':
          movePlayer(-1, 0);
          break;
        case 'ArrowRight':
          movePlayer(1, 0);
          break;
      }
    });

    // Handle "Get More Energy" button
    getEnergyButton.addEventListener('click', function() {
      // Send request to main window to get more energy
      window.opener.postMessage({ request: 'getEnergy' }, '*');
    });

    // Listen for messages from main window
    window.addEventListener('message', function(event) {
      if (event.data && event.data.value !== undefined && event.data.variable) {
        // Update individual variable
        gameState[event.data.variable] = parseInt(event.data.value);
        if (event.data.variable === 'seed') {
          generateMaze(gameState.seed);
        }
        drawGame();
      }
    }, false);

    // Initial drawing
    drawGame();
  </script>
</body>
</html>
|};

    /* JavaScript code to open the popup and set up communication */
    let popupScript =
      "var popupComm = "
      ++ create_popup_with_communication(~popup_content)
      ++ ";";

    /* JavaScript code to handle messages from the popup */
    let handlePopupMessageJs =
      {|
popupComm.setHandlePopupMessage(function(data) {
  if (data.variable && data.value !== undefined) {
    // Mapping from variable names to input IDs
    var inputIds = {
      x: '|}
      ++ "hidden-input-x-"
      ++ Uuidm.to_string(piece_x.id)
      ++ {|',
      y: '|}
      ++ "hidden-input-y-"
      ++ Uuidm.to_string(piece_y.id)
      ++ {|',
      energy: '|}
      ++ "hidden-input-energy-"
      ++ Uuidm.to_string(piece_energy.id)
      ++ {|',
      seed: '|}
      ++ "hidden-input-seed-"
      ++ Uuidm.to_string(piece_seed.id)
      ++ {|'
    };
    var inputId = inputIds[data.variable];
    if (inputId) {
      var input = document.getElementById(inputId);
      if (input) {
        input.value = data.value;
        var event = new Event('input', { bubbles: true });
        input.dispatchEvent(event);
      }
    }
  } else if (data.request === 'getEnergy') {
    // Simulate click on the "Get More Energy" button
    var getEnergyButton = document.getElementById('|}
      ++ "get-energy-button-"
      ++ Uuidm.to_string(piece_energy.id)
      ++ {|');
    if (getEnergyButton) {
      getEnergyButton.click();
    }
  }
});
|};

    /* Helper function to send a message to the popup */
    let send_message_to_popup = (variable: string, value: string): unit => {
      let js_code =
        "if (popupComm) { popupComm.updatePopup('"
        ++ variable
        ++ "', '"
        ++ value
        ++ "'); }";
      Js_of_ocaml.Js.Unsafe.eval_string(js_code);
    };

    /* Return the node */
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        /* Include the scripts */
        script(popupScript),
        script(handlePopupMessageJs),
        /* Hidden input elements */
        Node.input(
          ~attrs=[
            Attr.type_("hidden"),
            Attr.id("hidden-input-x-" ++ Uuidm.to_string(piece_x.id)),
            Attr.value(string_of_int(x)),
            Attr.on_input((_, v) => {
              // Update the model for player x position
              update(
                put(v, piece_x.id),
              )
            }),
          ],
          (),
        ),
        Node.input(
          ~attrs=[
            Attr.type_("hidden"),
            Attr.id("hidden-input-y-" ++ Uuidm.to_string(piece_y.id)),
            Attr.value(string_of_int(y)),
            Attr.on_input((_, v) => {
              // Update the model for player y position
              update(
                put(v, piece_y.id),
              )
            }),
          ],
          (),
        ),
        Node.input(
          ~attrs=[
            Attr.type_("hidden"),
            Attr.id(
              "hidden-input-energy-" ++ Uuidm.to_string(piece_energy.id),
            ),
            Attr.value(string_of_int(energy)),
            Attr.on_input((_, v) => {
              // Update the model for energy
              update(put(v, piece_energy.id))
            }),
          ],
          (),
        ),
        Node.input(
          ~attrs=[
            Attr.type_("hidden"),
            Attr.id("hidden-input-seed-" ++ Uuidm.to_string(piece_seed.id)),
            Attr.value(string_of_int(seed)),
            Attr.on_input((_, v) => {
              // Send new seed to popup to regenerate maze
              send_message_to_popup("seed", v);
              // Update the model for seed
              update(put(v, piece_seed.id));
            }),
          ],
          (),
        ),
        /* Button to get more energy */
        Node.button(
          ~attrs=[
            Attr.id(
              "get-energy-button-" ++ Uuidm.to_string(piece_energy.id),
            ),
            Attr.on_click(_event => {
              // Increase energy
              let new_energy = energy + 10;
              // Send updated energy to popup
              send_message_to_popup("energy", string_of_int(new_energy));
              // Update the model
              update(put(string_of_int(new_energy), piece_energy.id));
            }),
          ],
          [Node.text("Get More Energy")],
        ),
      ],
    );
  },
  size: 20,
};

let emotion: t = {
  name: "emotion",
  expansion_t: Typ.temp(String),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Int(n) =>
      DHExp.fresh(
        String(
          if (n < 40) {
            "sad";
          } else if (n > 70) {
            "happy";
          } else {
            "neutral";
          },
        ),
      )
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(Int),
  projector: (model: list(model_piece), update) => {
    let {model, piece} = List.nth(model, 0);
    let n =
      switch (model.term) {
      | Int(n) => n
      | _ => failwith("Emotion livelit: not given int")
      };

    /* Popup content with its own slider */
    let popup_content =
      {|
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Emotion Popup</title>
  <style>
    body { margin: 0; padding: 20px; display: flex; flex-direction: column; justify-content: center; align-items: center; }
  </style>
</head>
<body>
  <div id="face"></div>
  <input type="range" min="0" max="100" value="|}
      ++ string_of_int(n)
      ++ {|" id="emotionSlider">
  <script>
    function drawFace(emotionValue) {
      var svgNS = "http://www.w3.org/2000/svg";
      var svg = document.createElementNS(svgNS, "svg");
      svg.setAttribute("width", "200");
      svg.setAttribute("height", "200");

      // Face circle
      var circle = document.createElementNS(svgNS, "circle");
      circle.setAttribute("cx", "100");
      circle.setAttribute("cy", "100");
      circle.setAttribute("r", "90");
      circle.setAttribute("fill", "yellow");
      circle.setAttribute("stroke", "black");
      svg.appendChild(circle);

      // Left eye
      var leftEye = document.createElementNS(svgNS, "circle");
      leftEye.setAttribute("cx", "65");
      leftEye.setAttribute("cy", "80");
      leftEye.setAttribute("r", "10");
      leftEye.setAttribute("fill", "black");
      svg.appendChild(leftEye);

      // Right eye
      var rightEye = document.createElementNS(svgNS, "circle");
      rightEye.setAttribute("cx", "135");
      rightEye.setAttribute("cy", "80");
      rightEye.setAttribute("r", "10");
      rightEye.setAttribute("fill", "black");
      svg.appendChild(rightEye);

      // Mouth
      var mouth = document.createElementNS(svgNS, "path");
      var smile = ((100 - emotionValue) / 100) * 50; // Map emotionValue [0,100] to smile amplitude [0,50]
      smile = smile - 25; // Now smile ranges from -25 (sad) to 25 (happy)
      var pathData = "M 60 130 Q 100 " + (130 - smile) + " 140 130";
      mouth.setAttribute("d", pathData);
      mouth.setAttribute("stroke", "black");
      mouth.setAttribute("fill", "transparent");
      mouth.setAttribute("stroke-width", "5");
      svg.appendChild(mouth);

      var faceDiv = document.getElementById("face");
      faceDiv.innerHTML = "";
      faceDiv.appendChild(svg);
    }

    // Initial drawing
    drawFace(|}
      ++ string_of_int(n)
      ++ {|);

    // Handle slider input in popup
    document.getElementById('emotionSlider').addEventListener('input', function(event) {
      var value = event.target.value;
      drawFace(value);
      // Send message back to main window
      window.opener.postMessage({ variable: 'emotionValue', value: value }, '*');
    });

    // Listen for messages from main window
    window.addEventListener('message', function(event) {
      if (event.data && event.data.variable === 'emotionValue' && event.data.value !== undefined) {
        var value = event.data.value;
        document.getElementById('emotionSlider').value = value;
        drawFace(value);
      }
    }, false);
  </script>
</body>
</html>
|};

    /* JavaScript code to open the popup and set up communication */
    let popupScript =
      "var popupComm = "
      ++ create_popup_with_communication(~popup_content)
      ++ ";";

    /* JavaScript code to handle messages from the popup */
    let hidden_input_id = "hidden-input-" ++ Uuidm.to_string(piece.id);
    let handlePopupMessageJs =
      {|
popupComm.setHandlePopupMessage(function(data) {
  if (data.variable === 'emotionValue' && data.value !== undefined) {
    // Update the hidden input value and dispatch input event
    var input = document.getElementById('|}
      ++ hidden_input_id
      ++ {|');
    if (input) {
      input.value = data.value;
      var event = new Event('input', { bubbles: true });
      input.dispatchEvent(event);
    }
  }
});
|};

    /* Helper function to send a message to the popup */
    let send_message_to_popup = (variable: string, value: string): unit => {
      let js_code =
        "if (popupComm) { popupComm.updatePopup('"
        ++ variable
        ++ "', '"
        ++ value
        ++ "'); }";
      Js_of_ocaml.Js.Unsafe.eval_string(js_code);
    };

    /* Return the node */
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        /* Include the scripts */
        script(popupScript),
        script(handlePopupMessageJs),
        /* Hidden input element */
        Node.input(
          ~attrs=[
            Attr.type_("hidden"),
            Attr.id(hidden_input_id),
            Attr.value(string_of_int(n)),
            Attr.on_input((_, v) => {
              /* Update the model */
              update(put(v, piece.id))
            }),
          ],
          (),
        ),
        /* Visible slider in the main window */
        Util.Web.range(
          ~attrs=[
            Attr.class_("livelit slider"),
            Attr.id(Uuidm.to_string(piece.id)),
            Attr.on_input((_, v) => {
              /* Send message to the popup */
              send_message_to_popup("emotionValue", v);
              /* Update the model */
              update(put(v, piece.id));
            }),
          ],
          string_of_int(n),
        ),
      ],
    );
  },
  size: 20,
};

let websocket: t = {
  name: "websocket",
  expansion_t:
    Typ.temp(
      Prod([Typ.temp(String), Typ.temp(String), Typ.temp(String)]),
    ),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Tuple([url, send_msg, recv_msg]) =>
      DHExp.fresh(Tuple([url, send_msg, recv_msg]))
    | _ => DHExp.fresh(Undefined)
    },
  model_t:
    Typ.temp(
      Prod([Typ.temp(String), Typ.temp(String), Typ.temp(String)]),
    ),
  projector: (models: list(model_piece), update) => {
    let (
      (model_url, piece_url),
      (model_send, piece_send),
      (model_recv, piece_recv),
    ) =
      switch (models) {
      | [
          {model: m1, piece: p1},
          {model: m2, piece: p2},
          {model: m3, piece: p3},
        ] => (
          (m1, p1),
          (m2, p2),
          (m3, p3),
        )
      | _ => failwith("Expected exactly three model pieces")
      };

    let websocket_url =
      switch (model_url.term) {
      | String(s) => s
      | _ => failwith("WebSocket livelit: not given string")
      };

    let send_msg =
      switch (model_send.term) {
      | String(s) => s
      | _ => failwith("WebSocket livelit: not given string")
      };

    let recv_msg =
      switch (model_recv.term) {
      | String(s) => s
      | _ => failwith("WebSocket livelit: not given string")
      };

    /* JavaScript code to open the websocket and handle communication */
    let websocketScript =
      {|
      (function() {
        const socket = new WebSocket("|}
      ++ websocket_url
      ++ {|");
        socket.onopen = function() {
          console.log("WebSocket connection opened");
          socket.send("|}
      ++ send_msg
      ++ {|");
        };

        socket.onmessage = function(event) {
          console.log("Message from server: ", event.data);
          const hiddenInput = document.getElementById("hidden-input-|}
      ++ Uuidm.to_string(piece_recv.id)
      ++ {|");

          // Update the hidden input value
          hiddenInput.value = event.data;

          // Explicitly dispatch an input event
          const inputEvent = new Event('input', { bubbles: true });
          hiddenInput.dispatchEvent(inputEvent);
        };

        socket.onerror = function(error) {
          console.error("WebSocket error: ", error);
        };

        window.sendWebSocketMessage = function(message) {
          if (socket.readyState === WebSocket.OPEN) {
            socket.send(message);
          } else {
            console.warn("WebSocket is not open: message not sent");
          }
        };
      })();
    |};

    let send_message_to_websocket = (message: string): unit => {
      let js_code = "window.sendWebSocketMessage('" ++ message ++ "');";
      Js_of_ocaml.Js.Unsafe.eval_string(js_code);
    };

    let key_handler = evt => {
      let key = Key.mk(KeyDown, evt);

      switch (key.key) {
      | _ => Effect.Stop_propagation
      };
    };

    /* Return the Node */
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        /* Hidden input for the received message */
        Node.input(
          ~attrs=[
            Attr.id("hidden-input-" ++ Uuidm.to_string(piece_recv.id)),
            Attr.type_("hidden"),
            Attr.value(recv_msg),
            Attr.on_input((_, v) => {
              print_endline("Received message: " ++ v);
              update(put("\"" ++ v ++ "\"", piece_recv.id));
            }),
          ],
          (),
        ),
        /* Include the script for websocket setup */
        script(websocketScript),
        /* Input for the message to send */
        Node.div([
          Node.input(
            ~attrs=[
              Attr.type_("text"),
              Attr.placeholder("Message to send"),
              Attr.value(send_msg),
              Attr.on_keydown(key_handler),
              Attr.on_input((_, v) => {
                update(put("\"" ++ v ++ "\"", piece_send.id))
              }),
              Attr.id("websocket-send-input"),
              Attr.class_("websocket-send-input"),
            ],
            (),
          ),
          /* Send button to trigger WebSocket message send */
          Node.button(
            ~attrs=[
              Attr.on_click(_ => {
                send_message_to_websocket(send_msg);
                update(put("\"\"", piece_send.id));
              }),
            ],
            [Node.text("Send")],
          ),
        ]),
      ],
    );
  },
  size: 30,
};

let multiplayer_game: t = {
  name: "multiplayer_game",
  expansion_t:
    Typ.temp(
      Prod([
        Typ.temp(String), /* Player ID */
        Typ.temp(String), /* Game State */
        Typ.temp(String) /* Color */
      ]),
    ),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Tuple([player_id, game_state, color, _ws]) =>
      DHExp.fresh(Tuple([player_id, game_state, color]))
    | _ => DHExp.fresh(Undefined)
    },
  model_t:
    Typ.temp(
      Prod([
        Typ.temp(String),
        Typ.temp(String),
        Typ.temp(String),
        Typ.temp(String),
      ]),
    ),
  projector: (models: list(model_piece), update) => {
    let (
      (model_player_id, piece_player_id),
      (model_game_state, piece_game_state),
      (model_color, piece_color),
      (model_ws, piece_ws),
    ) =
      switch (models) {
      | [
          {model: m_id, piece: p_id},
          {model: m_state, piece: p_state},
          {model: m_color, piece: p_color},
          {model: m_ws, piece: p_ws},
        ] => (
          (m_id, p_id),
          (m_state, p_state),
          (m_color, p_color),
          (m_ws, p_ws),
        )
      | _ => failwith("Expected exactly three model pieces")
      };

    let ws =
      switch (model_ws.term) {
      | String(s) => s
      | _ => failwith("WebSocket URL is not a string")
      };

    let player_id =
      switch (model_player_id.term) {
      | String(s) => s
      | _ => failwith("Player ID is not a string")
      };

    let game_state =
      switch (model_game_state.term) {
      | String(s) => s
      | _ => failwith("Game state is not a string")
      };

    let color =
      switch (model_color.term) {
      | String(s) => s
      | _ => failwith("Color is not a string")
      };

    let popup_content =
      {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Multiplayer Game</title>
  <style>
    body { margin: 0; padding: 20px; font-family: Arial, sans-serif; }
    #gameCanvas { border: 1px solid black; }
  </style>
</head>
<body>
  <canvas id="gameCanvas" width="400" height="400"></canvas>
  <script>
    var canvas = document.getElementById('gameCanvas');
    var ctx = canvas.getContext('2d');

    var player = {
      id: "|}
      ++ player_id
      ++ {|",
      x: 200,
      y: 200,
      color: "|}
      ++ color
      ++ {|"
    };

    var gameState = JSON.parse('|}
      ++ game_state
      ++ {|');

    var socket = new WebSocket("|}
      ++ ws
      ++ {|");

    socket.onopen = function() {
      var joinMessage = JSON.stringify({ type: "join", player: player });
      socket.send(joinMessage);
    };

    socket.onmessage = function(event) {
      var data = JSON.parse(event.data);
      if (data.type === "state") {
        gameState = data.state;
        drawGame();
        window.opener.postMessage({ variable: 'game_state', value: JSON.stringify(gameState) }, '*');
      }
    };

    function drawGame() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      for (var id in gameState.players) {
        var p = gameState.players[id];
        ctx.fillStyle = p.color || 'black';
        ctx.fillRect(p.x, p.y, 10, 10);
      }
    }

    document.addEventListener('keydown', function(event) {
      var dx = 0, dy = 0;
      if (event.key === 'ArrowUp') {
        dy = -5;
      } else if (event.key === 'ArrowDown') {
        dy = 5;
      } else if (event.key === 'ArrowLeft') {
        dx = -5;
      } else if (event.key === 'ArrowRight') {
        dx = 5;
      }
      if (dx !== 0 || dy !== 0) {
        player.x += dx;
        player.y += dy;
        var moveMessage = JSON.stringify({ type: "move", player: player });
        socket.send(moveMessage);
        window.opener.postMessage({ variable: 'game_state', value: JSON.stringify(gameState) }, '*');
      }
    });

    window.addEventListener('message', function(event) {
      if (event.data && event.data.variable && event.data.value !== undefined) {
        if (event.data.variable === 'game_state') {
          gameState = JSON.parse(event.data.value);
          drawGame();
        }
      }
    }, false);
  </script>
</body>
</html>|};

    let popupScript =
      "var popupComm = "
      ++ create_popup_with_communication(~popup_content)
      ++ ";";

    let hidden_input_game_state_id =
      "hidden-input-game_state-" ++ Uuidm.to_string(piece_game_state.id);

    let handlePopupMessageJs =
      {|
popupComm.setHandlePopupMessage(function(data) {
  if (data.variable && data.value !== undefined) {
    var inputIds = {
      game_state: '|}
      ++ hidden_input_game_state_id
      ++ {|'
    };
    var inputId = inputIds[data.variable];
    if (inputId) {
      var input = document.getElementById(inputId);
      if (input) {
        input.value = data.value;
        var event = new Event('input', { bubbles: true });
        input.dispatchEvent(event);
      }
    }
  }
});
|};

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        script(popupScript),
        script(handlePopupMessageJs),
        Node.input(
          ~attrs=[
            Attr.type_("hidden"),
            Attr.id(hidden_input_game_state_id),
            Attr.value(game_state),
            Attr.on_input((_, v) => {
              update(put("\"" ++ v ++ "\"", piece_game_state.id))
            }),
          ],
          (),
        ),
        Node.div([Node.text("Player ID: " ++ player_id)]),
      ],
    );
  },
  size: 20,
};
let livelits: list(t) = [
  not_found,
  slider,
  js,
  timestamp,
  emotion,
  double_slider,
  game,
  websocket,
  multiplayer_game,
];

let find_livelit = (livelit_name: string): t => {
  switch (List.find_opt(l => l.name == livelit_name, livelits)) {
  | Some(l) => l
  | None =>
    print_endline("Livelit " ++ livelit_name ++ " not found");
    not_found;
  };
};

/*
  let slider = (^slider(50)) in
  case (slider)
    | n => "Slider value: " ++ string_of_int(n)
  end
 */

/*
  let emotion = (^emotion(5)) in
  case (emotion)
    | "happy" => "Hooray! What a pleasant day!"
    | "neutral" => "Things are medium, I suppose."
    | "sad" => "Sorrow sorrow, today and tomorrow."
    | _ => "You have broken my trust, and frankly, our friendship"
  end
 */

/*
 let game = (^game(10, 10, 10, 1234)) in
 case (game)
   | (x, y, energy, seed) =>
   "Player at (" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ") with "
   ++ string_of_int(energy) ++ " energy and seed " ++ string_of_int(seed)
 end
 */

/*
 let websocket = (^websocket("ws://localhost:8765", "Hello", "World")) in
  case (websocket)
    | (url, send, recv) =>
    "WebSocket connection to " ++ url ++ " with send message " ++ send
    ++ " and received message " ++ recv
  end
 */

/*let websocket = (
    ^websocket("ws://localhost:8765", "Send message", "Recieve message"))
    in
    case (websocket)
      | (_, send, recv) => "Send: " ++ send ++ " Receive: " ++ recv
    end
  */

/* {'players': {'player1': {'x': 100, 'y': 100, 'color': 'red'}}} */

/*let multiplayer_game = (^multiplayer_game("player1", "{}", "red", "ws://localhost:8765")) in
  case (multiplayer_game)
    | (player_id, game_state, color, ws) =>
      "Player " ++ player_id ++ " is playing with color " ++ color
  end
  */

/*let multiplayer_game = (^multiplayer_game("player1", "{}", "red", "ws://localhost:8765")) in
  case (multiplayer_game)
    | (player_id, game_state, color) =>
    "Player " ++ player_id ++ " is playing with color " ++ color
  end*/

/*let multiplayer_game = (^multiplayer_game("player1", "{}", "red", "ws://localhost:8765")) in
  case (multiplayer_game)
    | (player_id, game_state, color) =>
    "Player " ++ player_id ++ " is playing with color " ++ color
  end*/

/*let multiplayer_game = (^multiplayer_game("player2", "{}", "blue", "ws://localhost:8765")) in
  case (multiplayer_game)
    | (player_id, game_state, color) =>
    "Player " ++ player_id ++ " is playing with color " ++ color
  end*/
