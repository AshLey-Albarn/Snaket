# README.md

### Snaket

**Group Members:**

* Leonardo Aaron Ricetti
* Daniele Porro

---

## Implemented Features

* Implemented the full game loop using `big-bang` from `2htdp/universe`.
* Added a menu screen for selecting game speed (UP/DOWN keys) and starting the game (SPACE).
* Implemented snake movement with directional input and prevention of instant reversal.
* Added rendering of:

  * Grid, borders, and playfield background
  * Snake head oriented according to direction
  * Straight and curved body segments using a corner detection system (`turn?`, `direction`, `corner-image`)
  * Fruit with graphical leaf
  * Score bar displaying live score and session record
  * Restart button displayed during Game Over
* Implemented food generation in random valid positions avoiding the snake and obstacles.
* Added collision detection system:
  * Wall collision
  * Self-collision
  * Obstacle collision
* Implemented score increase, snake growth, and record updating.
* Added ability to restart the game via the button or starting new from the menu.
* Introduced speed-based tick control using `menu-speed` and a tick counter.
* Added mouse handler for clicking the Restart and Pause buttons.
* Designed custom graphics for snake head, body, turns, and fruit using `2htdp/image`.

---

## Changes Since Previous Submission

* Refactored `draw-tail`, `draw-snake`, and collision logic.
* Improved random food placement to avoid spawning on the snake, obstacles or other food.
* Added new game modes, speed setting, size of the game, color of the snake, number of fruit
* Finalized Game Over and Win behaviors with overlay text, score display, and restart instructions.

---

## Technical Overview

**Main Modules and Responsibilities:**

* **World Structure (`World`):** Stores the complete game state, including:

  * `world-mode` (menu or game)
  * `world-menu` (current menu settings)
  * `world-snake` (vector of snake positions)
  * `world-dir` (current snake direction)
  * `world-foods` (vector of food positions)
  * `world-obstacles` (list of static obstacles)
  * `world-score` and `world-record`
  * `world-tick-counter` (controls movement speed)
  * `world-free-spaces` (tracks remaining free cells)
  * `world-paused?` flag

* **Rendering Functions:**

  * `render-unified`: Dispatches between menu, game, Game Over, and Win screens.
  * `render-game`: Draws snake, food, obstacles, and UI elements.
  * `render-menu`: Displays menu with speed and size options.
  * `render-game-over` and `render-win`: Display end-game overlays.

* **Event Handlers:**

  * `handle-key-unified`: Handles keyboard input for both menu and game modes.
  * `handle-mouse-unified`: Handles mouse clicks on Restart and Pause buttons.
  * `update-unified`: Timer function calling `update-game` if the game is active and not paused.

* **Game Logic Functions:**

  * `compute-new-head`: Calculates the snake’s next head position based on current direction and game mode (Pacman wrap-around or normal).
  * `snake-ate?` and `handle-eat`: Detect food consumption, grow the snake, update score, and generate new food.
  * `handle-move` and `collision?`: Handles snake movement without eating and checks for collisions with walls, self, or obstacles.
  * `shrink-snake`: Maintains snake size when moving without eating.
  * Obstacle generation (`generate-obstacles-safe`, `generate-outer-obstacles`) ensures the board remains solvable and playable.

* **Tick Control:**

  * `tick-ready?` and `increment-tick` control the snake’s movement frequency based on the menu speed.

## Tools and AI Assistance

During development, we used ChatGPT to help debug and fix errors that were difficult to resolve on our own. We reviewed all suggestions carefully and implemented the solutions ourselves in the code.