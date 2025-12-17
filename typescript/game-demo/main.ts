// Grab the canvas and 2D context
const canvas = document.getElementById("game") as HTMLCanvasElement;
const ctx = canvas.getContext("2d");

if (!ctx) {
  throw new Error("Could not get 2D context");
}

// Track keyboard input
const keys: Record<string, boolean> = {};

window.addEventListener("keydown", (e) => {
  keys[e.key] = true;
});

window.addEventListener("keyup", (e) => {
  keys[e.key] = false;
});

// Paddle settings
const paddleWidth = 20;
const paddleHeight = 100;

// Player paddle (left)
const player = {
  x: 40,
  y: canvas.height / 2 - paddleHeight / 2,
  width: paddleWidth,
  height: paddleHeight,
  speed: 6,
};

// CPU paddle (right)
const cpu = {
  x: canvas.width - 40 - paddleWidth,
  y: canvas.height / 2 - paddleHeight / 2,
  width: paddleWidth,
  height: paddleHeight,
  speed: 4,
};

// Ball settings
const ball = {
  x: canvas.width / 2,
  y: canvas.height / 2,
  radius: 10,
  vx: 4,
  vy: 3,
};

let playerScore = 0;
let cpuScore = 0;

// Reset the ball to the center, sending it left (-1) or right (+1)
function resetBall(direction: number) {
  ball.x = canvas.width / 2;
  ball.y = canvas.height / 2;

  // Base horizontal speed
  ball.vx = 4 * direction;

  // Random vertical speed between -3 and 3, avoid 0
  const vy = Math.random() * 6 - 3;
  ball.vy = vy === 0 ? 2 : vy;
}

// Initialize ball moving in a random direction
resetBall(Math.random() < 0.5 ? -1 : 1);

// Game logic update
function update() {
  // --- Player paddle movement (ArrowUp / ArrowDown) ---
  if (keys["ArrowUp"]) {
    player.y -= player.speed;
  }
  if (keys["ArrowDown"]) {
    player.y += player.speed;
  }

  // Clamp player paddle inside canvas
  if (player.y < 0) player.y = 0;
  if (player.y + player.height > canvas.height) {
    player.y = canvas.height - player.height;
  }

  // --- CPU paddle movement (simple follow AI) ---
  const cpuCenter = cpu.y + cpu.height / 2;
  if (ball.y < cpuCenter - 10) {
    cpu.y -= cpu.speed;
  } else if (ball.y > cpuCenter + 10) {
    cpu.y += cpu.speed;
  }

  // Clamp CPU paddle inside canvas
  if (cpu.y < 0) cpu.y = 0;
  if (cpu.y + cpu.height > canvas.height) {
    cpu.y = canvas.height - cpu.height;
  }

  // --- Ball movement ---
  ball.x += ball.vx;
  ball.y += ball.vy;

  // Top / bottom wall bounce
  if (ball.y - ball.radius < 0) {
    ball.y = ball.radius;
    ball.vy *= -1;
  } else if (ball.y + ball.radius > canvas.height) {
    ball.y = canvas.height - ball.radius;
    ball.vy *= -1;
  }

  // --- Paddle collisions ---

  // Helper: collision with a paddle
  function collideWithPaddle(paddle: typeof player, isLeft: boolean) {
    // Check if ball is horizontally overlapping paddle
    if (isLeft) {
      if (
        ball.x - ball.radius < paddle.x + paddle.width &&
        ball.x - ball.radius > paddle.x &&
        ball.y > paddle.y &&
        ball.y < paddle.y + paddle.height
      ) {
        // Move ball just outside paddle to avoid sticking
        ball.x = paddle.x + paddle.width + ball.radius;

        // Reverse X direction
        ball.vx *= -1;

        // Add some "angle" based on where it hit the paddle
        const hitPos =
          (ball.y - (paddle.y + paddle.height / 2)) / (paddle.height / 2);
        ball.vy = hitPos * 5;
      }
    } else {
      if (
        ball.x + ball.radius > paddle.x &&
        ball.x + ball.radius < paddle.x + paddle.width &&
        ball.y > paddle.y &&
        ball.y < paddle.y + paddle.height
      ) {
        ball.x = paddle.x - ball.radius;
        ball.vx *= -1;

        const hitPos =
          (ball.y - (paddle.y + paddle.height / 2)) / (paddle.height / 2);
        ball.vy = hitPos * 5;
      }
    }
  }

  // Collide with player (left) and cpu (right)
  collideWithPaddle(player, true);
  collideWithPaddle(cpu, false);

  // --- Scoring ---
  // Ball goes off left side: CPU scores
  if (ball.x + ball.radius < 0) {
    cpuScore += 1;
    resetBall(1); // send ball towards player
  }

  // Ball goes off right side: Player scores
  if (ball.x - ball.radius > canvas.width) {
    playerScore += 1;
    resetBall(-1); // send ball towards CPU
  }
}

// Draw everything
function draw() {
  // Clear screen
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  // Background
  ctx.fillStyle = "#222";
  ctx.fillRect(0, 0, canvas.width, canvas.height);

  // Center line
  ctx.strokeStyle = "#555";
  ctx.setLineDash([10, 10]);
  ctx.beginPath();
  ctx.moveTo(canvas.width / 2, 0);
  ctx.lineTo(canvas.width / 2, canvas.height);
  ctx.stroke();
  ctx.setLineDash([]);

  // Player paddle
  ctx.fillStyle = "white";
  ctx.fillRect(player.x, player.y, player.width, player.height);

  // CPU paddle
  ctx.fillRect(cpu.x, cpu.y, cpu.width, cpu.height);

  // Ball
  ctx.beginPath();
  ctx.arc(ball.x, ball.y, ball.radius, 0, Math.PI * 2);
  ctx.fillStyle = "orange";
  ctx.fill();

  // Scores
  ctx.fillStyle = "white";
  ctx.font = "24px Arial";
  ctx.textAlign = "center";
  ctx.fillText(playerScore.toString(), canvas.width * 0.25, 40);
  ctx.fillText(cpuScore.toString(), canvas.width * 0.75, 40);

  // Instructions
  ctx.font = "16px Arial";
  ctx.fillText("Use ↑ and ↓ to move", canvas.width / 2, canvas.height - 20);
}

// Main loop
function loop() {
  update();
  draw();
  requestAnimationFrame(loop);
}

// Start the game
loop();
