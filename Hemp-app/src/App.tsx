import { useRef, useEffect } from "react";
import "./App.css";

const WIDTH = 800;
const HEIGHT = 600;

const tributeTexts: string[] = [
  "Hemp, the Border Collie Extraordinaire!",
  "Fast, smart, and always ready to catch a ball!",
  "A loyal friend with a heart of gold!",
  "No tennis ball is safe when Hemp is around!",
  "The ultimate fetch champion of all time!",
];

function App() {
  const canvasRef = useRef<HTMLCanvasElement | null>(null);

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) {
      console.error("No canvas element found");
      return;
    }

    const ctx = canvas.getContext("2d");
    if (!ctx) {
      console.error("Could not get 2D context");
      return;
    }

    // Ball state
    let ballX = Math.floor(Math.random() * (WIDTH - 100)) + 50;
    let ballY = Math.floor(Math.random() * (HEIGHT - 100)) + 50;
    let ballDX = 5;
    let ballDY = 5;

    // Text state
    let textIndex = 0;
    let textTimer = 0;
    const textDelay = 100;

    let animationFrameId: number;

    const drawFrame = () => {
      // Background - grassy green
      ctx.fillStyle = "#228B22";
      ctx.fillRect(0, 0, WIDTH, HEIGHT);

      // Update text timer
      textTimer += 1;
      if (textTimer > textDelay) {
        textTimer = 0;
        textIndex = (textIndex + 1) % tributeTexts.length;
      }

      // Draw tribute text
      const text = tributeTexts[textIndex];
      ctx.font = "24px sans-serif";
      ctx.fillStyle = "#ffffff";
      const textWidth = ctx.measureText(text).width;
      ctx.fillText(text, (WIDTH - textWidth) / 2, 50);

      // Move ball
      ballX += ballDX;
      ballY += ballDY;

      // Bounce off walls
      if (ballX <= 0 || ballX + 50 >= WIDTH) {
        ballDX = -ballDX;
      }
      if (ballY <= 0 || ballY + 50 >= HEIGHT) {
        ballDY = -ballDY;
      }

      // Draw a simple yellow circle as the ball
      ctx.beginPath();
      ctx.arc(ballX + 25, ballY + 25, 25, 0, Math.PI * 2);
      ctx.fillStyle = "#FFD700"; // tennis-ball-ish yellow
      ctx.fill();
      ctx.closePath();

      animationFrameId = window.requestAnimationFrame(drawFrame);
    };

    drawFrame();

    return () => {
      if (animationFrameId) {
        window.cancelAnimationFrame(animationFrameId);
      }
    };
  }, []);

  return (
    <div className="app-root">
      <h1 className="title">Tribute to Hemp - The Ultimate Fetch Champion</h1>
      <canvas
        ref={canvasRef}
        width={WIDTH}
        height={HEIGHT}
        className="hemp-canvas"
      />
      <p className="hint">
        Watch the tennis ball bounce around Hemp&apos;s grassy field. 🎾
      </p>
    </div>
  );
}

export default App;
