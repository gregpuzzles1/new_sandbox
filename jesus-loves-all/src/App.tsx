import React from "react";
import "./App.css";

type FigureProps = {
  skin: string;
  hair: string;
  body: string;
  accent?: string;
};

const Figure: React.FC<FigureProps> = ({ skin, hair, body, accent }) => {
  const style = {
    "--skin": skin,
    "--hair": hair,
    "--body": body,
    "--accent": accent ?? "#ffffff",
  } as React.CSSProperties;

  return (
    <div className="figure" style={style}>
      <div className="head">
        <div className="face">
          <div className="eyes" />
          <div className="mouth" />
        </div>
      </div>
      <div className="body">
        <div className="body-accent" />
      </div>
    </div>
  );
};

const figures: FigureProps[] = [
  { skin: "#f5c6a5", hair: "#2b2b2b", body: "#e11d48", accent: "#fee2e2" },
  { skin: "#f9e2af", hair: "#8b5a2b", body: "#eab308", accent: "#fef3c7" },
  { skin: "#4b3621", hair: "#111827", body: "#6366f1", accent: "#e0f2fe" },
  { skin: "#f9e7da", hair: "#6b7280", body: "#0ea5e9", accent: "#bae6fd" },
  { skin: "#f2d0c9", hair: "#000000", body: "#10b981", accent: "#d1fae5" },
  { skin: "#c08a5b", hair: "#1f2933", body: "#f97316", accent: "#ffedd5" },
  { skin: "#fbe4d8", hair: "#4b5563", body: "#3b82f6", accent: "#dbeafe" },
  { skin: "#8d5524", hair: "#000000", body: "#a855f7", accent: "#f5d0fe" },
];

const App: React.FC = () => {
  const radius = 190; // was 130 — increased to spread figures apart

  return (
    <div className="page">
      <header className="title">
        <h1>Jesus loves all the little children of the world!</h1>
      </header>

      <main className="circle-wrapper">
        <div className="circle">
          {figures.map((f, index) => {
            const angle = (index / figures.length) * 2 * Math.PI;
            const x = Math.cos(angle) * radius;
            const y = Math.sin(angle) * radius;

            return (
              <div
                key={index}
                className="figure-wrapper"
                style={{
                  transform: `translate(-50%, -50%) translate(${x}px, ${y}px)`,
                }}
              >
                <Figure {...f} />
              </div>
            );
          })}

          <div className="circle-center">
            <span>❤️</span>
          </div>
        </div>
      </main>
    </div>
  );
};

export default App;
