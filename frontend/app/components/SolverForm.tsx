import { FunctionComponent, useState, useEffect } from "react";
import clsx from "clsx";
import { Result } from "@/lib/definitions";

type SolverFormProps = {
  onSolve: (result: Result) => void;
  solvedGrid: Partial<Result> | null;
};

const SolverForm: FunctionComponent<SolverFormProps> = ({ onSolve, solvedGrid }) => {
  const [grid, setGrid] = useState<string[][]>(
    Array(9).fill(Array(9).fill(""))
  );

  useEffect(() => {
    document.querySelector("input")?.focus();

    document.addEventListener("keydown", (e) => {
      if (e.ctrlKey && e.key === "v") {
        e.preventDefault();
        navigator.clipboard.readText().then((text) => {
          // strip non-numeric characters
          const cleanedText = text.replace(/[^0-9.]/g, "");
          const numbers = cleanedText.split("").slice(0, 81);
          if (numbers.length < 81) {
            alert("Pasted data must contain at least 81 digits (0-9).");
            return;
          }
          // fill in grid with numbers, if 0 just leave blank
          const newGrid = Array(9)
            .fill(null)
            .map(() => Array(9).fill(""));
          numbers.forEach((num, idx) => {
            const r = Math.floor(idx / 9);
            const c = idx % 9;
            newGrid[r][c] = num === "0" ? "" : num;
          });
          setGrid(newGrid);
        });
      }
    });

    document.querySelectorAll("input").forEach((input) => {
      input.addEventListener("keydown", (e) => {
        switch (e.key) {
          case "ArrowUp": {
            e.preventDefault();
            const [row, col] = (e.target as HTMLInputElement).id
              .split("-")
              .slice(1)
              .map(Number);
            const prevRow = row === 0 ? 8 : row - 1;
            const prevInput = document.getElementById(`cell-${prevRow}-${col}`);
            prevInput?.focus();
            break;
          }
          case "ArrowDown": {
            e.preventDefault();
            const [row, col] = (e.target as HTMLInputElement).id
              .split("-")
              .slice(1)
              .map(Number);
            const nextRow = row === 8 ? 0 : row + 1;
            const nextInput = document.getElementById(`cell-${nextRow}-${col}`);
            nextInput?.focus();
            break;
          }
          case "ArrowLeft": {
            e.preventDefault();
            const [row, col] = (e.target as HTMLInputElement).id
              .split("-")
              .slice(1)
              .map(Number);
            const prevCol = col === 0 ? 8 : col - 1;
            const prevInput = document.getElementById(`cell-${row}-${prevCol}`);
            prevInput?.focus();
            break;
          }
          case "ArrowRight": {
            e.preventDefault();
            const [row, col] = (e.target as HTMLInputElement).id
              .split("-")
              .slice(1)
              .map(Number);
            const nextCol = col === 8 ? 0 : col + 1;
            const nextInput = document.getElementById(`cell-${row}-${nextCol}`);
            nextInput?.focus();
            break;
          }
          case "Backspace": {
            const target = e.target as HTMLInputElement;
            if (target.value !== "" && !/^[1-9]$/.test(target.value)) {
              break;
            }
            const [row, col] = target.id
              .split("-")
              .slice(1)
              .map(Number);
            const prevCol = col === 0 ? 8 : col - 1;
            const prevRow = col === 0 ? (row === 0 ? 8 : row - 1) : row;
            const prevInput = document.getElementById(`cell-${prevRow}-${prevCol}`) as HTMLInputElement;
            prevInput?.focus();
            break;
          }
          default:
            break;
        }
      });
    });

  }, []);

  useEffect(() => {
    if (solvedGrid?.solved_board) {
      const newGrid = solvedGrid.solved_board.map((row) =>
        row.map((cell) => (cell === 0 ? "" : cell.toString()))
      );
      setGrid(newGrid);
    }
  }, [solvedGrid]);

  const handleChange = (row: number, col: number, value: string) => {
    const newGrid = grid.map((r, i) =>
      r.map((cell, j) => (i === row && j === col ? value : cell))
    );

    setGrid(newGrid);
    if (/^[1-9]$/.test(value)) {
      const nextCol = col === 8 ? 0 : col + 1;
      const nextRow = col === 8 ? (row === 8 ? 0 : row + 1) : row;
      const nextInput = document.getElementById(`cell-${nextRow}-${nextCol}`);
      nextInput?.focus();
    }

    // Check for duplicates and update input color
    const hasDuplicate = (checkRow: number, checkCol: number, checkValue: string) => {
      if (checkValue === "") return false;
      
      // Check row for duplicates
      for (let c = 0; c < 9; c++) {
        if (c !== checkCol && newGrid[checkRow][c] === checkValue) return true;
      }
      
      // Check column for duplicates
      for (let r = 0; r < 9; r++) {
        if (r !== checkRow && newGrid[r][checkCol] === checkValue) return true;
      }
      
      return false;
    };

    // Update all input colors
    setTimeout(() => {
      for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
          const input = document.getElementById(`cell-${r}-${c}`) as HTMLInputElement;
          if (input) {
            if (hasDuplicate(r, c, newGrid[r][c])) {
              input.style.color = "red";
            } else {
              input.style.color = "black";
            }
          }
        }
      }
    }, 0);
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    const newGrid = grid.map((row) =>
      row.map((cell) => (cell === "" ? 0 : Number(cell)))
    );
    const response = await fetch("/api/solve", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ grid: newGrid }),
    });
    const result = await response.json();
    onSolve(result);
  };

  return (
    <div className="flex flex-col justify-center items-center">
      <div className="bg-gray-800 border border-gray-600 rounded-lg p-4 mb-6 max-w-md">
        <p className="text-sm mb-2 leading-relaxed text-blue-400 font-semibold">
          Tips:
        </p>
        <p className="text-sm leading-relaxed text-gray-400">
          Paste 81 numbers (use 0 for empty cells) with Ctrl+V.
        </p>
        <p className="text-sm leading-relaxed text-gray-400">
          Use backspace to move to the previous cell and clear it.
        </p>
        <p className="text-sm leading-relaxed text-gray-400">
          Numbers will turn <span className="text-red-500">red</span> if they
          cause duplicates.
        </p>
        <p className="text-sm leading-relaxed text-gray-400">
          Use arrow keys to navigate.
        </p>
      </div>
      <form onSubmit={handleSubmit} className="space-y-4">
        <div className="grid grid-cols-9 gap-0">
          {grid.map((row, i) =>
            row.map((cell, j) => (
              <div key={`${i}-${j}`}>
                <label className="sr-only" htmlFor={`cell-${i}-${j}`}>
                  Cell {i + 1} {j + 1}
                </label>
                <input
                  id={`cell-${i}-${j}`}
                  value={cell}
                  autoComplete="off"
                  inputMode="numeric"
                  onChange={(e) => handleChange(i, j, e.target.value)}
                  className={clsx(
                    "w-10 h-10 text-center border border-black text-black text-2xl",
                    {
                      "border-r-4": (j + 1) % 3 === 0 && j !== 0,
                      "border-b-4": (i + 1) % 3 === 0 && i !== 0,
                    }
                  )}
                />
              </div>
            ))
          )}
        </div>
        <div className="flex justify-between">
          <button
            type="submit"
            className="px-4 py-2 text-black font-semibold bg-blue-500 rounded hover:bg-blue-600"
          >
            Solve Sudoku
          </button>
          <button
            type="button"
            className="px-4 py-2 text-black font-semibold bg-orange-500 rounded hover:bg-orange-600"
            onClick={() => {
              setGrid(Array(9).fill(Array(9).fill("")));
              onSolve({ solved_board: [], status: "cleared", message: [] });
            }}
          >
            Clear Board
          </button>
        </div>
      </form>
      <footer className="mt-8">
        <p className="text-gray-400 text-sm">
          &copy; {new Date().getFullYear()} Sudoku Solver. All rights reserved.
        </p>
      </footer>
    </div>
  );
};

export default SolverForm;
