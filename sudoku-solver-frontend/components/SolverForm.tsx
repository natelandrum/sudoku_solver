import { FunctionComponent, useState, useEffect } from "react";
import clsx from "clsx";

type SolverFormProps = {
  onSolve: (result: any) => void;
  solvedGrid: number[][] | null;
};

const SolverForm: FunctionComponent<SolverFormProps> = ({ onSolve, solvedGrid }) => {
  const [grid, setGrid] = useState<string[][]>(
    Array(9).fill(Array(9).fill(""))
  );

  useEffect(() => {
    if (solvedGrid) {
      const newGrid = solvedGrid.map((row) =>
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
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    const newGrid = grid.map((row) =>
      row.map((cell) => (cell === "" ? 0 : Number(cell)))
    );
    console.log(newGrid);
    const response = await fetch("/api/solve", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ grid: newGrid }),
    });
    const result = await response.json();
    onSolve(result);
  };

  return (
    <div className="flex justify-center mt-8 min-h-screen">
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
                  type="text"
                  value={cell}
                  autoComplete="off"
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
            onClick={() => { setGrid(Array(9).fill(Array(9).fill(""))); onSolve([]); }}
            >
            Clear Board
            </button>
        </div>
        
      </form>
    </div>
  );
};

export default SolverForm;
