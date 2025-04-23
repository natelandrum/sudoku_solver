"use client";

import { useState, useEffect } from "react";
import clsx from "clsx";

export default function Game() {
  const [puzzle, setPuzzle] = useState<string[][]>(
    Array(9).fill(Array(9).fill(""))
  );

  const [solution, setSolution] = useState<string[][]>(
    Array(9).fill(Array(9).fill(""))
  );

  const [isSolved, setIsSolved] = useState(false);

  const [userError, setUserError] = useState(0);

  const [difficulty, setDifficulty] = useState("easy");

  const [selectedIndex, setSelectedIndex] = useState<number>(0);
  const [location, setLocation] = useState(0);

  useEffect(() => {
    const slider = document.getElementById("slider");
    if (slider) {
      slider.style.transform = `translateX(${location}px) translateY(-2.5rem)`;
    }
  }, [location]);

  const handleGenerate = async () => {
    setUserError(0);
    setIsSolved(false);
    try {
      const response = await fetch("/api/generate", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ difficulty: difficulty.toLowerCase() }),
      });
      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(`Network response was not ok: ${errorData.message}`);
      }
      const data = await response.json();
      
      const newPuzzle: number[][] = Array.from({ length: 9 }, (_, rowIndex) =>
        data.puzzle
          .slice(rowIndex * 9, rowIndex * 9 + 9)
          .split("")
          .map((char: string) => (char === "." ? 0 : Number(char)))
      );

      const newSolution: number[][] = Array.from({ length: 9 }, (_, rowIndex) =>
        data.solution
          .slice(rowIndex * 9, rowIndex * 9 + 9)
          .split("")
          .map((char: string) => Number(char))
      );

      setPuzzle(newPuzzle.map((row: number[]) =>
        row.map((cell: number) => (cell === 0 ? "" : cell.toString()))
        )
    );

      setSolution(
        newSolution.map((row: number[]) =>
          row.map((cell: number) => (cell === 0 ? "" : cell.toString()))
        )
      );
    } catch (error) {
      if (error instanceof Error) {
        console.error("Error:", error.message);
      } else {
        console.error("Unexpected error:", error);
      }
    }
  };
    const handleLocation = (element: HTMLButtonElement) => {
      const elementLeft = element.getBoundingClientRect().left;
      const elementParentLeft = element.parentElement?.getBoundingClientRect().left ?? 0;
      if (elementParentLeft === 0) {
        setLocation(elementLeft - 22);
        return;
      }
      setLocation(elementLeft - elementParentLeft - 12);
    }

    const handleDifficulty = (difficulty: string, index: number) => {
      setSelectedIndex(index);
      setDifficulty(difficulty);
    };

  const handleChange = (row: number, col: number, value: string) => {
    const newPuzzle = puzzle.map((r, i) =>
      r.map((cell, j) => (i === row && j === col ? value : cell))
    );
    if (value !== solution[row][col] && value !== "") {
      setUserError(userError + 1);
    }
    setPuzzle(newPuzzle);
  };

  useEffect(() => {
    const isPuzzleSolved = puzzle.every((row, rowIndex) =>
      row.every((cell, colIndex) => cell === solution[rowIndex][colIndex] && cell !== "")
      
    );
    setIsSolved(isPuzzleSolved);
  }, [puzzle, solution]);

  return (
    <>
      <div>
        <div className="relative w-full max-w-lg mx-auto mt-14">
          <div className="relative grid grid-cols-5 gap-4 bg-gray-200 p-4 rounded-lg">
            <div
              id="slider"
              className="absolute top-0 left-2 w-24 h-24 bg-gray-200 rounded-full transition-transform duration-500 ease-in-out"
            ></div>

            {["Easy", "Medium", "Hard", "Expert", "Evil"].map(
              (label, index) => (
                <button
                  key={`difficulty-${label}`}
                  className="relative hover:scale-110 flex items-center justify-center cursor-pointer text-black text-lg"
                  type="button"
                  tabIndex={0}
                  onKeyDown={(e) => {
                    if (e.key === 'Enter' || e.key === ' ') {
                      handleDifficulty(label, index);
                    }
                  }}
                  onClick={(e) => {
                    handleDifficulty(label, index)
                    handleLocation(e.currentTarget)
                  }
                  }
                >
                  <span
                    className={`transition-transform duration-500 ease-in-out ${
                      selectedIndex === index ? "-translate-y-9 text-black" : ""
                    }`}
                  >
                    {label}
                  </span>
                </button>
              )
            )}
          </div>
        </div>
      </div>
      <div className="flex flex-col mt-12 items-center min-h-screen">
        <div>
          {isSolved && (
            <div className="bg-green-500 text-black p-4 text-center">
              You solved the puzzle!
            </div>
          )}
        </div>
        <div>
          <div className="grid grid-cols-9 gap-0">
            {puzzle.map((row, i) =>
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
                    readOnly={
                      cell !== "" && cell === solution[i][j] && !isSolved
                    }
                    className={clsx(
                      "w-10 h-10 text-center border bg-gray-200  border-black text-black text-2xl",
                      {
                        "border-r-4": (j + 1) % 3 === 0 && j !== 0,
                        "border-b-4": (i + 1) % 3 === 0 && i !== 0,
                        "text-red-500": cell !== "" && cell !== solution[i][j],
                      }
                    )}
                  />
                </div>
              ))
            )}
          </div>
          <div>
            {userError > 0 && (
              <div className="bg-red-500 text-white p-4 text-center">
                {`You have made ${userError} errors`}
              </div>
            )}
          </div>
        </div>

        <button
          className="px-4 py-2 bg-blue-500 hover:bg-blue-700 text-black font-semibold rounded mt-4"
          onClick={handleGenerate}
        >
          Generate Sudoku Board
        </button>
      </div>
    </>
  );
};