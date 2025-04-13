"use client";

import { useState } from "react";
import SolverForm from "@/components/SolverForm";
import Link from "next/link";

const Home = () => {
  const [solution, setSolution] = useState<number[][] | null>(null);
  const [error, setError] = useState<string | null>(null);

  const handleSolve = (result: any) => {
    if (result.status === "success") {
      setSolution(result.solved_board);
      setError(null);
    } else if (result.status === "error") {
      setSolution(null);
      const errorAscii = result.message;
      const decodedError: string = errorAscii.map((code: number) => String.fromCharCode(code)).join('');
      setError(decodedError);
    }
    else {
      setSolution(null);
      setError(null);
    }
  };

  return (
    <main className="p-4 font-sans">
      <div className="flex justify-between">
        <h1 className="mb-6 text-2xl font-bold">Sudoku Solver</h1>
        <Link href={"/play"}>
          <button
            type="button"
            className="px-4 py-2 text-black font-semibold bg-green-500 rounded hover:bg-green-600"
          >
            Play Sudoku Here
          </button>
        </Link>
      </div>
      {error && (
        <div className="flex justify-center">
          <h2 className="text-red-500">Error:&nbsp;</h2>
          <p className="text-red-500">{error}</p>
        </div>
      )}
      {solution && (
        <div className="flex justify-center">
          <h2 className="text-green-500">Solution:&nbsp;</h2>
          <p className="text-green-500">Solved!</p>
        </div>
      )}
      <SolverForm onSolve={handleSolve} solvedGrid={solution} />
    </main>
  );
};

export default Home;
