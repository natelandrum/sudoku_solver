"use client";

import Link from "next/link";
import { usePathname } from 'next/navigation'

export default function Navbar() {
  const pathname = usePathname()
  const home = pathname === '/'

  return (
    <div className="flex m-6 justify-between">
        <h1 className="mb-6 text-2xl font-bold">Sudoku Solver</h1>
        <Link href={home ? "/play" : "/"}>
          <button
            type="button"
            className="px-4 py-2 text-black font-semibold bg-green-500 rounded hover:bg-green-600"
          >
            {home ? "Play Sudoku Here" : "Back to Solver"}
          </button>
        </Link>
      </div>
  )

}