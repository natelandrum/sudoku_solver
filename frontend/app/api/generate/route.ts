import { NextResponse } from "next/server";
import fs from 'fs/promises';
import path from 'path';
export async function POST(req: Request) {
  const { difficulty } = await req.json();

  try {
    const filePath = path.join(process.cwd(), 'json', `${difficulty}.json`);
    
    const fileContent = await fs.readFile(filePath, 'utf-8');
    const puzzles = JSON.parse(fileContent).puzzles;
    
    const randomIndex = Math.floor(Math.random() * puzzles.length);
    const randomPuzzle = puzzles[randomIndex];
    
    return NextResponse.json(randomPuzzle);
  } catch (error) {
    console.error("Error fetching puzzle:", error);
    return NextResponse.json(
      { status: "error", message: "Internal server error." },
      { status: 500 }
    );
  }
}
