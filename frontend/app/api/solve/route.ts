import { NextResponse } from "next/server";

export async function POST(req: Request) {
  const { grid } = await req.json();

  try {
    const response = await fetch("https://sudokusolver-production.up.railway.app/solve", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ grid }),
    });

    const data = await response.json();
    return NextResponse.json(data);
  } catch (error) {
    console.error("Error:", error);
    return NextResponse.json(
      { status: "error", message: "Internal server error." },
      { status: 500 }
    );
  }
}
