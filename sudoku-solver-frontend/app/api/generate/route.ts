import { NextResponse } from "next/server";

export async function POST(req: Request) {
  const { difficulty } = await req.json();

  try {
    const response = await fetch(`http://localhost:4000/api/json/${difficulty}.json`, {
      method: "GET",
      headers: { "Content-Type": "application/json" },
    });

    if (!response.ok) {
      return NextResponse.json(
        { status: "error", message: `Network response was not ok: ${response.statusText}` },
        { status: response.status }
      );
    }

    const data = await response.json();
    console.log(data);
    return NextResponse.json(data);
  } catch (error) {
    console.error("Error fetching puzzle:", error);
    return NextResponse.json(
      { status: "error", message: "Internal server error." },
      { status: 500 }
    );
  }
}
