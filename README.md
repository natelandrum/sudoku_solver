# Overview

As a software engineer, I work to expand my knowledge of different programming languages. In this project, I developed a Sudoku solver in Erlang to demonstrate how recursion can be applied to solve a Sudoku board while furthering my understanding of Erlang's syntax and funtional programming.

The software solves a given Sudoku puzzle by filling in the blanks (represented by 0s) while ensuring that the numbers in each row, column, and 3x3 subgrid follow Sudoku's rules. The solver uses recursive backtracking to try different possibilities for each empty cell until the puzzle is solved.

The primary purpose was to deepend my understanding of Erlang's strengths such as recursion, pattern matching, and list handling. These features make Erlang a powerful tool for this type of problem.

[Software Demo Video](https://youtu.be/SZsfai3xQdk)

# Development Environment

* Editor/IDE: I developed the project using Visual Studio Code with the Erlang extension for syntax highlighting and code management.
* Programming Language: The project is written entirely in Erlang, focusing on functional programming techniques like recursion, pattern matching, and list manipulation.
* Build Tool: The project can be managed and compiled using Rebar3, Erlang’s build tool, which helps with compilation and project structuring.

# Useful Websites

* [Erlang Official Documentation](https://www.erlang.org/docs) - For language-specific syntax and standard library functions.
* [Rebar3 Official Documentation](https://rebar3.org/docs/) - For learning how to structure and build Erlang projects.
* [Learn You Some Erlang](https://learnyousomeerlang.com/content) - A comprehensive guide to Erlang for beginners and intermediate users.

# Future Work

* Python OCR Integration: Implement a Python-based Optical Character Recognition (OCR) to automatically generate the Sudoku grid by scanning an image. This will convert an image of a Sudoku puzzle into the necessary 2D list format for the solver.
