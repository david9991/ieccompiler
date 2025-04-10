# IEC 61131-3 Structured Text Compiler

This project is a compiler for the IEC 61131-3 Structured Text (ST) programming language. It parses ST code and generates LLVM Intermediate Representation (IR).

## Features

*   Parses IEC 61131-3 Structured Text files (.st).
*   Generates Abstract Syntax Trees (AST).
*   Compiles ST code to LLVM IR (.ll).
*   Supports basic ST constructs like Programs, Function Blocks, Variables (local, global, input, output), assignments, arithmetic operations, logical operations (AND, OR), conditional statements (IF/ELSIF/ELSE), and loops (WHILE, REPEAT).
*   Handles different data types (BOOL, INT, DINT, REAL, etc.).
*   Supports direct memory mapping using AT directives (e.g., `%IX`, `%QX`).

## License

This project is licensed under the Apache License, Version 2.0. See the LICENSE file for details.
