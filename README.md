# Assembly Language Interpreter in Haskell

This repository contains a Haskell implementation of a parser and evaluator for an assembly-like language using the [Parsec](https://hackage.haskell.org/package/parsec) library. The project includes the following modules:

- **`Grammar`**: Defines the data structures used in the abstract syntax tree (AST) and CPU state.
- **`Lexer`**: Provides lexical analysis functions and basic parsing utilities.
- **`Parser`**: Parses assembly source code and converts it into an AST.
- **`Evaluator`**: Executes the parsed instructions by simulating a CPU state.
- **`Interface`**: Offers convenient functions to run programs and interact with the evaluator.
- **`Main`**: A sample program demonstrating how to use the modules to parse and execute assembly code.

## Features

- **Instruction Parsing and Execution**: Supports a comprehensive set of assembly instructions, including arithmetic, bitwise, shift, control flow, and stack operations.
- **Label Handling**: Parses and executes label declarations and references for jump and call instructions.
- **Argument Parsing and Evaluation**: Handles registers, values, and special arguments, including support for negative numbers and two's complement.
- **Message Handling**: Parses and displays messages from the `msg` instruction, supporting string literals and arguments.
- **CPU Simulation**: Simulates a CPU with registers, flags, stack, and program counter.
- **Flag Management**: Accurately updates CPU flags (Zero Flag, Sign Flag, Overflow Flag, etc.) based on operation results.
- **Error Handling**: Provides meaningful error messages for invalid instructions or labels.
- **Program Interface**: Offers functions to run programs from strings or files.
- **Lexical Analysis**: Uses a custom lexer to tokenize the assembly source code, handling comments, identifiers, reserved words, literals, and symbols.
- **Sample Execution**: Includes a `Main` module that serves as an example of how to use the parser and evaluator.

## Modules Overview

### Grammar Module

Defines the data structures used throughout the parser and evaluator, including:

- **Registers and Values**: Types for registers (`Reg`), unsigned values (`Val`), and signed values (`SVal`).
- **Labels**: For control flow instructions.
- **Messages**: Structure for the `msg` instruction.
- **Arguments**: Instruction arguments, either registers or values.
- **Program Lines**: Lines in the program—instructions, labels, or empty lines.
- **Instructions**: All supported instructions and their arguments.

### Lexer Module

Provides lexical analysis functions and basic parsing utilities. Handles:

- **Comments**: Lines starting with `;`.
- **Identifiers**: Register names, labels, variable names.
- **Reserved Words**: Instruction mnemonics.
- **Literals**: Numeric literals (integers, naturals, octal, hexadecimal) and string literals.
- **Symbols and Operators**: Commas, colons, and other punctuation.

### Parser Module

Reads assembly source code and constructs an AST representing the program structure.

#### Supported Instructions

- **Arithmetic Operations**: `inc`, `dec`, `add`, `sub`, `mul`, `div`
- **Bitwise Operations**: `xor`, `and`, `or`, `not`, `neg`
- **Shift Operations**: `shr`, `shl`, `rol`, `ror`
- **Control Flow**: `jmp`, `je`, `jne`, `jg`, `jl`, `jge`, `jle`, `jz`, `jo`, `jc`, `jp`, `js`, `jnz`, `jno`, `jnc`, `jnp`, `jns`, `call`, `ret`
- **Stack Operations**: `push`, `pop`, `pushf`, `popf`
- **Miscellaneous**: `mov`, `cmp`, `msg`, `end`

### Evaluator Module

Simulates a CPU to execute the parsed assembly instructions.

#### CPU State Components

- **Registers**: Hash map with register names as keys.
- **Stack**: For `push` and `pop` operations.
- **Call Stack**: Manages return addresses for `call` and `ret`.
- **Flags**: CPU flags like Zero Flag (ZF), Sign Flag (SF), Overflow Flag (OF).
- **Program Counter**: Tracks the current instruction line.
- **Output**: Collects messages from `msg` instructions.
- **Program**: The parsed program to execute.
- **Termination Status**: Indicates if the program has terminated.

### Interface Module

Provides functions to run assembly programs and interact with the evaluator.

#### Key Functions

- **Running Programs from Strings**:
  - `runStr :: String -> CPUState`: Parses and runs a program from a string.
  - `runStrIO :: String -> IO ()`: Parses and runs a program from a string, printing outputs.
- **Running Programs from Files**:
  - `runFile :: String -> IO CPUState`: Parses and runs a program from a file.
  - `runFileIO :: String -> IO ()`: Parses and runs a program from a file, printing outputs.
- **CPU State Monad**:
  - `type CPU a = State CPUState a`: State monad for manipulating the CPU state during execution.

### Main Module

An example program (`Main.hs`) demonstrating how to use the modules to parse and execute assembly code.

#### Example Code

```haskell
module Main where

import System.IO
import qualified Interface as I

main :: IO ()
main = do
  putStrLn "What file would you like to read from?"
  filename <- getLine
  contents <- readFile filename
  I.runStrIO contents
```

#### Explanation

- **Purpose**: Provides a simple command-line interface to run assembly programs.
- **Functionality**:
  - Prompts the user for a filename.
  - Reads the content of the specified file.
  - Uses `runStrIO` from the `Interface` module to parse and execute the program, printing any output messages.

## Usage

### Parsing and Executing an Assembly Code String

```haskell
import Interface (runStrIO)

main :: IO ()
main = do
  let code = "mov ax, 5\nmsg 'ax is ', ax\nend"
  runStrIO code
```

### Parsing and Executing an Assembly Code File

```haskell
import Interface (runFileIO)

main :: IO ()
main = do
  runFileIO "example.asm"
```

### Using the Main Program

Compile and run the `Main.hs` file:

```bash
ghc Main.hs -o assembler
./assembler
```

When prompted, enter the path to your assembly code file (e.g., `example.asm`).

## Example Assembly Program

Given an assembly file `example.asm`:

```
start:
  mov ax, 10
  dec ax
  cmp ax, 0
  jne start
end:
  msg 'Final value of ax: ', ax
  end
```

Running this program using the `Main` module or `runFileIO` function will execute the loop until `ax` reaches `0` and then display the final value.

## Dependencies

- **GHC (The Glasgow Haskell Compiler)**: Available via the [Haskell Platform](https://www.haskell.org/platform/).
- **Parsec Library**: For constructing parser combinators.
- **Data Structures**:
  - `Data.Vector`: For program lines.
  - `Data.Sequence`: For output messages.
  - `Data.HashMap.Strict`: For registers and CPU state.
- **Custom Modules**:
  - `Grammar`: AST and CPU state definitions.
  - `Lexer`: Lexical analysis and parsing utilities.
  - `Parser`: Code parsing into an AST.
  - `Evaluator`: Instruction execution.
  - `Interface`: User-friendly program functions.

## Building and Running

1. **Clone the Repository**:

   ```bash
   git clone https://github.com/yourusername/assembly-parser-evaluator.git
   cd assembly-parser-evaluator
   ```

2. **Install Dependencies**:

   Use Stack to install Haskell dependencies.

3. **Compile the Program**:

   Using Stack:

   ```bash
   stack build
   ```

4. **Run the Program**:

   ```bash
   stack exec assembler
   ```

   When prompted, enter the filename of your assembly code (e.g., `example.asm`).

## Contributing

Contributions are welcome! Please open an issue or submit a pull request to discuss improvements, bug fixes, or new features.

## License

This project is licensed under the GNU General Public License v3.0. See the [LICENSE](LICENSE) file for details.


## Contact

For questions or suggestions, feel free to reach out via [email](mailto:programmerstevie@gmail.com).
