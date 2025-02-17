#Tents and Trees Puzzle Solver

## Overview

This project is a Prolog implementation of a solver for the **"Tents and Trees" puzzle**. The puzzle involves placing tents on a grid next to trees, following specific rules. Each tree must have exactly one tent adjacent to it (horizontally or vertically), and tents cannot be adjacent to each other, not even diagonally. Additionally, the number of tents in each row and column must match the given constraints.

The project was developed as part of the **"Logic for Programming"** course during the 2023-2024 academic year. It demonstrates the use of **Prolog** for solving logical puzzles by implementing a set of predicates that simulate the rules of the game and apply strategies to find a valid solution.

---

## Features

### Puzzle Representation
- The puzzle is represented as a grid (matrix) where:
  - `a` represents a **tree**.
  - `t` represents a **tent**.
  - `r` represents **grass** (empty space).
- The grid also includes constraints on the number of tents per row and column.

### Core Functionality
- **Consultation Predicates**:
  - `vizinhanca/2`: Finds the immediate neighbors (up, down, left, right) of a given cell.
  - `vizinhancaAlargada/2`: Finds the extended neighbors (including diagonals) of a given cell.
  - `todasCelulas/2`: Retrieves all cells in the grid.
  - `todasCelulas/3`: Retrieves all cells containing a specific object (e.g., tree, tent, grass).
  - `calculaObjectosTabuleiro/4`: Counts the number of specific objects in each row and column.
  - `celulaVazia/2`: Checks if a cell is empty or contains grass.

- **Insertion Predicates**:
  - `insereObjectoCelula/3`: Inserts an object (tent or grass) into a specific cell.
  - `insereObjectoEntrePosicoes/4`: Inserts an object between two positions in a row.

- **Strategy Predicates**:
  - `relva/1`: Fills rows and columns with grass if the number of tents matches the required count.
  - `inacessiveis/1`: Fills inaccessible cells (those not adjacent to any tree) with grass.
  - `aproveita/1`: Places tents in rows or columns where the number of empty cells matches the number of tents needed.
  - `limpaVizinhancas/1`: Fills the extended neighborhood of tents with grass to prevent adjacent tents.
  - `unicaHipotese/1`: Places a tent in the only available position next to a tree.

- **Trial and Error Predicates**:
  - `valida/2`: Validates that each tree has exactly one tent adjacent to it.
  - `resolve/1`: Solves the puzzle by applying the strategies and using trial and error when necessary.

---

## How It Works

The solver uses a combination of **logical rules** and **strategies** to place tents on the grid. It starts by applying basic strategies to fill in obvious placements (e.g., placing tents where only one position is available). If the puzzle cannot be solved using these strategies alone, the solver switches to a **trial-and-error approach**, where it tentatively places a tent in a valid position and checks if the puzzle can be solved from there. If not, it backtracks and tries a different position.

---

## Usage

To use the solver, you need to have **SWI-Prolog** installed. The project includes a file `puzzlesAcampar.pl` that contains predefined puzzles. You can load the solver and run the `resolve/1` predicate to solve a puzzle.

### Example:

```prolog
:- use_module(library(clpfd)).
:- [puzzlesAcampar.pl].

% Solve a specific puzzle
?- puzzle(6-14, P), resolve(P).
