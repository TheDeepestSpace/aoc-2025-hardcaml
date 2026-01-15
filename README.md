### Advent of Code 2025 -- Day 1 solution

This repo contains Hardcaml implementation for [Day 1 of Advent of Code 2025](https://adventofcode.com/2025/day/1).

The solution models the safe dial as a state machine. An FSM consumes the input file character by
character, updates dial position and wraps around as necessary, and keeps track of the number of
times the dial reaches zero. Once the end of input is reached, the current value of the code is
emitted.

My implementation for Day 10: https://github.com/TheDeepestSpace/aoc-2025
