# TinyRAM emulator test plan

Scope: tests of the user-facing functionality of the TinyRAM emulator. (Not in scope: tests of individual components, i.e., unit tests.)

## Coq TinyRAM cross check

[COQ-TINYRAM-1] Coq TinyRAM passes a smoke test. When given a program that just answers 0,
it answers 0.

[COQ-TINYRAM-2] When given a program that just answers 4, Coq TinyRAM answers 4.

[COQ-TINYRAM-3] When given a program that just answers the contents of register 2,
Coq TinyRAM answers 0.

[COQ-TINYRAM-4] When given a program that just answers with the first word in the primary input
tape, and a primary input tape containing just the number 2, CoqTinyRAM answers 2.

[COQ-TINYRAM-5] When given a program that just answers with the first word in the secondary
input tape, and a secondary input tape containing just the number 2, CoqTinyRAM answers 2.

[COQ-TINYRAM-6] When given a program that just answers with the first word in a non-existent
input tape, Coq TinyRAM answers 0. (Also, this program contains random crud in the unused
bits of the instruction.)

[COQ-TINYRAM-7] When given a specific program that does not execute an answer instruction,
Coq TinyRAM answers 1.

[COQ-TINYRAM-PROPERTY] For any program and any maximum number of steps, Coq TinyRAM
gives the same answer as Haskell TinyRAM. This test uses a 16 bit word size and four registers.
