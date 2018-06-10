# haskell-lfsr

This is the source code for the (Stackoverflow question)[].

The project struture is as follows:

`lfsr/` has the C implementing a maximum length 16-bit [LFSR](https://en.wikipedia.org/wiki/Linear-feedback_shift_register). The malloc'ed `struct` is unnecessary, but mimicks how a more complicated C library would work. The point is not to have the most efficient LFSR implementation, but a somewhat meaningful workload for benchmarking and investigating the efficiency of Haskell programs.

`src/` has module `LFSR` which is the Haskell wrapper for the C source code.

`RepeatAvg.hs` compares the run-times for a run of the LFSR.

```bash
=== RunRepeat =======
Baseline: 1.9927e-2
IO:       3.4791e-2
```

`RunAvg.hs` compares the run-times for the calculation of the average over a run of the LFSR.

```bash
=== RunAvg =========
Baseline: 2.0739999999999998e-2
IO:       1.399719
```

The `Makefile` has some useful targets:

- `build`: Builds the executables
- `run`: Builds and runs the executables
- `profile`: Builds the profile executables and runs the profiler
