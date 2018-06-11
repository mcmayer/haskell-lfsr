# haskell-lfsr

## Overview

This is the source code for the Stackoverflow question ["Squeezing more performance out of monadic streams in Haskel"](https://stackoverflow.com/questions/50791698/squeezing-more-performance-out-of-monadic-streams-in-haskell).

## Project Structure

The project struture is as follows:

#### `lfsr/`

has the C implementing of a maximum length 16-bit LFSR. For more information on LFSRs see the [Wikipedia article](https://en.wikipedia.org/wiki/Linear-feedback_shift_register), from where the LFSR implementation was taken. Note that the malloc'ed `struct` is unnecessary, but mimicks how a more complicated C library would work. The point is not to have the most efficient LFSR implementation, but some workload for benchmarking and investigating the efficiency of Haskell programs.

#### `src/`

has module `LFSR` which is the Haskell wrapper for the C source code.

#### `RepeatAvg.hs`

compares the run-times for a run of the LFSR.

```bash
=== RunRepeat =======
Baseline: 2.2168e-2
IO:       3.9276e-2
factor:   1.7717430530494405
```

#### `RunAvg.hs`

compares the run-times for the calculation of the average over a run of the LFSR, using Haskell's lists:

```bash
=== RunAvg =========
Baseline: 1.944e-2
IO:       1.418907
factor:   72.98904320987654
```

#### `RunAvgStreaming.hs`

does the same as `RunAvg.hs`, but it uses the [`streaming`](http://hackage.haskell.org/package/streaming) library.

```bash
=== RunAvgStreaming ===
Baseline: 2.2519e-2
IO:       0.15731599999999998
factor:   6.985922998356942
```

#### `RunAvgVector.hs`

does the same as `RunAvg.hs`, but it uses the [`Data.Vector.Fusion.Stream.Monadic`](http://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Fusion-Stream-Monadic.html) library.

```bash
=== RunVector =========
Baseline: 1.9986e-2
IO:       4.9146e-2
factor:   2.4590213149204443
```

#### `Makefile`

has some useful targets:

- `build`: Builds the executables
- `run`: Builds and runs the executables
- `profile`: Builds the profile executables and runs the profiler, the results are in the `*.prof` files.
