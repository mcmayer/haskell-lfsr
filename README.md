# haskell-lfsr

## Overview

This is the source code for the Stackoverflow question ["Squeezing more performance out of monadic streams in Haskell"](https://stackoverflow.com/questions/50791698/squeezing-more-performance-out-of-monadic-streams-in-haskell).

## Project Structure

The project struture is as follows:

#### `lfsr/`

has the C implementing of a maximum length 16-bit LFSR. For more information on LFSRs see the [Wikipedia article](https://en.wikipedia.org/wiki/Linear-feedback_shift_register), from where the LFSR implementation was taken. Note that the malloc'ed `struct` is unnecessary, but mimicks how a more complicated C library would work. The point is not to have the most efficient LFSR implementation, but some workload for benchmarking and investigating the efficiency of Haskell programs.

#### `src/`

has module `LFSR` which is the Haskell wrapper for the C source code.

#### `RunRepeat.hs`

compares the run-times for a run of the LFSR.

```bash
=== RunRepeat =======
#iter:    100000000
Baseline: 0.193323
IO:       0.335369
factor:   1.7347599613082767
```

#### `RunRepeatAlloca.hs`

do the same as `RunRepeat.hs`, but with a different FFI that avoids the `withForeignPtr` calls by
introducing a [`Storable`](http://hackage.haskell.org/package/base-4.11.1.0/docs/Foreign-Storable.html) that gets allocated and used with [`alloca`](http://hackage.haskell.org/package/base-4.11.1.0/docs/Foreign-Marshal-Alloc.html#alloca).

```bash
=== RunRepeatAlloca =======
#iter:    100000000
Baseline: 0.194293
IO:       0.333789
factor:   1.7179671938772885
```

#### `RunAvg.hs`

compares the run-times for the calculation of the average over a run of the LFSR, using Haskell's lists:

```bash
=== RunAvg =========
#iter:    10000000
Baseline: 2.1159999999999998e-2
IO:       1.358567
factor:   64.20448960302458
```

#### `RunAvgStreaming.hs`

does the same as `RunAvg.hs`, but it uses the [`streaming`](http://hackage.haskell.org/package/streaming) library.

```bash
=== RunAvgStreaming ===
#iter:    50000000
Baseline: 0.102534
IO:       0.7660629999999999
factor:   7.471307078627577
```

#### `RunAvgVector.hs`

does the same as `RunAvg.hs`, but it uses the [`Data.Vector.Fusion.Stream.Monadic`](http://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Fusion-Stream-Monadic.html) library.

```bash
=== RunVector =========
#iter:    200000000
Baseline: 0.393445
IO:       0.889796
factor:   2.261551169795016
```

#### `Makefile`

has some useful targets:

- `build`: Builds the executables
- `run`: Builds and runs the executables
- `profile`: Builds the profile executables and runs the profiler, the results are in the `*.prof` files.
