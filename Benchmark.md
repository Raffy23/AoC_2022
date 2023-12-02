Benchmark results
=================
**REMEMBER**: The numbers below are just data. To gain reusable insights, you need to follow up on
why the numbers are the way they are.

The Benchmark was run with `stack bench` on an AMD Ryzen 5800X @ 4,4Ghz.

*Notes:* 
* On Windows don't forget to set `chcp 65001` before running the benchmark.
* Running specific test with `stack bench --benchmark-arguments "--match glob main/DayXX/*"`


| Benchmark         |    Mean      |     |   std dev  |
|:------------------|-------------:|-----|-----------:|
| Day01 Part 1      |  143.9  μs   | ±   | 582.8  ns  |
| Day01 Part 2      |  142.1  μs   | ±   | 615.7  ns  |
| Day02 Part 1      |  400.1  μs   | ±   | 9.157  μs  |
| Day02 Part 2      |  411.1  μs   | ±   | 9.442  μs  |
| Day03 Part 1      |  713.8  μs   | ±   | 8.173  μs  |
| Day03 Part 2      |  721.8  μs   | ±   | 6.837  μs  |
| Day04 Part 1      |  359.5  μs   | ±   | 5.796  μs  |
| Day04 Part 2      |  369.5  μs   | ±   | 4.968  μs  |
| Day05 Part 1      |  349.5  μs   | ±   | 5.088  μs  |
| Day05 Part 2      |  346.3  μs   | ±   | 3.440  μs  |
| Day06 Part 1      |  6.679  μs   | ±   | 33.41  ns  |
| Day06 Part 2      |  739.9  μs   | ±   | 9.422  μs  |
| Day07 Part 1      |  173.1  μs   | ±   | 1.047  ns  |
| Day07 Part 2      |  174.1  μs   | ±   | 3.179  μs  |
| Day08 Part 1      |  6.787  ms   | ±   | 49.56  μs  |
| Day08 Part 2      |  9.138  ms   | ±   | 45.03  μs  |
| Day09 Part 1      |  3.152  ms   | ±   | 41.68  μs  |
| Day09 Part 2      |  4.726  ms   | ±   | 61.77  μs  |
| Day10 Part 1      |  24.37  µs   | ±   | 231.5  ns  |
| Day10 Part 2      |  15.32  ns   | ±   | 152.9  ps  |
| Day11 Part 1      |  131.6  µs   | ±   | 2.207  µs  |
| Day11 Part 2      |  84.67  ms   | ±   | 1.493  ms  |
| Day12 Part 1      |  2.555  ms   | ±   | 52.78  µs  |
| Day12 Part 2      |  2.314  ms   | ±   | 46.66  µs  |
| Day13 Part 1      |  1.294  ms   | ±   | 20.28  µs  |
| Day13 Part 2      |  2.051  ms   | ±   | 67.55  µs  |
| Day14 Part 1      |  11.84  ms   | ±   | 199.3  µs  |
| Day14 Part 2      |  476.0  ms   | ±   | 1.727  ms  |
| Day15 Part 1      |  23.39  µs   | ±   | 236.8  ns  |
| Day15 Part 2      |  3.105  s    | ±   | 46.87  ms  |
| Day16 Part 1      |  502.3  ms   | ±   | 6.641  ms  |
| Day16 Part 2      |  28.68  s    | ±   | 268.5  ms  |
| Day17 Part 1      |  18.63  ms   | ±   | 529.1  µs  |
| Day17 Part 2      |  40.26  ms   | ±   | 532.1  µs  |
| Day18 Part 1      |  2.133  ms   | ±   | 20.57  µs  |
| Day18 Part 2      |  82.38  ms   | ±   | 3.017  ms  |
| Day19 Part 1      |  28.85  s    | ±   | 460.6  ms  |
| Day19 Part 2      |  11.17  s    | ±   | 210.2  ms  |
| Day20 Part 1      |  4.039  s    | ±   | 24.71  ms  |
| Day20 Part 2      |  42.60  s    | ±   | 1.605  s   |
| Day21 Part 1      |  2.169  ms   | ±   | 327.8  µs  |
| Day21 Part 2      |  2.255  ms   | ±   | 39.95  µs  |
| Day22 Part 1      |  2.348  ms   | ±   | 66.14  µs  |
| Day22 Part 2      |  2.229  ms   | ±   | 118.2  µs  |
| Day23 Part 1      |  31.87  ms   | ±   | 714.7  µs  |
| Day23 Part 2      |  3.349  s    | ±   | 20.25  ms  |
| Day24 Part 1      |  13.03  s    | ±   | 61.69  ms  |
| Day24 Part 2      |  34.87  s    | ±   | 82.70  ms  |
| Day25 Part 1      |  93.46  µs   | ±   | 1.357  µs  |