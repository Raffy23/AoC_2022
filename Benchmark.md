Benchmark results
=================
**REMEMBER**: The numbers below are just data. To gain reusable insights, you need to follow up on
why the numbers are the way they are.

The Benchmark was run in sbt with `stack bench` on an AMD Ryzen 5800X @ 4,4Ghz.

*Note:* On Windows don't forget to set `chcp 65001` before running the benchmark.


| Benchmark         | Mode | Cnt |     Score   |     |   std dev  |
|:------------------|:----:|:---:|------------:|-----|-----------:|
| main/Day01/Part 1 | mean | 100 | 1.677  ms   | ±   |    ---     |
| main/Day01/Part 2 | mean | 100 | 1.757  ms   | ±   | 21.00  μs  |
| main/Day02/Part 1 | mean | 100 | 400.1  μs   | ±   | 9.157  μs  |
| main/Day02/Part 2 | mean | 100 | 411.1  μs   | ±   | 9.442  μs  |
| main/Day03/Part 1 | mean | 100 | 713.8  μs   | ±   | 8.173  μs  |
| main/Day03/Part 2 | mean | 100 | 721.8  μs   | ±   | 6.837  μs  |