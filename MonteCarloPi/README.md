
A sandbox to play with different libraries for Monte Carlo, Random Number Generators, etc., used from F# 

Pi estimation using three different random geneartors shows little difference (Math.Net MersenneTwister is the slower, but not by orders of magnitude)

10000000
RNG: BuiltIn; Parallel: None
value: 3.1424764; error: 0.028%; msec: 2862
RNG: BuiltIn; Parallel: ArrayParallel
value: 3.1417324; error: 0.004%; msec: 1318
RNG: BuiltIn; Parallel: PLINQ
value: 3.1417712; error: 0.006%; msec: 1441
RNG: AlgLib; Parallel: None
value: 3.1418; error: 0.007%; msec: 3151
RNG: AlgLib; Parallel: ArrayParallel
value: 3.1418068; error: 0.007%; msec: 1335
RNG: AlgLib; Parallel: PLINQ
value: 3.1419476; error: 0.011%; msec: 1283
RNG: MathDotNet; Parallel: None
value: 3.1415524; error: -0.001%; msec: 4310
RNG: MathDotNet; Parallel: ArrayParallel
value: 3.1407308; error: -0.027%; msec: 1863
RNG: MathDotNet; Parallel: PLINQ
value: 3.1418756; error: 0.009%; msec: 1933





GPL v.3 license (using free version http://www.alglib.net/)