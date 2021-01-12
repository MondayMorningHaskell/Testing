# Testing in Haskell

This repository accompanies the [Testing Series](https://mmhaskell.com/testing) on the Monday Morning Haskell blog. The series give a tutorial for a few different essential testing tools in Haskell, including [HUnit](https://hackage.haskell.org/package/HUnit), [HSpec](https://hackage.haskell.org/package/hspec) and [Criterion](https://hackage.haskell.org/package/criterion). You can read the code in the articles and follow along with the examples here. There are a couple special "practice" modules where you can try out the examples for yourself. See [FencesPractice.hs](https://github.com/MondayMorningHaskell/Testing/blob/master/src/FencesPractice.hs) and [FencesPracticeTests.hs](https://github.com/MondayMorningHaskell/Testing/blob/master/test/FencesPracticeTests.hs), as referenced in [part 3](https://mmhaskell.com/testing/performance) of the series.

## Repository Instructions

Start by building the code:

```
>> stack build
```

You can then run a series of test suites and benchmarks associated with the library:

```
>> stack build Testing:test:unit-test
>> stack build Testing:test:spec-test
>> stack build Testing:test:fences-tests
>> stack build Testing:bench:fences-benchmark
>> stack build Testing:test:fences-fast-tests
>> stack build Testing:bench:fences-fast-benchmark
```

You're also encouraged to try running the benchmarks with the `--profile` option. This will enable you to see the profiling output for better debugging.

```
>> stack build Testing:bench:fences-benchmark
>> stack build Testing:bench:fences-fast-benchmark
```

Note that when you switch between compiling any part of the project with `--profile` and without the option, Stack will need to recompile all modules and dependencies, which can be a length process.

## Practice for Yourself

[Part 3](https://mmhaskell.com/testing/performance) of the series presents some data structure improvements we can make to an algorithm introduced in Part 2. You can try writing some of the code for that part yourself in [this practice module](https://github.com/MondayMorningHaskell/Testing/blob/master/src/FencesPractice.hs). You are encouraged to use a "Test-Driven-Development" style and write your tests in [this file](https://github.com/MondayMorningHaskell/Testing/blob/master/test/FencesPracticeTests.hs). You can run this test suite like so:

```
>> stack build Testing:test:fences-practice-tests
```
