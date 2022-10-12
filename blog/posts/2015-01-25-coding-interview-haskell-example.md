---
title: Coding interview, haskell example
status: published
tags:
- functional-programming
- interview
- haskell
- algorithm
---

Recently I've been looking for a job and as usual in the IT field, I got lots of tests and programming quizz (more on this in another post).
Since I wanted to flex my haskell muscles, I tried to solve one of the problem I got with it.

### Count harmonic slices
Here's the problem:
Given an array `A` of integer count the number of harmonic slices in this array.
An harmonic array is an array where all consecutive elements differ only by the same constant. For example `[-1, 1, 3]` is harmonic as well as `[-1, -1, -1]` but `[-1, 1, 1]` is not. If the array's length is less than 2, it's defined as non harmonic.
An harmonic slice is a sub array: `A.slice(i, j)`.

So for example, the array `A = [-1, 1, 3, 3, 3, 2, 1, 0]` has 5 harmonic slices defined by the indices: `(0, 2) (2, 4) (4, 6) (4, 7) (5, 7)`.

### Classical solution
I'll not spoil the solution here, but it fairly easy to come up with a `O(n)` time complexity, and `O(1)` space complexity. And btw, the platform used for the test did not allow haskell anyway.

### Haskell solution
I first started to replicate the classical solution using Haskell. While it would have lead to a more efficient algorithm, it was not very educational in learning Haskell. So I switched to a very declarative approach:

#### Get all potential slices
First, given a list of `Int`, return all the potential slices for this list. This is easily achieved with list comprehension:

```haskell
subLists :: [Int] -> [[Int]]
subLists xs = [(take a) . (drop b) $ xs | a <- [3..n],
                                          b <- [0..n],
                                          a+b <= n
              ]
              where n = length xs
```

#### Check if a slice is harmonic
This one had me thinking a lot. I tried various approach using recursion or `foldl` trying to replicate a `for` loop approach I would've used in other languages but with limited success.
A declarative approach is actually easier. The trick is to zip the list with itself (minus the first element) to be able to compare an element with the next one.

```haskell
isHarmonic :: [Int] -> Bool
isHarmonic xs
  | length xs < 3 = False
  | otherwise = allEqual $ zipWith (-) xs $ drop 1 xs
  
-- takes a list and return true iff all elements are equals
allEqual :: [Int] -> Bool
allEqual xs = and $ zipWith (==) xs $ drop 1 xs
```

#### Putting it all together
The end result is given by the length of the filtered list:
```
countHarmonicSlices :: [Int] -> Int
countHarmonicSlices = length $ filter isHarmonic $ subLists
```

Which gives in ghci:

```
λ: countHarmonicSlices [-1, 1, 3, 3, 3, 2, 1, 0]
5
```

yeah!

### Final thoughts
Reasoning about space complexity in Haskell is very hard because of lazyness. For the same reason, time complexity is also harder to evaluate. I would tend to think though that this algorithm is vastly less efficient and most surely not `O(n)` since I'm actually evaluating all possible slices.

I'm not convinced "regular" Haskell is a good tool for this kind of task, but that wasn't the point of the exercise anyway.
