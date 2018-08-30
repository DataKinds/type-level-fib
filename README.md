# type-level-fib

_An exercise in the futility of type systems._

This program finds very large fibonacci sums at compile time using the power of the Haskell type system. I made this at 1:30 AM on a night I _really_ should have been doing homework.

## Running this program

```bash
ghc -o type-level-fib main.hs
# wait a bit...
./type-level-fib
```

## Help required

The source of this program is a mess since I couldn't figure out how to iterate the `nextFib` function properly, since its type constantly changes as it contains the data for the sequence itself. I suspect that the proper implementation of `heterogeneousIterate` will have to be a `type family` of some sort, but I am too tired to implement it right now. Alternatively, I could use Template Haskell to ismply repeat `nextFib .` for me.
