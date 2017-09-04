# pi-digits

Calculate requested digits of Pi.

Built using [The BBP Algorithm for Pi](http://www.davidhbailey.com/dhbpapers/bbp-alg.pdf).

## Building

Install `stack`, then run `stack build` from inside the repository.

## Running

Run `stack exec pi-digits-exe`, or execute the binary found in `./.stack-work/install/`.

## TODO

- [X] Add a function for converting results to decimal (and binary).
- [ ] Implement a faster mod operation, to allow for larger numbers (like 12345678901234567890). It will likely be implemented with the algorithm explained in the paper.
- [ ] Replace the mess in `prompt` with a monad.
- [ ] Implement threading?
