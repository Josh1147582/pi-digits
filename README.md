# pi-digits

Calculate requested digits of Pi.

Built using [The BBP Algorithm for Pi](http://www.davidhbailey.com/dhbpapers/bbp-alg.pdf).

## Building

Install `stack`, then run `stack build` from inside the repository.

## Running

Run `stack exec pi-digits-exe`, or execute the binary found in `./.stack-work/install/`.

## TODO

- [X] Add a function for converting results to decimal (and binary).
- [X] Implement threading.
- [X] Implement `optparse-applicative` for more formal/extensive arg parsing.
- [X] ~~Replace the mess in `prompt` with a monad.~~ Issue corrected by using `optparse-applicative`.
- [X] Separate into different files. Possibly parsing and logic files.
- [X] ~~Find a better way to transfer "globals", like `delim` and `printFun`.~~ `parseIndex` handles all parsing and calling logic now.
- [X] Implement a faster mod operation, to allow for larger numbers (like 12345678901234567890). ~~It will likely be implemented with the algorithm explained in the paper.~~ Used a [slightly faster, less iterative way](https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation), implemented with [this gist](https://gist.github.com/trevordixon/6788535).
- [ ] Customize print behavior and frequency. Flush output every N digits, or something similar.
