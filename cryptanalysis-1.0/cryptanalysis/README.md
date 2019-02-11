# CS152 Assignemnt 1 - Cryptanalysis
__Student repository__

The problem statement is in the accompanying `.pdf`. This document is to inform
you concretely about what is expected in your submission and some tips on how to
navigate the various modules in here. We also detail how to make use of the
sample implementation.

Make sure you follow the crisp instructions below to help us automatically grade
your submission. **Most importantly do not commit plagiarism. See distinction
between plagiarism and discussion
[here](https://www.cse.iitb.ac.in/~supratik/courses/copying-and-discussion.html)**.

The assignment essentially requires you to implement `dictionary-closure`,
`secret-word-enumeration`, some statistical analysis functions, and a driver
function: `crack-cipher`.

# Instructions
1. EDIT:
   - main.rkt
   - dict-closure.rkt
   - statistics.rkt
   - strategies.rkt
2. DO NOT EDIT:
   - utils.rkt
3. `crack-cipher` in main.rkt should implicitly use `utils:ciphertext` and
   `utils:cipher-word-list`.
4. For the function skeletons we provide,
   - do not change the arguments,
   - add or reduce arguments,
   - remove them.
5. Make sure you take a look at the utilities in `utils.rkt`, before starting
   the assignment.
6. Each module has instructions or some text to guide you. Don't ignore them.
7. Keep your code nicely indented and documented, if you wish to get any partial
   credits. This requires conscious effort and takes up time - pace your work
   well!
8. You are free to add more modules to your submission. They must be implemented
   by you. You must `require` `utils.rkt` in your new module.
9. Make use of built-in racket functions as much as you can. Why re-invent the
   wheel? Getting good at reading docs is a valuable skill.
10. Detailed submission instructions will be intimated on Moodle later!

## FAQs
1. I can't wrap my head around the `prefix-in` business.
   - `require` imports all top-level functions. The `prefix-in` adds a prefix,
     to group them.
	 This allows you to define functions with the same name in different modules
     without name clashes. More importantly, it clearly indicates where this
     function was imported from, making your code more readable.
2. What should I start with?
   - We recommend that you begin with the statistics module and then a dumb
     `etai` strategy. At this point you can start working on `crack-cipher`.
   - Add in `dictionary-closure`, improving the strategy, and
     `secret-word-enumeration` incrementally. Don't try to do everything at
     once.
3. How will you grade my submission?
   - We'll replace `utils.rkt` with our own (which has strictly more functions
     than the one we've provided).
   - We won't run your `main.rkt`, just require `crack-cipher` from your
     `main.rkt` (and other functions of interest from resp. modules) into our
     test-script.
   - You better respect the function signatures.
4. Any tips for an additional module that I'm including in my submission?
   - Yes! See point 8 in "Instructions"
5. I think I found a bug in the starter kit.
   - Please report it on a Moodle thread.

# Using the sample implementation
We've provided an executable, for a few platforms. We highly recommend that you
use a Linux machine for this assignment. If you don't have one, and you cannot
use an SL3 or SL2 machine (perhaps even remotely), contact the TA.
**We have not tested Windows and Mac executables rigorously. Contact the TA if
they don't work.**

You can invoke the,
0. `strat:etai` with the `-e` flag
1. `dictionary-closure` with the `-d` flag
2. `secret-word-enumeration` with the `-s` flag
3. `crack-cipher` with the `-c` flag

Only one of the flags must be applied in each invocation.
To get detailed information on how to use it, call it with the `-h` flag:
```
demo [ <option> ... ] <key-file> <cipher-file>
  
  * You must always supply the input files (key and ciphertext) to this executable.
  * The <key-file> is ignored in --crack-cipher and --strat-etai modes, a blank key
    is used instead.
  * The <cipher-file> is ignored in --secret-word-enum.
  
 where <option> is one of
/ -e, --strat-etai : List substitutions computed by strategy etai on <cipher-file> on a blank key
| -d, --dict-closure : Extend partial key in <key-file> using ciphertext from <input-file> using dictionary closure
| -s, --secret-word-enum : Perform secret word enumeration using partial key in <key-file>
\ -c, --crack-cipher : Crack cipher using a blank key and ciphertext from <cipher-file>
 

  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 /|\ Brackets indicate mutually exclusive options.
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
 
 KEY FORMAT
 ----------
 The key should look like: "g r a _ _ _ _ p s _ _ _ _ _ z b c _ _ _ _ k l _ _ _ _"
 It should 26 space separated characters, lowercase, monoalphabetic. If a badly formatted
 key is provided, behaviour is unspecified.
```

## Examples
The executable is present in the `demo-*/bin/` directory, do not descend into that directory.
Call it like so from the root directory of this strater-kit:

```
./demo-linux-7.0/bin/demo -h
./demo-win-7.0/bin/demo   -c <path to key.txt> encrypted/01.txt
./demo-mac-7.0/bin/demo   -s <path to key.txt> encrypted/01.txt
./demo-linux-7.0/bin/demo -d <path to key.txt> encrypted/01.txt
./demo-linux-7.0/bin/demo -e <path to key.txt> encrypted/01.txt
```

# References
* Simon Singh's [supplementary
  website](http://www.simonsingh.net/The_Black_Chamber/chamberguide.html "The
  BLACK Chamber") with some hints on cryptanalysis.
* Another good source of strategies and generic approaches on [Practical
  Cryptography](http://practicalcryptography.com/cryptanalysis/).
* We've sourced words (ie, dictionary), and statistics from [Peter Norvig's
  works](http://norvig.com/mayzner.html).
* A [fun tool](https://www.guballa.de/vigenere-solver) on a different cipher
  scheme.

------- Hope you enjoy doing this assignment as much as we did making it! ------
---------------------------------- Best luck! ----------------------------------
