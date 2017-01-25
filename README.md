# hs-task1

Test Haskell task implementing simple Git tool.

## Task

- Get GIT difference between any 2 commits on master branch at a local bare
  repository of any files or filtered by extensions (*.cpp, *.h, etc)
- Search in the text difference for "bad" words (like "strcpy" and "sprintf")
- If there are - report in a HTML file:
  - which files have those words
  - show lines with highlighted words and the line numbers

## Implementation

In this task, in contrast to hs-task2, I avoided to use big packages which can
help to solve the task (like parsec, even regex-tdfa - for parsing), because I
want to demonstrate more Haskell code.

All functions are located in the `Lib.hs`. This module is splitted on sections:

- Types: all types declarations
- Parse utilities: parse diff output
- Read instances: read diff line to appropriate data type
- Search words utilities: search words and highlight them
- Process input utilities: process diff line w/ context
- HTML output utilities: output result in HTML
- Git utilities: run Git and process its output

Git output is parsed by extracting one of the declared data types - which
"understand" current input line. It's acomplish by instantiation of `Read` class
for that types. A tail of the line (which can nnot be parsed) is kept in a state
(`State` monad is used for this goal).

The parsing looks as a rules set a little bit due to `_isExpectedBefore`
function, based on `MaybeT guard`. It's not BNF but helps to parse :)

Searching of the words does not use `words` because I need to keep "gaps" too:
the text is formatted, it's a code, so I need to save gaps of spaces.

Then all found words are highlighted by `HiStr` constructor.

A processing of input lines keeps the state of processing in `Ctx` value. It's
run w/ `foldl` function over all input lines. The context keeps current files,
range, current lines number (they are modified while lines are processed),
output, etc.

HTML output escapes symbols (to avoid HTML breaking and to keep spaces like in
the code). Generated HTML are encoded as UTF-8. Output file is linked w/ CSS
"task1.css" which can be used to customize a representation. All output types
instantiate special type class `ShowHtml` w/ one function: `showHtml`.

Git utilities run git (w/ specified path to its binary file), process output,
generate HTML. They consume revisions, git binary, "bad" words, file extensions...

## Alternative implementation

If we want to have less code, next packages can be used for this task:

- lens
- parsec or regex-*
- blaze
- conduit
- etc

## Usage

```
stack exec task1-exe -- -b "word0 word1" --output=some-file.json -git C:\git\bin
```
Options syntax:

```
SYNTAX: task1 [options...]
  -h, -?   --help          print this help
  -r STR   --rev0=STR      from revision (default: HEAD~1)
  -R STR   --rev1=STR      to revision (default: HEAD)
  -b STR   --badwords=STR  badwords string
  -o FILE  --output=FILE   output file (default: output.html)
  -g PATH  --git=PATH      path to Git binary (default: git)
  -x STR   --exts=STR      extensions, string like "*.cpp *.hpp" (default: all)
```

## Example of output

See `example-output/output.html` (w/ `task1.css`).
