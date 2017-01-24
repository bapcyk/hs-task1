# hs-task1

Test Haskell task implementing simple Git tool.

## Task

- Get GIT difference between any 2 commits on master branch at a local bare
  repository
- Search in the text difference for "bad" words (like "strcpy" and "sprintf")
- If there are - report in a HTML file:
  - which files have those words
  - show lines with highlighted words and the line numbers

## Implementation

TODO

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
```

## Example of output

TODO
