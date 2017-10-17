# CodeStats

Creates statistics about source codes, much like cloc or similar 
programs. Same-same, only different.

WARNING: this project is **highly** experimental.

CodeStat can be used in a directory to scan a whole project.

Example output:

```$ codestats 
All lines:       507
Comments:        14
Empty lines:     100
Effective lines: 393
Repeating lines: 15
Repetitions:     156
Net lines:       351
Languages: Cabal, Haskell, Yaml, Text
```

Meaning:

* All lines: every line in any know file encountered
* Comments: lines that are entirely comments and maybe whitespace
* Empty lines: non-comment lines containing only whitespace
* Effective lines: lines that contain more than comments or whitespace
* Repeating lines: number of distinct lines that are appearing more than once
* Repetitions: the "repeating lines" counted as many times as they occur
* Net lines: number of distinct non-empty, non-comment lines
* Languages: list of recognized languages including plain text
