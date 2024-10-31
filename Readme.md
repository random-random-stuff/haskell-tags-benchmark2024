# Intro

Following is results of running popular Haskell tag generation program on the sources in this repository. This mimics the workflow where tags are collected for both current project and its dependencies downloaded from Hackage.

Size of sources is
```
$ cloc all-packages/
   17091 text files.
   10669 unique files.
    6482 files ignored.

github.com/AlDanial/cloc v 2.00  T=5.48 s (1947.4 files/s, 759645.3 lines/s)
--------------------------------------------------------------------------------
Language                      files          blank        comment           code
--------------------------------------------------------------------------------
JSON                            748             62              0        1548372
Haskell                        8218         231940         438414        1106907
C                               375          31581          90526         340734
Bourne Shell                     31          13024          23030          57581
C/C++ Header                    345           7173          29689          56985
XML                               4            498            405          50798
Markdown                        533          16093             35          39644
JavaScript                       28           3035           3110          13008
C++                               7           1145           2072          11471
Text                            185           1591              0           9450
Fortran 77                        2              0              0           6853
PHP                               2              3              4           6845
yacc                              1            750              0           3837
Logos                             3            558             11           3814
Objective-C                       5            718            343           3006
m4                               16            238             41           1248
SVG                               7              6            124            919
YAML                             36             77             84            647
HTML                              6             32              7            571
Java                              3            113            207            547
TOML                             78             83             46            355
CSV                               1              0              0            273
Python                            4             39             27            171
make                             11             67             34            169
CSS                               2             12              0            132
Coq                               1             19              1             76
GLSL                              6             22             20             74
XSLT                              1              9              8             59
TeX                               2             11              1             37
Ant                               1              5              7             26
Nix                               2              2              0             20
Bourne Again Shell                1              6              3             18
DOS Batch                         3              0              0              4
HCL                               1              3              0              2
--------------------------------------------------------------------------------
SUM:                          10669         308915         588249        3264653
--------------------------------------------------------------------------------
```

In order to compare raw tag generation capabilities, all the generators were executed on the precomputed list of files created by

```
$ find . -name '*.hs' -o -name '*.lhs' -o -name '*.x' -o -name '*.y' >files.txt
```

# Results

### `hasktags`

Does not seem to support using multiple threads, needs to be linked with `-threaded` RTS for that.

```
$ hasktags --ctags -o tags.hasktags STDIN +RTS -s <files.txt
  31,339,613,424 bytes allocated in the heap
   2,811,243,456 bytes copied during GC
     456,168,000 bytes maximum residency (13 sample(s))
       9,543,848 bytes maximum slop
            1010 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      7488 colls,     0 par    1.085s   1.088s     0.0001s    0.0009s
  Gen  1        13 colls,     0 par    0.815s   0.817s     0.0629s    0.3049s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    6.930s  (  6.960s elapsed)
  GC      time    1.900s  (  1.905s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    8.831s  (  8.865s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    4,522,129,079 bytes per MUT second

  Productivity  78.5% of total user, 78.5% of total elapsed
```

### `fast-tags`

Is linked with `-threaded` RTS but uses only one core by default.

```
$ fast-tags -o tags.fasttags - +RTS -s -N <files.txt
...
  15,367,043,536 bytes allocated in the heap
   1,508,890,464 bytes copied during GC
     367,165,080 bytes maximum residency (11 sample(s))
       3,632,056 bytes maximum slop
             782 MiB total memory in use (12 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       546 colls,   546 par    1.725s   0.372s     0.0007s    0.0038s
  Gen  1        11 colls,    10 par    1.294s   0.108s     0.0098s    0.0201s

  Parallel GC work balance: 50.54% (serial 0%, perfect 100%)

  TASKS: 132 (1 bound, 131 peak workers (131 total), using -N32)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.006s  (  0.002s elapsed)
  MUT     time    7.761s  (  0.783s elapsed)
  GC      time    3.019s  (  0.480s elapsed)
  EXIT    time    0.018s  (  0.002s elapsed)
  Total   time   10.805s  (  1.268s elapsed)

  Alloc rate    1,979,923,004 bytes per MUT second

  Productivity  71.8% of total user, 61.8% of total elapsed
```

### `ghc-tags`

This one seems to use all the available cores.

Cold run:

```
$ ghc-tags -c -o tags.ghctags +RTS -s <files.txt
...

  65,148,503,384 bytes allocated in the heap
   9,412,282,856 bytes copied during GC
     254,849,560 bytes maximum residency (26 sample(s))
       7,196,136 bytes maximum slop
             744 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      3411 colls,  3012 par    7.028s   2.334s     0.0007s    0.0039s
  Gen  1        26 colls,    21 par    4.060s   0.627s     0.0241s    0.0820s

  Parallel GC work balance: 51.46% (serial 0%, perfect 100%)

  TASKS: 62 (1 bound, 61 peak workers (61 total), using -N16)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   19.168s  (  3.316s elapsed)
  GC      time   11.089s  (  2.961s elapsed)
  EXIT    time    0.003s  (  0.001s elapsed)
  Total   time   30.259s  (  6.278s elapsed)

  Alloc rate    3,398,801,107 bytes per MUT second

  Productivity  63.3% of total user, 52.8% of total elapsed
```

Subsequent run is much faster thanks to the created `tags.ghctags.mtime`:
```
   4,398,803,080 bytes allocated in the heap
     776,146,408 bytes copied during GC
      96,741,952 bytes maximum residency (7 sample(s))
       3,765,608 bytes maximum slop
             345 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       774 colls,   401 par    0.377s   0.249s     0.0003s    0.0032s
  Gen  1         7 colls,     1 par    0.350s   0.199s     0.0284s    0.0610s

  Parallel GC work balance: 24.79% (serial 0%, perfect 100%)

  TASKS: 61 (1 bound, 60 peak workers (60 total), using -N16)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    1.854s  (  1.830s elapsed)
  GC      time    0.726s  (  0.447s elapsed)
  EXIT    time    0.002s  (  0.001s elapsed)
  Total   time    2.583s  (  2.278s elapsed)

  Alloc rate    2,372,010,326 bytes per MUT second

  Productivity  71.8% of total user, 80.3% of total elapsed
```
