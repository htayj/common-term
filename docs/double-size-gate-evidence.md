# Double-size gate evidence

Recorded 2026-11-08. This commit adds the double-size area driver, six-pause
manifest, oracle, pixel comparator, recorder, strict native receipt contract,
owned native adapter and focused acceptance tests.

## Completed checks

- Boundary controls365, recovered by371: 126 Lisp assertions and 47 shell
  controls passed, numeric exits zero and empty stderr. Earlier unsuccessful
  preparation attempts are retained separately in the evidence directory.
- Oracle evidence adopted by371: 152 synthetic controls and 24 real-font
  controls passed. The public font raster/comparator evidence is report362.
- Public positive407: original public MAIN and driver, with only INVOKE-NATIVE
  replaced by the synthetic fixture, reported PASS6/6 and exit0. All six full
  RGBA comparisons matched with no tolerance or masks. Encoder, identify and
  decoder completed; recorder import, publication and positive assertions passed.
  Controller, SBCL and assertion exits were zero. Forward-reference style
  warnings are retained in the execution log.
- Incomplete-native negative427: the runner and assertions exited0 while the
  original public command correctly exited1, FAIL/INCOMPLETE-OR-FAILED, with all
  six scenarios unverified. The incomplete native receipt was refused even
  though all six pixel comparisons matched.

## Retained evidence

Workspace-relative paths under .build/evidence/:

- double-size-handoff-371-20261108-a/HANDOFF.md
- double-size-real-font-362-20261108-a/
- double-first-public-execution-407-20261108-a/REPORT.md
- double-first-public-execution-407-20261108-a/PNG.tsv
- double-incomplete-negative-execution-427-20261108-a/REPORT.md
- double-incomplete-negative-execution-427-20261108-a/PNG.tsv
- vttest-area-runs/run-EzO6R0/RESULT.sexp
- vttest-area-runs/run-wUICAl/RESULT.sexp

Positive RESULT SHA256:
129c2fd59fce1432a5ea855a19def306875b0bebf1aeb8a5622b65aa270cdab9

Expected-negative RESULT SHA256:
83fff73fa2e228ff3181fc1c880c41eab971c12d443d444bd1160e36077cbd37

These are synthetic public-plumbing checks, not native X11/Wayland acceptance.
They do not establish a successful build or real native lifecycle. The original
reports, captures and historical failures are retained locally, not rewritten.
No successful identical check was rerun for this commit. Git's staged whitespace
check passed. Character-set dispatch and unrelated geometry changes are excluded.
