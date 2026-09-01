# Grading Scamper work on Gradescope

A Gradescope autograder for Scamper is an ordinary Scamper program. It imports
the student's file, runs the tests you wrote with the `test` library, and hands
them to `gradescope-test-suite`, which prints the JSON Gradescope reads.

The three files an autograder is built from live in `gradescope/`:
`autograder.scm`, `setup.sh`, and `run_autograder`.

## Setting one up

1. Write your harness. Start from `gradescope/autograder.scm`: import the file
   the assignment asks students to submit, and end with a call to
   `gradescope-test-suite`.
2. Zip `setup.sh`, `run_autograder`, and your `autograder.scm` together — the
   three files at the top level of the archive, not inside a folder.
3. Upload the zip on the assignment's *Configure Autograder* page, with
   **Ubuntu 22.04** as the base image, and let it build.

Set the assignment's point value to the number of tests you wrote: every test
is worth one point, awarded only if it passed.

**The assignment must ask for a specific file name.** `autograder.scm` imports
the student's file by name, so a submission named anything else fails to import
and scores zero.

A student uploading their own `autograder.scm` does not displace yours —
`run_autograder` copies the submission first and your harness over it — but a
submission may still shadow any *other* file your harness imports, so import
only the files the assignment asks for.

## Writing the harness

`gradescope-test-suite` takes a list of the test results the `test` library
produces — `test-case` and `test-exn` — and returns a value that prints as
Gradescope's results JSON:

```scheme
(gradescope-test-suite
  (list (test-case "double 4 is 8" equal? 8 (lambda () (double 4)))
        (test-exn "double rejects a string" (lambda () (double "four")))))
```

Each test becomes one Gradescope test case, named by its description, and its
output is the same message Scamper would have shown in the IDE — so a student
reads "Expected 8, received 16" rather than just a cross.

For a case that is not simply one point — a bonus mark, or something you scored
by hand — build it yourself and put it in the same list:

```scheme
(gradescope-test-suite
  (list (test-case "double 4 is 8" equal? 8 (lambda () (double 4)))
        (gradescope-test-result "style" "passed" 2 3 "Nicely done.")))
```

Two things to watch:

+ **The program's output is the results file.** Only the call to
  `gradescope-test-suite` may print. A stray top-level expression in
  *your* harness corrupts the JSON. A stray one in the *student's* file is
  fine — an imported file's output is discarded.
+ **A test that fails is not an error.** `test-case` catches the exception a
  student's code throws and reports it as a failed test. An error outside a
  test — in your harness, or at the top level of a student's file — stops the
  run, and the student sees a zero with the error message instead.

## Which Scamper it grades with

`setup.sh` clones this repository's `main` when the autograder is built. To pin
a semester to a release, so that later work on Scamper cannot change how
already-submitted work is graded, set `SCAMPER_REF` at the top of `setup.sh` to
a tag:

```bash
SCAMPER_REF="${SCAMPER_REF:-v4.1.0}"
```

Rebuilding the autograder is what picks up a change either way.

## When a submission will not run

If the student's file does not compile, imports fail, or the run takes longer
than `SCAMPER_TIMEOUT` seconds (120 by default), `run_autograder` writes a zero
with the error message as the submission's output, so the student sees what went
wrong rather than a broken autograder.
