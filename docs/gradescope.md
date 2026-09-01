# Grading Scamper work on Gradescope

A Gradescope autograder for Scamper is an ordinary Scamper program.
It imports the student's file, runs the tests written with the `test` library, and passes them to `gradescope-test-suite`, which prints the JSON Gradescope reads.

An autograder is built from three files in `gradescope/`:

| File | Role |
| --- | --- |
| `gradescope/autograder.scm` | the example harness, to copy and adapt |
| `gradescope/setup.sh` | installs Scamper when the autograder image is built |
| `gradescope/run_autograder` | runs the harness against a submission |

## Setup

1. Write the harness.
   Start from `gradescope/autograder.scm`: import the file the assignment asks students to submit, and end with a call to `gradescope-test-suite`.
2. Zip `setup.sh`, `run_autograder`, and the harness together, as three files at the top level of the archive rather than inside a folder.
3. Upload the zip on the assignment's *Configure Autograder* page with **Ubuntu 22.04** as the base image, and let it build.

Set the assignment's point value to the number of tests written.
Each test is worth one point, awarded only if it passed.

**The assignment must ask for a specific file name.**
The harness imports the student's file by name, so a submission named anything else fails to import and scores zero.

A student uploading their own `autograder.scm` does not displace the harness: `run_autograder` copies the submission first and the harness over it.
A submission can still shadow any *other* file the harness imports, so import only the files the assignment asks for.

## Writing the harness

`gradescope-test-suite` takes a list of the test results produced by the `test` library's `test-case` and `test-exn`, and returns a value that prints as Gradescope's results JSON:

```scheme
(gradescope-test-suite
  (list (test-case "double 4 is 8" equal? 8 (lambda () (double 4)))
        (test-exn "double rejects a string" (lambda () (double "four")))))
```

Each test becomes one Gradescope test case, named by its description.
Its output is the message Scamper shows in the IDE, such as "Expected 8, received 16".

For a case that is not worth one point — a bonus mark, or something scored by hand — construct it directly and include it in the same list:

```scheme
(gradescope-test-suite
  (list (test-case "double 4 is 8" equal? 8 (lambda () (double 4)))
        (gradescope-test-result "style" "passed" 2 3 "Nicely done.")))
```

Two constraints:

+ **The program's output is the results file.**
  Only the call to `gradescope-test-suite` may print.
  A stray top-level expression in the harness corrupts the JSON.
  A stray one in the student's file is harmless, since an imported file's output is discarded.
+ **A failing test is not an error.**
  `test-case` catches the exception a student's code throws and reports it as a failed test.
  An error *outside* a test — in the harness, or at the top level of a student's file — stops the run, and the student receives a zero with the error message.

## Versioning

`gradescope/setup.sh` clones this repository's `main` when the autograder image is built.
To pin a semester to a release, so that later work on Scamper cannot change how already-submitted work is graded, set `SCAMPER_REF` at the top of `gradescope/setup.sh` to a tag:

```bash
SCAMPER_REF="${SCAMPER_REF:-v4.1.0}"
```

Either way, the change takes effect when the autograder is rebuilt.

## When a submission will not run

If the student's file does not compile, its imports fail, or the run exceeds `SCAMPER_TIMEOUT` seconds (120 by default), `gradescope/run_autograder` writes a zero with the error message as the submission's output.
