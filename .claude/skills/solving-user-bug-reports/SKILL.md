---
name: solving-user-bug-reports
description: Triages and solves a bug reported by an external user on a constructive GitHub issue - checks scope, captures or infers R/package versions (noting assumptions), reproduces locally or on CI, and if it is a real in-scope bug implements the fix following constructive's shim, constructor and test conventions. Use when given a constructive issue number/URL or a pasted user bug report.
---

# Solving User Bug Reports

Takes one user report from intake to a ready-to-push fix, or to a reasoned "not a bug / out of scope"
verdict. Stop at each **Gate**: if it fails, skip to step 6 and report.

Follow the conventions in CLAUDE.md and in [conventions.md](conventions.md), and commit through
`committing-version-control`. Give a one-line status before each long step.

**Never without the user's explicit go:** commenting on, labelling or closing the issue, pushing a branch,
opening a PR, dispatching CI. These are public. Draft the text, show it, wait.

## 1. Intake and scope

```bash
gh issue view <N> --repo cynkra/constructive --comments
gh issue list --repo cynkra/constructive --state all --search "<key words>" --limit 10   # duplicates
```

Restate the report in two lines: the object, the call, what happened, what was expected.

constructive's job is to generate code that recreates an object: `construct()` and friends
(`construct_dput()`, `construct_base()`, `construct_diff()`, `construct_multi()`, `construct_reprex()`, `construct_dump()`,
`construct_signature()`, `construct_serialize()`, `deparse_call()`), the `opts_*()` functions and the
extension API (`.cstr_*()`). Classify:

| Verdict | Examples |
|---|---|
| **Bug** (in scope) | `construct()` errors on an object, including a corrupted one (corrupted objects must fall back via `is_corrupted_*()` + `NextMethod()`, never error); generated code doesn't parse, errors, or doesn't evaluate to an identical object (`construct()` warns, `construct_issues()` shows the diff); an `opts_*()` argument is ignored or misbehaves; output changes when an unrelated package with S3 methods is loaded (dispatch leak) |
| **Feature request** | class not supported, so output is a faithful but low-level `structure()` call; wish for a more idiomatic constructor or layout. Correct output is not a bug: label `feature` / `classes` |
| **Documented limitation** | things constructive can't recreate exactly (some environments, external pointers, srcrefs...) when the docs or messages already say so |
| **Upstream** | the class's own constructor or methods are broken; constructive's output is right |
| **Duplicate / question / user error** | |

**Gate 1:** only "Bug" continues. Otherwise draft a reply (and suggested label) and go to step 6.

## 2. Environment: capture, infer, record

Fill this table and keep it; it goes in the PR body and the issue reply.

| Item | Value | Source |
|---|---|---|
| R version / OS / locale | | given / inferred |
| constructive version | | |
| packages owning the involved classes (+ vctrs, rlang if relevant) | | |
| timezone / encoding, if dates or strings are involved | | |

Take values from `sessionInfo()`, `reprex` footers or text in the issue (source: **given**). When missing,
**infer** and say how, never silently:
- issue date vs release dates: `gh release list` / `git tag` for constructive, the CRAN archive
  (`https://cran.r-project.org/src/contrib/Archive/<pkg>/`) or `pak::pkg_history()` for other packages;
  assume the latest CRAN release at the issue date unless the text suggests the GitHub version
- syntax and messages: `|>` means R >= 4.1, `\(x)` R >= 4.1, error wording that changed between versions,
  file paths (`C:\` means Windows), non-ASCII output (locale)
- the code path: if the bug sits in a `with_versions(pkg < "x")` branch, that bounds the version

Write each inference as an **Assumption: ... because ...** line. If an assumption is decisive and can't be
checked (e.g. only one version range could trigger it), ask the user/reporter rather than guess.

## 3. Reproduce

Work in a worktree off up-to-date main, never `git checkout` in the main checkout:

```bash
git fetch origin
git worktree add .claude/worktrees/b-<N>-<slug> -b b-<N>-<slug> origin/main
```

Write the minimal repro in the scratchpad, not the repo. Run R with the env this machine needs
(the C locale gives spurious failures; anytime must load before testthat to avoid a tsibble crash):

```bash
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 NOT_CRAN=true Rscript -e 'library(anytime); pkgload::load_all("<worktree>"); source("<scratch>/repro.R")'
```

In the repro, check more than "does it error": `code <- construct(x)`, `construct_issues()`, and
`waldo::compare(x, eval(parse(text = code$code)))`. Shrink the object until removing anything makes
the problem disappear.

Try in this order, stopping at the first that reproduces:
1. **Current main** with locally installed packages. If it doesn't reproduce, also try the **reported
   constructive version** (`git worktree add ... v<x.y.z>`); fixed on main means "already fixed", gate fails.
2. **Reported package versions**: install them into a throwaway library and put it first in `.libPaths()`:
   `pak::pkg_install("<pkg>@<version>", lib = "<scratch>/lib-<pkg>-<version>")`.
3. **Reported R version / OS**: use `rig list` / `rig run <ver>` if available, docker `rocker/r-ver:<ver>` if
   available; otherwise go to CI.
4. **CI**: write the regression test (step 5 conventions) with the skips that match the suspected
   conditions and push the branch (ask first). PR CI only runs the smoke job (ubuntu, one R); the full
   OS x R matrix must be dispatched (ask first):
   `gh workflow run rcc --repo cynkra/constructive --ref b-<N>-<slug> -f ref=b-<N>-<slug> -f versions-matrix=true -f run-rcc-full=true`
   Note: the smoke job auto-commits `.new.md` snapshots and roxygen changes back to PR branches a few
   minutes after each push; inspect and revert those that come from the wrong environment.

**Gate 2:** reproduced (locally or on CI), or the root cause is proven by reading the code (state the exact
line and the inputs that reach it). If not reproducible after reasonable effort, draft a reply asking for
a reprex + `sessionInfo()` and go to step 6.

## 4. Root cause

Find the line, then explain *why*: wrong constructor choice, missing `is_corrupted_*()` check, attribute
repair, version-dependent behaviour of an upstream package, or **accidental S3 dispatch** (a base generic
dispatched on the user's object; frequent, see conventions.md). Check whether the same pattern exists in
sibling constructors (`git grep`); fix siblings only if they are demonstrably affected, mention the rest.

## 5. Fix and test

Read [conventions.md](conventions.md) before writing code. In short: minimal diff that reuses existing
machinery, no accidental dispatch (shims in `R/bypass.R`, methods called by name), constructor anatomy
respected, regression snapshot test in `tests/testthat/test-s3-<class>.R`.

1. Write the regression test first and see it fail (or show the wrong snapshot).
2. Fix; the test passes and the snapshot shows the correct, idiomatic output. Read the snapshot diff.
3. `devtools::document()` if roxygen changed; keep unrelated `RoxygenNote` / NAMESPACE churn out.
4. Run the touched test file, then the full suite **in the background**:
   `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 NOT_CRAN=true Rscript -e 'library(anytime); devtools::test("<worktree>")'`
   Expect 0 failures; any `.new.md` you didn't intend is a regression or an environment artefact: explain it.
5. Don't edit NEWS.md (the maintainer writes it); propose a NEWS line in the PR body instead.
6. Commit via `committing-version-control`.

## 6. Report

To the user, concisely:
- verdict (bug / feature / limitation / upstream / duplicate / can't reproduce) and why
- environment table with given vs inferred values and the **assumptions**
- how it was reproduced (local, versions used, or CI run link)
- root cause (`file:line`) and fix summary, test added, suite result
- drafts, not posted: PR title/body (`Fixes #<N>`, environment, repro, root cause, proposed NEWS line,
  ending with the Claude Code attribution line) and a reply to the reporter (thank them, what was wrong,
  how to get the fix, e.g. `pak::pak("cynkra/constructive")` once merged)

Then wait for the go to push / open the PR / comment.
