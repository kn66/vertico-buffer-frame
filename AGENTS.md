# Repository Guidelines

## Project Structure & Module Organization

This repository is an Emacs Lisp package for displaying Vertico completions in
child frames. Source files live at the repository root:

- `vertico-buffer-frame.el`: main minor mode, child-frame display action, layout,
  fallback, lifecycle, and cleanup.
- `test/vertico-buffer-frame-test.el`: ERT coverage for mode behavior, cleanup,
  fallback, and child-frame display cases.
- `FEATURES.md`: required behavior, explicit non-goals, and feature-scoped
  simplicity constraints.
- `gif/`: README demonstration assets.

Keep README-facing behavior documented in `README.org`.

## Build, Test, and Development Commands

- `make test`: run the ERT suite in batch Emacs.
- `make compile`: byte-compile sources and tests with warnings treated as errors.
- `make package-lint`: run `package-lint` on the main package file.
- `make checkdoc`: check docstrings and documentation conventions.
- `make check-declare`: validate external declarations.
- `make check`: run the full validation set above.
- `make clean-elc`: remove generated `.elc` files and the temporary compile
  directory.

Use `EMACS=/path/to/emacs make check` to validate against a specific Emacs
binary.

## Coding Style & Naming Conventions

Use standard Emacs Lisp indentation and keep `lexical-binding: t` in all Lisp
files. Public names should use the `vertico-buffer-frame-` prefix; internal
helpers and state should use `vertico-buffer-frame--`. Prefer clear names tied
to Vertico, minibuffer ownership, display actions, frame parameters, preview
targets, and cleanup behavior.

Add constants only for shared defaults or non-obvious values. Comments and
docstrings should explain why a fallback, compatibility path, or Emacs behavior
matters; avoid comments that restate the code.

## Testing Guidelines

Tests use ERT and live in `test/vertico-buffer-frame-test.el`. Name tests with
the package prefix and the behavior under test, for example
`vertico-buffer-frame-mode-restores-state`. Add focused coverage when changing
cleanup, layout, fallback, or child-frame display behavior. Run `make test` for
focused validation and `make check` before submitting broader changes.

## Commit & Pull Request Guidelines

Recent commits use short, imperative summaries. Keep commits focused on one
behavioral change when practical.

Pull requests should describe the user-visible behavior, list validation
commands run, and call out any GUI or child-frame smoke testing performed. Add
screenshots or GIF updates only when visible frame placement, sizing, or preview
behavior changes.

## Agent-Specific Instructions

Prefer readable, domain-shaped code over mechanical deduplication. Split
functions around named responsibilities such as child-frame layout, minibuffer
ownership, cleanup, fallback, or compatibility handling.

Before making non-trivial code changes, read `FEATURES.md` and keep the
implementation limited to the listed behavior. When adding or changing
behavior, update `FEATURES.md` first or in the same change so the required
functionality remains explicit.

Implement the simplest maintainable code that satisfies `FEATURES.md`. Do not
add abstractions, options, compatibility paths, optimizations, or unrelated
refactors unless they are needed for a listed feature. Treat performance
requirements as features, and document the reason or target when an optimization
adds complexity.
