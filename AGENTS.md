# AGENTS.md

## Maintenance Guidelines

- Prefer readable code. The control flow, Emacs/Vertico behavior being modeled,
  and likely scope of future changes should be easy to follow.
- Add abstraction, sharing, or constants only when they make the code easier to
  read or change. Avoid mechanical deduplication.
- Split functions only around behavior or responsibilities that can be named in
  the package domain, such as child-frame layout, minibuffer ownership, cleanup,
  preview target resolution, or compatibility handling. Avoid adding tiny
  one-off helper fragments.
- Use comments and docstrings to explain why code exists, which Emacs or package
  behavior it relies on, and why fallbacks or exceptions are needed. Do not
  restate what the code already says.
- Prefer clear names over short names, and keep terminology aligned with the
  existing code, Vertico, Emacs completion, frame parameters, display actions,
  and preview targets.
- Use constants to name non-obvious values or shared defaults. Direct literals
  are acceptable when they are clearer in context, especially for standard Emacs
  symbols such as frame parameters, hook names, face names, completion
  categories, display-buffer actions, and text-property names.
- When changing cleanup, layout, target resolution, caching, or defensive error
  handling, add focused ERT coverage for the behavior and update the graphical
  smoke-test checklist if the visible child-frame behavior changes.
