# Features

This file defines the feature boundary for `vertico-buffer-frame`. Keep it
short: list user-visible behavior, compatibility commitments, explicit
performance requirements, and non-goals. Put implementation details in tests,
code, or `README.org`.

## Required Behavior

- Display Vertico's `vertico-buffer-mode` candidate buffer in a centered child
  frame when child frames can be used.
- Fall back to the previous Vertico buffer display action when child-frame
  display is unavailable or fails.
- Own and clean up child frames per minibuffer session, and restore saved
  Vertico state when `vertico-buffer-frame-mode` is disabled.
- Support recursive minibuffer sessions without reusing or deleting another
  session's candidate child frame.
- Size the candidate frame from the parent frame using the golden ratio, and
  keep it centered as the parent frame changes size.
- When Consult is loaded, mirror Consult's active window preview in a preview
  child frame overlaid on the lower-right of the candidate frame.
- Size the Consult preview frame from the candidate frame using the golden
  ratio.
- Provide customization only for golden-ratio scale, border width, focus
  acceptance, Consult preview enablement, and extra frame parameters.
- Keep the core display path small: one candidate frame and at most one Consult
  preview frame per minibuffer session, reused while live.

## Non-Goals

- Per-command local mode, fixed-size display, candidate-width auto-resizing, and
  per-command Consult preview adapters.
- Speculative options, compatibility paths, refactors, or optimizations without
  a listed behavior, clear bug, or measured target.
