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
- Provide customization only for frame size, border width, focus acceptance, and
  extra frame parameters.
- Keep the core display path small: one fixed candidate frame per minibuffer
  session, reused while live.

## Non-Goals

- Preview support, Consult-specific adapters, per-command local mode,
  golden-ratio sizing, and candidate-width auto-resizing.
- Speculative options, compatibility paths, refactors, or optimizations without
  a listed behavior, clear bug, or measured target.
