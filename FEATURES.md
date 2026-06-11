# Features

This file defines the feature boundary for `vertico-buffer-frame`. Keep it as a
short development guardrail: user-visible behavior, compatibility commitments,
performance-sensitive behavior, and non-goals. Put usage details in
`README.org` and implementation details in tests or code.

## Core Behavior

- Display Vertico's `vertico-buffer-mode` candidate buffer in a centered child
  frame when child frames can be used.
- Fall back to the previous Vertico buffer display action when child-frame use
  is unavailable or fails.
- Own child frames per minibuffer session, including recursive sessions, and
  restore saved Vertico state when frame display is disabled.
- Provide `vertico-buffer-frame-local-mode` so `vertico-multiform-mode` can
  enable the child-frame display per command or completion category.
- Size the candidate frame from the parent frame using the golden ratio, and
  keep it centered as parent geometry or visible candidate width changes.
- Keep candidate and Consult preview child frames opaque by default.
- When Consult is loaded, mirror Consult's active window preview in a preview
  child frame overlaid on the lower-right of the candidate frame, including
  window-local insertion preview overlays.
- Keep Embark collect/export buffers visible after the minibuffer exits, like
  they are when Vertico's buffer display is not shown in a child frame.
- Allow Consult preview mirroring to be toggled globally or for the current
  minibuffer session.

## Compatibility Commitments

- Candidate-frame painting belongs to Vertico's redisplay path; Consult preview
  mirroring must not directly show an unpainted candidate frame.
- Public customization is limited to layout scale, border/fringe/margin
  appearance, focus acceptance, Consult preview enablement, candidate-width
  auto-resizing, candidate-frame reuse, and extra frame parameters.
- Cleanup must be safe to call repeatedly, and disabling the mode or running the
  cleanup command must delete pooled frames.

## Performance Commitments

- Pool candidate child frames across minibuffer sessions when their
  creation-time configuration still matches. Creating a child frame realizes
  every face for the new frame (measured around 260 ms per session under a
  loaded theme on w32, versus around 4 ms to reshow a hidden frame).
- Do not show stale candidates while a pooled frame is reused.
- Warm up child-frame display once per Emacs session in a quiet context after
  startup, so the first minibuffer session does not pay first-child-frame face,
  font, and display-backend initialization, which has crashed Emacs on some
  platforms.
- Keep the core display path small: one candidate frame and at most one Consult
  preview frame per minibuffer session, reused while live.

## Non-Goals

- Fixed-size display, built-in candidate preview resolvers, and per-command
  Consult preview adapters.
- Custom prompt or input line wrapping; the prompt line follows
  `vertico-buffer-mode`'s own truncation and wrapping behavior unchanged.
- Speculative options, compatibility paths, refactors, or optimizations without
  a listed behavior, clear bug, or measured target.
