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
- Provide `vertico-buffer-frame-local-mode` so `vertico-multiform-mode` can
  enable the child-frame display per command or completion category.
- Size the candidate frame from the parent frame using the golden ratio, and
  keep it centered as the parent frame changes size.
- Keep candidate and Consult preview child frames opaque by default, even when
  ordinary frames are configured as transparent.
- When Consult is loaded, mirror Consult's active window preview in a preview
  child frame overlaid on the lower-right of the candidate frame.
- Keep candidate-frame painting owned by Vertico's redisplay path; Consult
  preview mirroring must not directly show an unpainted candidate frame.
- Mirror Consult's window-local insertion preview overlays, such as
  `consult-yank-from-kill-ring` previews, in the preview child frame.
- Keep Embark collect/export buffers visible after the minibuffer exits, like
  they are when Vertico's buffer display is not shown in a child frame.
- Size the Consult preview frame from the candidate frame using the golden
  ratio.
- Allow Consult preview mirroring to be toggled globally or for the current
  minibuffer session.
- Provide customization only for golden-ratio scale, border width, focus
  acceptance, Consult preview enablement, candidate-width auto-resizing,
  candidate-frame reuse, and extra frame parameters.
- Pool candidate child frames across minibuffer sessions instead of deleting
  them on exit: creating a child frame realizes every face for the new frame
  (measured ~260 ms per session under a loaded theme on w32, versus ~4 ms to
  reshow a hidden frame). Reuse a pooled frame only when its creation-time
  configuration fingerprint still matches; otherwise, or on any error, fall
  back to the create-and-delete behavior. Do not show stale candidates from
  the previous minibuffer session while a pooled frame is being reused.
  Disabling the mode or running the cleanup command deletes pooled frames.
- Optionally grow the candidate frame width to fit the widest visible
  candidate, keeping the frame centered, capped below the parent frame width so
  a margin remains, and never below the golden-ratio width.
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
