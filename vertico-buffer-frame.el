;;; vertico-buffer-frame.el --- Child-frame display for Vertico completions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: Nobuyuki Kamimoto
;; Assisted-by: OpenAI Codex:GPT-5
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (vertico "2.8"))
;; Keywords: convenience, frames, minibuffer
;; URL: https://github.com/kn66/vertico-buffer-frame
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `vertico-buffer-frame-mode' displays Vertico's `vertico-buffer-mode'
;; candidate buffer in a centered child frame.
;;
;; Vertico renders the candidates.  This package only installs a
;; `vertico-buffer-display-action', creates one golden-ratio-sized child frame
;; per minibuffer session, falls back to the previous display action when child
;; frames are unavailable, and deletes owned child frames when the minibuffer
;; exits.  When Consult is loaded, Consult's ordinary window preview can be
;; mirrored in a second child frame overlaid on the candidate frame.

;;; Code:

(require 'cl-lib)
(require 'vertico)
(require 'vertico-buffer)

(defgroup vertico-buffer-frame nil
  "Display `vertico-buffer-mode' windows in child frames."
  :group 'vertico
  :prefix "vertico-buffer-frame-")

(defcustom vertico-buffer-frame-golden-ratio-scale 1.0
  "Scale factor applied to the golden-ratio child frame size.
A value of 1.0 uses a frame one golden-ratio step smaller than the largest
golden rectangle fitting in the parent frame.  Values at or above the golden
ratio use the full fitting golden rectangle."
  :type 'number)

(defcustom vertico-buffer-frame-border-width 1
  "Width of the child frame border in pixels."
  :type 'natnum)

(defcustom vertico-buffer-frame-fringes 'inherit
  "Fringe widths of the candidate child frame.
The value controls the left and right fringes so they need not be configured
individually:

  `inherit'  Use the parent frame's fringe widths.  This is the default and
             lets candidate formatting that relies on the fringe work without
             extra setup, such as a `left-fringe' display specification marking
             the current candidate.
  nil        Hide both fringes (the compact look of earlier versions).
  NUMBER     Use NUMBER pixels for both fringes.
  (LEFT . RIGHT)  Use LEFT and RIGHT pixels respectively.

The Consult preview child frame is unaffected and keeps no fringe."
  :type '(choice (const :tag "Inherit from parent frame" inherit)
                 (const :tag "No fringe" nil)
                 (natnum :tag "Both fringes (pixels)")
                 (cons :tag "Left and right (pixels)"
                       (natnum :tag "Left")
                       (natnum :tag "Right"))))

(defcustom vertico-buffer-frame-margins 'inherit
  "Window margins of the candidate child frame.
Like `vertico-buffer-frame-fringes', this avoids configuring the left and right
margins individually:

  `inherit'  Use the parent window's margins (default).
  nil        Use no margins.
  NUMBER     Use NUMBER columns for both margins.
  (LEFT . RIGHT)  Use LEFT and RIGHT columns respectively."
  :type '(choice (const :tag "Inherit from parent window" inherit)
                 (const :tag "No margin" nil)
                 (natnum :tag "Both margins (columns)")
                 (cons :tag "Left and right (columns)"
                       (natnum :tag "Left")
                       (natnum :tag "Right"))))

(defcustom vertico-buffer-frame-candidate-accept-focus nil
  "Non-nil means candidate child frames may accept input focus.
This allows `vertico-mouse' clicks and wheel events in the candidate child
frame.  The default is nil because deleting a focused child frame can lose
input focus on some window-system backends."
  :type 'boolean)

(defcustom vertico-buffer-frame-consult-preview t
  "Non-nil means mirror Consult previews in a child frame.
Consult still computes previews in its original window.  This package mirrors
the resulting buffer, point, and window start in a non-focusable child frame
overlaid on the lower-right of the Vertico candidate frame."
  :type 'boolean)

(defcustom vertico-buffer-frame-auto-width nil
  "Non-nil means grow the candidate frame to fit the widest visible candidate.
The width grows up to the parent frame width and never shrinks below the
golden-ratio width.  Prompt and input wrapping is left to `vertico-buffer-mode',
which wraps that line while the cursor is past the frame width."
  :type 'boolean)

(defcustom vertico-buffer-frame-parameters nil
  "Additional child frame parameters.
These parameters are appended to the package defaults before calling
`display-buffer-in-child-frame'.  When a parameter name is already present in
the defaults, the value in this option replaces the default value."
  :type '(alist :key-type symbol :value-type sexp))

(defvar vertico-buffer-frame--saved-state nil
  "Plist storing Vertico state saved while frame mode is enabled.")
(defvar vertico-buffer-frame--minibuffers nil)
(defvar vertico-buffer-frame-mode)
(defvar-local vertico-buffer-frame-local-mode nil)
(defvar consult--preview-function)

(defvar-local vertico-buffer-frame--frame nil)
(defvar-local vertico-buffer-frame--window nil)
(defvar-local vertico-buffer-frame--parent nil)
(defvar-local vertico-buffer-frame--preview-frame nil)
(defvar-local vertico-buffer-frame--preview-window nil)
(defvar-local vertico-buffer-frame--preview-overlays nil)
(defvar-local vertico-buffer-frame--local-saved-state nil)
(defvar-local vertico-buffer-frame--session nil)
(defvar-local vertico-buffer-frame--auto-width-floor nil
  "Widest auto-width applied to the candidate frame this session.
With `vertico-buffer-frame-auto-width', the frame grows to fit candidates but
never shrinks below this floor until the frame is recreated.")

(defconst vertico-buffer-frame--owner-buffer-parameter
  'vertico-buffer-frame-owner-buffer
  "Frame parameter storing the minibuffer buffer that owns a child frame.")

(defconst vertico-buffer-frame--owner-session-parameter
  'vertico-buffer-frame-owner-session
  "Frame parameter storing the minibuffer session that owns a child frame.")

(defconst vertico-buffer-frame--golden-ratio (/ (+ 1.0 (sqrt 5.0)) 2.0)
  "Golden ratio used for automatic child-frame layout.")

(defconst vertico-buffer-frame--auto-width-max-ratio 0.9
  "Maximum auto-width candidate frame width as a fraction of the parent frame.
Auto-width never grows past this fraction, so the frame stays smaller than the
parent frame and keeps a centered margin instead of filling it edge to edge.")

(defconst vertico-buffer-frame--mirrored-overlay-properties
  '(before-string after-string display invisible face category priority)
  "Overlay properties copied into the Consult preview child frame.")

;;;###autoload
(defun vertico-buffer-frame-display-action ()
  "Return the display action used by `vertico-buffer-frame-mode'."
  '(vertico-buffer-frame--display-buffer))

(defun vertico-buffer-frame--enabled-p ()
  "Return non-nil when global or local frame display is enabled."
  (or vertico-buffer-frame-mode
      (bound-and-true-p vertico-buffer-frame-local-mode)))

(defun vertico-buffer-frame--parent-frame ()
  "Return the parent frame for a candidate child frame."
  (window-frame
   (or (ignore-errors
         (minibuffer-selected-window))
       (selected-window))))

(defun vertico-buffer-frame--number-or (value fallback)
  "Return VALUE as a float when it is numeric, otherwise FALLBACK."
  (float (if (numberp value) value fallback)))

(defun vertico-buffer-frame--positive-pixels (value)
  "Return VALUE rounded to a positive pixel count."
  (max 1 (round value)))

(defun vertico-buffer-frame--pixels-to-chars (pixels char-size)
  "Return PIXELS rounded to a positive character count using CHAR-SIZE."
  (max 1 (round (/ (float (max 1 pixels))
                   (max 1 char-size)))))

(defun vertico-buffer-frame--parent-pixel-size (parent)
  "Return PARENT frame size in pixels."
  (cons (max 1 (frame-pixel-width parent))
        (max 1 (frame-pixel-height parent))))

(defun vertico-buffer-frame--golden-pixel-size-from-size
    (parent-width parent-height scale-value)
  "Return golden-ratio size for PARENT-WIDTH and PARENT-HEIGHT.
SCALE-VALUE is interpreted like `vertico-buffer-frame-golden-ratio-scale'."
  (let* ((parent-width (max 1 parent-width))
         (parent-height (max 1 parent-height))
         (parent-ratio (/ (float parent-width) parent-height))
         (golden-width (if (> parent-ratio
                              vertico-buffer-frame--golden-ratio)
                           (* parent-height
                              vertico-buffer-frame--golden-ratio)
                         parent-width))
         (golden-height (if (> parent-ratio
                               vertico-buffer-frame--golden-ratio)
                            parent-height
                          (/ parent-width
                             vertico-buffer-frame--golden-ratio)))
         (scale (min 1.0
                     (max 0.0
                          (/ (max 0.0
                                  (vertico-buffer-frame--number-or
                                   scale-value 1.0))
                             vertico-buffer-frame--golden-ratio)))))
    (cons (vertico-buffer-frame--positive-pixels
           (* golden-width scale))
          (vertico-buffer-frame--positive-pixels
           (* golden-height scale)))))

(defun vertico-buffer-frame--golden-pixel-size (parent)
  "Return golden-ratio child frame size for PARENT in pixels."
  (let ((parent-size (vertico-buffer-frame--parent-pixel-size parent)))
    (vertico-buffer-frame--golden-pixel-size-from-size
     (car parent-size)
     (cdr parent-size)
     vertico-buffer-frame-golden-ratio-scale)))

(defun vertico-buffer-frame--candidate-frame-size (parent)
  "Return child frame size parameters for PARENT in characters."
  (let ((size (vertico-buffer-frame--golden-pixel-size parent)))
    (cons (vertico-buffer-frame--pixels-to-chars
           (car size)
           (frame-char-width parent))
          (vertico-buffer-frame--pixels-to-chars
           (cdr size)
           (frame-char-height parent)))))

(defun vertico-buffer-frame--visible-candidates ()
  "Return the currently visible Vertico candidates as a list of strings."
  (when (and (boundp 'vertico--candidates)
             (listp vertico--candidates)
             vertico--candidates)
    (let* ((scroll (if (boundp 'vertico--scroll) (or vertico--scroll 0) 0))
           (count (if (boundp 'vertico-count) (or vertico-count 10) 10))
           (total (length vertico--candidates))
           (start (max 0 (min scroll total)))
           (end (min total (+ start (max 1 count)))))
      (when (< start end)
        (cl-subseq vertico--candidates start end)))))

(defun vertico-buffer-frame--content-pixel-width ()
  "Return the widest pixel width of visible Vertico candidate lines, or nil.
Both the rendered candidate overlay and the raw visible candidates are measured
because `vertico-buffer-mode' clips the rendered lines to the current frame
width, hiding the natural width that drives auto-width growth."
  (let (widths)
    (when (and (boundp 'vertico--candidates-ov)
               (overlayp vertico--candidates-ov))
      (let ((rendered (overlay-get vertico--candidates-ov 'before-string)))
        (when (and (stringp rendered) (> (length rendered) 0))
          (push (string-pixel-width rendered) widths))))
    (let* ((visible (vertico-buffer-frame--visible-candidates))
           (text (and visible
                      (mapconcat (lambda (candidate)
                                   (if (stringp candidate) candidate ""))
                                 visible "\n"))))
      (when (and text (> (length text) 0))
        (push (string-pixel-width text) widths)))
    (and widths (apply #'max widths))))

(defun vertico-buffer-frame--candidate-target-width (parent golden-width)
  "Return the candidate frame width in characters for PARENT.
GOLDEN-WIDTH is the golden-ratio width in characters.  When
`vertico-buffer-frame-auto-width' is non-nil, grow the width to fit the widest
visible candidate, capped below the PARENT frame width so a centered margin
remains, and never below GOLDEN-WIDTH."
  (if (not vertico-buffer-frame-auto-width)
      golden-width
    (let ((pixels (vertico-buffer-frame--content-pixel-width)))
      (if (not pixels)
          golden-width
        (let* ((char-width (frame-char-width parent))
               ;; Margin covers the column reserve `vertico-buffer-mode' keeps
               ;; for truncation plus pixel-to-character rounding slack.
               (content-width
                (+ 6 (vertico-buffer-frame--pixels-to-chars pixels char-width)))
               (parent-width
                (vertico-buffer-frame--pixels-to-chars
                 (frame-pixel-width parent) char-width))
               ;; Stay below the parent width so the frame keeps a margin and
               ;; can be centered instead of filling the parent edge to edge.
               (max-width
                (max golden-width
                     (floor (* parent-width
                               vertico-buffer-frame--auto-width-max-ratio)))))
          (max golden-width (min content-width max-width)))))))

(defun vertico-buffer-frame--effective-target-width (parent golden-width)
  "Return the candidate frame width for PARENT, ratcheted when auto-width is on.
GOLDEN-WIDTH is the golden-ratio width in characters.  With
`vertico-buffer-frame-auto-width', the width never drops below the widest value
already applied this session, so the frame grows to fit candidates but does not
shrink back; the floor resets when the frame is recreated."
  (let ((target (vertico-buffer-frame--candidate-target-width
                 parent golden-width)))
    (if (not vertico-buffer-frame-auto-width)
        target
      (setq-local vertico-buffer-frame--auto-width-floor
                  (max (or vertico-buffer-frame--auto-width-floor 0)
                       target)))))

(defun vertico-buffer-frame--frame-pixel-size-from-chars (frame)
  "Return FRAME's pixel size derived from its character dimensions.
This tracks a fresh `set-frame-size' immediately, unlike `frame-pixel-width',
which can return a stale value until the next redisplay."
  (cons (* (frame-width frame) (frame-char-width frame))
        (* (frame-height frame) (frame-char-height frame))))

(defun vertico-buffer-frame--preview-frame-size (parent candidate-frame)
  "Return Consult preview child frame size for PARENT and CANDIDATE-FRAME.
The preview is CANDIDATE-FRAME scaled down by the inverse golden ratio in both
dimensions.  Scaling both dimensions keeps the candidate's aspect ratio and
makes the preview width track the candidate width, so the preview resizes with
auto-width instead of staying fixed at a height-bound golden rectangle."
  (let* ((candidate-pixels
          (vertico-buffer-frame--frame-pixel-size-from-chars candidate-frame))
         (scale (/ 1.0 vertico-buffer-frame--golden-ratio)))
    (cons (vertico-buffer-frame--pixels-to-chars
           (* (car candidate-pixels) scale)
           (frame-char-width parent))
          (vertico-buffer-frame--pixels-to-chars
           (* (cdr candidate-pixels) scale)
           (frame-char-height parent)))))

(defun vertico-buffer-frame--pair-spec (spec)
  "Return SPEC as a cons of LEFT and RIGHT explicit values, or nil to inherit.
SPEC follows `vertico-buffer-frame-fringes' or `vertico-buffer-frame-margins'.
A return value of nil means the caller should inherit from the parent."
  (cond
   ((eq spec 'inherit) nil)
   ((null spec) (cons 0 0))
   ((numberp spec) (cons spec spec))
   ((consp spec) (cons (or (car spec) 0) (or (cdr spec) 0)))
   (t (cons 0 0))))

(defun vertico-buffer-frame--candidate-fringes (parent)
  "Return (LEFT . RIGHT) fringe pixel widths for a candidate frame on PARENT.
With the default `inherit' setting, the widths follow PARENT's own fringes so
fringe-based candidate indicators render without extra configuration."
  (or (vertico-buffer-frame--pair-spec vertico-buffer-frame-fringes)
      (let ((fringes (window-fringes (frame-selected-window parent))))
        (cons (or (nth 0 fringes) 0)
              (or (nth 1 fringes) 0)))))

(defun vertico-buffer-frame--candidate-margins (parent)
  "Return (LEFT . RIGHT) margin columns for a candidate frame on PARENT.
With the default `inherit' setting, the margins follow PARENT's selected
window margins."
  (or (vertico-buffer-frame--pair-spec vertico-buffer-frame-margins)
      (let ((margins (window-margins (frame-selected-window parent))))
        (cons (or (car margins) 0)
              (or (cdr margins) 0)))))

(defun vertico-buffer-frame--apply-border (frame)
  "Draw a simple visible border around child FRAME.
Themes often leave the `child-frame-border' face unspecified, which blends the
border into the background and hides it.  Setting the per-frame border faces to
FRAME's `default' foreground draws a plain line that follows the active theme
without affecting any other frame."
  (when-let* (((frame-live-p frame))
              (color (face-foreground 'default frame t)))
    (dolist (face '(child-frame-border internal-border))
      (when (facep face)
        (set-face-background face color frame)))))

(defun vertico-buffer-frame--base-parameters (parent name &optional size role)
  "Return child frame parameters for PARENT with frame NAME.
SIZE is a cons of width and height in characters.  ROLE is `candidate' or
`preview'; nil means `candidate'."
  (let* ((size (or size (vertico-buffer-frame--candidate-frame-size parent)))
         (fringes (if (eq role 'preview)
                      (cons 0 0)
                    (vertico-buffer-frame--candidate-fringes parent)))
         (extra (and (proper-list-p vertico-buffer-frame-parameters)
                     vertico-buffer-frame-parameters))
         (defaults
          `((parent-frame . ,parent)
            (name . ,name)
            (title . "")
            (minibuffer . ,(minibuffer-window parent))
            (width . ,(car size))
            (height . ,(cdr size))
            (visibility . nil)
            (alpha . 100)
            (alpha-background . 100)
            (undecorated . t)
            (no-accept-focus
             . ,(or (eq role 'preview)
                    (not vertico-buffer-frame-candidate-accept-focus)))
            (no-focus-on-map . t)
            (skip-taskbar . t)
            (unsplittable . t)
            (border-width . 0)
            (child-frame-border-width . ,vertico-buffer-frame-border-width)
            (internal-border-width . 0)
            (left-fringe . ,(car fringes))
            (right-fringe . ,(cdr fringes))
            (right-divider-width . 0)
            (bottom-divider-width . 0)
            (vertical-scroll-bars . nil)
            (horizontal-scroll-bars . nil)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0)
            (tab-bar-lines . 0)
            (line-spacing . 0))))
    (append
     (if extra
         (cl-remove-if (lambda (parameter)
                         (assq (car-safe parameter) extra))
                       defaults)
       defaults)
     extra)))

(defun vertico-buffer-frame--prepare-window (window &optional fringes margins)
  "Remove chrome and spacing from child frame WINDOW.
FRINGES is a cons of left and right fringe widths in pixels; MARGINS is a cons
of left and right margin columns.  Either nil means no fringe or margin.  The
frame parameters already request the fringes, but the window fringes and margins
are set explicitly so a reused window does not keep stale values."
  (set-window-parameter window 'mode-line-format 'none)
  (set-window-parameter window 'header-line-format 'none)
  (set-window-parameter window 'tab-line-format 'none)
  (set-window-margins window (car margins) (cdr margins))
  (set-window-fringes window (car fringes) (cdr fringes) nil)
  (set-window-scroll-bars window nil nil nil nil))

(defun vertico-buffer-frame--auto-width-redisplay (_window)
  "Resize the candidate child frame to fit the visible candidates.
`vertico-buffer-mode' updates candidates through overlays without re-running
the display action, so this runs from `pre-redisplay-functions' to keep the
auto-width frame sized to its content.  The frame is only synced when the
target width changes, so a settled frame triggers no further resizing."
  (when (and vertico-buffer-frame-auto-width
             (vertico-buffer-frame--enabled-p)
             (frame-live-p vertico-buffer-frame--frame)
             (frame-live-p vertico-buffer-frame--parent))
    (let* ((golden (car (vertico-buffer-frame--candidate-frame-size
                         vertico-buffer-frame--parent)))
           (target (vertico-buffer-frame--effective-target-width
                    vertico-buffer-frame--parent golden)))
      (unless (= (frame-width vertico-buffer-frame--frame) target)
        (vertico-buffer-frame--sync-frame)
        ;; Resizing the candidate frame restacks it above the Consult preview
        ;; frame, so re-sync the visible preview to follow the new geometry and
        ;; return it to the front.
        (when (and (frame-live-p vertico-buffer-frame--preview-frame)
                   (eq (frame-visible-p vertico-buffer-frame--preview-frame) t))
          (vertico-buffer-frame--sync-preview-frame))))))

(defun vertico-buffer-frame--start-session ()
  "Start a new child-frame ownership session."
  (setq-local vertico-buffer-frame--session
              (make-symbol "vertico-buffer-frame-session")))

(defun vertico-buffer-frame--ensure-session ()
  "Return the current child-frame ownership session."
  (or vertico-buffer-frame--session
      (vertico-buffer-frame--start-session)))

(defun vertico-buffer-frame--set-frame-owner-buffer (frame buffer &optional share)
  "Mark FRAME as owned by minibuffer BUFFER.
SHARE is the `share-child-frame' value used for child-frame reuse."
  (when (frame-live-p frame)
    (let ((session (and (buffer-live-p buffer)
                        (buffer-local-value
                         'vertico-buffer-frame--session buffer))))
      (modify-frame-parameters
       frame
       `((,vertico-buffer-frame--owner-buffer-parameter . ,buffer)
         (,vertico-buffer-frame--owner-session-parameter . ,session)
         (minibuffer-exit . nil)
         (share-child-frame . ,(or share buffer)))))))

(defun vertico-buffer-frame--clear-vertico-overlay-window (window)
  "Clear Vertico overlay window state when it points at WINDOW."
  (dolist (symbol '(vertico--candidates-ov vertico--count-ov))
    (when (and (boundp symbol)
               (overlayp (symbol-value symbol))
               (eq (overlay-get (symbol-value symbol) 'window)
                   window))
      (overlay-put (symbol-value symbol) 'window nil))))

(defun vertico-buffer-frame--clear-vertico-overlays-for-frame (frame)
  "Clear Vertico overlay window state before deleting FRAME."
  (let ((owner (ignore-errors
                 (and (frame-live-p frame)
                      (frame-parameter
                       frame
                       vertico-buffer-frame--owner-buffer-parameter))))
        (window (ignore-errors
                  (and (frame-live-p frame)
                       (frame-root-window frame)))))
    (when (and (buffer-live-p owner)
               window)
      (with-current-buffer owner
        (vertico-buffer-frame--clear-vertico-overlay-window window)))))

(defun vertico-buffer-frame--delete-frame (frame)
  "Delete FRAME if it is live."
  (ignore-errors
    (when (frame-live-p frame)
      (vertico-buffer-frame--clear-vertico-overlays-for-frame frame)
      (modify-frame-parameters
       frame
       `((,vertico-buffer-frame--owner-buffer-parameter . nil)
         (,vertico-buffer-frame--owner-session-parameter . nil)))
      (delete-frame frame t))))

(defun vertico-buffer-frame--hide-frame (frame)
  "Hide FRAME if it is live."
  (ignore-errors
    (when (frame-live-p frame)
      (vertico-buffer-frame--clear-vertico-overlays-for-frame frame)
      (make-frame-invisible frame t))))

(defun vertico-buffer-frame--hide-frame-state ()
  "Hide child frames recorded in the current minibuffer state."
  (vertico-buffer-frame--hide-frame vertico-buffer-frame--preview-frame)
  (vertico-buffer-frame--hide-frame vertico-buffer-frame--frame))

(defun vertico-buffer-frame--delete-frames-owned-by-buffer
    (buffer &optional session)
  "Delete child frames owned by minibuffer BUFFER.
When SESSION is non-nil, only delete frames owned by that minibuffer session."
  (dolist (frame (frame-list))
    (when (and (eq (frame-parameter
                    frame vertico-buffer-frame--owner-buffer-parameter)
                   buffer)
               (let ((owner-session
                      (frame-parameter
                       frame
                       vertico-buffer-frame--owner-session-parameter)))
                 (or (null session)
                     (eq owner-session session)
                     ;; Delete frames created before session ownership existed.
                     (null owner-session))))
      (vertico-buffer-frame--delete-frame frame))))

(defun vertico-buffer-frame--delete-frames-owned-by-buffer-later
    (buffer session)
  "Delete child frames owned by minibuffer BUFFER and SESSION after teardown."
  ;; `minibuffer-exit-hook' can run before Emacs restores the pre-minibuffer
  ;; window configuration.  Deleting child frames there may run zero-delay
  ;; timers, including Embark's deferred collect/export display, too early.
  (run-at-time 0 nil
               #'vertico-buffer-frame--delete-frames-owned-by-buffer
               buffer session))

(defun vertico-buffer-frame--delete-owned-frames ()
  "Delete all child frames owned by `vertico-buffer-frame'."
  (dolist (frame (frame-list))
    (when (frame-parameter frame vertico-buffer-frame--owner-buffer-parameter)
      (vertico-buffer-frame--delete-frame frame))))

(defun vertico-buffer-frame--clear-frame-state ()
  "Clear buffer-local child-frame state."
  (setq-local vertico-buffer-frame--frame nil
              vertico-buffer-frame--window nil
              vertico-buffer-frame--parent nil
              vertico-buffer-frame--preview-frame nil
              vertico-buffer-frame--preview-window nil
              vertico-buffer-frame--auto-width-floor nil))

(defun vertico-buffer-frame--clear-preview-overlays ()
  "Delete Consult preview overlays owned by the current minibuffer."
  (mapc #'delete-overlay vertico-buffer-frame--preview-overlays)
  (setq-local vertico-buffer-frame--preview-overlays nil))

(defun vertico-buffer-frame--cleanup-minibuffer (buffer &optional delay-delete)
  "Clean up child frames owned by minibuffer BUFFER.
When DELAY-DELETE is non-nil, delete owned frames after minibuffer teardown."
  (let ((session (and (buffer-live-p buffer)
                      (buffer-local-value
                       'vertico-buffer-frame--session buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'minibuffer-exit-hook
                     #'vertico-buffer-frame--minibuffer-exit t)
        (vertico-buffer-frame--clear-preview-overlays)
        (when delay-delete
          (vertico-buffer-frame--hide-frame-state))
        (vertico-buffer-frame--clear-frame-state)
        (setq-local vertico-buffer-frame--session nil)))
    (if delay-delete
        (vertico-buffer-frame--delete-frames-owned-by-buffer-later
         buffer session)
      (vertico-buffer-frame--delete-frames-owned-by-buffer buffer session))
    (setq vertico-buffer-frame--minibuffers
          (delq buffer vertico-buffer-frame--minibuffers))))

;;;###autoload
(defun vertico-buffer-frame-cleanup ()
  "Release all child frames currently owned by active minibuffers."
  (interactive)
  (mapc #'vertico-buffer-frame--cleanup-minibuffer
        (copy-sequence vertico-buffer-frame--minibuffers)))

(defun vertico-buffer-frame--install-cleanup ()
  "Install cleanup hooks for the current minibuffer buffer."
  (vertico-buffer-frame--ensure-session)
  (cl-pushnew (current-buffer) vertico-buffer-frame--minibuffers)
  (add-hook 'minibuffer-exit-hook
            #'vertico-buffer-frame--minibuffer-exit 90 t))

(defun vertico-buffer-frame--minibuffer-exit ()
  "Clean up the child frame owned by the current minibuffer."
  (vertico-buffer-frame--cleanup-minibuffer (current-buffer) t))

(defun vertico-buffer-frame--sync-frame ()
  "Resize, center, and show the current child frame."
  (when (and (frame-live-p vertico-buffer-frame--frame)
             (frame-live-p vertico-buffer-frame--parent))
    (let* ((parent vertico-buffer-frame--parent)
           (frame vertico-buffer-frame--frame)
           (size (vertico-buffer-frame--candidate-frame-size parent))
           (width (vertico-buffer-frame--effective-target-width
                   parent (car size)))
           (height (cdr size))
           ;; Derive the pixel size from the characters being set so centering
           ;; does not read a stale `frame-pixel-width' right after a resize.
           (target-pixel-width (* width (frame-char-width parent)))
           (target-pixel-height (* height (frame-char-height parent)))
           (window-min-height 1)
           (window-min-width 1)
           (inhibit-redisplay t))
      (unless (and (= (frame-width frame) width)
                   (= (frame-height frame) height))
        (set-frame-size frame width height))
      (set-frame-position
       frame
       (max 0 (/ (- (frame-pixel-width parent) target-pixel-width) 2))
       (max 0 (/ (- (frame-pixel-height parent) target-pixel-height) 2)))
      (unless (eq (frame-visible-p frame) t)
        (make-frame-visible frame)))))

(defun vertico-buffer-frame--candidate-frame-current-p (parent)
  "Return non-nil when the current child frame can be reused under PARENT."
  (and (frame-live-p vertico-buffer-frame--frame)
       (window-live-p vertico-buffer-frame--window)
       (eq vertico-buffer-frame--parent parent)
       (eq (frame-parameter vertico-buffer-frame--frame 'parent-frame)
           parent)))

(defun vertico-buffer-frame--discard-frame ()
  "Delete the current child frame and clear local state."
  (vertico-buffer-frame--discard-preview-frame)
  (vertico-buffer-frame--delete-frame vertico-buffer-frame--frame)
  (vertico-buffer-frame--clear-frame-state))

(defun vertico-buffer-frame--display-buffer-in-child-frame
    (buffer parent name alist &optional size role)
  "Display BUFFER in a child frame under PARENT named NAME.
ALIST is appended to the display action passed to
`display-buffer-in-child-frame'.  SIZE and ROLE are passed to
`vertico-buffer-frame--base-parameters'."
  (let ((frame nil)
        (success nil))
    (vertico-buffer-frame--ensure-session)
    (unwind-protect
        (let* ((role (or role 'candidate))
               (owner (current-buffer))
               (share (if (eq role 'candidate)
                          owner
                        (list owner role)))
               (parameters
                (append
                 `((share-child-frame . ,share))
                 (vertico-buffer-frame--base-parameters parent name size role)
                 `((vertico-buffer-frame-role . ,role))))
               (action
                (append `((child-frame-parameters . ,parameters)
                          (inhibit-switch-frame . t))
                        alist))
               (window
                (display-buffer-in-child-frame buffer action)))
          (unless (window-live-p window)
            (error "Child frame display did not return a live window"))
          (setq frame (window-frame window))
          (unless (frame-live-p frame)
            (error "Child frame window did not have a live frame"))
          (vertico-buffer-frame--apply-border frame)
          (if (eq role 'candidate)
              (vertico-buffer-frame--prepare-window
               window
               (vertico-buffer-frame--candidate-fringes parent)
               (vertico-buffer-frame--candidate-margins parent))
            (vertico-buffer-frame--prepare-window window '(0 . 0) '(0 . 0)))
          (if (eq role 'candidate)
              (vertico-buffer-frame--set-frame-owner-buffer frame owner)
            (vertico-buffer-frame--set-frame-owner-buffer frame owner share))
          (setq success t)
          window)
      (unless success
        (vertico-buffer-frame--delete-frame frame)))))

(defun vertico-buffer-frame--ensure-window (buffer parent alist)
  "Return a live child-frame window for BUFFER under PARENT."
  (unless (vertico-buffer-frame--candidate-frame-current-p parent)
    (vertico-buffer-frame--discard-frame))
  (unless (window-live-p vertico-buffer-frame--window)
    (setq-local vertico-buffer-frame--window
                (vertico-buffer-frame--display-buffer-in-child-frame
                 buffer parent (format "Vertico %s" (minibuffer-depth)) alist)
                vertico-buffer-frame--frame
                (window-frame vertico-buffer-frame--window)
                vertico-buffer-frame--parent
                parent))
  vertico-buffer-frame--window)

(defun vertico-buffer-frame--display-buffer-in-candidate-frame
    (buffer alist parent)
  "Display BUFFER in the current minibuffer's candidate child frame.
ALIST is the display action alist passed by `display-buffer'.  PARENT is the
frame that should own the candidate child frame."
  (let ((window (vertico-buffer-frame--ensure-window buffer parent alist)))
    (vertico-buffer-frame--install-cleanup)
    (unless (eq (window-buffer window) buffer)
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer))
    (vertico-buffer-frame--sync-frame)
    window))

(defun vertico-buffer-frame--saved-action-entry (state)
  "Return a cons whose cdr is STATE's saved display action."
  (when state
    (let ((action (plist-get state :display-action)))
      (unless (equal action (vertico-buffer-frame-display-action))
        (cons t action)))))

(defun vertico-buffer-frame--fallback-action-entry ()
  "Return a cons whose cdr is the saved fallback display action."
  (or (vertico-buffer-frame--saved-action-entry
       vertico-buffer-frame--local-saved-state)
      (vertico-buffer-frame--saved-action-entry
       vertico-buffer-frame--saved-state)))

(defun vertico-buffer-frame--fallback-action ()
  "Return the display action to use when child frames are unavailable."
  (let ((entry (vertico-buffer-frame--fallback-action-entry)))
    (if entry
        (cdr entry)
      '(display-buffer-use-least-recent-window))))

(defun vertico-buffer-frame--display-buffer-fallback (buffer alist)
  "Display BUFFER with the saved Vertico display action plus ALIST."
  (let ((action (vertico-buffer-frame--fallback-action)))
    (display-buffer
     buffer
     (cond
      ((null action)
       (and alist
            (cons nil alist)))
      ((proper-list-p action)
       (append action alist))
      (t action)))))

(defun vertico-buffer-frame--release-display-state-for-fallback (owner)
  "Release child-frame state owned by OWNER before normal-window fallback."
  (when (buffer-live-p owner)
    (with-current-buffer owner
      (vertico-buffer-frame--discard-frame)
      (vertico-buffer-frame--delete-frames-owned-by-buffer owner))))

(defun vertico-buffer-frame--number-position (value)
  "Return VALUE when it is a number, otherwise zero."
  (if (numberp value) value 0))

(defun vertico-buffer-frame--preview-frame-current-p (parent)
  "Return non-nil when the current preview child frame can be reused."
  (and (frame-live-p vertico-buffer-frame--preview-frame)
       (window-live-p vertico-buffer-frame--preview-window)
       (eq (frame-parameter vertico-buffer-frame--preview-frame
                            'parent-frame)
           parent)))

(defun vertico-buffer-frame--discard-preview-frame ()
  "Delete the Consult preview frame and clear local preview state."
  (vertico-buffer-frame--clear-preview-overlays)
  (vertico-buffer-frame--delete-frame
   vertico-buffer-frame--preview-frame)
  (setq-local vertico-buffer-frame--preview-frame nil
              vertico-buffer-frame--preview-window nil))

(defun vertico-buffer-frame--preview-frame-position ()
  "Return lower-right overlay position for the current Consult preview frame.
Pixel sizes are derived from character dimensions so the position follows a
fresh resize of either frame instead of a stale `frame-pixel-width'."
  (let ((candidate-position (frame-position vertico-buffer-frame--frame))
        (candidate-pixels
         (vertico-buffer-frame--frame-pixel-size-from-chars
          vertico-buffer-frame--frame))
        (preview-pixels
         (vertico-buffer-frame--frame-pixel-size-from-chars
          vertico-buffer-frame--preview-frame)))
    (cons (+ (vertico-buffer-frame--number-position
              (car candidate-position))
             (max 0 (- (car candidate-pixels) (car preview-pixels))))
          (+ (vertico-buffer-frame--number-position
              (cdr candidate-position))
             (max 0 (- (cdr candidate-pixels) (cdr preview-pixels)))))))

(defun vertico-buffer-frame--sync-preview-frame ()
  "Resize, place, and show the current Consult preview frame."
  (when (and (frame-live-p vertico-buffer-frame--frame)
             (frame-live-p vertico-buffer-frame--preview-frame)
             (frame-live-p vertico-buffer-frame--parent))
    (let* ((size (vertico-buffer-frame--preview-frame-size
                  vertico-buffer-frame--parent
                  vertico-buffer-frame--frame))
           (width (car size))
           (height (cdr size))
           (window-min-height 1)
           (window-min-width 1)
           (inhibit-redisplay t))
      (unless (and (= (frame-width vertico-buffer-frame--preview-frame)
                      width)
                   (= (frame-height vertico-buffer-frame--preview-frame)
                      height))
        (set-frame-size vertico-buffer-frame--preview-frame width height))
      (let ((position (vertico-buffer-frame--preview-frame-position)))
        (set-frame-position
         vertico-buffer-frame--preview-frame
         (max 0 (car position))
         (max 0 (cdr position))))
      (unless (eq (frame-visible-p vertico-buffer-frame--preview-frame) t)
        (make-frame-visible vertico-buffer-frame--preview-frame))
      ;; Keep the preview stacked above the candidate frame, which is restacked
      ;; to the front whenever it is resized (for example by auto-width).
      (ignore-errors
        (raise-frame vertico-buffer-frame--preview-frame)))))

(defun vertico-buffer-frame--ensure-preview-window (buffer parent)
  "Return a live Consult preview child-frame window for BUFFER under PARENT."
  (unless (vertico-buffer-frame--preview-frame-current-p parent)
    (vertico-buffer-frame--discard-preview-frame))
  (unless (window-live-p vertico-buffer-frame--preview-window)
    (setq-local vertico-buffer-frame--preview-window
                (vertico-buffer-frame--display-buffer-in-child-frame
                 buffer
                 parent
                 (format "Vertico Consult Preview %s" (minibuffer-depth))
                 nil
                 (vertico-buffer-frame--preview-frame-size
                  parent vertico-buffer-frame--frame)
                 'preview)
                vertico-buffer-frame--preview-frame
                (window-frame vertico-buffer-frame--preview-window)))
  vertico-buffer-frame--preview-window)

(defun vertico-buffer-frame--set-preview-window-buffer (window buffer)
  "Display BUFFER in Consult preview WINDOW."
  (unless (eq (window-buffer window) buffer)
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer))
  (set-window-dedicated-p window t))

(defun vertico-buffer-frame--copy-window-view (source target)
  "Copy SOURCE window view into TARGET."
  (let ((point (window-point source))
        (start (window-start source)))
    (with-current-buffer (window-buffer source)
      (set-window-point target point)
      (set-window-start target start t))
    (ignore-errors
      (set-window-hscroll target (window-hscroll source)))))

(defun vertico-buffer-frame--preview-overlay-p (overlay source-window)
  "Return non-nil when OVERLAY should be mirrored from SOURCE-WINDOW."
  (and (eq (overlay-get overlay 'window) source-window)
       (or (overlay-get overlay 'before-string)
           (overlay-get overlay 'after-string)
           (overlay-get overlay 'display)
           (overlay-get overlay 'invisible))))

(defun vertico-buffer-frame--source-window-overlays (source-window)
  "Return visible window-local preview overlays from SOURCE-WINDOW."
  (let ((buffer (window-buffer source-window))
        (start (window-start source-window))
        (end (window-end source-window t))
        (point (window-point source-window))
        overlays)
    (with-current-buffer buffer
      (dolist (overlay (append (overlays-in start end)
                               (overlays-at point)))
        (when (and (vertico-buffer-frame--preview-overlay-p
                    overlay source-window)
                   (not (memq overlay overlays)))
          (push overlay overlays))))
    (nreverse overlays)))

(defun vertico-buffer-frame--copy-overlay-properties (source target)
  "Copy supported preview overlay properties from SOURCE to TARGET."
  (dolist (property vertico-buffer-frame--mirrored-overlay-properties)
    (when (overlay-get source property)
      (overlay-put target property (overlay-get source property)))))

(defun vertico-buffer-frame--mirror-preview-overlays
    (source-window target-window)
  "Mirror visible Consult preview overlays from SOURCE-WINDOW to TARGET-WINDOW."
  (let ((target-buffer (window-buffer target-window)))
    (dolist (overlay (vertico-buffer-frame--source-window-overlays
                      source-window))
      (let ((copy (make-overlay (overlay-start overlay)
                                (overlay-end overlay)
                                target-buffer
                                nil t)))
        (vertico-buffer-frame--copy-overlay-properties overlay copy)
        (overlay-put copy 'window target-window)
        (push copy vertico-buffer-frame--preview-overlays)))))

(defun vertico-buffer-frame--highlight-preview-line (window)
  "Highlight WINDOW's current line in the Consult preview child frame."
  (let (overlay)
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char (window-point window))
        (setq overlay
              (make-overlay (line-beginning-position)
                            (min (point-max)
                                 (1+ (line-end-position)))
                            (current-buffer)
                            nil t))
        (overlay-put overlay 'window window)
        (overlay-put overlay 'face
                     (if (facep 'consult-preview-line)
                         'consult-preview-line
                       'highlight))))
    (setq-local vertico-buffer-frame--preview-overlays
                (and overlay
                     (cons overlay vertico-buffer-frame--preview-overlays)))))

(defun vertico-buffer-frame-consult-preview-hide ()
  "Hide the active Consult preview child frame."
  (when-let* ((minibuffer-window (active-minibuffer-window))
              ((window-live-p minibuffer-window))
              (owner (window-buffer minibuffer-window))
              ((buffer-live-p owner)))
    (with-current-buffer owner
      (vertico-buffer-frame--clear-preview-overlays)
      (ignore-errors
        (when (frame-live-p vertico-buffer-frame--preview-frame)
          (make-frame-invisible vertico-buffer-frame--preview-frame t))))))

(defun vertico-buffer-frame-consult-preview-mirror-window (source-window)
  "Mirror Consult preview SOURCE-WINDOW in a lower-right child frame."
  (when-let* (((and (vertico-buffer-frame--enabled-p)
                    vertico-buffer-frame-consult-preview))
              ((window-live-p source-window))
              (minibuffer-window (active-minibuffer-window))
              ((window-live-p minibuffer-window))
              (owner (window-buffer minibuffer-window))
              ((buffer-live-p owner))
              (buffer (window-buffer source-window))
              ((buffer-live-p buffer)))
    (with-current-buffer owner
      (if (and (frame-live-p vertico-buffer-frame--frame)
               (frame-live-p vertico-buffer-frame--parent)
               (or (display-graphic-p vertico-buffer-frame--parent)
                   (featurep 'tty-child-frames)))
          (let ((window (vertico-buffer-frame--ensure-preview-window
                         buffer vertico-buffer-frame--parent)))
            (vertico-buffer-frame--clear-preview-overlays)
            (vertico-buffer-frame--set-preview-window-buffer window buffer)
            (vertico-buffer-frame--copy-window-view source-window window)
            (vertico-buffer-frame--mirror-preview-overlays
             source-window window)
            (vertico-buffer-frame--highlight-preview-line window)
            (vertico-buffer-frame--sync-preview-frame)
            window)
        (vertico-buffer-frame-consult-preview-hide)
        nil))))

(defun vertico-buffer-frame--display-buffer (buffer alist)
  "Display BUFFER in a child frame owned by the active minibuffer.
ALIST is the display action alist passed by `display-buffer'."
  (let* ((minibuffer-window (active-minibuffer-window))
         (owner (or (and (window-live-p minibuffer-window)
                         (window-buffer minibuffer-window))
                    (current-buffer))))
    (condition-case-unless-debug _error
        (with-current-buffer owner
          (let ((parent (vertico-buffer-frame--parent-frame)))
            (if (or (display-graphic-p parent)
                    (featurep 'tty-child-frames))
                (vertico-buffer-frame--display-buffer-in-candidate-frame
                 buffer alist parent)
              (vertico-buffer-frame--release-display-state-for-fallback
               owner)
              (vertico-buffer-frame--display-buffer-fallback buffer alist))))
      (error
       (vertico-buffer-frame--release-display-state-for-fallback owner)
       (vertico-buffer-frame--display-buffer-fallback buffer alist)))))

(defun vertico-buffer-frame--setup-minibuffer-session ()
  "Install child-frame cleanup hooks for the current minibuffer session."
  (setq-local mode-line-format nil
              header-line-format nil
              tab-line-format nil)
  ;; Depth 90 runs after `vertico-buffer--redisplay' (added at the default
  ;; depth), so it resizes the frame once the candidate overlay is up to date.
  (add-hook 'pre-redisplay-functions
            #'vertico-buffer-frame--auto-width-redisplay 90 t)
  (vertico-buffer-frame--start-session)
  (vertico-buffer-frame--install-cleanup))

(defun vertico-buffer-frame--setup-minibuffer ()
  "Install child-frame cleanup hooks for global frame mode."
  (when vertico-buffer-frame-mode
    (vertico-buffer-frame--setup-minibuffer-session)))

(defun vertico-buffer-frame--save-state ()
  "Save Vertico state while `vertico-buffer-frame-mode' is enabled."
  (unless vertico-buffer-frame--saved-state
    (setq vertico-buffer-frame--saved-state
          (list :display-action vertico-buffer-display-action
                :buffer-mode (bound-and-true-p vertico-buffer-mode)))))

(defun vertico-buffer-frame--using-display-action-p ()
  "Return non-nil when Vertico still uses this package's display action."
  (equal vertico-buffer-display-action
         (vertico-buffer-frame-display-action)))

(defun vertico-buffer-frame--restore-state ()
  "Restore Vertico state saved by `vertico-buffer-frame--save-state'."
  (when vertico-buffer-frame--saved-state
    (let* ((saved-state vertico-buffer-frame--saved-state)
           (saved-action (plist-get saved-state :display-action))
           (saved-buffer-mode (plist-get saved-state :buffer-mode))
           (using-frame-action (vertico-buffer-frame--using-display-action-p)))
      (when using-frame-action
        (setq vertico-buffer-display-action saved-action))
      (when (and using-frame-action (not saved-buffer-mode))
        (vertico-buffer-mode -1)))
    (setq vertico-buffer-frame--saved-state nil)))

;;;###autoload
(defun vertico-buffer-frame-toggle-preview (&optional arg)
  "Toggle Consult preview mirroring.
With prefix ARG, enable preview mirroring if ARG is positive, otherwise disable
it.  Inside an active minibuffer, the change is buffer-local to that session."
  (interactive "P")
  (let* ((local (minibufferp))
         (enabled (if arg
                      (> (prefix-numeric-value arg) 0)
                    (not (if local
                             vertico-buffer-frame-consult-preview
                           (default-value
                            'vertico-buffer-frame-consult-preview))))))
    (if local
        (setq-local vertico-buffer-frame-consult-preview enabled)
      (setq-default vertico-buffer-frame-consult-preview enabled))
    (if enabled
        (when (and local
                   (boundp 'consult--preview-function)
                   consult--preview-function)
          (funcall consult--preview-function))
      (vertico-buffer-frame-consult-preview-hide))))

(defun vertico-buffer-frame--set-local-variable (symbol value)
  "Set SYMBOL's buffer-local value to VALUE."
  (set (make-local-variable symbol) value))

(defun vertico-buffer-frame--restore-local-variable
    (symbol local-p value)
  "Restore SYMBOL to VALUE when LOCAL-P, otherwise remove its local binding."
  (if local-p
      (vertico-buffer-frame--set-local-variable symbol value)
    (kill-local-variable symbol)))

(defun vertico-buffer-frame--set-local-mode-state ()
  "Set buffer-local Vertico display state for local frame mode."
  (unless vertico-buffer-frame--local-saved-state
    (setq-local
     vertico-buffer-frame--local-saved-state
     (list :display-action vertico-buffer-display-action
           :display-action-local-p
           (local-variable-p 'vertico-buffer-display-action)
           :buffer-mode (bound-and-true-p vertico-buffer-mode)
           :buffer-mode-local-p
           (local-variable-p 'vertico-buffer-mode))))
  (setq-local vertico-buffer-mode t
              vertico-buffer-display-action
              (vertico-buffer-frame-display-action)))

(defun vertico-buffer-frame--restore-local-mode-state ()
  "Restore buffer-local Vertico display state saved by local frame mode."
  (when vertico-buffer-frame--local-saved-state
    (let ((state vertico-buffer-frame--local-saved-state))
      (vertico-buffer-frame--restore-local-variable
       'vertico-buffer-display-action
       (plist-get state :display-action-local-p)
       (plist-get state :display-action))
      (vertico-buffer-frame--restore-local-variable
       'vertico-buffer-mode
       (plist-get state :buffer-mode-local-p)
       (plist-get state :buffer-mode)))
    (setq-local vertico-buffer-frame--local-saved-state nil)))

(defun vertico-buffer-frame--restore-vertico-buffer-window ()
  "Restore Vertico's buffer window when local frame mode is disabled early."
  (when (and (boundp 'vertico-buffer--restore)
             vertico-buffer--restore)
    (ignore-errors
      (funcall vertico-buffer--restore))))

(defun vertico-buffer-frame--maybe-setup-vertico-buffer-window ()
  "Set up Vertico's buffer window when local mode is toggled interactively."
  (when (and (bound-and-true-p vertico--input)
             (boundp 'vertico-buffer--restore)
             (not vertico-buffer--restore))
    (ignore-errors
      (vertico-buffer--setup))))

;;;###autoload
(define-minor-mode vertico-buffer-frame-local-mode
  "Display Vertico candidates in child frames for this minibuffer.
This local mode is intended for `vertico-multiform-mode' per-command and
per-category rules.  It does not change global Vertico display state."
  :global nil
  :group 'vertico-buffer-frame
  (if vertico-buffer-frame-local-mode
      (if (minibufferp)
          (progn
            (vertico-buffer-frame--set-local-mode-state)
            (vertico-buffer-frame--setup-minibuffer-session)
            (vertico-buffer-frame--maybe-setup-vertico-buffer-window))
        (setq vertico-buffer-frame-local-mode nil)
        (user-error
         "`vertico-buffer-frame-local-mode' must be enabled in a minibuffer"))
    (vertico-buffer-frame--restore-vertico-buffer-window)
    (when (minibufferp)
      (vertico-buffer-frame--cleanup-minibuffer (current-buffer)))
    (vertico-buffer-frame--restore-local-mode-state)))

(defvar vertico-buffer-frame--warmed-up nil
  "Non-nil means a warm-up child frame was already created in this session.")

(defun vertico-buffer-frame--warmup-parent-frame ()
  "Return a frame suitable for warming up child-frame display, or nil."
  (let ((frame (selected-frame)))
    (and (frame-live-p frame)
         (display-graphic-p frame)
         (not (frame-parameter frame 'parent-frame))
         (eq (frame-visible-p frame) t)
         frame)))

;;;###autoload
(defun vertico-buffer-frame-warmup ()
  "Create and discard a child frame to warm up child-frame display.
The first child frame of an Emacs session realizes faces and fonts and
initializes display-backend state for child frames.  Paying that cost
inside the first minibuffer session has crashed Emacs on some platforms,
so `vertico-buffer-frame-mode' runs this once in a quiet context after
startup instead.  Return non-nil when a warm-up frame was created."
  (interactive)
  (when-let* ((parent (vertico-buffer-frame--warmup-parent-frame)))
    (let ((frame nil))
      (unwind-protect
          (progn
            (setq frame (make-frame
                         (vertico-buffer-frame--base-parameters
                          parent "Vertico warmup" '(2 . 2))))
            (vertico-buffer-frame--apply-border frame)
            (set-frame-position frame 0 0)
            (make-frame-visible frame)
            (redisplay t)
            (setq vertico-buffer-frame--warmed-up t))
        (vertico-buffer-frame--delete-frame frame)))))

(defun vertico-buffer-frame--warmup-startup ()
  "Run the child-frame warm-up once a graphical frame is available.
This runs from startup hooks, an idle timer, and
`server-after-make-frame-hook' because the warm-up needs a visible
graphical frame, which may not exist when the mode is enabled.  The
hooks deregister themselves once the warm-up has run or the mode has
been disabled."
  (if (and vertico-buffer-frame-mode
           (not vertico-buffer-frame--warmed-up))
      (when (vertico-buffer-frame--warmup-parent-frame)
        (ignore-errors (vertico-buffer-frame-warmup))
        (when vertico-buffer-frame--warmed-up
          (vertico-buffer-frame--remove-warmup)))
    (vertico-buffer-frame--remove-warmup)))

(defun vertico-buffer-frame--install-warmup ()
  "Schedule the child-frame warm-up for the current or next graphical frame."
  (unless vertico-buffer-frame--warmed-up
    (if after-init-time
        (run-with-idle-timer 0 nil #'vertico-buffer-frame--warmup-startup)
      (add-hook 'window-setup-hook #'vertico-buffer-frame--warmup-startup))
    (add-hook 'server-after-make-frame-hook
              #'vertico-buffer-frame--warmup-startup)))

(defun vertico-buffer-frame--remove-warmup ()
  "Remove the startup hooks installed for the child-frame warm-up."
  (remove-hook 'window-setup-hook #'vertico-buffer-frame--warmup-startup)
  (remove-hook 'server-after-make-frame-hook
               #'vertico-buffer-frame--warmup-startup))

;;;###autoload
(define-minor-mode vertico-buffer-frame-mode
  "Display `vertico-buffer-mode' candidates in child frames."
  :global t
  :group 'vertico-buffer-frame
  (if vertico-buffer-frame-mode
      (progn
        (vertico-buffer-frame--save-state)
        (setq vertico-buffer-display-action
              (vertico-buffer-frame-display-action))
        (add-hook 'minibuffer-setup-hook
                  #'vertico-buffer-frame--setup-minibuffer)
        (vertico-buffer-frame--install-warmup)
        (vertico-buffer-mode 1))
    (remove-hook 'minibuffer-setup-hook
                 #'vertico-buffer-frame--setup-minibuffer)
    (vertico-buffer-frame--remove-warmup)
    (vertico-buffer-frame-cleanup)
    (vertico-buffer-frame--delete-owned-frames)
    (vertico-buffer-frame--restore-state)))

(provide 'vertico-buffer-frame)
(require 'vertico-buffer-frame-consult nil t)
;;; vertico-buffer-frame.el ends here
