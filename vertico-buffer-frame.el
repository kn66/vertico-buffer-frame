;;; vertico-buffer-frame.el --- Child-frame display for Vertico completions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: Nobuyuki Kamimoto
;; Assisted-by: OpenAI Codex:GPT-5
;; Version: 0.2.0
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
;; candidate buffer in a centered child frame sized from the selected frame.
;; It installs its display action by enabling `vertico-buffer-mode' and
;; replacing `vertico-buffer-display-action' while the mode is active.
;;
;; The package can also show a second, optional child frame for previews of the
;; current candidate.  Preview targets are resolved from completion
;; categories, Vertico metadata, text properties, and hook functions in
;; `vertico-buffer-frame-preview-target-functions'.  Built-in targets cover
;; common file, buffer, documentation, xref, imenu, bookmark, and Consult-style
;; candidates.
;;
;; The preview frame is placed over the lower-right side of the candidate frame
;; so the minibuffer prompt keeps its normal width for long input, such as path
;; editing and file renaming.  Candidate frames are hidden after minibuffer
;; exit and reused when the parent frame and display parameters still match.
;; Preview frames and temporary preview buffers are owned by the minibuffer
;; session that created them, which keeps recursive minibuffers isolated.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vertico)
(require 'vertico-buffer)

(defgroup vertico-buffer-frame nil
  "Display `vertico-buffer-mode' windows in child frames."
  :group 'vertico
  :prefix "vertico-buffer-frame-")

(require 'vertico-buffer-frame-preview)
(require 'vertico-buffer-frame-consult)

(defcustom vertico-buffer-frame-width 90
  "Width of the Vertico child frame in characters."
  :type 'natnum)

(defcustom vertico-buffer-frame-height 14
  "Height of the Vertico child frame in characters."
  :type 'natnum)

(defcustom vertico-buffer-frame-size-method 'golden-ratio
  "How to size the Vertico child frame.
When this is `golden-ratio', derive the size from the selected frame and round
it to character cells.
When this is `fixed', use `vertico-buffer-frame-width' and
`vertico-buffer-frame-height'."
  :type '(choice (const :tag "Golden ratio" golden-ratio)
                 (const :tag "Fixed character size" fixed)))

(defconst vertico-buffer-frame--default-border-width 1
  "Default width of the child frame border in pixels.")

(defcustom vertico-buffer-frame-border-width
  vertico-buffer-frame--default-border-width
  "Width of the child frame border in pixels."
  :type 'natnum)

(defcustom vertico-buffer-frame-golden-ratio-scale 1.0
  "Scale factor applied to the golden-ratio child frame size.
A value of 1.0 uses a frame one golden-ratio step smaller than the largest
golden rectangle fitting in the parent frame.  Values at or above the golden
ratio use the full fitting golden rectangle."
  :type 'number)

(defcustom vertico-buffer-frame-parameters nil
  "Additional child frame parameters.
These parameters are appended to the package defaults before calling
`display-buffer-in-child-frame'."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom vertico-buffer-frame-report-display-errors nil
  "Non-nil means report child-frame display errors before fallback.
When child-frame display fails, the package falls back to a normal display
buffer action.  Enabling this option reports the error with `message', which is
useful when diagnosing backend-specific child-frame failures."
  :type 'boolean)

(defconst vertico-buffer-frame--default-candidate-frame-pool-limit 8
  "Default number of hidden candidate child frames kept for reuse.")

(defcustom vertico-buffer-frame-candidate-frame-pool-limit
  vertico-buffer-frame--default-candidate-frame-pool-limit
  "Maximum number of hidden candidate child frames kept for reuse.
When nil, keep all reusable candidate frames.  Set this to 0 to disable
candidate frame reuse after minibuffer exit."
  :type '(choice (const :tag "Unlimited" nil) natnum))

(defconst vertico-buffer-frame--golden-ratio (/ (+ 1.0 (sqrt 5.0)) 2.0)
  "Golden ratio used for automatic child-frame layout.")

(defvar vertico-buffer-frame--saved-display-action nil)
(defvar vertico-buffer-frame--saved-buffer-mode nil)
(defvar vertico-buffer-frame--saved-embark-display-action-present nil)
(defvar vertico-buffer-frame--saved-state nil)
(defvar vertico-buffer-frame--minibuffers nil)
(defvar vertico-buffer-frame--candidate-frame-pool nil)
(defvar vertico-buffer-frame--last-display-error-message nil)
(defvar vertico-buffer-frame-mode)
(defvar vertico-buffer-frame-mode-map)
(defvar vertico-buffer-frame-preview)
(defvar vertico-buffer-frame--preview-frame)

(defvar-local vertico-buffer-frame--candidate-frame nil)
(defvar-local vertico-buffer-frame--candidate-window nil)
(defvar-local vertico-buffer-frame--candidate-layout-state nil)
(defvar-local vertico-buffer-frame--candidate-needs-update nil)
(defvar-local vertico-buffer-frame--cleanup-function nil)

(defconst vertico-buffer-frame--embark-display-action
  '("\\`\\*Embark \\(?:Collect:\\|Export\\)"
    vertico-buffer-frame--display-minibuffer-selected-window)
  "Display action for Embark buffers opened from the minibuffer.")

(defconst vertico-buffer-frame--owner-buffer-parameter
  'vertico-buffer-frame-owner-buffer
  "Frame parameter storing the minibuffer buffer that owns a child frame.")

;;;###autoload
(defun vertico-buffer-frame-display-action ()
  "Return the display action used by `vertico-buffer-frame-mode'."
  '(vertico-buffer-frame--display-buffer))

(defun vertico-buffer-frame--parent-frame ()
  "Return the parent frame for a new child frame."
  (let ((window (or (ignore-errors
                      (minibuffer-selected-window))
                    (ignore-errors
                      (selected-window)))))
    (or (vertico-buffer-frame--window-frame window)
        (selected-frame))))

(defun vertico-buffer-frame--frame-live-p (frame)
  "Return non-nil when FRAME is live, ignoring stale frame errors."
  (condition-case-unless-debug nil
      (frame-live-p frame)
    (error nil)))

(defun vertico-buffer-frame--window-live-p (window)
  "Return non-nil when WINDOW is live, ignoring stale window errors."
  (condition-case-unless-debug nil
      (window-live-p window)
    (error nil)))

(defun vertico-buffer-frame--window-frame (window)
  "Return WINDOW's frame when WINDOW is live, ignoring stale window errors."
  (ignore-errors
    (and (vertico-buffer-frame--window-live-p window)
         (window-frame window))))

(defun vertico-buffer-frame--border-width ()
  "Return the effective child frame border width."
  (if (natnump vertico-buffer-frame-border-width)
      vertico-buffer-frame-border-width
    vertico-buffer-frame--default-border-width))

(defun vertico-buffer-frame--parameters ()
  "Return well-formed additional child frame parameters."
  (when (proper-list-p vertico-buffer-frame-parameters)
    (cl-remove-if-not
     (lambda (parameter)
       (and (consp parameter)
            (symbolp (car parameter))))
     vertico-buffer-frame-parameters)))

(defun vertico-buffer-frame--candidate-frame-pool ()
  "Return the candidate frame pool when it is a proper list."
  (or (and (proper-list-p vertico-buffer-frame--candidate-frame-pool)
           vertico-buffer-frame--candidate-frame-pool)
      (progn
        (setq vertico-buffer-frame--candidate-frame-pool nil)
        nil)))

(defun vertico-buffer-frame--minibuffer-list ()
  "Return active minibuffer buffers when the list is well-formed."
  (or (and (proper-list-p vertico-buffer-frame--minibuffers)
           vertico-buffer-frame--minibuffers)
      (progn
        (setq vertico-buffer-frame--minibuffers nil)
        nil)))

(defun vertico-buffer-frame--display-buffer-alist ()
  "Return `display-buffer-alist' when it is a proper list."
  (and (proper-list-p display-buffer-alist)
       display-buffer-alist))

(defun vertico-buffer-frame--embark-display-action-present-p ()
  "Return non-nil when the package Embark display action is already present."
  (and (vertico-buffer-frame--display-buffer-alist)
       (member vertico-buffer-frame--embark-display-action
               display-buffer-alist)))

(defun vertico-buffer-frame--add-embark-display-action ()
  "Add the package Embark display action when `display-buffer-alist' is usable."
  (when (proper-list-p display-buffer-alist)
    (add-to-list 'display-buffer-alist
                 vertico-buffer-frame--embark-display-action
                 t)))

(defun vertico-buffer-frame--remove-embark-display-action ()
  "Remove the package Embark display action when `display-buffer-alist' is usable."
  (when (proper-list-p display-buffer-alist)
    (setq display-buffer-alist
          (delq vertico-buffer-frame--embark-display-action
                display-buffer-alist))))

(defun vertico-buffer-frame--base-parameters (parent name width height)
  "Return child frame parameters for PARENT, NAME, WIDTH and HEIGHT."
  (append
   `((parent-frame . ,parent)
     (name . ,name)
     (title . "")
     (minibuffer . nil)
     (width . ,width)
     (height . ,height)
     (visibility . nil)
     (undecorated . t)
     (no-accept-focus . t)
     (no-focus-on-map . t)
     (skip-taskbar . t)
     (unsplittable . t)
     (border-width . 0)
     (child-frame-border-width . ,(vertico-buffer-frame--border-width))
     (internal-border-width . 0)
     (left-fringe . 0)
     (right-fringe . 0)
     (right-divider-width . 0)
     (bottom-divider-width . 0)
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (tab-bar-lines . 0)
     (line-spacing . 0)
     (alpha . 100)
     (alpha-background . 100))
   (when-let* ((background
                (vertico-buffer-frame--default-background parent)))
     `((background-color . ,background)))
   (when-let* ((foreground
                (vertico-buffer-frame--default-foreground parent)))
     `((foreground-color . ,foreground)))
   (vertico-buffer-frame--parameters)))

(defun vertico-buffer-frame--specified-color (color)
  "Return COLOR unless it is an unspecified face color."
  (and (stringp color)
       (not (member color '("unspecified-bg" "unspecified-fg")))
       color))

(defun vertico-buffer-frame--default-background (frame)
  "Return the specified default background color for FRAME."
  (condition-case-unless-debug nil
      (vertico-buffer-frame--specified-color
       (face-background 'default frame 'default))
    (error nil)))

(defun vertico-buffer-frame--default-foreground (frame)
  "Return the specified default foreground color for FRAME."
  (condition-case-unless-debug nil
      (vertico-buffer-frame--specified-color
       (face-foreground 'default frame 'default))
    (error nil)))

(defun vertico-buffer-frame--apply-border-face (frame)
  "Use FRAME's default foreground color for its child frame border."
  (ignore-errors
    (when-let* ((color (vertico-buffer-frame--default-foreground frame)))
      (set-face-background 'child-frame-border color frame)))
  frame)

(defun vertico-buffer-frame--frame-parameter (frame parameter)
  "Return FRAME's PARAMETER value, ignoring stale frame errors."
  (ignore-errors
    (and (vertico-buffer-frame--frame-live-p frame)
         (frame-parameter frame parameter))))

(defun vertico-buffer-frame--frame-root-window (frame)
  "Return FRAME's root window, ignoring stale frame errors."
  (ignore-errors
    (and (vertico-buffer-frame--frame-live-p frame)
         (frame-root-window frame))))

(defun vertico-buffer-frame--set-frame-owner-buffer (frame buffer)
  "Record BUFFER as FRAME's minibuffer owner.
When BUFFER is nil, clear the owner.  Ignore stale frame objects because this
  function is used from cleanup paths."
  (ignore-errors
    (when (vertico-buffer-frame--frame-live-p frame)
      (set-frame-parameter
       frame vertico-buffer-frame--owner-buffer-parameter buffer))))

(defun vertico-buffer-frame--delete-frames-owned-by-buffer (buffer)
  "Delete child frames whose owner buffer is BUFFER."
  (dolist (frame (frame-list))
    (when (eq (vertico-buffer-frame--frame-parameter
               frame vertico-buffer-frame--owner-buffer-parameter)
              buffer)
      (vertico-buffer-frame--delete-frame frame))))

(defun vertico-buffer-frame--frame-reuse-key (parent name role)
  "Return a key describing reusable child frame state.
PARENT, NAME and ROLE identify the requested child frame."
  (list parent
        name
        role
        (vertico-buffer-frame--border-width)
        (vertico-buffer-frame--parameters)
        (ignore-errors
          (vertico-buffer-frame--default-background parent))
        (ignore-errors
          (vertico-buffer-frame--default-foreground parent))))

(defun vertico-buffer-frame--clamp-number (value minimum maximum)
  "Clamp VALUE between MINIMUM and MAXIMUM."
  (min (max value minimum) maximum))

(defun vertico-buffer-frame--number-or (value fallback)
  "Return VALUE as a float when it is numeric, otherwise FALLBACK."
  (float (if (numberp value) value fallback)))

(defun vertico-buffer-frame--positive-pixels (value)
  "Return VALUE rounded to a positive pixel count."
  (max 1 (round value)))

(defun vertico-buffer-frame--positive-chars (value)
  "Return VALUE as a positive character count."
  (if (and (integerp value)
           (> value 0))
      value
    1))

(defun vertico-buffer-frame--pixels-to-chars (pixels char-size)
  "Return PIXELS rounded to a positive character count using CHAR-SIZE."
  (max 1 (round (/ (float (max 1 pixels))
                   (max 1 char-size)))))

(defun vertico-buffer-frame--parent-pixel-size (parent)
  "Return PARENT frame size in pixels."
  (cons (max 1 (frame-pixel-width parent))
        (max 1 (frame-pixel-height parent))))

(defun vertico-buffer-frame--golden-pixel-size (parent)
  "Return golden-ratio child frame size for PARENT in pixels."
  (let* ((parent-size (vertico-buffer-frame--parent-pixel-size parent))
         (parent-width (car parent-size))
         (parent-height (cdr parent-size))
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
         (scale (vertico-buffer-frame--clamp-number
                 (/ (max 0.0
                         (vertico-buffer-frame--number-or
                          vertico-buffer-frame-golden-ratio-scale
                          1.0))
                    vertico-buffer-frame--golden-ratio)
                 0.0
                 1.0)))
    (cons (vertico-buffer-frame--positive-pixels
           (* golden-width scale))
          (vertico-buffer-frame--positive-pixels
           (* golden-height scale)))))

(defun vertico-buffer-frame--golden-frame-size (parent)
  "Return golden-ratio child frame size in characters for PARENT."
  (let ((size (vertico-buffer-frame--golden-pixel-size parent)))
    (cons (vertico-buffer-frame--pixels-to-chars
           (car size)
           (frame-char-width parent))
          (vertico-buffer-frame--pixels-to-chars
           (cdr size)
           (frame-char-height parent)))))

(defun vertico-buffer-frame--fixed-frame-size ()
  "Return fixed child frame size parameters in characters."
  (cons (vertico-buffer-frame--positive-chars
         vertico-buffer-frame-width)
        (vertico-buffer-frame--positive-chars
         vertico-buffer-frame-height)))

(defun vertico-buffer-frame--candidate-frame-size (parent)
  "Return child frame size parameters for PARENT."
  (pcase vertico-buffer-frame-size-method
    ('fixed
     (vertico-buffer-frame--fixed-frame-size))
    (_
     (vertico-buffer-frame--golden-frame-size parent))))

(defun vertico-buffer-frame--resize-frame-to-size (frame size)
  "Resize FRAME to character SIZE."
  (condition-case-unless-debug nil
      (when (vertico-buffer-frame--frame-live-p frame)
        (let ((width (car size))
              (height (cdr size)))
          (when (and (natnump width)
                     (natnump height))
            (set-frame-size frame
                            (vertico-buffer-frame--positive-chars width)
                            (vertico-buffer-frame--positive-chars height))
            t)))
    (error nil)))

(defun vertico-buffer-frame--prepare-window (window)
  "Remove chrome and spacing from child frame WINDOW."
  (set-window-parameter window 'mode-line-format 'none)
  (set-window-parameter window 'header-line-format 'none)
  (set-window-parameter window 'tab-line-format 'none)
  (set-window-margins window 0 0)
  (set-window-fringes window 0 0 nil)
  (set-window-scroll-bars window nil nil nil nil))

(defun vertico-buffer-frame--center-left (parent frame)
  "Return a centered left pixel position for FRAME in PARENT."
  (max 0 (/ (- (frame-pixel-width parent)
               (frame-pixel-width frame))
            2)))

(defun vertico-buffer-frame--center-top (parent frame)
  "Return a centered top pixel position for FRAME in PARENT."
  (max 0 (/ (- (frame-pixel-height parent)
               (frame-pixel-height frame))
            2)))

(defun vertico-buffer-frame--place-candidate-frame (frame parent)
  "Place candidate FRAME inside PARENT."
  (condition-case-unless-debug nil
      (progn
        (set-frame-position
         frame
         (vertico-buffer-frame--center-left parent frame)
         (vertico-buffer-frame--center-top parent frame))
        t)
    (error nil)))

(defun vertico-buffer-frame--place-preview-frame (frame parent)
  "Place preview FRAME at the bottom-right inside PARENT."
  (condition-case-unless-debug nil
      (progn
        (set-frame-position
         frame
         (max 0 (- (frame-pixel-width parent)
                   (frame-pixel-width frame)
                   8))
         (max 0 (- (frame-pixel-height parent)
                   (frame-pixel-height frame)
                   8)))
        t)
    (error nil)))

(defun vertico-buffer-frame--frame-layout-state (frame parent size)
  "Return layout state for FRAME in PARENT with character SIZE."
  (list parent
        (frame-pixel-width parent)
        (frame-pixel-height parent)
        (frame-pixel-width frame)
        (frame-pixel-height frame)
        size))

(defun vertico-buffer-frame--display-buffer-in-child-frame
    (buffer parent name size &optional role alist)
  "Display BUFFER in a child frame under PARENT.
NAME is the child frame name and SIZE is a cons of character width and height.
ROLE is stored as a frame parameter when non-nil.  ALIST is appended to the
display action used by `display-buffer-in-child-frame'."
  (let* ((parameters
          (append
           (vertico-buffer-frame--base-parameters
            parent name
            (vertico-buffer-frame--positive-chars (car size))
            (vertico-buffer-frame--positive-chars (cdr size)))
           `((vertico-buffer-frame-owner . t)
             (vertico-buffer-frame-role . ,role)
             (vertico-buffer-frame-reuse-key
              . ,(vertico-buffer-frame--frame-reuse-key parent name role))
             (share-child-frame . ,(list 'vertico-buffer-frame role name)))))
         (window
          (display-buffer-in-child-frame
           buffer
           (append
            `((child-frame-parameters . ,parameters)
              (inhibit-switch-frame . t))
            alist)))
         frame
         success)
    (unwind-protect
        (progn
          (unless (vertico-buffer-frame--window-live-p window)
            (error "Child frame display did not return a live window"))
          (setq frame (or (vertico-buffer-frame--window-frame window)
                          (error "Child frame window did not have a live frame")))
          (vertico-buffer-frame--prepare-window window)
          (vertico-buffer-frame--apply-border-face frame)
          (vertico-buffer-frame--set-frame-owner-buffer
           frame
           (current-buffer))
          (setq success t)
          window)
      (unless success
        (vertico-buffer-frame--delete-frame frame)))))

(defun vertico-buffer-frame--show-frame (frame)
  "Make FRAME visible if it is live and currently hidden."
  (condition-case-unless-debug nil
      (when (and (vertico-buffer-frame--frame-live-p frame)
                 (not (eq (frame-visible-p frame) t)))
        (make-frame-visible frame)
        t)
    (error nil)))

(defun vertico-buffer-frame--hide-frame (frame)
  "Make FRAME invisible if it is live and currently visible."
  (condition-case-unless-debug nil
      (when (and (vertico-buffer-frame--frame-live-p frame)
                 (frame-visible-p frame))
        (make-frame-invisible frame)
        t)
    (error nil)))

(defun vertico-buffer-frame--active-minibuffer-buffer ()
  "Return the active minibuffer buffer, if any."
  (when-let* ((window (ignore-errors
                        (active-minibuffer-window))))
    (and (vertico-buffer-frame--window-live-p window)
         (ignore-errors
           (window-buffer window)))))

(defun vertico-buffer-frame--candidate-window-live-p ()
  "Return non-nil when the current minibuffer owns a live candidate window."
  (and (vertico-buffer-frame--frame-live-p
        vertico-buffer-frame--candidate-frame)
       (vertico-buffer-frame--window-live-p
        vertico-buffer-frame--candidate-window)))

(defun vertico-buffer-frame--sync-candidate-frame-layout (parent size)
  "Resize and position the current candidate frame when layout changed.
PARENT is the expected parent frame and SIZE is the target character size."
  (condition-case-unless-debug nil
      (when (vertico-buffer-frame--candidate-window-live-p)
        (let ((state (vertico-buffer-frame--frame-layout-state
                      vertico-buffer-frame--candidate-frame parent size)))
          (unless (equal state vertico-buffer-frame--candidate-layout-state)
            (when (and (vertico-buffer-frame--resize-frame-to-size
                        vertico-buffer-frame--candidate-frame size)
                       (vertico-buffer-frame--place-candidate-frame
                        vertico-buffer-frame--candidate-frame parent))
              (setq-local vertico-buffer-frame--candidate-layout-state
                          (vertico-buffer-frame--frame-layout-state
                           vertico-buffer-frame--candidate-frame parent size))
              t))))
    (error nil)))

(defun vertico-buffer-frame--candidate-frame-reusable-p (frame reuse-key)
  "Return non-nil when FRAME can be reused for REUSE-KEY."
  (and (vertico-buffer-frame--frame-live-p frame)
       (eq (vertico-buffer-frame--frame-parameter
            frame 'vertico-buffer-frame-role)
           'candidate)
       (equal (vertico-buffer-frame--frame-parameter
               frame 'vertico-buffer-frame-reuse-key)
              reuse-key)))

(defun vertico-buffer-frame--frame-hidden-p (frame)
  "Return non-nil when FRAME is live but not visible."
  (condition-case-unless-debug nil
      (and (vertico-buffer-frame--frame-live-p frame)
           (not (frame-visible-p frame)))
    (error nil)))

(defun vertico-buffer-frame--pooled-candidate-frame-p (frame)
  "Return non-nil when FRAME is safe to keep in the candidate frame pool."
  (and (vertico-buffer-frame--frame-hidden-p frame)
       (eq (vertico-buffer-frame--frame-parameter
            frame 'vertico-buffer-frame-role)
           'candidate)
       (vertico-buffer-frame--window-live-p
        (vertico-buffer-frame--frame-root-window frame))
       (not (vertico-buffer-frame--frame-parameter
             frame vertico-buffer-frame--owner-buffer-parameter))
       (vertico-buffer-frame--frame-parameter
        frame 'vertico-buffer-frame-reuse-key)))

(defun vertico-buffer-frame--candidate-frame-pool-limit ()
  "Return the effective candidate frame pool limit."
  (cond
   ((null vertico-buffer-frame-candidate-frame-pool-limit)
    nil)
   ((natnump vertico-buffer-frame-candidate-frame-pool-limit)
    vertico-buffer-frame-candidate-frame-pool-limit)
   (t
    vertico-buffer-frame--default-candidate-frame-pool-limit)))

(defun vertico-buffer-frame--prune-candidate-frame-pool ()
  "Drop unusable frames from the candidate frame pool."
  (let (kept)
    (dolist (frame (vertico-buffer-frame--candidate-frame-pool))
      (if (vertico-buffer-frame--pooled-candidate-frame-p frame)
          (push frame kept)
        (vertico-buffer-frame--delete-frame frame)))
    (setq vertico-buffer-frame--candidate-frame-pool
          (nreverse kept))))

(defun vertico-buffer-frame--trim-candidate-frame-pool ()
  "Delete candidate frames beyond the configured pool limit."
  (when-let* ((limit (vertico-buffer-frame--candidate-frame-pool-limit)))
    (let (kept
          (excess (vertico-buffer-frame--candidate-frame-pool))
          (count 0))
      (while (and excess
                  (< count limit))
        (push (pop excess) kept)
        (cl-incf count))
      (mapc #'vertico-buffer-frame--delete-frame
            (copy-sequence excess))
      (setq vertico-buffer-frame--candidate-frame-pool
            (nreverse kept)))))

(defun vertico-buffer-frame--stash-candidate-frame (frame)
  "Hide FRAME and keep it for a later candidate display."
  (when (vertico-buffer-frame--frame-live-p frame)
    (ignore-errors
      (vertico-buffer-frame--hide-frame frame))
    (if (and (vertico-buffer-frame--frame-hidden-p frame)
             (eq (vertico-buffer-frame--frame-parameter
                  frame 'vertico-buffer-frame-role)
                 'candidate)
             (vertico-buffer-frame--window-live-p
              (vertico-buffer-frame--frame-root-window frame))
             (vertico-buffer-frame--frame-parameter
              frame 'vertico-buffer-frame-reuse-key))
        (progn
          (vertico-buffer-frame--set-frame-owner-buffer frame nil)
          (vertico-buffer-frame--candidate-frame-pool)
          (cl-pushnew frame vertico-buffer-frame--candidate-frame-pool)
          (vertico-buffer-frame--trim-candidate-frame-pool))
      (vertico-buffer-frame--delete-frame frame))))

(defun vertico-buffer-frame--reusable-candidate-window (reuse-key)
  "Return a hidden candidate window matching REUSE-KEY, if available."
  (vertico-buffer-frame--prune-candidate-frame-pool)
  (when-let* ((frame
               (cl-find-if
                (lambda (candidate-frame)
                  (and (vertico-buffer-frame--candidate-frame-reusable-p
                        candidate-frame reuse-key)
                       (vertico-buffer-frame--pooled-candidate-frame-p
                        candidate-frame)))
                vertico-buffer-frame--candidate-frame-pool)))
    (let ((window (vertico-buffer-frame--frame-root-window frame)))
      (if (vertico-buffer-frame--window-live-p window)
          (progn
            (setq vertico-buffer-frame--candidate-frame-pool
                  (delq frame vertico-buffer-frame--candidate-frame-pool))
            window)
        (vertico-buffer-frame--delete-frame frame)
        nil))))

(defun vertico-buffer-frame--with-selected-live-window (window function)
  "Call FUNCTION with WINDOW selected, returning nil on stale window errors."
  (condition-case-unless-debug nil
      (when (vertico-buffer-frame--window-live-p window)
        (with-selected-window window
          (funcall function)))
    (error nil)))

(defun vertico-buffer-frame--pop-to-buffer-in-window (buffer window)
  "Pop BUFFER to WINDOW when both objects are live."
  (when (and (buffer-live-p buffer)
             (vertico-buffer-frame--window-live-p window))
    (vertico-buffer-frame--with-selected-live-window
     window
     (lambda ()
       (pop-to-buffer
        buffer
        '(nil . ((inhibit-same-window . t))))))))

(defun vertico-buffer-frame--report-display-error (error)
  "Report child-frame display ERROR when diagnostics are enabled."
  (when vertico-buffer-frame-report-display-errors
    (let ((message (error-message-string error)))
      (unless (equal message vertico-buffer-frame--last-display-error-message)
        (setq vertico-buffer-frame--last-display-error-message message)
        (message "vertico-buffer-frame display error: %s" message)))))

(defun vertico-buffer-frame--reset-display-state-after-error (owner)
  "Release child-frame state owned by OWNER after display failure."
  (when (buffer-live-p owner)
    (with-current-buffer owner
      (vertico-buffer-frame--cancel-preview-timer)
      (vertico-buffer-frame--delete-frame
       vertico-buffer-frame--candidate-frame)
      (vertico-buffer-frame--delete-frame
       vertico-buffer-frame--preview-frame)
      (vertico-buffer-frame--delete-frames-owned-by-buffer owner)
      (vertico-buffer-frame--kill-preview-buffer)
      (setq-local vertico-buffer-frame--candidate-frame nil
                  vertico-buffer-frame--candidate-window nil
                  vertico-buffer-frame--candidate-layout-state nil
                  vertico-buffer-frame--candidate-needs-update nil
                  vertico-buffer-frame--preview-frame nil
                  vertico-buffer-frame--preview-window nil
                  vertico-buffer-frame--preview-layout-state nil
                  vertico-buffer-frame--preview-last-error-message nil
                  vertico-buffer-frame--preview-last-target-error-message nil
                  vertico-buffer-frame--preview-scheduled-state nil
                  vertico-buffer-frame--file-preview-cache nil
                  vertico-buffer-frame--imenu-cache nil
                  vertico-buffer-frame--consult-imenu-entry-table-cache nil))))

(defun vertico-buffer-frame--display-buffer (buffer alist)
  "Display BUFFER in a child frame owned by the active minibuffer.
ALIST is the display action alist passed by `display-buffer'.
This function is intended for `vertico-buffer-display-action'."
  (let ((owner (or (vertico-buffer-frame--active-minibuffer-buffer)
                   (current-buffer))))
    (condition-case-unless-debug error
        (with-current-buffer owner
          (let* ((parent (vertico-buffer-frame--parent-frame))
                 (size (vertico-buffer-frame--candidate-frame-size parent))
                 (name (format "Vertico %s" (minibuffer-depth)))
                 (reuse-key
                  (vertico-buffer-frame--frame-reuse-key
                   parent name 'candidate)))
            (unless (and (vertico-buffer-frame--candidate-window-live-p)
                         (vertico-buffer-frame--candidate-frame-reusable-p
                          vertico-buffer-frame--candidate-frame reuse-key))
              (vertico-buffer-frame--stash-candidate-frame
               vertico-buffer-frame--candidate-frame)
              (setq-local vertico-buffer-frame--candidate-frame nil
                          vertico-buffer-frame--candidate-window nil))
            (unless (vertico-buffer-frame--candidate-window-live-p)
              (setq-local
               vertico-buffer-frame--candidate-window
               (or (vertico-buffer-frame--reusable-candidate-window
                    reuse-key)
                   (vertico-buffer-frame--display-buffer-in-child-frame
                    buffer parent name size 'candidate alist))
               vertico-buffer-frame--candidate-frame
               (or (vertico-buffer-frame--window-frame
                    vertico-buffer-frame--candidate-window)
                   (error "Candidate window did not have a live frame"))
               vertico-buffer-frame--candidate-needs-update t))
            (vertico-buffer-frame--set-frame-owner-buffer
             vertico-buffer-frame--candidate-frame
             (current-buffer))
            (unless (eq (window-buffer
                         vertico-buffer-frame--candidate-window)
                        buffer)
              (set-window-dedicated-p
               vertico-buffer-frame--candidate-window nil)
              (set-window-buffer
               vertico-buffer-frame--candidate-window buffer)
              (setq-local vertico-buffer-frame--candidate-needs-update t))
            (vertico-buffer-frame--install-cleanup)
            (vertico-buffer-frame--sync-candidate-frame-layout parent size)
            (setq vertico-buffer-frame--last-display-error-message nil)
            vertico-buffer-frame--candidate-window))
      (error
       (vertico-buffer-frame--reset-display-state-after-error owner)
       (vertico-buffer-frame--report-display-error error)
       (display-buffer-use-least-recent-window buffer alist)))))

(defun vertico-buffer-frame--reveal-candidate-frame ()
  "Reveal and refresh the current minibuffer's candidate frame."
  (condition-case-unless-debug nil
      (when (vertico-buffer-frame--candidate-window-live-p)
        (let* ((parent (vertico-buffer-frame--parent-frame))
               (size (vertico-buffer-frame--candidate-frame-size parent))
               (layout-changed
                (vertico-buffer-frame--sync-candidate-frame-layout parent
                                                                   size))
               (shown
                (vertico-buffer-frame--show-frame
                 vertico-buffer-frame--candidate-frame)))
          (vertico-buffer-frame--refresh-preview-frame)
          (when (or layout-changed
                    shown
                    vertico-buffer-frame--candidate-needs-update)
            (force-window-update
             (window-buffer vertico-buffer-frame--candidate-window))
            (setq-local vertico-buffer-frame--candidate-needs-update nil))))
    (error nil)))

(defun vertico-buffer-frame--pre-redisplay (_window)
  "Reveal and refresh the current minibuffer's candidate frame before redisplay."
  (vertico-buffer-frame--reveal-candidate-frame))

(defun vertico-buffer-frame--install-cleanup ()
  "Install cleanup for the current minibuffer buffer."
  (unless vertico-buffer-frame--cleanup-function
    (let ((buffer (current-buffer))
          function)
      (setq function
            (lambda ()
              (when (or (not (buffer-live-p buffer))
                        (eq (current-buffer) buffer)
                        (eq (vertico-buffer-frame--active-minibuffer-buffer)
                            buffer))
                (vertico-buffer-frame--remove-hook
                 'minibuffer-exit-hook function)
                (vertico-buffer-frame--cleanup-minibuffer buffer))))
      (setq-local vertico-buffer-frame--cleanup-function function)
      (cl-pushnew buffer vertico-buffer-frame--minibuffers)
      (vertico-buffer-frame--add-hook
       'minibuffer-exit-hook function t))))

(defun vertico-buffer-frame--delete-frame (frame)
  "Delete FRAME if it is live."
  (setq vertico-buffer-frame--candidate-frame-pool
        (delq frame (vertico-buffer-frame--candidate-frame-pool)))
  (ignore-errors
    (when (vertico-buffer-frame--frame-live-p frame)
      (delete-frame frame t))))

(defun vertico-buffer-frame--delete-owned-frames ()
  "Delete child frames owned by `vertico-buffer-frame'."
  (dolist (frame (frame-list))
    (when (or (vertico-buffer-frame--frame-parameter
               frame 'vertico-buffer-frame-owner)
              (vertico-buffer-frame--frame-parameter
               frame vertico-buffer-frame--owner-buffer-parameter))
      (vertico-buffer-frame--delete-frame frame))))

(defun vertico-buffer-frame--delete-candidate-frame-pool ()
  "Delete candidate frames kept for reuse."
  (mapc #'vertico-buffer-frame--delete-frame
        (copy-sequence (vertico-buffer-frame--candidate-frame-pool)))
  (setq vertico-buffer-frame--candidate-frame-pool nil))

(defun vertico-buffer-frame--display-minibuffer-selected-window
    (buffer alist)
  "Display BUFFER in the window that started the active minibuffer.
ALIST is the display action alist passed by `display-buffer'.
The final display is deferred until minibuffer window restoration has finished,
which keeps Embark collect/export buffers in the user's current window instead
of a Vertico child frame."
  (when-let* (((minibufferp))
              (window (ignore-errors
                        (minibuffer-selected-window)))
              ((vertico-buffer-frame--window-live-p window)))
    (let (function
          displayed-window)
      (setq function
            (lambda ()
              (vertico-buffer-frame--remove-hook
               'minibuffer-exit-hook function)
              (when (and (buffer-live-p buffer)
                         (vertico-buffer-frame--window-live-p window))
                (condition-case-unless-debug nil
                    (run-at-time
                     0 nil
                     (lambda ()
                       (vertico-buffer-frame--pop-to-buffer-in-window
                        buffer window)))
                  (error nil)))))
      (setq displayed-window
            (vertico-buffer-frame--with-selected-live-window
             window
             (lambda ()
               (display-buffer-same-window buffer alist))))
      (when displayed-window
        (vertico-buffer-frame--add-hook
         'minibuffer-exit-hook function 90)
        displayed-window))))

(defun vertico-buffer-frame--cleanup-minibuffer (buffer)
  "Clean up child frames owned by minibuffer BUFFER."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((cleanup vertico-buffer-frame--cleanup-function))
          (vertico-buffer-frame--remove-hook
           'pre-redisplay-functions
           #'vertico-buffer-frame--pre-redisplay t)
          (vertico-buffer-frame--remove-hook
           'post-command-hook
           #'vertico-buffer-frame--preview-post-command t)
          (vertico-buffer-frame--cancel-preview-timer)
          (when cleanup
            (vertico-buffer-frame--remove-hook
             'minibuffer-exit-hook cleanup))
          (vertico-buffer-frame--stash-candidate-frame
           vertico-buffer-frame--candidate-frame)
          (vertico-buffer-frame--hide-frame
           vertico-buffer-frame--preview-frame)
          (vertico-buffer-frame--delete-frame
           vertico-buffer-frame--preview-frame)
          (vertico-buffer-frame--delete-frames-owned-by-buffer buffer)
          (vertico-buffer-frame--kill-preview-buffer)
          (setq-local vertico-buffer-frame--candidate-frame nil
                      vertico-buffer-frame--candidate-window nil
                      vertico-buffer-frame--candidate-layout-state nil
                      vertico-buffer-frame--candidate-needs-update nil
                      vertico-buffer-frame--preview-frame nil
                      vertico-buffer-frame--preview-window nil
                      vertico-buffer-frame--preview-layout-state nil
                      vertico-buffer-frame--preview-buffer nil
                      vertico-buffer-frame--preview-timer nil
                      vertico-buffer-frame--preview-last-error-message nil
                      vertico-buffer-frame--preview-last-target-error-message
                      nil
                      vertico-buffer-frame--preview-scheduled-state nil
                      vertico-buffer-frame--project-root-cache nil
                      vertico-buffer-frame--file-preview-cache nil
                      vertico-buffer-frame--imenu-cache nil
                      vertico-buffer-frame--consult-imenu-entry-table-cache nil
                      vertico-buffer-frame--cleanup-function nil)))
    (vertico-buffer-frame--delete-frames-owned-by-buffer buffer)
    (vertico-buffer-frame--kill-preview-buffers-owned-by-buffer buffer))
  (setq vertico-buffer-frame--minibuffers
        (delq buffer (vertico-buffer-frame--minibuffer-list))))

;;;###autoload
(defun vertico-buffer-frame-cleanup ()
  "Release all child frames currently owned by active minibuffers."
  (interactive)
  (mapc #'vertico-buffer-frame--cleanup-minibuffer
        (copy-sequence (vertico-buffer-frame--minibuffer-list)))
  (vertico-buffer-frame--delete-candidate-frame-pool))

;;;###autoload
(defun vertico-buffer-frame-toggle-preview (&optional arg)
  "Toggle candidate preview.
With prefix ARG, enable preview if ARG is positive, otherwise disable it.
Inside an active minibuffer, the change is buffer-local to that session."
  (interactive "P")
  (let ((enabled (if arg
                     (> (prefix-numeric-value arg) 0)
                   (not vertico-buffer-frame-preview))))
    (if (minibufferp)
        (progn
          (setq-local vertico-buffer-frame-preview enabled)
          (if enabled
              (vertico-buffer-frame--schedule-preview)
            (vertico-buffer-frame--hide-preview)))
      (setq vertico-buffer-frame-preview enabled))))

(defun vertico-buffer-frame--minibuffer-setup ()
  "Install per-minibuffer preview hooks."
  (when vertico-buffer-frame-mode
    (setq-local mode-line-format nil
                header-line-format nil
                tab-line-format nil)
    (vertico-buffer-frame--add-hook
     'pre-redisplay-functions
     #'vertico-buffer-frame--pre-redisplay nil t)
    (vertico-buffer-frame--add-hook
     'post-command-hook
     #'vertico-buffer-frame--preview-post-command t t)
    (when (minibufferp)
      (vertico-buffer-frame--install-cleanup))))

;;;###autoload
(define-minor-mode vertico-buffer-frame-mode
  "Display `vertico-buffer-mode' candidates in child frames."
  :global t
  :group 'vertico-buffer-frame
  (if vertico-buffer-frame-mode
      (progn
        (unless vertico-buffer-frame--saved-state
          (setq vertico-buffer-frame--saved-display-action
                vertico-buffer-display-action
                vertico-buffer-frame--saved-buffer-mode
                (bound-and-true-p vertico-buffer-mode)
                vertico-buffer-frame--saved-embark-display-action-present
                (not
                 (null
                  (vertico-buffer-frame--embark-display-action-present-p)))
                vertico-buffer-frame--saved-state t))
        (setq vertico-buffer-display-action
              (vertico-buffer-frame-display-action))
        (vertico-buffer-frame--add-hook
         'minibuffer-setup-hook
         #'vertico-buffer-frame--minibuffer-setup)
        (unless vertico-buffer-frame--saved-embark-display-action-present
          (vertico-buffer-frame--add-embark-display-action))
        (vertico-buffer-mode 1))
    (vertico-buffer-frame--remove-hook
     'minibuffer-setup-hook
     #'vertico-buffer-frame--minibuffer-setup)
    (unless vertico-buffer-frame--saved-embark-display-action-present
      (vertico-buffer-frame--remove-embark-display-action))
    (vertico-buffer-frame-cleanup)
    (vertico-buffer-frame--delete-owned-frames)
    (when vertico-buffer-frame--saved-state
      (let ((using-frame-action
             (equal vertico-buffer-display-action
                    (vertico-buffer-frame-display-action))))
        (when using-frame-action
          (setq vertico-buffer-display-action
                vertico-buffer-frame--saved-display-action))
        (unless (or vertico-buffer-frame--saved-buffer-mode
                    (not using-frame-action))
          (vertico-buffer-mode -1)))
      (setq vertico-buffer-frame--saved-display-action nil
            vertico-buffer-frame--saved-buffer-mode nil
            vertico-buffer-frame--saved-embark-display-action-present nil
            vertico-buffer-frame--saved-state nil))))
(provide 'vertico-buffer-frame)
;;; vertico-buffer-frame.el ends here
