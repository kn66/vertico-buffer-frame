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
;; editing and file renaming.  Frames and temporary preview buffers are owned
;; by the minibuffer session that created them, which keeps recursive
;; minibuffers isolated and allows `vertico-buffer-frame-cleanup' to delete
;; active frames.

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

(defcustom vertico-buffer-frame-border-width 1
  "Width of the child frame border in pixels."
  :type 'natnum)

(defcustom vertico-buffer-frame-golden-ratio-scale 1.0
  "Scale factor applied to the golden-ratio child frame size.
A value of 1.0 uses a frame one golden-ratio step smaller than the largest
golden rectangle fitting in the parent frame.  Values at or above the golden
ratio use the full fitting golden rectangle."
  :type 'number)

(defcustom vertico-buffer-frame-warm-up nil
  "Non-nil means warm up child-frame creation while Emacs is idle.
The warm-up creates and hides a tiny child frame after the mode is
enabled.  This moves the first GUI child-frame and font setup out of Vertico's
minibuffer setup path, which can be more fragile on some PGTK builds.

Some PGTK builds are also fragile when child frames are created from idle
timers, so this is disabled by default."
  :type 'boolean)

(defcustom vertico-buffer-frame-delete-hidden-frames nil
  "Non-nil means delete child frames when releasing them.
When nil, released child frames are hidden and reused.  Keeping this nil avoids
calling `delete-frame' during normal Vertico use, which is more stable on PGTK
builds where child-frame deletion can segfault."
  :type 'boolean)

(defconst vertico-buffer-frame--golden-ratio (/ (+ 1.0 (sqrt 5.0)) 2.0)
  "Golden ratio used for automatic child-frame layout.")

(defvar vertico-buffer-frame--saved-display-action nil)
(defvar vertico-buffer-frame--saved-buffer-mode nil)
(defvar vertico-buffer-frame--saved-state nil)
(defvar vertico-buffer-frame--minibuffers nil)
(defvar vertico-buffer-frame--warm-up-done nil)
(defvar vertico-buffer-frame--warm-up-timer nil)
(defvar vertico-buffer-frame--hidden-frames nil)
(defvar vertico-buffer-frame-mode)
(defvar vertico-buffer-frame-mode-map)
(defvar vertico-buffer-frame-preview)
(defvar vertico-buffer-frame--preview-frame)

(defvar-local vertico-buffer-frame--candidate-frame nil)
(defvar-local vertico-buffer-frame--candidate-window nil)
(defvar-local vertico-buffer-frame--candidate-layout-state nil)
(defvar-local vertico-buffer-frame--cleanup-function nil)

;;;###autoload
(defun vertico-buffer-frame-display-action ()
  "Return the display action used by `vertico-buffer-frame-mode'."
  '(vertico-buffer-frame--display-buffer))

(defun vertico-buffer-frame--parent-frame ()
  "Return the parent frame for a new child frame."
  (let ((window (or (minibuffer-selected-window)
                    (selected-window))))
    (if (window-live-p window)
        (window-frame window)
      (selected-frame))))

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
     (child-frame-border-width . ,vertico-buffer-frame-border-width)
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
     (alpha . 100)
     (alpha-background . 100))
   (when-let* ((background
                (vertico-buffer-frame--default-background parent)))
     `((background-color . ,background)))
   (when-let* ((foreground
                (vertico-buffer-frame--default-foreground parent)))
     `((foreground-color . ,foreground)))))

(defun vertico-buffer-frame--default-background (frame)
  "Return the default background color for FRAME."
  (face-background 'default frame 'default))

(defun vertico-buffer-frame--default-foreground (frame)
  "Return the default foreground color for FRAME."
  (face-foreground 'default frame 'default))

(defun vertico-buffer-frame--apply-border-face (frame)
  "Use FRAME's default foreground color for its child frame border."
  (when-let* ((color (vertico-buffer-frame--default-foreground frame)))
    (set-face-background 'child-frame-border color frame))
  frame)

(defun vertico-buffer-frame--clamp-number (value minimum maximum)
  "Clamp VALUE between MINIMUM and MAXIMUM."
  (min (max value minimum) maximum))

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
                         (float vertico-buffer-frame-golden-ratio-scale))
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
  (when (frame-live-p frame)
    (let ((width (car size))
          (height (cdr size)))
      (when (and (natnump width)
                 (natnump height))
        (set-frame-size frame
                        (vertico-buffer-frame--positive-chars width)
                        (vertico-buffer-frame--positive-chars height))))))

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
  (set-frame-position
   frame
   (vertico-buffer-frame--center-left parent frame)
   (vertico-buffer-frame--center-top parent frame)))

(defun vertico-buffer-frame--place-preview-frame (frame parent)
  "Place preview FRAME at the bottom-right inside PARENT."
  (set-frame-position
   frame
   (max 0 (- (frame-pixel-width parent)
             (frame-pixel-width frame)
             8))
   (max 0 (- (frame-pixel-height parent)
             (frame-pixel-height frame)
             8))))

(defun vertico-buffer-frame--frame-layout-state (frame parent size)
  "Return layout state for FRAME in PARENT with character SIZE."
  (list parent
        (frame-pixel-width parent)
        (frame-pixel-height parent)
        (frame-pixel-width frame)
        (frame-pixel-height frame)
        size))

(defun vertico-buffer-frame--make-child-frame (parent name width height
                                                     &optional role)
  "Create a fresh child frame under PARENT named NAME with WIDTH and HEIGHT.
When ROLE is non-nil, store it in the frame parameters."
  (let ((frame (make-frame
                (append
                 (vertico-buffer-frame--base-parameters
                  parent name 1 1)
                 (when role
                   `((vertico-buffer-frame-role . ,role)))))))
    (vertico-buffer-frame--resize-frame-to-size frame (cons width height))
    (vertico-buffer-frame--apply-border-face frame)))

(defun vertico-buffer-frame--show-frame (frame)
  "Make FRAME visible if it is live and currently hidden."
  (when (and (frame-live-p frame)
             (not (eq (frame-visible-p frame) t)))
    (make-frame-visible frame)))

(defun vertico-buffer-frame--hidden-buffer ()
  "Return the buffer displayed by hidden pooled child frames."
  (let ((buffer (get-buffer-create " *vertico-buffer-frame-hidden*")))
    (with-current-buffer buffer
      (setq-local cursor-type nil
                  mode-line-format nil
                  header-line-format nil
                  tab-line-format nil))
    buffer))

(defun vertico-buffer-frame--remove-hidden-frame (frame)
  "Remove FRAME from the hidden child-frame pool."
  (setq vertico-buffer-frame--hidden-frames
        (cl-remove-if (lambda (entry)
                        (eq (cdr entry) frame))
                      vertico-buffer-frame--hidden-frames)))

(defun vertico-buffer-frame--take-hidden-frame (role parent)
  "Take a hidden child frame for ROLE under PARENT from the pool."
  (let (frame kept)
    (dolist (entry vertico-buffer-frame--hidden-frames)
      (let ((candidate (cdr entry)))
        (cond
         ((not (frame-live-p candidate)))
         ((and (not frame)
               (eq (car entry) role)
               (eq (frame-parameter candidate 'parent-frame) parent))
          (setq frame candidate))
         (t
          (push entry kept)))))
    (setq vertico-buffer-frame--hidden-frames (nreverse kept))
    frame))

(defun vertico-buffer-frame--release-frame (role frame)
  "Hide FRAME and keep it for later reuse as ROLE.
If `vertico-buffer-frame-delete-hidden-frames' is non-nil, delete FRAME
instead."
  (when (frame-live-p frame)
    (vertico-buffer-frame--remove-hidden-frame frame)
    (if vertico-buffer-frame-delete-hidden-frames
        (vertico-buffer-frame--delete-frame frame)
      (let ((window (frame-root-window frame)))
        (when (window-live-p window)
          (set-window-dedicated-p window nil)
          (set-window-buffer window (vertico-buffer-frame--hidden-buffer))
          (vertico-buffer-frame--prepare-window window))
        (make-frame-invisible frame t)
        (push (cons role frame) vertico-buffer-frame--hidden-frames)))))

(defun vertico-buffer-frame--obtain-child-frame (role parent name width height)
  "Return a hidden or fresh child frame named NAME for ROLE under PARENT."
  (let ((frame (or (vertico-buffer-frame--take-hidden-frame role parent)
                   (vertico-buffer-frame--make-child-frame
                    parent name width height role))))
    (modify-frame-parameters frame
                             `((name . ,name)
                               (title . "")
                               (vertico-buffer-frame-role . ,role)))
    (vertico-buffer-frame--resize-frame-to-size frame (cons width height))
    (vertico-buffer-frame--apply-border-face frame)))

(defun vertico-buffer-frame--active-minibuffer-buffer ()
  "Return the active minibuffer buffer, if any."
  (when-let* ((window (active-minibuffer-window)))
    (window-buffer window)))

(defun vertico-buffer-frame--candidate-window-live-p ()
  "Return non-nil when the current minibuffer owns a live candidate window."
  (and (frame-live-p vertico-buffer-frame--candidate-frame)
       (window-live-p vertico-buffer-frame--candidate-window)))

(defun vertico-buffer-frame--display-buffer (buffer _alist)
  "Display BUFFER in a child frame owned by the active minibuffer.
This function is intended for `vertico-buffer-display-action'."
  (let* ((owner (or (vertico-buffer-frame--active-minibuffer-buffer)
                    (current-buffer))))
    (with-current-buffer owner
      (let* ((parent (vertico-buffer-frame--parent-frame))
             (size (vertico-buffer-frame--candidate-frame-size parent))
             (name (format "Vertico %s" (minibuffer-depth))))
        (unless (and (frame-live-p vertico-buffer-frame--candidate-frame)
                     (eq (frame-parameter
                          vertico-buffer-frame--candidate-frame
                         'parent-frame)
                         parent))
          (when vertico-buffer-frame--candidate-frame
            (vertico-buffer-frame--release-frame
             'candidate vertico-buffer-frame--candidate-frame))
          (setq-local vertico-buffer-frame--candidate-frame nil
                      vertico-buffer-frame--candidate-window nil))
        (setq-local vertico-buffer-frame--candidate-frame
                    (or vertico-buffer-frame--candidate-frame
                        (vertico-buffer-frame--obtain-child-frame
                         'candidate parent name (car size) (cdr size)))
                    vertico-buffer-frame--candidate-window
                    (frame-root-window
                     vertico-buffer-frame--candidate-frame))
        (vertico-buffer-frame--resize-frame-to-size
         vertico-buffer-frame--candidate-frame size)
        (modify-frame-parameters
         vertico-buffer-frame--candidate-frame
         `((name . ,name)
           (title . "")
           (vertico-buffer-frame-role . candidate)))
        (setq-local vertico-buffer-frame--candidate-window
                    (frame-root-window
                     vertico-buffer-frame--candidate-frame))
        (set-window-dedicated-p vertico-buffer-frame--candidate-window nil)
        (vertico-buffer-frame--prepare-window
         vertico-buffer-frame--candidate-window)
        (set-window-buffer vertico-buffer-frame--candidate-window buffer)
        (vertico-buffer-frame--install-cleanup)
        (vertico-buffer-frame--place-candidate-frame
         vertico-buffer-frame--candidate-frame parent)
        (setq-local vertico-buffer-frame--candidate-layout-state
                    (vertico-buffer-frame--frame-layout-state
                     vertico-buffer-frame--candidate-frame parent size))
        vertico-buffer-frame--candidate-window))))

(defun vertico-buffer-frame--reveal-candidate-frame ()
  "Reveal and refresh the current minibuffer's candidate frame."
  (when (vertico-buffer-frame--candidate-window-live-p)
    (let* ((parent (vertico-buffer-frame--parent-frame))
           (size (vertico-buffer-frame--candidate-frame-size parent))
           (state (vertico-buffer-frame--frame-layout-state
                   vertico-buffer-frame--candidate-frame parent size)))
      (unless (equal state vertico-buffer-frame--candidate-layout-state)
        (vertico-buffer-frame--resize-frame-to-size
         vertico-buffer-frame--candidate-frame size)
        (vertico-buffer-frame--place-candidate-frame
         vertico-buffer-frame--candidate-frame parent)
        (setq-local vertico-buffer-frame--candidate-layout-state
                    (vertico-buffer-frame--frame-layout-state
                     vertico-buffer-frame--candidate-frame parent size)))
      (vertico-buffer-frame--refresh-preview-frame)
      (vertico-buffer-frame--show-frame
       vertico-buffer-frame--candidate-frame)
      (force-window-update
       (window-buffer vertico-buffer-frame--candidate-window)))))

(defun vertico-buffer-frame--pre-redisplay (_window)
  "Reveal and refresh the current minibuffer's candidate frame before redisplay."
  (vertico-buffer-frame--reveal-candidate-frame))

(defun vertico-buffer-frame--install-cleanup ()
  "Install cleanup for the current minibuffer buffer."
  (unless vertico-buffer-frame--cleanup-function
    (let* ((buffer (current-buffer))
           (function (make-symbol "vertico-buffer-frame--cleanup")))
      (fset function
            (lambda ()
              (when (eq (current-buffer) buffer)
                (remove-hook 'minibuffer-exit-hook function)
                (vertico-buffer-frame--cleanup-minibuffer buffer))))
      (setq-local vertico-buffer-frame--cleanup-function function)
      (cl-pushnew buffer vertico-buffer-frame--minibuffers)
      (add-hook 'minibuffer-exit-hook function t))))

(defun vertico-buffer-frame--delete-frame (frame)
  "Delete FRAME if it is live."
  (when (frame-live-p frame)
    (delete-frame frame t)))

(defun vertico-buffer-frame--delete-hidden-frames ()
  "Delete all child frames kept in the hidden frame pool."
  (let ((frames (delete-dups
                 (mapcar #'cdr vertico-buffer-frame--hidden-frames))))
    (setq vertico-buffer-frame--hidden-frames nil)
    (dolist (frame frames)
      (vertico-buffer-frame--delete-frame frame))))

(defun vertico-buffer-frame--cancel-warm-up ()
  "Cancel any pending child-frame warm-up."
  (when (timerp vertico-buffer-frame--warm-up-timer)
    (cancel-timer vertico-buffer-frame--warm-up-timer))
  (setq vertico-buffer-frame--warm-up-timer nil))

(defun vertico-buffer-frame--warm-up ()
  "Create and hide a tiny child frame once."
  (setq vertico-buffer-frame--warm-up-timer nil)
  (when (and vertico-buffer-frame-mode
             vertico-buffer-frame-warm-up
             (not vertico-buffer-frame--warm-up-done)
             (display-graphic-p (selected-frame)))
    (let (frame)
      (setq vertico-buffer-frame--warm-up-done t)
      (condition-case-unless-debug error
          (setq frame
                (vertico-buffer-frame--obtain-child-frame
                 'candidate
                 (selected-frame)
                 "Vertico Warm Up"
                 1
                 1))
        (error
         (setq vertico-buffer-frame--warm-up-done nil)
         (message "vertico-buffer-frame warm-up error: %s"
                  (error-message-string error))))
      (vertico-buffer-frame--release-frame 'candidate frame))))

(defun vertico-buffer-frame--schedule-warm-up ()
  "Schedule child-frame warm-up if requested."
  (vertico-buffer-frame--cancel-warm-up)
  (when (and vertico-buffer-frame-mode
             vertico-buffer-frame-warm-up
             (not vertico-buffer-frame--warm-up-done))
    (setq vertico-buffer-frame--warm-up-timer
          (run-with-idle-timer
           0.2 nil #'vertico-buffer-frame--warm-up))))

(defun vertico-buffer-frame--after-make-frame (_frame)
  "Schedule child-frame warm-up after a new graphical frame appears."
  (when vertico-buffer-frame-mode
    (vertico-buffer-frame--schedule-warm-up)))

(defun vertico-buffer-frame--cleanup-minibuffer (buffer)
  "Clean up child frames owned by minibuffer BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((cleanup vertico-buffer-frame--cleanup-function))
        (remove-hook 'pre-redisplay-functions
                     #'vertico-buffer-frame--pre-redisplay t)
        (remove-hook 'post-command-hook
                     #'vertico-buffer-frame--preview-post-command t)
        (vertico-buffer-frame--cancel-preview-timer)
        (when cleanup
          (remove-hook 'minibuffer-exit-hook cleanup)
          (fmakunbound cleanup))
        (vertico-buffer-frame--release-frame
         'preview
         vertico-buffer-frame--preview-frame)
        (vertico-buffer-frame--release-frame
         'candidate
         vertico-buffer-frame--candidate-frame)
        (vertico-buffer-frame--kill-preview-buffer)
        (setq-local vertico-buffer-frame--candidate-frame nil
                    vertico-buffer-frame--candidate-window nil
                    vertico-buffer-frame--candidate-layout-state nil
                    vertico-buffer-frame--preview-frame nil
                    vertico-buffer-frame--preview-window nil
                    vertico-buffer-frame--preview-layout-state nil
                    vertico-buffer-frame--preview-buffer nil
                    vertico-buffer-frame--preview-timer nil
                    vertico-buffer-frame--preview-last-error-message nil
                    vertico-buffer-frame--preview-scheduled-state nil
                    vertico-buffer-frame--project-root-cache nil
                    vertico-buffer-frame--file-preview-cache nil
                    vertico-buffer-frame--imenu-cache nil
                    vertico-buffer-frame--cleanup-function nil))))
  (setq vertico-buffer-frame--minibuffers
        (delq buffer vertico-buffer-frame--minibuffers)))

;;;###autoload
(defun vertico-buffer-frame-cleanup ()
  "Release all child frames currently owned by active minibuffers."
  (interactive)
  (mapc #'vertico-buffer-frame--cleanup-minibuffer
        (copy-sequence vertico-buffer-frame--minibuffers)))

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
    (add-hook 'pre-redisplay-functions
              #'vertico-buffer-frame--pre-redisplay nil t)
    (add-hook 'post-command-hook
              #'vertico-buffer-frame--preview-post-command t t)))

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
                vertico-buffer-frame--saved-state t))
        (setq vertico-buffer-display-action
              (vertico-buffer-frame-display-action))
        (add-hook 'minibuffer-setup-hook
                  #'vertico-buffer-frame--minibuffer-setup)
        (add-hook 'after-make-frame-functions
                  #'vertico-buffer-frame--after-make-frame)
        (vertico-buffer-mode 1)
        (vertico-buffer-frame--schedule-warm-up))
    (remove-hook 'minibuffer-setup-hook
                 #'vertico-buffer-frame--minibuffer-setup)
    (remove-hook 'after-make-frame-functions
                 #'vertico-buffer-frame--after-make-frame)
    (vertico-buffer-frame--cancel-warm-up)
    (vertico-buffer-frame-cleanup)
    (vertico-buffer-frame--delete-hidden-frames)
    (when vertico-buffer-frame--saved-state
      (setq vertico-buffer-display-action
            vertico-buffer-frame--saved-display-action)
      (unless vertico-buffer-frame--saved-buffer-mode
        (vertico-buffer-mode -1))
      (setq vertico-buffer-frame--saved-display-action nil
            vertico-buffer-frame--saved-buffer-mode nil
            vertico-buffer-frame--saved-state nil))))
(provide 'vertico-buffer-frame)
;;; vertico-buffer-frame.el ends here
