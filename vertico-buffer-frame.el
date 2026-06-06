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

(defcustom vertico-buffer-frame-parameters nil
  "Additional child frame parameters.
These parameters are appended to the package defaults before calling
`display-buffer-in-child-frame'."
  :type '(alist :key-type symbol :value-type sexp))

(defvar vertico-buffer-frame--saved-state nil
  "Plist storing Vertico state saved while frame mode is enabled.")
(defvar vertico-buffer-frame--minibuffers nil)
(defvar vertico-buffer-frame-mode)

(defvar-local vertico-buffer-frame--frame nil)
(defvar-local vertico-buffer-frame--window nil)
(defvar-local vertico-buffer-frame--parent nil)
(defvar-local vertico-buffer-frame--preview-frame nil)
(defvar-local vertico-buffer-frame--preview-window nil)
(defvar-local vertico-buffer-frame--preview-overlays nil)

(defconst vertico-buffer-frame--owner-buffer-parameter
  'vertico-buffer-frame-owner-buffer
  "Frame parameter storing the minibuffer buffer that owns a child frame.")

(defconst vertico-buffer-frame--golden-ratio (/ (+ 1.0 (sqrt 5.0)) 2.0)
  "Golden ratio used for automatic child-frame layout.")

;;;###autoload
(defun vertico-buffer-frame-display-action ()
  "Return the display action used by `vertico-buffer-frame-mode'."
  '(vertico-buffer-frame--display-buffer))

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

(defun vertico-buffer-frame--preview-frame-size (parent candidate-frame)
  "Return Consult preview child frame size for PARENT and CANDIDATE-FRAME."
  (let ((size (vertico-buffer-frame--golden-pixel-size-from-size
               (frame-pixel-width candidate-frame)
               (frame-pixel-height candidate-frame)
               1.0)))
    (cons (vertico-buffer-frame--pixels-to-chars
           (car size)
           (frame-char-width parent))
          (vertico-buffer-frame--pixels-to-chars
           (cdr size)
           (frame-char-height parent)))))

(defun vertico-buffer-frame--base-parameters (parent name &optional size role)
  "Return child frame parameters for PARENT with frame NAME.
SIZE is a cons of width and height in characters.  ROLE is `candidate' or
`preview'; nil means `candidate'."
  (let ((size (or size (vertico-buffer-frame--candidate-frame-size parent))))
    (append
     `((parent-frame . ,parent)
       (name . ,name)
       (title . "")
       (minibuffer . ,(minibuffer-window parent))
       (width . ,(car size))
       (height . ,(cdr size))
       (visibility . nil)
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
       (left-fringe . 0)
       (right-fringe . 0)
       (right-divider-width . 0)
       (bottom-divider-width . 0)
       (vertical-scroll-bars . nil)
       (horizontal-scroll-bars . nil)
       (menu-bar-lines . 0)
       (tool-bar-lines . 0)
       (tab-bar-lines . 0)
       (line-spacing . 0))
     (and (proper-list-p vertico-buffer-frame-parameters)
          vertico-buffer-frame-parameters))))

(defun vertico-buffer-frame--prepare-window (window)
  "Remove chrome and spacing from child frame WINDOW."
  (set-window-parameter window 'mode-line-format 'none)
  (set-window-parameter window 'header-line-format 'none)
  (set-window-parameter window 'tab-line-format 'none)
  (set-window-margins window 0 0)
  (set-window-fringes window 0 0 nil)
  (set-window-scroll-bars window nil nil nil nil))

(defun vertico-buffer-frame--set-frame-owner-buffer (frame buffer &optional share)
  "Mark FRAME as owned by minibuffer BUFFER.
SHARE is the `share-child-frame' value used for child-frame reuse."
  (when (frame-live-p frame)
    (modify-frame-parameters
     frame
     `((,vertico-buffer-frame--owner-buffer-parameter . ,buffer)
       (minibuffer-exit . nil)
       (share-child-frame . ,(or share buffer))))))

(defun vertico-buffer-frame--delete-frame (frame)
  "Delete FRAME if it is live."
  (ignore-errors
    (when (frame-live-p frame)
      (delete-frame frame t))))

(defun vertico-buffer-frame--delete-frames-owned-by-buffer (buffer)
  "Delete child frames owned by minibuffer BUFFER."
  (dolist (frame (frame-list))
    (when (eq (frame-parameter frame vertico-buffer-frame--owner-buffer-parameter)
              buffer)
      (vertico-buffer-frame--delete-frame frame))))

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
              vertico-buffer-frame--preview-window nil))

(defun vertico-buffer-frame--clear-preview-overlays ()
  "Delete Consult preview overlays owned by the current minibuffer."
  (mapc #'delete-overlay vertico-buffer-frame--preview-overlays)
  (setq-local vertico-buffer-frame--preview-overlays nil))

(defun vertico-buffer-frame--cleanup-minibuffer (buffer)
  "Clean up child frames owned by minibuffer BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (remove-hook 'minibuffer-exit-hook
                   #'vertico-buffer-frame--minibuffer-exit t)
      (vertico-buffer-frame--clear-preview-overlays)
      (vertico-buffer-frame--clear-frame-state)))
  (vertico-buffer-frame--delete-frames-owned-by-buffer buffer)
  (setq vertico-buffer-frame--minibuffers
        (delq buffer vertico-buffer-frame--minibuffers)))

;;;###autoload
(defun vertico-buffer-frame-cleanup ()
  "Release all child frames currently owned by active minibuffers."
  (interactive)
  (mapc #'vertico-buffer-frame--cleanup-minibuffer
        (copy-sequence vertico-buffer-frame--minibuffers)))

(defun vertico-buffer-frame--install-cleanup ()
  "Install cleanup hooks for the current minibuffer buffer."
  (cl-pushnew (current-buffer) vertico-buffer-frame--minibuffers)
  (add-hook 'minibuffer-exit-hook
            #'vertico-buffer-frame--minibuffer-exit 90 t))

(defun vertico-buffer-frame--minibuffer-exit ()
  "Clean up the child frame owned by the current minibuffer."
  (vertico-buffer-frame--cleanup-minibuffer (current-buffer)))

(defun vertico-buffer-frame--sync-frame ()
  "Resize, center, and show the current child frame."
  (when (and (frame-live-p vertico-buffer-frame--frame)
             (frame-live-p vertico-buffer-frame--parent))
    (let* ((size
            (vertico-buffer-frame--candidate-frame-size
             vertico-buffer-frame--parent))
           (width (car size))
           (height (cdr size))
           (window-min-height 1)
           (window-min-width 1)
           (inhibit-redisplay t))
      (unless (and (= (frame-width vertico-buffer-frame--frame) width)
                   (= (frame-height vertico-buffer-frame--frame) height))
        (set-frame-size vertico-buffer-frame--frame width height))
      (set-frame-position
       vertico-buffer-frame--frame
       (max 0 (/ (- (frame-pixel-width vertico-buffer-frame--parent)
                    (frame-pixel-width vertico-buffer-frame--frame))
                 2))
       (max 0 (/ (- (frame-pixel-height vertico-buffer-frame--parent)
                    (frame-pixel-height vertico-buffer-frame--frame))
                 2)))
      (unless (eq (frame-visible-p vertico-buffer-frame--frame) t)
        (make-frame-visible vertico-buffer-frame--frame)))))

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
          (vertico-buffer-frame--prepare-window window)
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

(defun vertico-buffer-frame--fallback-action ()
  "Return the display action to use when child frames are unavailable."
  (let ((saved-action
         (plist-get vertico-buffer-frame--saved-state :display-action)))
    (if (and vertico-buffer-frame--saved-state
             (not (equal saved-action
                         (vertico-buffer-frame-display-action))))
        saved-action
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
  "Return lower-right overlay position for the current Consult preview frame."
  (let ((candidate-position (frame-position vertico-buffer-frame--frame)))
    (cons (+ (vertico-buffer-frame--number-position
              (car candidate-position))
             (max 0 (- (frame-pixel-width vertico-buffer-frame--frame)
                       (frame-pixel-width
                        vertico-buffer-frame--preview-frame))))
          (+ (vertico-buffer-frame--number-position
              (cdr candidate-position))
             (max 0 (- (frame-pixel-height vertico-buffer-frame--frame)
                       (frame-pixel-height
                        vertico-buffer-frame--preview-frame)))))))

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
        (make-frame-visible vertico-buffer-frame--preview-frame)))))

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

(defun vertico-buffer-frame--highlight-preview-line (window)
  "Highlight WINDOW's current line in the Consult preview child frame."
  (let (overlay)
    (vertico-buffer-frame--clear-preview-overlays)
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
                (and overlay (list overlay)))))

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
  (when-let* (((and vertico-buffer-frame-mode
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
            (vertico-buffer-frame--set-preview-window-buffer window buffer)
            (vertico-buffer-frame--copy-window-view source-window window)
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

(defun vertico-buffer-frame--setup-minibuffer ()
  "Install child-frame cleanup hooks for the current minibuffer."
  (when vertico-buffer-frame-mode
    (setq-local mode-line-format nil
                header-line-format nil
                tab-line-format nil)
    (vertico-buffer-frame--install-cleanup)))

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
        (vertico-buffer-mode 1))
    (remove-hook 'minibuffer-setup-hook
                 #'vertico-buffer-frame--setup-minibuffer)
    (vertico-buffer-frame-cleanup)
    (vertico-buffer-frame--delete-owned-frames)
    (vertico-buffer-frame--restore-state)))

(provide 'vertico-buffer-frame)
(require 'vertico-buffer-frame-consult nil t)
;;; vertico-buffer-frame.el ends here
