;;; vertico-buffer-frame.el --- Show vertico-buffer in a child frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: Nobuyuki Kamimoto
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (vertico "2.8"))
;; Keywords: convenience, frames, minibuffer
;; URL: https://github.com/kn66/vertico-buffer-frame
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Experimental display action for `vertico-buffer-mode' which uses Emacs'
;; built-in `display-buffer-in-child-frame'.  This package intentionally avoids
;; posframe or other popup dependencies.
;;
;; Usage:
;;
;;   (require 'vertico-buffer-frame)
;;   (vertico-buffer-frame-mode 1)
;;
;; This enables `vertico-buffer-mode' and makes its display action create or
;; reuse a child frame.  Terminal child frames are used when Emacs provides the
;; `tty-child-frames' feature.
;; The preview layout defaults to an overlay, and can be changed to a
;; side-by-side layout with `vertico-buffer-frame-preview-layout'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'vertico-buffer)

(declare-function vertico-buffer-frame-preview-default
                  "vertico-buffer-frame-preview")
(declare-function vertico-buffer--redisplay "vertico-buffer")
(defvar vertico--candidates-ov)
(defvar vertico--count-ov)
(defvar vertico--input)

(defgroup vertico-buffer-frame nil
  "Show `vertico-buffer-mode' in a child frame."
  :group 'vertico
  :prefix "vertico-buffer-frame-")

(defcustom vertico-buffer-frame-background-color nil
  "Background color of the Vertico child frame.
When nil, use the background of the `default' face."
  :type '(choice (const :tag "Default face background" nil)
                 string)
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-foreground-color nil
  "Foreground color of the Vertico child frame.
When nil, use the foreground of the `default' face."
  :type '(choice (const :tag "Default face foreground" nil)
                 string)
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-alpha 100
  "Opacity of Vertico child frames.
When nil, do not set the `alpha' frame parameter.  The default value keeps
child frames opaque even when normal frames use transparency."
  :type '(choice (const :tag "Do not set" nil)
                 (integer :tag "Opacity percent"))
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-alpha-background 100
  "Background opacity of Vertico child frames.
When nil, do not set the `alpha-background' frame parameter.  The default value
keeps child-frame backgrounds opaque while leaving foreground text unchanged."
  :type '(choice (const :tag "Do not set" nil)
                 (integer :tag "Background opacity percent"))
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview t
  "When non-nil, show a preview child frame for the current candidate."
  :type 'boolean
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-layout 'overlay
  "Layout used when showing the preview child frame.
The value `overlay' shows the preview over the lower-right side of the
candidate frame.  The value `side-by-side' places the candidate and preview
frames next to each other without overlap while preview is enabled."
  :type '(choice (const :tag "Overlay preview on candidates" overlay)
                 (const :tag "Place preview beside candidates" side-by-side))
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-delay 0.2
  "Idle delay in seconds before updating the preview child frame.
This keeps rapid candidate navigation responsive by avoiding preview work for
intermediate candidates."
  :type 'number
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-max-size 20000
  "Maximum number of characters copied into the preview child frame."
  :type 'natnum
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-directory-max-entries 200
  "Maximum number of directory entries shown in file previews.
Set this to nil to list all entries."
  :type '(choice (const nil) natnum)
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-io-timeout 0.3
  "Seconds to wait for filesystem I/O while generating a preview.
Remote paths are skipped before this timeout is used.  Set this to nil to
disable the local I/O timeout."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-binary-detect-bytes 512
  "Number of leading bytes inspected to decide if a file is binary.
When the inspected bytes contain a NUL byte, the file is treated as binary and
skipped.  Set this to nil or 0 to disable the check."
  :type '(choice (const :tag "Disabled" nil) natnum)
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-redraw-after-show
  (not (memq system-type '(windows-nt cygwin)))
  "When non-nil, force a redraw after making child frames visible.
Forcing redraws can help on some graphical builds, but it can also make child
frame preview display noticeably stall on Windows.  The default therefore skips
the explicit `redraw-frame' call on Windows and relies on Emacs' normal
redisplay."
  :type 'boolean
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-golden-ratio-scale 1.00
  "Scale factor for golden-ratio child-frame sizing.
Overlay layout derives its candidate frame from a golden rectangle.
Side-by-side layout uses this factor when splitting the available width and
when deriving height from the parent frame."
  :type 'number
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-tty-cell-height-ratio 2.0
  "Estimated TTY character-cell height divided by character-cell width.
TTY frames report geometry in character cells, not real font pixels.  This
factor compensates for terminal cells usually being taller than wide, so
golden-ratio frame sizing remains visually similar to graphical frames.
Increase it if TTY child frames look too narrow."
  :type 'number
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-function
  #'vertico-buffer-frame-preview-default
  "Function used to render preview for the current Vertico candidate.
The function is called with the current candidate string in the minibuffer
buffer.  It should return a string, a buffer whose contents will be copied, or
nil to hide the preview child frame."
  :type '(choice (const nil) function)
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-category-functions
  '((file . vertico-buffer-frame-preview-file)
    (project-file . vertico-buffer-frame-preview-file)
    (buffer . vertico-buffer-frame-preview-buffer)
    (project-buffer . vertico-buffer-frame-preview-buffer)
    (bookmark . vertico-buffer-frame-preview-bookmark)
    (command . vertico-buffer-frame-preview-command)
    (function . vertico-buffer-frame-preview-function)
    (variable . vertico-buffer-frame-preview-variable)
    (symbol . vertico-buffer-frame-preview-symbol)
    (symbol-help . vertico-buffer-frame-preview-symbol)
    (face . vertico-buffer-frame-preview-face)
    (color . vertico-buffer-frame-preview-color)
    (environment-variable . vertico-buffer-frame-preview-environment-variable)
    (unicode-name . vertico-buffer-frame-preview-unicode-name)
    (info-menu . vertico-buffer-frame-preview-info-menu)
    (calendar-month . vertico-buffer-frame-preview-calendar-month)
    (minor-mode . vertico-buffer-frame-preview-command)
    (custom-group . vertico-buffer-frame-preview-custom-group)
    (theme . vertico-buffer-frame-preview-theme)
    (package . vertico-buffer-frame-preview-package)
    (dabbrev . vertico-buffer-frame-preview-string)
    (ecomplete . vertico-buffer-frame-preview-string)
    (email . vertico-buffer-frame-preview-string)
    (bibtex-key . vertico-buffer-frame-preview-string)
    (bibtex-string . vertico-buffer-frame-preview-string)
    (kill-ring . vertico-buffer-frame-preview-string)
    (expression . vertico-buffer-frame-preview-string)
    (consult-location . vertico-buffer-frame-preview-location)
    (consult-grep . vertico-buffer-frame-preview-grep)
    (consult-compile-error . vertico-buffer-frame-preview-consult-candidate)
    (consult-flymake-error . vertico-buffer-frame-preview-consult-candidate)
    (consult-info . vertico-buffer-frame-preview-info)
    (consult-man . vertico-buffer-frame-preview-man)
    (consult-kmacro . vertico-buffer-frame-preview-string)
    (imenu . vertico-buffer-frame-preview-imenu)
    (org-heading . vertico-buffer-frame-preview-org-heading)
    (xref-location . vertico-buffer-frame-preview-xref)
    (consult-xref . vertico-buffer-frame-preview-xref))
  "Alist of completion categories and preview functions.
Each preview function is called with the current candidate string."
  :type '(alist :key-type symbol :value-type function)
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-command-functions
  '((switch-to-buffer . vertico-buffer-frame-preview-buffer)
    (switch-to-buffer-other-window . vertico-buffer-frame-preview-buffer)
    (switch-to-buffer-other-frame . vertico-buffer-frame-preview-buffer)
    (project-switch-to-buffer . vertico-buffer-frame-preview-buffer)
    (customize-option . vertico-buffer-frame-preview-variable)
    (customize-variable . vertico-buffer-frame-preview-variable)
    (customize-option-other-window . vertico-buffer-frame-preview-variable)
    (customize-face . vertico-buffer-frame-preview-face)
    (customize-face-other-window . vertico-buffer-frame-preview-face)
    (customize-group . vertico-buffer-frame-preview-custom-group)
    (customize-group-other-window . vertico-buffer-frame-preview-custom-group)
    (customize-mode . vertico-buffer-frame-preview-custom-mode))
  "Alist of commands and preview functions.
Each preview function is called with the current candidate string."
  :type '(alist :key-type symbol :value-type function)
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-embark-select-source-window t
  "When non-nil, display Embark buffers from the source window.
Embark Collect displays its buffer while the minibuffer is still active, and
Embark Collect/Export can defer `pop-to-buffer' via
`embark--run-after-command'.  Selecting the original non-minibuffer window for
those displays restores the same display context as normal
`vertico-buffer-mode', while still leaving final placement to `display-buffer'."
  :type 'boolean
  :group 'vertico-buffer-frame)

(defvar vertico-buffer-frame--preview-buffer " *vertico-buffer-frame-preview*")
(defvar vertico-buffer-frame--preview-frame nil)
(defvar vertico-buffer-frame--saved-display-action nil
  "List containing the previous value of `vertico-buffer-display-action'.")
(defvar vertico-buffer-frame--saved-buffer-mode nil
  "List containing whether `vertico-buffer-mode' was enabled before setup.")
(defvar vertico-buffer-frame--theme-timer nil
  "Timer used to refresh child frame colors after theme changes.")
(defvar vertico-buffer-frame--minibuffer-buffers nil
  "Live minibuffer buffers using `vertico-buffer-frame-mode'.
The most recently set up minibuffer is first.  This lets recursive minibuffers
refresh the parent preview after they exit.")
(defvar vertico-buffer-frame--last-source-window nil
  "Most recent live non-minibuffer source window.")

(defconst vertico-buffer-frame--embark-buffer-regexp
  "\\`\\*Embark \\(?:Collect\\|Live\\|Export\\)"
  "Regexp matching Embark buffers displayed from the source window.")

(defconst vertico-buffer-frame--golden-ratio 1.61803398875)
(defconst vertico-buffer-frame--internal-border-width 8)
(defconst vertico-buffer-frame--border-width 1)
(defconst vertico-buffer-frame--preview-side-by-side-gap 2
  "Columns reserved between candidate and preview frames.")
(defconst vertico-buffer-frame--preview-frame-name
  "vertico-buffer-frame-preview")
(defconst vertico-buffer-frame--window-parameters
  '((header-line-format . none)
    (mode-line-format . none)
    (tab-line-format . none))
  "Window parameters used for child-frame windows.")

(defvar-local vertico-buffer-frame--candidate-frame nil
  "Candidate child frame used by the current minibuffer.")

(defvar-local vertico-buffer-frame--candidate-share-key nil
  "Session-specific `share-child-frame' value for candidate frames.")

(defvar-local vertico-buffer-frame--candidate-frame-chrome-hidden nil
  "Non-nil when chrome has been hidden for the current candidate frame.")

(defvar-local vertico-buffer-frame--source-window nil
  "Original non-minibuffer window for the current minibuffer session.")

(defvar-local vertico-buffer-frame--completion-command nil
  "Command that started the current minibuffer completion session.")

(defvar-local vertico-buffer-frame--preview-timer nil
  "Timer used to debounce preview updates for the current minibuffer.")

(defvar-local vertico-buffer-frame--preview-enabled nil
  "Non-nil when preview is enabled for the current minibuffer.")

(defvar-local vertico-buffer-frame--exiting nil
  "Non-nil while the current minibuffer is exiting.")

(defun vertico-buffer-frame--active-minibuffer-buffer ()
  "Return the active minibuffer buffer, or nil."
  (when-let* ((window (active-minibuffer-window))
              ((window-live-p window)))
    (window-buffer window)))

(defun vertico-buffer-frame--live-minibuffer-buffer-p (buffer)
  "Return non-nil when BUFFER is a live minibuffer buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (minibufferp))))

(defun vertico-buffer-frame--remember-minibuffer-buffer (buffer)
  "Remember BUFFER as the most recent minibuffer buffer."
  (setq vertico-buffer-frame--minibuffer-buffers
        (cons buffer
              (delq buffer vertico-buffer-frame--minibuffer-buffers))))

(defun vertico-buffer-frame--forget-minibuffer-buffer (buffer)
  "Forget BUFFER from the active minibuffer buffer list."
  (setq vertico-buffer-frame--minibuffer-buffers
        (delq buffer vertico-buffer-frame--minibuffer-buffers)))

(defun vertico-buffer-frame--top-minibuffer-buffer ()
  "Return the most recent live minibuffer buffer, or nil."
  (setq vertico-buffer-frame--minibuffer-buffers
        (seq-filter #'vertico-buffer-frame--live-minibuffer-buffer-p
                    vertico-buffer-frame--minibuffer-buffers))
  (car vertico-buffer-frame--minibuffer-buffers))

(defun vertico-buffer-frame--minibuffer-buffer ()
  "Return the current or active minibuffer buffer, or nil."
  (or (and (minibufferp) (current-buffer))
      (vertico-buffer-frame--active-minibuffer-buffer)
      (vertico-buffer-frame--top-minibuffer-buffer)))

(defun vertico-buffer-frame--live-source-window-p (window)
  "Return non-nil when WINDOW can be used as a source window."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (frame-parent (window-frame window)))
       (not (with-current-buffer (window-buffer window)
              (minibufferp)))))

(defun vertico-buffer-frame--minibuffer-source-window (&optional buffer)
  "Return the original non-minibuffer window for minibuffer BUFFER."
  (when (buffer-live-p buffer)
    (let ((window (buffer-local-value 'vertico-buffer-frame--source-window
                                      buffer)))
      (and (vertico-buffer-frame--live-source-window-p window)
           window))))

(defun vertico-buffer-frame--remember-source-window (window)
  "Remember WINDOW as the most recent live source window."
  (when (vertico-buffer-frame--live-source-window-p window)
    (setq vertico-buffer-frame--last-source-window window)))

(defun vertico-buffer-frame--current-source-window ()
  "Return the best original non-minibuffer window for the current context."
  (or (and (vertico-buffer-frame--live-source-window-p
            (minibuffer-selected-window))
           (minibuffer-selected-window))
      (seq-some #'vertico-buffer-frame--minibuffer-source-window
                vertico-buffer-frame--minibuffer-buffers)
      (and (vertico-buffer-frame--live-source-window-p
            vertico-buffer-frame--last-source-window)
           vertico-buffer-frame--last-source-window)
      (and (vertico-buffer-frame--live-source-window-p (selected-window))
           (selected-window))))

(defun vertico-buffer-frame--preview-enabled-p (&optional buffer)
  "Return non-nil when preview is enabled for BUFFER.
When BUFFER has no minibuffer-local preview state, fall back to
`vertico-buffer-frame-preview'."
  (if (and (buffer-live-p buffer)
           (local-variable-p 'vertico-buffer-frame--preview-enabled buffer))
      (buffer-local-value 'vertico-buffer-frame--preview-enabled buffer)
    vertico-buffer-frame-preview))

(defun vertico-buffer-frame--session-candidate-share-key ()
  "Return a new candidate child-frame sharing key."
  (list 'vertico-buffer-frame (current-buffer)))

(require 'vertico-buffer-frame-display)
(require 'vertico-buffer-frame-preview)

;;;###autoload
(defun vertico-buffer-frame-toggle-preview (&optional arg)
  "Toggle Vertico child-frame preview visibility.
When called in an active minibuffer, affect only the current completion
session.  Outside a minibuffer, update the global default
`vertico-buffer-frame-preview'.  With prefix ARG, enable preview if ARG is
positive, otherwise disable it."
  (interactive "P")
  (let* ((buffer (vertico-buffer-frame--minibuffer-buffer))
         (enabled
          (if (null arg)
              (not (vertico-buffer-frame--preview-enabled-p buffer))
            (> (prefix-numeric-value arg) 0))))
    (vertico-buffer-frame--set-preview-enabled buffer enabled)
    (if enabled
        (progn
          (vertico-buffer-frame--refresh-frames)
          (vertico-buffer-frame--refresh-active-preview))
      (vertico-buffer-frame--hide-preview buffer)
      (vertico-buffer-frame--refresh-frames))
    (message "vertico-buffer-frame preview %s (%s)"
             (if enabled "enabled" "disabled")
             (vertico-buffer-frame--preview-scope buffer))))

(defun vertico-buffer-frame--post-command ()
  "Update preview after Vertico displays candidates."
  (when (and (bound-and-true-p vertico-buffer-frame-mode)
             (minibufferp)
             (bound-and-true-p vertico--input))
    (when-let* ((candidate-frame (vertico-buffer-frame--candidate-frame)))
      (unless vertico-buffer-frame--candidate-frame-chrome-hidden
        (vertico-buffer-frame--hide-frame-chrome candidate-frame)
        (setq-local vertico-buffer-frame--candidate-frame-chrome-hidden t))
      (vertico-buffer-frame--schedule-preview))))

(defun vertico-buffer-frame--minibuffer-exit ()
  "Clean up child frames associated with the current minibuffer."
  (let ((buffer (current-buffer)))
    (remove-hook 'minibuffer-exit-hook
                 #'vertico-buffer-frame--minibuffer-exit
                 'local)
    (remove-hook 'post-command-hook
                 #'vertico-buffer-frame--post-command
                 'local)
    (unless vertico-buffer-frame--exiting
      (setq-local vertico-buffer-frame--exiting t
                  vertico-buffer-frame--candidate-frame nil)
      (vertico-buffer-frame--forget-minibuffer-buffer buffer)
      (vertico-buffer-frame--hide-preview buffer)
      (run-at-time 0 nil
                   #'vertico-buffer-frame--refresh-minibuffer-preview
                   (vertico-buffer-frame--top-minibuffer-buffer)))))

(defun vertico-buffer-frame--ensure-minibuffer-session (&optional buffer)
  "Ensure BUFFER has `vertico-buffer-frame' minibuffer-local state.
BUFFER defaults to the current or active minibuffer buffer."
  (when-let* ((buffer (or buffer
                          (and (minibufferp) (current-buffer))
                          (vertico-buffer-frame--active-minibuffer-buffer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (minibufferp)
        (vertico-buffer-frame--remember-minibuffer-buffer buffer)
        (unless vertico-buffer-frame--candidate-share-key
          (vertico-buffer-frame--remember-source-window
           (vertico-buffer-frame--current-source-window))
          (setq-local vertico-buffer-frame--exiting nil
                      vertico-buffer-frame--candidate-share-key
                      (vertico-buffer-frame--session-candidate-share-key)
                      vertico-buffer-frame--candidate-frame-chrome-hidden nil
                      vertico-buffer-frame--source-window
                      vertico-buffer-frame--last-source-window
                      vertico-buffer-frame--completion-command this-command
                      vertico-buffer-frame--preview-enabled
                      vertico-buffer-frame-preview))
        (add-hook 'minibuffer-exit-hook
                  #'vertico-buffer-frame--minibuffer-exit
                  nil 'local)
        (add-hook 'post-command-hook
                  #'vertico-buffer-frame--post-command
                  90
                  'local)
        buffer))))

(defun vertico-buffer-frame--setup-minibuffer ()
  "Setup `vertico-buffer-frame' preview for the current minibuffer."
  (when (and (bound-and-true-p vertico-buffer-frame-mode)
             (minibufferp))
    (vertico-buffer-frame--ensure-minibuffer-session (current-buffer))))

(defun vertico-buffer-frame--stale-overlay-window-p (overlay)
  "Return non-nil when OVERLAY targets a deleted window."
  (when (overlayp overlay)
    (let ((window (overlay-get overlay 'window)))
      (and (windowp window)
           (not (window-live-p window))))))

(defun vertico-buffer-frame--clear-stale-overlay-window (overlay)
  "Clear OVERLAY's deleted window target.
Return non-nil when a stale target was cleared."
  (when (vertico-buffer-frame--stale-overlay-window-p overlay)
    (overlay-put overlay 'window nil)
    t))

(defun vertico-buffer-frame--clear-stale-vertico-overlay-windows ()
  "Clear deleted Vertico overlay windows from the current minibuffer."
  (let ((cleared nil))
    (when (boundp 'vertico--candidates-ov)
      (setq cleared
            (or (vertico-buffer-frame--clear-stale-overlay-window
                 vertico--candidates-ov)
                cleared)))
    (when (boundp 'vertico--count-ov)
      (setq cleared
            (or (vertico-buffer-frame--clear-stale-overlay-window
                 vertico--count-ov)
                cleared)))
    cleared))

(defun vertico-buffer-frame--vertico-buffer-redisplay-advice (orig window)
  "Run Vertico buffer redisplay ORIG for WINDOW unless its target is stale.
`vertico-buffer--redisplay' assumes the Vertico overlay window is live.  Child
frames can be deleted by minibuffer cleanup before Vertico has restored its
overlay state, for example when `consult-dir' is invoked inside `find-file'."
  (unless (and (bound-and-true-p vertico-buffer-frame-mode)
               (vertico-buffer-frame--clear-stale-vertico-overlay-windows))
    (funcall orig window)))

(defun vertico-buffer-frame--advice-add (symbol where function)
  "Add FUNCTION as advice to SYMBOL at WHERE unless it is already present."
  (when (and (fboundp symbol)
             (not (advice-member-p function symbol)))
    (advice-add symbol where function)))

(defun vertico-buffer-frame--embark-source-window-enabled-p ()
  "Return non-nil when Embark displays should use the source window."
  (and (bound-and-true-p vertico-buffer-frame-mode)
       vertico-buffer-frame-embark-select-source-window))

(defun vertico-buffer-frame--embark-source-window ()
  "Return the source window for Embark minibuffer displays."
  (and (vertico-buffer-frame--embark-source-window-enabled-p)
       (or (minibufferp)
           (active-minibuffer-window)
           (seq-some #'vertico-buffer-frame--live-minibuffer-buffer-p
                     vertico-buffer-frame--minibuffer-buffers)
           (frame-parent (selected-frame)))
       (vertico-buffer-frame--current-source-window)))

(defun vertico-buffer-frame--source-frame-display-base-action (window)
  "Return `display-buffer-base-action' restricted to WINDOW's frame."
  (let ((functions (car-safe display-buffer-base-action))
        (alist (assq-delete-all
                'post-command-select-window
                (assq-delete-all
                 'reusable-frames
                 (copy-sequence (cdr-safe display-buffer-base-action))))))
    (cons functions
          `((reusable-frames . ,(window-frame window))
            (post-command-select-window . t)
            ,@alist))))

(defun vertico-buffer-frame--source-frame-overriding-action (window)
  "Return `display-buffer-overriding-action' restricted to WINDOW's frame."
  (let ((functions (car-safe display-buffer-overriding-action))
        (alist (assq-delete-all
                'post-command-select-window
                (assq-delete-all
                 'reusable-frames
                 (copy-sequence
                  (cdr-safe display-buffer-overriding-action))))))
    (cons functions
          `((reusable-frames . ,(window-frame window))
            (post-command-select-window . t)
            ,@alist))))

(defun vertico-buffer-frame--embark-buffer-p (buffer-or-name)
  "Return non-nil when BUFFER-OR-NAME names an Embark display buffer."
  (and buffer-or-name
       (string-match-p vertico-buffer-frame--embark-buffer-regexp
                       (if (bufferp buffer-or-name)
                           (buffer-name buffer-or-name)
                         buffer-or-name))))

(defun vertico-buffer-frame--display-buffer-advice
    (orig buffer-or-name &optional action frame)
  "Run ORIG so Embark buffers are displayed from the source window."
  (if-let* (((vertico-buffer-frame--embark-buffer-p buffer-or-name))
            (window (vertico-buffer-frame--embark-source-window)))
      (let ((display-buffer-overriding-action
             (vertico-buffer-frame--source-frame-overriding-action window)))
        (with-selected-window window
          (funcall orig buffer-or-name action (or frame (window-frame window)))))
    (funcall orig buffer-or-name action frame)))

(defun vertico-buffer-frame--embark-run-after-command-advice
    (orig fn &rest args)
  "Run ORIG around deferred Embark function FN with ARGS.
When possible, select the original non-minibuffer source window before running
FN so Embark's later `display-buffer' or `pop-to-buffer' calls use the same
window context they use without a Vertico child frame."
  (if-let* ((window (vertico-buffer-frame--embark-source-window)))
      (funcall orig
               (lambda ()
                 (if (vertico-buffer-frame--live-source-window-p window)
                     (let ((display-buffer-overriding-action
                            (vertico-buffer-frame--source-frame-overriding-action
                             window)))
                       (select-window window)
                       (apply fn args))
                   (apply fn args))))
    (apply orig fn args)))

(defun vertico-buffer-frame--install-embark-advice ()
  "Install Embark compatibility advice when Embark is loaded."
  (when (and (bound-and-true-p vertico-buffer-frame-mode)
             (featurep 'embark))
    (vertico-buffer-frame--advice-add
     'display-buffer
     :around
     #'vertico-buffer-frame--display-buffer-advice)
    (vertico-buffer-frame--advice-add
     'embark--run-after-command
     :around
     #'vertico-buffer-frame--embark-run-after-command-advice)))

(defun vertico-buffer-frame--remove-embark-advice ()
  "Remove Embark compatibility advice."
  (when (fboundp 'display-buffer)
    (advice-remove 'display-buffer
                   #'vertico-buffer-frame--display-buffer-advice))
  (when (fboundp 'embark--run-after-command)
    (advice-remove 'embark--run-after-command
                   #'vertico-buffer-frame--embark-run-after-command-advice)))

(with-eval-after-load 'embark
  (vertico-buffer-frame--install-embark-advice))

(defun vertico-buffer-frame--install-vertico-buffer-advice ()
  "Install Vertico buffer compatibility advice."
  (vertico-buffer-frame--advice-add
   'vertico-buffer--redisplay
   :around
   #'vertico-buffer-frame--vertico-buffer-redisplay-advice))

(defun vertico-buffer-frame--remove-vertico-buffer-advice ()
  "Remove Vertico buffer compatibility advice."
  (when (fboundp 'vertico-buffer--redisplay)
    (advice-remove 'vertico-buffer--redisplay
                   #'vertico-buffer-frame--vertico-buffer-redisplay-advice)))

(defun vertico-buffer-frame--enable ()
  "Enable `vertico-buffer-frame-mode' internals."
  (unless vertico-buffer-frame--saved-display-action
    (setq vertico-buffer-frame--saved-display-action
          (list vertico-buffer-display-action)))
  (unless vertico-buffer-frame--saved-buffer-mode
    (setq vertico-buffer-frame--saved-buffer-mode
          (list (bound-and-true-p vertico-buffer-mode))))
  (setq vertico-buffer-display-action
        (vertico-buffer-frame-display-action))
  (add-hook 'minibuffer-setup-hook
            #'vertico-buffer-frame--setup-minibuffer)
  (add-hook 'enable-theme-functions
            #'vertico-buffer-frame--theme-change-advice)
  (add-hook 'disable-theme-functions
            #'vertico-buffer-frame--theme-change-advice)
  (vertico-buffer-frame--install-vertico-buffer-advice)
  (vertico-buffer-frame--install-embark-advice)
  (vertico-buffer-mode 1))

(defun vertico-buffer-frame--cancel-theme-timer ()
  "Cancel pending child-frame theme refresh timer."
  (when (timerp vertico-buffer-frame--theme-timer)
    (cancel-timer vertico-buffer-frame--theme-timer)
    (setq vertico-buffer-frame--theme-timer nil)))

(defun vertico-buffer-frame--restore-display-action ()
  "Restore `vertico-buffer-display-action' saved before mode enablement."
  (when vertico-buffer-frame--saved-display-action
    (setq vertico-buffer-display-action
          (car vertico-buffer-frame--saved-display-action)
          vertico-buffer-frame--saved-display-action nil)))

(defun vertico-buffer-frame--restore-buffer-mode ()
  "Restore `vertico-buffer-mode' state saved before mode enablement."
  (when vertico-buffer-frame--saved-buffer-mode
    (if (car vertico-buffer-frame--saved-buffer-mode)
        (vertico-buffer-mode 1)
      (vertico-buffer-mode -1))
    (setq vertico-buffer-frame--saved-buffer-mode nil)))

(defun vertico-buffer-frame--disable ()
  "Disable `vertico-buffer-frame-mode' internals."
  (remove-hook 'minibuffer-setup-hook
               #'vertico-buffer-frame--setup-minibuffer)
  (remove-hook 'enable-theme-functions
               #'vertico-buffer-frame--theme-change-advice)
  (remove-hook 'disable-theme-functions
               #'vertico-buffer-frame--theme-change-advice)
  (vertico-buffer-frame--cancel-theme-timer)
  (vertico-buffer-frame--hide-preview)
  (vertico-buffer-frame--remove-vertico-buffer-advice)
  (vertico-buffer-frame--remove-embark-advice)
  (vertico-buffer-frame--restore-display-action)
  (vertico-buffer-frame--restore-buffer-mode))

;;;###autoload
(define-minor-mode vertico-buffer-frame-mode
  "Show `vertico-buffer-mode' using an Emacs child frame."
  :global t
  :group 'vertico-buffer-frame
  (if vertico-buffer-frame-mode
      (vertico-buffer-frame--enable)
    (vertico-buffer-frame--disable)))

(when (bound-and-true-p vertico-buffer-frame-mode)
  (vertico-buffer-frame--install-embark-advice)
  (vertico-buffer-frame--install-vertico-buffer-advice))

(provide 'vertico-buffer-frame)
;;; vertico-buffer-frame.el ends here
