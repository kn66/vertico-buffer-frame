;;; vertico-buffer-frame.el --- Show vertico-buffer in a child frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: Nobuyuki Kamimoto
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (vertico "2.8"))
;; Keywords: convenience, frames, minibuffer
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
;; reuse a child frame.

;;; Code:

(require 'subr-x)
(require 'vertico-buffer)

(declare-function vertico--candidate "vertico")
(declare-function vertico--metadata-get "vertico")
(declare-function consult--grep-position "consult")
(declare-function consult-compile--lookup "consult-compile")
(declare-function consult-imenu--items-safe "consult-imenu")
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")
(declare-function bookmark-get-filename "bookmark")
(declare-function bookmark-get-position "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function package-desc-name "package")
(declare-function package-desc-summary "package")
(declare-function package-desc-version "package")
(declare-function package-version-join "package")
(declare-function xref-item-location "xref")
(declare-function xref-location-marker "xref")
(defvar bookmark-alist)
(defvar imenu-space-replacement)
(defvar package-alist)
(defvar package-archive-contents)

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

(defcustom vertico-buffer-frame-preview t
  "When non-nil, show a preview child frame for the current candidate."
  :type 'boolean
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-preview-delay 0.05
  "Idle delay in seconds before updating the preview child frame.
This keeps rapid candidate navigation responsive by avoiding preview work for
intermediate candidates."
  :type 'number
  :group 'vertico-buffer-frame)

(defcustom vertico-buffer-frame-golden-ratio-scale 1.30
  "Scale factor for the golden-ratio candidate frame size.
The base size is the parent frame size divided by the golden ratio.  This
factor is applied to both width and height, so the overlay layout keeps its
relative proportions while becoming larger or smaller."
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
    (face . vertico-buffer-frame-preview-face)
    (minor-mode . vertico-buffer-frame-preview-command)
    (theme . vertico-buffer-frame-preview-theme)
    (package . vertico-buffer-frame-preview-package)
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
    (project-switch-to-buffer . vertico-buffer-frame-preview-buffer))
  "Alist of commands and preview functions.
Each preview function is called with the current candidate string."
  :type '(alist :key-type symbol :value-type function)
  :group 'vertico-buffer-frame)

(defface vertico-buffer-frame-preview-line
  '((t :inherit highlight))
  "Face used to highlight the current preview line."
  :group 'vertico-buffer-frame)

(defface vertico-buffer-frame-preview-match
  '((t :inherit match))
  "Face used to highlight matches in preview content."
  :group 'vertico-buffer-frame)

(defvar vertico-buffer-frame--preview-buffer " *vertico-buffer-frame-preview*")
(defvar vertico-buffer-frame--preview-frame nil)
(defvar vertico-buffer-frame--saved-display-action nil
  "Previous value of `vertico-buffer-display-action'.")
(defvar vertico-buffer-frame--theme-timer nil
  "Timer used to refresh child frame colors after theme changes.")

(defconst vertico-buffer-frame--golden-ratio 1.61803398875)
(defconst vertico-buffer-frame--top-offset 64)
(defconst vertico-buffer-frame--internal-border-width 8)
(defconst vertico-buffer-frame--border-width 1)
(defconst vertico-buffer-frame--preview-max-size 20000)
(defconst vertico-buffer-frame--preview-location-context 8
  "Fallback line context for location previews.")
(defconst vertico-buffer-frame--preview-fallback-columns 120
  "Fallback column limit for position preview line slices.")

(defvar-local vertico-buffer-frame--candidate-frame nil
  "Candidate child frame used by the current minibuffer.")

(defvar-local vertico-buffer-frame--candidate-frame-chrome-hidden nil
  "Non-nil when chrome has been hidden for the current candidate frame.")

(defvar-local vertico-buffer-frame--preview-timer nil
  "Timer used to debounce preview updates for the current minibuffer.")

(defvar-local vertico-buffer-frame--last-preview-candidate nil
  "Last candidate rendered in the preview frame.")

(defvar-local vertico-buffer-frame--preview-category nil
  "Completion category captured for delayed preview rendering.")

(defvar-local vertico-buffer-frame--preview-command nil
  "Command captured for delayed preview rendering.")

(defvar-local vertico-buffer-frame--preview-completion-table nil
  "Completion table captured for delayed preview rendering.")

(defvar-local vertico-buffer-frame--preview-completing-file-name nil
  "File completion state captured for delayed preview rendering.")

(defvar-local vertico-buffer-frame--imenu-index-buffer nil
  "Source buffer for `vertico-buffer-frame--imenu-index-cache'.")

(defvar-local vertico-buffer-frame--imenu-index-cache nil
  "Cached imenu index for the current minibuffer.")

(defvar-local vertico-buffer-frame--exiting nil
  "Non-nil while the current minibuffer is exiting.")

(defun vertico-buffer-frame--compact-alist (alist)
  "Return ALIST without entries whose value is nil."
  (delq nil
        (mapcar (lambda (entry)
                  (and (cdr entry) entry))
                alist)))

(defun vertico-buffer-frame--face-color (function face)
  "Return FACE color from FUNCTION, or nil when it is unspecified."
  (let ((color (funcall function face nil t)))
    (unless (member color '("unspecified-bg" "unspecified-fg"))
      color)))

(defun vertico-buffer-frame--border-color ()
  "Return the border color for child frames."
  (or (vertico-buffer-frame--face-color #'face-foreground 'default)
      "black"))

(defun vertico-buffer-frame--round-positive (number)
  "Round NUMBER to a positive integer."
  (max 1 (round number)))

(defun vertico-buffer-frame--golden-scale ()
  "Return a positive golden-ratio scale factor."
  (max 0.1 (float vertico-buffer-frame-golden-ratio-scale)))

(defun vertico-buffer-frame--golden-size (parent)
  "Return golden-ratio candidate frame size for PARENT.
The return value is a cons cell (WIDTH . HEIGHT), in character columns and
lines."
  (let* ((scale (vertico-buffer-frame--golden-scale))
         (pixel-width (min (frame-pixel-width parent)
                           (* (/ (float (frame-pixel-width parent))
                                 vertico-buffer-frame--golden-ratio)
                              scale)))
         (pixel-height (min (frame-pixel-height parent)
                            (* (/ (float (frame-pixel-height parent))
                                  vertico-buffer-frame--golden-ratio)
                               scale))))
    (cons (vertico-buffer-frame--round-positive
           (/ pixel-width (frame-char-width parent)))
          (vertico-buffer-frame--round-positive
           (/ pixel-height (frame-char-height parent))))))

(defun vertico-buffer-frame--frame-height-lines (frame)
  "Return FRAME height in lines."
  (or (frame-parameter frame 'height)
      (vertico-buffer-frame--round-positive
       (/ (float (frame-pixel-height frame))
          (frame-char-height frame)))))

(defun vertico-buffer-frame--golden-preview-top-lines (candidate-frame)
  "Return preview top inset for CANDIDATE-FRAME in lines.
The inset is the candidate frame line count divided by the fourth power of the
golden ratio."
  (max 1
       (round (/ (float (vertico-buffer-frame--frame-height-lines
                         candidate-frame))
                 (expt vertico-buffer-frame--golden-ratio 4)))))

(defun vertico-buffer-frame--golden-preview-size (candidate-frame)
  "Return golden-ratio preview size for CANDIDATE-FRAME.
The return value is a cons cell (WIDTH . HEIGHT), in character columns and
lines."
  (let* ((height-lines (vertico-buffer-frame--frame-height-lines
                        candidate-frame))
         (top-lines (vertico-buffer-frame--golden-preview-top-lines
                     candidate-frame)))
    (cons (vertico-buffer-frame--round-positive
           (/ (/ (float (frame-pixel-width candidate-frame))
                 vertico-buffer-frame--golden-ratio)
              (frame-char-width candidate-frame)))
          (max 1 (- height-lines top-lines)))))

(defun vertico-buffer-frame--preview-height-lines ()
  "Return current preview frame height in lines."
  (or (and (frame-live-p vertico-buffer-frame--preview-frame)
           (vertico-buffer-frame--frame-height-lines
            vertico-buffer-frame--preview-frame))
      (when-let* ((candidate-frame (vertico-buffer-frame--candidate-frame))
                  ((frame-live-p candidate-frame)))
        (cdr (vertico-buffer-frame--golden-preview-size candidate-frame)))))

(defun vertico-buffer-frame--preview-title-lines (title)
  "Return line count occupied by preview TITLE."
  (let ((title (or title "")))
    (if (string-empty-p title)
        0
      (let ((count 0)
            (index 0))
        (while (setq index (string-search "\n" title index))
          (setq count (1+ count)
                index (1+ index)))
        count))))

(defun vertico-buffer-frame--location-content-lines (title)
  "Return visible content lines for a location preview.
TITLE is included to reserve vertical space for the preview heading."
  (if-let* ((height (vertico-buffer-frame--preview-height-lines)))
      (let* ((title-lines (vertico-buffer-frame--preview-title-lines title))
             (content-lines (max 1 (- height title-lines))))
        content-lines)
    (1+ (* vertico-buffer-frame--preview-location-context 2))))

(defun vertico-buffer-frame--preview-content-columns ()
  "Return column count to copy for each line in position previews."
  (or (and (frame-live-p vertico-buffer-frame--preview-frame)
           (frame-parameter vertico-buffer-frame--preview-frame 'width))
      (when-let* ((candidate-frame (vertico-buffer-frame--candidate-frame))
                  ((frame-live-p candidate-frame)))
        (car (vertico-buffer-frame--golden-preview-size candidate-frame)))
      vertico-buffer-frame--preview-fallback-columns))

(defun vertico-buffer-frame--child-frame-parameters ()
  "Return frame parameters for the Vertico child frame."
  (let* ((parent (selected-frame))
         (golden-size (vertico-buffer-frame--golden-size parent))
         (parameters
          (vertico-buffer-frame--compact-alist
           `((name . "vertico-buffer-frame")
             (width . ,(car golden-size))
             (height . ,(cdr golden-size))
             (internal-border-width . ,vertico-buffer-frame--internal-border-width)
             (child-frame-border-width . ,vertico-buffer-frame--border-width)
             (border-color . ,(vertico-buffer-frame--border-color))
             (background-color . ,(or vertico-buffer-frame-background-color
                                      (vertico-buffer-frame--face-color
                                       #'face-background 'default)))
             (foreground-color . ,(or vertico-buffer-frame-foreground-color
                                      (vertico-buffer-frame--face-color
                                       #'face-foreground 'default)))
             (menu-bar-lines . 0)
             (tab-bar-lines . 0)
             (tool-bar-lines . 0)
             (undecorated . t)
             (no-accept-focus . t)
             (no-other-frame . t)
             (skip-taskbar . t)
             (unsplittable . t)
             (minibuffer-exit . delete-frame)
             (share-child-frame . vertico-buffer-frame)))))
    parameters))

(defun vertico-buffer-frame--preview-frame-parameters (&optional candidate-frame)
  "Return frame parameters for the preview child frame.
When CANDIDATE-FRAME is live, derive the preview size from it."
  (let* ((golden-size (and (frame-live-p candidate-frame)
                           (vertico-buffer-frame--golden-preview-size
                            candidate-frame)))
         (parameters
          (vertico-buffer-frame--compact-alist
           `((name . "vertico-buffer-frame-preview")
             (width . ,(car-safe golden-size))
             (height . ,(cdr-safe golden-size))
             (internal-border-width . ,vertico-buffer-frame--internal-border-width)
             (child-frame-border-width . ,vertico-buffer-frame--border-width)
             (border-color . ,(vertico-buffer-frame--border-color))
             (background-color . ,(or vertico-buffer-frame-background-color
                                      (vertico-buffer-frame--face-color
                                       #'face-background 'default)))
             (foreground-color . ,(or vertico-buffer-frame-foreground-color
                                      (vertico-buffer-frame--face-color
                                       #'face-foreground 'default)))
             (menu-bar-lines . 0)
             (tab-bar-lines . 0)
             (tool-bar-lines . 0)
             (undecorated . t)
             (no-accept-focus . t)
             (no-other-frame . t)
             (skip-taskbar . t)
             (unsplittable . t)
             (minibuffer-exit . delete-frame)
             (share-child-frame . vertico-buffer-frame-preview)))))
    parameters))

(defun vertico-buffer-frame--apply-frame-parameters (frame parameters)
  "Apply current child-frame PARAMETERS to FRAME when FRAME is live."
  (when (frame-live-p frame)
    (modify-frame-parameters frame parameters)
    (let ((border-color (vertico-buffer-frame--border-color)))
      (set-face-background 'child-frame-border border-color frame)
      (set-face-background 'internal-border border-color frame))))

(defun vertico-buffer-frame--hide-window-chrome (window)
  "Hide mode, header, and tab lines in WINDOW."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (setq-local header-line-format nil
                  mode-line-format nil
                  tab-line-format nil
                  truncate-lines t))
    (set-window-parameter window 'header-line-format 'none)
    (set-window-parameter window 'mode-line-format 'none)
    (set-window-parameter window 'tab-line-format 'none)))

(defun vertico-buffer-frame--hide-frame-chrome (frame)
  "Hide mode, header, and tab lines in every live window of FRAME."
  (when (frame-live-p frame)
    (walk-windows #'vertico-buffer-frame--hide-window-chrome nil frame)))

(defun vertico-buffer-frame--position-frame (frame)
  "Position child FRAME according to `vertico-buffer-frame' options."
  (let* ((parent (or (frame-parent frame) (selected-frame)))
         (left (vertico-buffer-frame--candidate-left parent frame))
         (top (vertico-buffer-frame--candidate-top)))
    (when (and left top)
      (set-frame-position frame left top))))

(defun vertico-buffer-frame--candidate-left (parent candidate)
  "Return left pixel position for CANDIDATE in PARENT."
  (max 0 (/ (- (frame-pixel-width parent)
               (frame-pixel-width candidate))
            2)))

(defun vertico-buffer-frame--candidate-top ()
  "Return top pixel position for the candidate child frame."
  vertico-buffer-frame--top-offset)

(defun vertico-buffer-frame--preview-top-offset (candidate-frame)
  "Return preview top offset in pixels for CANDIDATE-FRAME."
  (* (frame-char-height candidate-frame)
     (vertico-buffer-frame--golden-preview-top-lines candidate-frame)))

(defun vertico-buffer-frame--position-pair (candidate-frame &optional preview-frame)
  "Position CANDIDATE-FRAME and overlay PREVIEW-FRAME on its right side."
  (let* ((parent (or (frame-parent candidate-frame) (selected-frame)))
         (top (vertico-buffer-frame--candidate-top))
         (left (vertico-buffer-frame--candidate-left parent candidate-frame)))
    (when (and left top)
      (set-frame-position candidate-frame left top)
      (when (frame-live-p preview-frame)
        (set-frame-position preview-frame
                            (max left
                                 (- (+ left
                                       (frame-pixel-width candidate-frame))
                                    (frame-pixel-width preview-frame)))
                            (+ top
                               (vertico-buffer-frame--preview-top-offset
                                candidate-frame)))))))

(defun vertico-buffer-frame--display-buffer (buffer alist)
  "Display BUFFER in a child frame using ALIST.
This wraps `display-buffer-in-child-frame' so the reused child frame can be
repositioned after Emacs knows its actual pixel size."
  (when-let* ((window (display-buffer-in-child-frame buffer alist)))
    (let ((frame (window-frame window)))
      (vertico-buffer-frame--hide-window-chrome window)
      (vertico-buffer-frame--apply-frame-parameters
       frame (vertico-buffer-frame--child-frame-parameters))
      (when-let* ((minibuffer-window (active-minibuffer-window)))
        (with-current-buffer (window-buffer minibuffer-window)
          (setq-local vertico-buffer-frame--candidate-frame
                      frame
                      vertico-buffer-frame--candidate-frame-chrome-hidden
                      t)))
      (vertico-buffer-frame--position-frame frame))
    window))

(defun vertico-buffer-frame-display-action ()
  "Return a `display-buffer' action for showing Vertico in a child frame."
  `(vertico-buffer-frame--display-buffer
    (inhibit-switch-frame . t)
    (child-frame-parameters . ,(vertico-buffer-frame--child-frame-parameters))))

(defun vertico-buffer-frame--refresh-display-action ()
  "Refresh `vertico-buffer-display-action' with current theme colors."
  (when (and (bound-and-true-p vertico-buffer-frame-mode)
             (boundp 'vertico-buffer-display-action))
    (setq vertico-buffer-display-action
          (vertico-buffer-frame-display-action))))

(defun vertico-buffer-frame--refresh-frames ()
  "Refresh live child frames after face or theme changes."
  (vertico-buffer-frame--refresh-display-action)
  (let ((candidate-frame (vertico-buffer-frame--candidate-frame)))
    (when (frame-live-p candidate-frame)
      (vertico-buffer-frame--apply-frame-parameters
       candidate-frame (vertico-buffer-frame--child-frame-parameters))
      (vertico-buffer-frame--hide-frame-chrome candidate-frame))
    (when (frame-live-p vertico-buffer-frame--preview-frame)
      (vertico-buffer-frame--apply-frame-parameters
       vertico-buffer-frame--preview-frame
       (vertico-buffer-frame--preview-frame-parameters candidate-frame))
      (vertico-buffer-frame--hide-frame-chrome
       vertico-buffer-frame--preview-frame))))

(defun vertico-buffer-frame--theme-change-advice (&rest _)
  "Refresh child frames after a theme command changes faces."
  (when (timerp vertico-buffer-frame--theme-timer)
    (cancel-timer vertico-buffer-frame--theme-timer))
  (setq vertico-buffer-frame--theme-timer
        (run-at-time 0 nil #'vertico-buffer-frame--refresh-frames)))

(defun vertico-buffer-frame--current-candidate ()
  "Return the current Vertico candidate string, or nil."
  (when (and (boundp 'vertico--index)
             (boundp 'vertico--total)
             (> vertico--total 0)
             (>= vertico--index 0))
    (ignore-errors
      (vertico--candidate))))

(defun vertico-buffer-frame--completion-category ()
  "Return the current completion category, or nil."
  (or (ignore-errors
        (vertico--metadata-get 'category))
      vertico-buffer-frame--preview-category))

(defun vertico-buffer-frame--target (candidate)
  "Return preview category and target for CANDIDATE."
  (let ((category (vertico-buffer-frame--completion-category)))
    (if (eq category 'multi-category)
        (if-let* ((multi (get-text-property 0 'multi-category candidate)))
            (cons (car multi) (cdr multi))
          (cons category candidate))
      (cons category candidate))))

(defun vertico-buffer-frame--candidate-string (candidate)
  "Return CANDIDATE as a plain string."
  (substring-no-properties (format "%s" candidate)))

(defun vertico-buffer-frame--candidate-key (candidate)
  "Return a stable comparison key for CANDIDATE."
  (and candidate (vertico-buffer-frame--candidate-string candidate)))

(defun vertico-buffer-frame--candidate-symbol (candidate)
  "Return CANDIDATE as an interned symbol."
  (intern-soft (vertico-buffer-frame--candidate-string candidate)))

(defun vertico-buffer-frame--documentation (title body)
  "Return preview text from TITLE and BODY."
  (when body
    (concat title "\n\n" (string-trim (substitute-command-keys body)))))

(defun vertico-buffer-frame-preview-default (candidate)
  "Return preview content for CANDIDATE using built-in preview rules."
  (let* ((preview (vertico-buffer-frame--target candidate))
         (category (car preview))
         (target (cdr preview)))
    (or (when-let* ((function (cdr (assoc category
                                          vertico-buffer-frame-preview-category-functions))))
          (funcall function target))
        (when-let* ((function (cdr (assq this-command
                                         vertico-buffer-frame-preview-command-functions))))
          (funcall function target))
        (when minibuffer-completing-file-name
          (vertico-buffer-frame-preview-file candidate)))))

(defun vertico-buffer-frame-preview-file (candidate)
  "Return file preview content for CANDIDATE."
  (let ((file (expand-file-name (substring-no-properties candidate))))
    (cond
     ((file-regular-p file)
      (with-temp-buffer
        (insert-file-contents file nil 0
                              vertico-buffer-frame--preview-max-size)
        (buffer-string)))
     ((file-directory-p file)
      (mapconcat #'identity
                 (directory-files file nil directory-files-no-dot-files-regexp)
                 "\n")))))

(defun vertico-buffer-frame-preview-buffer (candidate)
  "Return buffer preview content for CANDIDATE."
  (when-let* ((buffer (if (bufferp candidate)
                         candidate
                       (get-buffer (substring-no-properties candidate)))))
    buffer))

(defun vertico-buffer-frame-preview-string (candidate)
  "Return CANDIDATE as preview content."
  (vertico-buffer-frame--candidate-string candidate))

(defun vertico-buffer-frame-preview-command (candidate)
  "Return command documentation preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((commandp symbol)))
    (vertico-buffer-frame--documentation
     (format "%s" symbol)
     (documentation symbol t))))

(defun vertico-buffer-frame-preview-function (candidate)
  "Return function documentation preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((fboundp symbol)))
    (vertico-buffer-frame--documentation
     (format "%s" symbol)
     (documentation symbol t))))

(defun vertico-buffer-frame-preview-variable (candidate)
  "Return variable documentation preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((boundp symbol)))
    (let ((doc (documentation-property symbol 'variable-documentation t)))
      (concat
       (format "%s\n\n" symbol)
       (if doc (string-trim (substitute-command-keys doc)) "Undocumented.")
       "\n\n"
       (format "Value:\n%S" (symbol-value symbol))))))

(defun vertico-buffer-frame-preview-symbol (candidate)
  "Return documentation preview for symbol CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate)))
    (or (vertico-buffer-frame-preview-function (symbol-name symbol))
        (vertico-buffer-frame-preview-variable (symbol-name symbol))
        (format "%s" symbol))))

(defun vertico-buffer-frame-preview-face (candidate)
  "Return face preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((facep symbol)))
    (let ((doc (face-documentation symbol)))
      (concat
       (format "%s\n\n" symbol)
       (if doc (string-trim doc) "Undocumented face.")
       "\n\n"
       (format "Foreground: %S\nBackground: %S\nWeight: %S\nSlant: %S"
               (face-attribute symbol :foreground nil t)
               (face-attribute symbol :background nil t)
               (face-attribute symbol :weight nil t)
               (face-attribute symbol :slant nil t))))))

(defun vertico-buffer-frame-preview-theme (candidate)
  "Return theme preview text for CANDIDATE."
  (let* ((name (vertico-buffer-frame--candidate-string candidate))
         (symbol (intern-soft name)))
    (when symbol
      (format "%s\n\n%s"
              symbol
              (if (memq symbol custom-enabled-themes)
                  "This theme is currently enabled."
                "Theme candidate.")))))

(defun vertico-buffer-frame-preview-package (candidate)
  "Return package preview text for CANDIDATE."
  (when (require 'package nil t)
    (let* ((name (vertico-buffer-frame--candidate-symbol candidate))
           (desc (or (cadr (assq name package-alist))
                     (cadr (assq name package-archive-contents)))))
      (when desc
        (format "%s %s\n\n%s"
                (package-desc-name desc)
                (package-version-join (package-desc-version desc))
                (or (package-desc-summary desc) ""))))))

(defun vertico-buffer-frame-preview-bookmark (candidate)
  "Return bookmark preview content for CANDIDATE."
  (when (require 'bookmark nil t)
    (bookmark-maybe-load-default-file)
    (when-let* ((bookmark (assoc (vertico-buffer-frame--candidate-string candidate)
                                 bookmark-alist)))
      (or (when-let* ((file (bookmark-get-filename bookmark))
                      ((file-readable-p file)))
            (let ((buffer (find-file-noselect file)))
              (if-let* ((position (bookmark-get-position bookmark)))
                  (vertico-buffer-frame--position-preview
                   (set-marker (make-marker) position buffer)
                   (format "%s\n%s:%s\n\n"
                           (car bookmark)
                           (abbreviate-file-name file)
                           position))
                buffer)))
          (format "%s\n\n%S" (car bookmark) (cdr bookmark))))))

(defun vertico-buffer-frame--position-marker (position)
  "Return a marker for POSITION."
  (cond
   ((markerp position) position)
   ((integerp position)
    (copy-marker position))
   ((and (consp position) (bufferp (car position)))
    (set-marker (make-marker) (cdr position) (car position)))))

(defun vertico-buffer-frame--add-face (string beg end face)
  "Add FACE to STRING between BEG and END, clipping to string bounds."
  (let ((beg (max 0 beg))
        (end (min (length string) end)))
    (when (< beg end)
      (add-face-text-property beg end face nil string))))

(defun vertico-buffer-frame--line-slice (point matches columns)
  "Return a bounded preview slice for the current line.
POINT is the target buffer position.  MATCHES is a list of match begin/end
pairs relative to POINT.  COLUMNS limits how much of the line is copied."
  (let* ((line-beg (line-beginning-position))
         (line-end (line-end-position))
         (target-line-p (and (<= line-beg point)
                             (<= point line-end)))
         (target-column (and target-line-p (- point line-beg)))
         (start-offset (if target-column
                           (max 0 (- target-column (/ columns 2)))
                         0))
         (slice-beg (min line-end (+ line-beg start-offset)))
         (slice-end (min line-end (+ slice-beg columns)))
         (slice (buffer-substring-no-properties slice-beg slice-end)))
    (when target-line-p
      (vertico-buffer-frame--add-face
       slice 0 (length slice) 'vertico-buffer-frame-preview-line)
      (dolist (match matches)
        (when (and (consp match)
                   (integerp (car match))
                   (integerp (cdr match)))
          (let ((match-beg (+ point (car match)))
                (match-end (+ point (cdr match))))
            (vertico-buffer-frame--add-face
             slice
             (- match-beg slice-beg)
             (- match-end slice-beg)
             'vertico-buffer-frame-preview-match)))))
    slice))

(defun vertico-buffer-frame--position-lines (point content-lines columns matches)
  "Return bounded preview text around POINT.
CONTENT-LINES is the maximum number of lines to include.  COLUMNS limits each
line slice.  MATCHES is a list of match begin/end pairs relative to POINT."
  (let ((lines nil)
        (before-lines (/ (1- content-lines) 2)))
    (goto-char point)
    (forward-line (- before-lines))
    (while (and (< (length lines) content-lines)
                (not (eobp)))
      (push (vertico-buffer-frame--line-slice point matches columns)
            lines)
      (forward-line 1))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun vertico-buffer-frame--position-content (point title matches)
  "Return preview content around POINT in the current buffer.
TITLE is inserted above the preview when non-nil.
MATCHES is a list of match begin/end pairs relative to POINT."
  (goto-char point)
  (let* ((title (or title (format "%s:%d\n\n"
                                  (buffer-name)
                                  (line-number-at-pos point))))
         (content-lines (vertico-buffer-frame--location-content-lines title))
         (columns (vertico-buffer-frame--preview-content-columns)))
    (concat
     title
     (vertico-buffer-frame--position-lines
      point content-lines columns matches))))

(defun vertico-buffer-frame--position-preview (position &optional title matches)
  "Return preview content around POSITION.
TITLE is inserted above the preview when non-nil.
MATCHES is a list of match begin/end pairs relative to POSITION."
  (let* ((marker (vertico-buffer-frame--position-marker position))
         (buffer (and marker (marker-buffer marker)))
         (point (and marker (marker-position marker))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (vertico-buffer-frame--position-content point title matches)))))))

(defun vertico-buffer-frame-preview-location (candidate)
  "Return location preview content for CANDIDATE."
  (if-let* ((location (and (stringp candidate)
                           (get-text-property 0 'consult-location candidate))))
      (vertico-buffer-frame--position-preview (car location))
    (vertico-buffer-frame--position-preview (or (car-safe candidate) candidate))))

(defun vertico-buffer-frame--consult-imenu-item (candidate)
  "Return Consult imenu item matching CANDIDATE."
  (when (and (stringp candidate)
             (fboundp 'consult-imenu--items-safe))
    (let ((name (vertico-buffer-frame--candidate-string candidate)))
      (when-let* ((window (minibuffer-selected-window))
                  ((window-live-p window)))
        (with-current-buffer (window-buffer window)
          (catch 'item
            (dolist (item (consult-imenu--items-safe))
              (when (and (consp item)
                         (stringp (car item))
                         (equal name
                                (substring-no-properties (car item))))
                (throw 'item item)))))))))

(defun vertico-buffer-frame--imenu-minibuffer-item (candidate)
  "Return imenu item for CANDIDATE from the current completion table."
  (let ((table (or vertico-buffer-frame--preview-completion-table
                   minibuffer-completion-table)))
    (when (and (stringp candidate)
               (listp table))
      (let ((name (vertico-buffer-frame--candidate-string candidate)))
        (catch 'item
          (dolist (item table)
            (when (and (consp item)
                       (stringp (car item))
                       (equal name
                              (substring-no-properties (car item))))
              (throw 'item
                     (or (get-text-property 0 'imenu-choice (car item))
                         item)))))))))

(defun vertico-buffer-frame--imenu-display-name (name prefix)
  "Return possible display names for imenu NAME under PREFIX."
  (let* ((name (format "%s" name))
         (flat (if prefix
                   (concat prefix " " name)
                 name))
         (space (and imenu-space-replacement
                     (aref imenu-space-replacement 0))))
    (delete-dups
     (delq nil
           (list name
                 flat
                 (and space (subst-char-in-string ?\s space name))
                 (and space (subst-char-in-string ?\s space flat)))))))

(defun vertico-buffer-frame--imenu-find-item (name items &optional prefix)
  "Return imenu item in ITEMS whose display name matches NAME.
PREFIX is the flattened submenu prefix."
  (catch 'item
    (dolist (item items)
      (when (consp item)
        (let ((item-name (car item)))
          (if (and (fboundp 'imenu--subalist-p)
                   (imenu--subalist-p item))
              (when-let* ((found
                           (vertico-buffer-frame--imenu-find-item
                            name
                            (cdr item)
                            (if prefix
                                (concat prefix " " (format "%s" item-name))
                              (format "%s" item-name)))))
                (throw 'item found))
            (when (member name
                          (vertico-buffer-frame--imenu-display-name
                           item-name prefix))
              (throw 'item item))))))))

(defun vertico-buffer-frame--imenu-index (buffer)
  "Return cached imenu index for BUFFER in the current minibuffer."
  (if (eq buffer vertico-buffer-frame--imenu-index-buffer)
      vertico-buffer-frame--imenu-index-cache
    (setq-local vertico-buffer-frame--imenu-index-buffer buffer
                vertico-buffer-frame--imenu-index-cache
                (with-current-buffer buffer
                  (imenu--make-index-alist 'noerror)))))

(defun vertico-buffer-frame--imenu-item (candidate)
  "Return standard imenu item matching CANDIDATE."
  (when (and (stringp candidate)
             (require 'imenu nil t)
             (fboundp 'imenu--make-index-alist))
    (let ((name (vertico-buffer-frame--candidate-string candidate)))
      (when-let* ((window (minibuffer-selected-window))
                  ((window-live-p window)))
        (vertico-buffer-frame--imenu-find-item
         name
         (vertico-buffer-frame--imenu-index (window-buffer window)))))))

(defun vertico-buffer-frame-preview-consult-candidate (candidate)
  "Return preview content from CANDIDATE's `consult--candidate' property."
  (when-let* ((position (and (stringp candidate)
                             (get-text-property 0 'consult--candidate candidate))))
    (cond
     ((and (markerp position)
           (fboundp 'consult-compile--lookup))
      (vertico-buffer-frame--position-preview
       (or (ignore-errors (consult-compile--lookup position)) position)))
     ((and (consp position) (markerp (car position)))
      (vertico-buffer-frame--position-preview (car position) nil (cdr position)))
     (t
      (vertico-buffer-frame--position-preview position)))))

(defun vertico-buffer-frame-preview-grep (candidate)
  "Return grep location preview content for CANDIDATE."
  (when (and (stringp candidate)
             (fboundp 'consult--grep-position))
    (when-let* ((position (ignore-errors
                            (consult--grep-position candidate #'find-file-noselect)))
                (marker (car-safe position)))
      (vertico-buffer-frame--position-preview marker nil (cdr position)))))

(defun vertico-buffer-frame-preview-imenu (candidate)
  "Return imenu preview content for CANDIDATE."
  (let* ((item (cond
                ((or (markerp candidate) (integerp candidate))
                 candidate)
                ((consp candidate)
                 candidate)
                ((stringp candidate)
                 (or (get-text-property 0 'imenu-choice candidate)
                     (vertico-buffer-frame--imenu-minibuffer-item candidate)
                     (vertico-buffer-frame--consult-imenu-item candidate)
                     (vertico-buffer-frame--imenu-item candidate)))))
         (position (cond
                    ((or (markerp item) (integerp item)) item)
                    ((or (markerp (cdr-safe item))
                         (integerp (cdr-safe item)))
                     (cdr item))
                    ((and (consp (cdr-safe item))
                          (or (markerp (cadr item))
                              (integerp (cadr item))))
                     (cadr item)))))
    (vertico-buffer-frame--position-preview position)))

(defun vertico-buffer-frame-preview-org-heading (candidate)
  "Return Org heading preview content for CANDIDATE."
  (when-let* ((marker (and (stringp candidate)
                           (get-text-property 0 'org-marker candidate))))
    (vertico-buffer-frame--position-preview marker)))

(defun vertico-buffer-frame-preview-info (candidate)
  "Return Info search preview content for CANDIDATE."
  (pcase (and (stringp candidate)
              (get-text-property 0 'consult--info candidate))
    (`(,node ,position ,buffer)
     (when (buffer-live-p buffer)
       (vertico-buffer-frame--position-preview
        (set-marker (make-marker) position buffer)
        (format "%s\n\n" node))))))

(defun vertico-buffer-frame-preview-man (candidate)
  "Return man page preview text for CANDIDATE."
  (let ((page (or (and (stringp candidate)
                       (get-text-property 0 'consult-man candidate))
                  (vertico-buffer-frame--candidate-string candidate))))
    (format "%s\n\n%s" page (vertico-buffer-frame--candidate-string candidate))))

(defun vertico-buffer-frame-preview-xref (candidate)
  "Return xref preview content for CANDIDATE."
  (when (and (fboundp 'xref-item-location)
             (fboundp 'xref-location-marker))
    (let* ((xref (or (and (stringp candidate)
                          (get-text-property 0 'consult-xref candidate))
                     (if (consp candidate) (cdr candidate) candidate)))
           (location (ignore-errors (xref-item-location xref)))
           (marker (and location
                        (ignore-errors (xref-location-marker location)))))
      (vertico-buffer-frame--position-preview marker))))

(defun vertico-buffer-frame--insert-preview-content (content)
  "Insert preview CONTENT into the current preview buffer."
  (erase-buffer)
  (cond
   ((stringp content)
    (insert (substring content
                       0
                       (min (length content)
                            vertico-buffer-frame--preview-max-size))))
   ((bufferp content)
    (insert-buffer-substring content
                             (with-current-buffer content (point-min))
                             (with-current-buffer content
                               (min (point-max)
                                    (+ (point-min)
                                       vertico-buffer-frame--preview-max-size))))))
  (goto-char (point-min)))

(defun vertico-buffer-frame--preview-content ()
  "Return preview content for the current candidate."
  (when-let* (((not vertico-buffer-frame--exiting))
              (function vertico-buffer-frame-preview-function)
              (candidate (vertico-buffer-frame--current-candidate)))
    (condition-case err
        (let ((this-command (or vertico-buffer-frame--preview-command
                                this-command))
              (minibuffer-completion-table
               (or vertico-buffer-frame--preview-completion-table
                   minibuffer-completion-table))
              (minibuffer-completing-file-name
               (or vertico-buffer-frame--preview-completing-file-name
                   minibuffer-completing-file-name)))
          (funcall function candidate))
      (error
       (message "vertico-buffer-frame preview: %s" (error-message-string err))
       nil))))

(defun vertico-buffer-frame--cancel-preview-timer ()
  "Cancel the pending preview update timer for the current minibuffer."
  (when (timerp vertico-buffer-frame--preview-timer)
    (cancel-timer vertico-buffer-frame--preview-timer))
  (setq-local vertico-buffer-frame--preview-timer nil))

(defun vertico-buffer-frame--hide-preview ()
  "Hide the preview child frame."
  (vertico-buffer-frame--cancel-preview-timer)
  (when (frame-live-p vertico-buffer-frame--preview-frame)
    (let ((delete-frame-functions nil))
      (delete-frame vertico-buffer-frame--preview-frame)))
  (setq vertico-buffer-frame--preview-frame nil
        vertico-buffer-frame--last-preview-candidate nil))

(defun vertico-buffer-frame--candidate-frame ()
  "Return the current Vertico candidate child frame."
  (or (and (frame-live-p vertico-buffer-frame--candidate-frame)
           vertico-buffer-frame--candidate-frame)
      (when-let* ((window (active-minibuffer-window))
                  (buffer (window-buffer window)))
        (buffer-local-value 'vertico-buffer-frame--candidate-frame buffer))))

(defun vertico-buffer-frame--reset-preview-window (window)
  "Reset WINDOW viewport to the beginning of its preview buffer."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (set-window-point window (point-min))
      (set-window-start window (point-min) t))
    (set-window-hscroll window 0)))

(defun vertico-buffer-frame--truncate-preview-lines (window)
  "Limit preview buffer lines to WINDOW's visible width."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (let ((inhibit-read-only t)
            (width (max 1 (window-body-width window))))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((end (line-end-position)))
              (move-to-column width)
              (when (< (point) end)
                (delete-region (point) end)))
            (forward-line 1)))))))

(defun vertico-buffer-frame--show-preview-content (content)
  "Show preview CONTENT in the preview child frame."
  (if-let* (((and (bound-and-true-p vertico-buffer-frame-mode)
                 vertico-buffer-frame-preview))
            (candidate-frame (vertico-buffer-frame--candidate-frame))
            ((frame-live-p candidate-frame))
            (content))
      (let* ((preview-buffer (get-buffer-create
                              vertico-buffer-frame--preview-buffer))
             (window (display-buffer-in-child-frame
                      preview-buffer
                      `((inhibit-switch-frame . t)
                        (child-frame-parameters
                         . ,(vertico-buffer-frame--preview-frame-parameters
                             candidate-frame))))))
        (with-current-buffer preview-buffer
          (let ((inhibit-read-only t))
            (setq-local cursor-type nil
                        truncate-lines t
                        mode-line-format nil)
            (vertico-buffer-frame--insert-preview-content content)))
        (when window
          (vertico-buffer-frame--truncate-preview-lines window)
          (vertico-buffer-frame--reset-preview-window window)
          (setq vertico-buffer-frame--preview-frame (window-frame window))
          (vertico-buffer-frame--hide-window-chrome window)
          (vertico-buffer-frame--apply-frame-parameters
           vertico-buffer-frame--preview-frame
           (vertico-buffer-frame--preview-frame-parameters candidate-frame))
          (vertico-buffer-frame--position-pair
           candidate-frame vertico-buffer-frame--preview-frame)))
    (vertico-buffer-frame--hide-preview)))

(defun vertico-buffer-frame--show-preview ()
  "Show preview for the current Vertico candidate."
  (when-let* ((candidate (vertico-buffer-frame--current-candidate)))
    (setq-local vertico-buffer-frame--last-preview-candidate
                (vertico-buffer-frame--candidate-key candidate))
    (vertico-buffer-frame--show-preview-content
     (vertico-buffer-frame--preview-content))))

(defun vertico-buffer-frame--show-preview-if-current (buffer candidate-key)
  "Show preview in BUFFER when CANDIDATE-KEY is still current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local vertico-buffer-frame--preview-timer nil)
      (when-let* (((not vertico-buffer-frame--exiting))
                  (candidate (vertico-buffer-frame--current-candidate))
                  ((equal candidate-key
                          (vertico-buffer-frame--candidate-key candidate))))
        (vertico-buffer-frame--show-preview)))))

(defun vertico-buffer-frame--schedule-preview ()
  "Schedule a preview update for the current Vertico candidate."
  (if (and vertico-buffer-frame-preview
           (not vertico-buffer-frame--exiting))
      (if-let* ((candidate (vertico-buffer-frame--current-candidate)))
          (unless (and (equal (vertico-buffer-frame--candidate-key candidate)
                              vertico-buffer-frame--last-preview-candidate)
                       (frame-live-p vertico-buffer-frame--preview-frame))
            (vertico-buffer-frame--cancel-preview-timer)
            (setq-local
             vertico-buffer-frame--preview-category
             (ignore-errors (vertico--metadata-get 'category))
             vertico-buffer-frame--preview-command
             this-command
             vertico-buffer-frame--preview-completion-table
             minibuffer-completion-table
             vertico-buffer-frame--preview-completing-file-name
             minibuffer-completing-file-name)
            (let ((delay (max 0 (float vertico-buffer-frame-preview-delay)))
                  (candidate-key (vertico-buffer-frame--candidate-key candidate))
                  (buffer (current-buffer)))
              (if (zerop delay)
                  (vertico-buffer-frame--show-preview)
                (setq-local
                 vertico-buffer-frame--preview-timer
                 (run-with-idle-timer
                  delay nil
                  #'vertico-buffer-frame--show-preview-if-current
                  buffer candidate-key)))))
        (vertico-buffer-frame--hide-preview))
    (vertico-buffer-frame--hide-preview)))

(defun vertico-buffer-frame--refresh-active-preview ()
  "Refresh preview for the active minibuffer, when any."
  (if-let* ((window (active-minibuffer-window))
            ((window-live-p window))
            (buffer (window-buffer window)))
      (with-current-buffer buffer
        (vertico-buffer-frame--show-preview))
    (vertico-buffer-frame--hide-preview)))

;;;###autoload
(defun vertico-buffer-frame-toggle-preview (&optional arg)
  "Toggle `vertico-buffer-frame-preview'.
With prefix ARG, enable preview if ARG is positive, otherwise disable it."
  (interactive "P")
  (setq vertico-buffer-frame-preview
        (if (null arg)
            (not vertico-buffer-frame-preview)
          (> (prefix-numeric-value arg) 0)))
  (if vertico-buffer-frame-preview
      (vertico-buffer-frame--refresh-active-preview)
    (vertico-buffer-frame--hide-preview))
  (message "vertico-buffer-frame preview %s"
           (if vertico-buffer-frame-preview "enabled" "disabled")))

(defun vertico-buffer-frame--exhibit-advice (&rest _)
  "Update preview after Vertico displays candidates."
  (when (and (bound-and-true-p vertico-buffer-frame-mode)
             (minibufferp)
             vertico-buffer-frame--candidate-frame)
    (unless vertico-buffer-frame--candidate-frame-chrome-hidden
      (vertico-buffer-frame--hide-frame-chrome
       vertico-buffer-frame--candidate-frame)
      (setq-local vertico-buffer-frame--candidate-frame-chrome-hidden t))
    (vertico-buffer-frame--schedule-preview)))

(defun vertico-buffer-frame--minibuffer-exit ()
  "Clean up child frames associated with the current minibuffer."
  (setq-local vertico-buffer-frame--exiting t)
  (vertico-buffer-frame--hide-preview))

(defun vertico-buffer-frame--setup-advice (&rest _)
  "Setup `vertico-buffer-frame' preview for the current minibuffer."
  (when (and (bound-and-true-p vertico-buffer-frame-mode)
             (minibufferp))
    (setq-local vertico-buffer-frame--exiting nil
                vertico-buffer-frame--candidate-frame-chrome-hidden nil
                vertico-buffer-frame--last-preview-candidate nil
                vertico-buffer-frame--preview-category nil
                vertico-buffer-frame--preview-command nil
                vertico-buffer-frame--preview-completion-table nil
                vertico-buffer-frame--preview-completing-file-name nil
                vertico-buffer-frame--imenu-index-buffer nil
                vertico-buffer-frame--imenu-index-cache nil)
    (add-hook 'minibuffer-exit-hook
              #'vertico-buffer-frame--minibuffer-exit
              nil 'local)))

;;;###autoload
(define-minor-mode vertico-buffer-frame-mode
  "Show `vertico-buffer-mode' using an Emacs child frame."
  :global t
  :group 'vertico-buffer-frame
  (if vertico-buffer-frame-mode
      (progn
        (unless vertico-buffer-frame--saved-display-action
          (setq vertico-buffer-frame--saved-display-action
                vertico-buffer-display-action))
        (setq vertico-buffer-display-action
              (vertico-buffer-frame-display-action))
        (advice-add #'vertico--setup
                    :after #'vertico-buffer-frame--setup-advice)
        (advice-add #'vertico--exhibit
                    :after #'vertico-buffer-frame--exhibit-advice)
        (advice-add #'load-theme
                    :after #'vertico-buffer-frame--theme-change-advice)
        (advice-add #'enable-theme
                    :after #'vertico-buffer-frame--theme-change-advice)
        (advice-add #'disable-theme
                    :after #'vertico-buffer-frame--theme-change-advice)
        (vertico-buffer-mode 1))
    (advice-remove #'vertico--setup
                   #'vertico-buffer-frame--setup-advice)
    (advice-remove #'vertico--exhibit
                   #'vertico-buffer-frame--exhibit-advice)
    (advice-remove #'load-theme
                   #'vertico-buffer-frame--theme-change-advice)
    (advice-remove #'enable-theme
                   #'vertico-buffer-frame--theme-change-advice)
    (advice-remove #'disable-theme
                   #'vertico-buffer-frame--theme-change-advice)
    (when (timerp vertico-buffer-frame--theme-timer)
      (cancel-timer vertico-buffer-frame--theme-timer)
      (setq vertico-buffer-frame--theme-timer nil))
    (vertico-buffer-frame--hide-preview)
    (when vertico-buffer-frame--saved-display-action
      (setq vertico-buffer-display-action
            vertico-buffer-frame--saved-display-action
            vertico-buffer-frame--saved-display-action nil))
    (vertico-buffer-mode -1)))

(provide 'vertico-buffer-frame)
;;; vertico-buffer-frame.el ends here
