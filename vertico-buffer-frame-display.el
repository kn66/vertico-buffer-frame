;;; vertico-buffer-frame-display.el --- Child-frame display for vertico-buffer-frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Internal child-frame display helpers for `vertico-buffer-frame'.

;;; Code:

(require 'subr-x)

(declare-function vertico-buffer-frame--active-minibuffer-buffer "vertico-buffer-frame")
(declare-function vertico-buffer-frame--ensure-minibuffer-session "vertico-buffer-frame")
(declare-function vertico-buffer-frame--minibuffer-buffer "vertico-buffer-frame")
(declare-function vertico-buffer-frame--preview-enabled-p "vertico-buffer-frame")

(defvar vertico--candidates-ov)
(defvar vertico-buffer-display-action)
(defvar vertico-buffer-frame-background-color)
(defvar vertico-buffer-frame-foreground-color)
(defvar vertico-buffer-frame-alpha)
(defvar vertico-buffer-frame-alpha-background)
(defvar vertico-buffer-frame-golden-ratio-scale)
(defvar vertico-buffer-frame-mode)
(defvar vertico-buffer-frame-preview)
(defvar vertico-buffer-frame-preview-layout)
(defvar vertico-buffer-frame-redraw-after-show)
(defvar vertico-buffer-frame-tty-cell-height-ratio)
(defvar vertico-buffer-frame--border-width)
(defvar vertico-buffer-frame--candidate-frame)
(defvar vertico-buffer-frame--candidate-frame-chrome-hidden)
(defvar vertico-buffer-frame--candidate-share-key)
(defvar vertico-buffer-frame--golden-ratio)
(defvar vertico-buffer-frame--internal-border-width)
(defvar vertico-buffer-frame--preview-frame)
(defvar vertico-buffer-frame--preview-frame-name)
(defvar vertico-buffer-frame--preview-side-by-side-gap)
(defvar vertico-buffer-frame--theme-timer)
(defvar vertico-buffer-frame--window-parameters)
(defvar x-fast-protocol-requests)
(defvar x-gtk-resize-child-frames)

(defvar vertico-buffer-frame--gtk-resize-child-frames
  (let ((case-fold-search t))
    (and (string-match-p "gtk3" system-configuration-features)
         (string-match-p "gnome\\|cinnamon"
                         (or (getenv "XDG_CURRENT_DESKTOP")
                             (getenv "DESKTOP_SESSION") ""))
         'resize-mode))
  "Value bound to `x-gtk-resize-child-frames' while displaying child frames.")

(defun vertico-buffer-frame--compact-alist (alist)
  "Return ALIST without entries whose value is nil."
  (delq nil
        (mapcar (lambda (entry)
                  (and (cdr entry) entry))
                alist)))

(defun vertico-buffer-frame--frame-minibuffer-window ()
  "Return the active minibuffer window for child frame sharing."
  (when-let* ((window (active-minibuffer-window))
              ((window-live-p window)))
    window))

(defun vertico-buffer-frame--mutable-frame-parameters (parameters)
  "Return mutable frame PARAMETERS."
  (assq-delete-all 'parent-frame
                   (assq-delete-all 'minibuffer
                                    (copy-sequence parameters))))

(defun vertico-buffer-frame--changed-frame-parameters (frame parameters)
  "Return PARAMETERS whose values differ from FRAME's current parameters."
  (let (changed)
    (dolist (parameter parameters (nreverse changed))
      (unless (equal (frame-parameter frame (car parameter)) (cdr parameter))
        (push parameter changed)))))

(defun vertico-buffer-frame--display-alist (alist parameters)
  "Return display ALIST with child-frame PARAMETERS."
  (let ((alist (assq-delete-all 'window-parameters
                                (assq-delete-all 'child-frame-parameters
                                                 (copy-sequence alist)))))
    `((child-frame-parameters . ,parameters)
      (window-parameters . ,vertico-buffer-frame--window-parameters)
      ,@alist)))

(defun vertico-buffer-frame--display-buffer-in-child-frame (buffer alist)
  "Display BUFFER in a child frame using ALIST with stable frame bindings."
  (let ((window-min-height 1)
        (window-min-width 1)
        (inhibit-redisplay t)
        (x-fast-protocol-requests t)
        (x-gtk-resize-child-frames
         vertico-buffer-frame--gtk-resize-child-frames)
        (before-make-frame-hook nil)
        (after-make-frame-functions nil))
    (display-buffer-in-child-frame buffer alist)))

(defun vertico-buffer-frame--delete-frame (frame)
  "Delete FRAME when it is live."
  (when (frame-live-p frame)
    (let ((delete-frame-functions nil))
      (delete-frame frame t))))

(defun vertico-buffer-frame--tty-child-frames-p (&optional frame)
  "Return non-nil when FRAME is a TTY frame with child-frame support."
  (and (featurep 'tty-child-frames)
       (not (display-graphic-p frame))))

(defun vertico-buffer-frame--child-frame-supported-p (&optional frame)
  "Return non-nil when child frames can be used on FRAME."
  (and (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p frame)
           (featurep 'tty-child-frames))))

(defun vertico-buffer-frame--face-color (function face &optional frame)
  "Return FACE color from FUNCTION on FRAME, or nil when unspecified."
  (let ((color (funcall function face frame t)))
    (unless (member color '("unspecified-bg" "unspecified-fg"))
      color)))

(defun vertico-buffer-frame--frame-color (parameter &optional frame)
  "Return color frame PARAMETER from FRAME, or nil when unspecified."
  (let ((color (frame-parameter (or frame (selected-frame)) parameter)))
    (unless (member color '("unspecified-bg" "unspecified-fg"))
      color)))

(defun vertico-buffer-frame--background-color (&optional frame)
  "Return the child-frame background color for FRAME."
  (or vertico-buffer-frame-background-color
      (vertico-buffer-frame--face-color #'face-background 'default frame)
      (vertico-buffer-frame--frame-color 'background-color frame)))

(defun vertico-buffer-frame--foreground-color (&optional frame)
  "Return the child-frame foreground color for FRAME."
  (or vertico-buffer-frame-foreground-color
      (vertico-buffer-frame--face-color #'face-foreground 'default frame)
      (vertico-buffer-frame--frame-color 'foreground-color frame)))

(defun vertico-buffer-frame--border-color ()
  "Return the border color for child frames."
  (or (vertico-buffer-frame--foreground-color)
      "black"))

(defun vertico-buffer-frame--decoration-frame-parameters (&optional frame
                                                                    tty-cursor)
  "Return child-frame decoration parameters for FRAME's display.
When TTY-CURSOR is non-nil, request a visible cursor in TTY child frames."
  (if (vertico-buffer-frame--tty-child-frames-p frame)
      `((internal-border-width . 0)
        (child-frame-border-width . 0)
        (undecorated . nil)
        ,@(and tty-cursor '((tty-non-selected-cursor . t))))
    `((internal-border-width . ,vertico-buffer-frame--internal-border-width)
      (child-frame-border-width . ,vertico-buffer-frame--border-width)
      (undecorated . t))))

(defun vertico-buffer-frame--opacity-frame-parameters (&optional frame)
  "Return child-frame opacity parameters for graphical FRAME."
  (when (display-graphic-p frame)
    (vertico-buffer-frame--compact-alist
     `((alpha . ,vertico-buffer-frame-alpha)
       (alpha-background . ,vertico-buffer-frame-alpha-background)))))

(defun vertico-buffer-frame--frame-parameters
    (parent name size left top share &optional tty-cursor)
  "Return child-frame parameters.
PARENT is the parent frame.  NAME is the child frame name.  SIZE is a cons
cell (WIDTH . HEIGHT).  LEFT and TOP are display-unit positions.  SHARE is the
`share-child-frame' value.  When TTY-CURSOR is non-nil, request a TTY cursor."
  (append
   (vertico-buffer-frame--compact-alist
    `((name . ,name)
      (parent-frame . ,parent)
      (width . ,(car-safe size))
      (height . ,(cdr-safe size))
      (left . ,left)
      (top . ,top)
      (border-color . ,(vertico-buffer-frame--border-color))
      (background-color . ,(vertico-buffer-frame--background-color parent))
      (foreground-color . ,(vertico-buffer-frame--foreground-color parent))
      (font . ,(frame-parameter parent 'font))
      (menu-bar-lines . 0)
      (tab-bar-lines . 0)
      (tool-bar-lines . 0)
      (no-accept-focus . t)
      (no-focus-on-map . t)
      (no-other-frame . t)
      (skip-taskbar . t)
      (unsplittable . t)
      (auto-hide-function . delete-frame)
      (minibuffer-exit . delete-frame)
      (keep-ratio . t)
      (minibuffer . ,(vertico-buffer-frame--frame-minibuffer-window))
      (share-child-frame . ,share)))
   '((fullscreen . nil)
     (cursor-type . nil)
     (no-special-glyphs . t)
     (desktop-dont-save . t)
     (inhibit-double-buffering . nil))
   (vertico-buffer-frame--opacity-frame-parameters parent)
   (vertico-buffer-frame--decoration-frame-parameters parent tty-cursor)))

(defun vertico-buffer-frame--face-remapping (parameters)
  "Return a default-face remapping from frame PARAMETERS."
  (let (spec)
    (when-let* ((background (cdr (assq 'background-color parameters))))
      (setq spec (plist-put spec :background background)))
    (when-let* ((foreground (cdr (assq 'foreground-color parameters))))
      (setq spec (plist-put spec :foreground foreground)))
    (when spec
      `((default ,spec)))))

(defun vertico-buffer-frame--round-positive (number)
  "Round NUMBER to a positive integer."
  (max 1 (round number)))

(defun vertico-buffer-frame--golden-scale ()
  "Return a positive golden-ratio scale factor."
  (max 0.1 (float vertico-buffer-frame-golden-ratio-scale)))

(defun vertico-buffer-frame--tty-cell-height-ratio ()
  "Return a positive TTY cell height-to-width ratio."
  (max 0.1 (float vertico-buffer-frame-tty-cell-height-ratio)))

(defun vertico-buffer-frame--tty-golden-size (parent)
  "Return golden-ratio candidate frame size for TTY PARENT.
The return value is a cons cell (WIDTH . HEIGHT), in character columns and
lines."
  (let* ((scale (vertico-buffer-frame--golden-scale))
         (cell-height-ratio
          (vertico-buffer-frame--tty-cell-height-ratio))
         (parent-width (frame-width parent))
         (parent-height (frame-height parent))
         (height (min parent-height
                      (* (/ (float parent-height)
                            vertico-buffer-frame--golden-ratio)
                         scale)))
         (width (* height
                   vertico-buffer-frame--golden-ratio
                   cell-height-ratio)))
    (when (> width parent-width)
      (setq width (float parent-width)
            height (/ width
                      (* vertico-buffer-frame--golden-ratio
                         cell-height-ratio))))
    (cons (vertico-buffer-frame--round-positive width)
          (vertico-buffer-frame--round-positive height))))

(defun vertico-buffer-frame--graphical-golden-size-ratio (parent)
  "Return golden-ratio child-frame size ratios for graphical PARENT."
  (let* ((scale (vertico-buffer-frame--golden-scale))
         (parent-pixel-width (max 1 (frame-pixel-width parent)))
         (parent-pixel-height (max 1 (frame-pixel-height parent)))
         (height-ratio (min 1.0 (/ scale vertico-buffer-frame--golden-ratio)))
         (width-ratio (/ (* parent-pixel-height
                            height-ratio
                            vertico-buffer-frame--golden-ratio)
                         parent-pixel-width)))
    (when (> width-ratio 1.0)
      (setq width-ratio 1.0
            height-ratio (/ (* parent-pixel-width width-ratio)
                            (* parent-pixel-height
                               vertico-buffer-frame--golden-ratio))))
    (cons width-ratio height-ratio)))

(defun vertico-buffer-frame--golden-size (parent)
  "Return golden-ratio candidate frame size for PARENT.
The return value is a cons cell (WIDTH . HEIGHT), in character columns and
lines."
  (if (vertico-buffer-frame--tty-child-frames-p parent)
      (vertico-buffer-frame--tty-golden-size parent)
    (let* ((scale (vertico-buffer-frame--golden-scale))
           (parent-pixel-width (frame-pixel-width parent))
           (parent-pixel-height (frame-pixel-height parent))
           (pixel-height (min parent-pixel-height
                              (* (/ (float parent-pixel-height)
                                    vertico-buffer-frame--golden-ratio)
                                 scale)))
           (pixel-width (* pixel-height vertico-buffer-frame--golden-ratio)))
      (when (> pixel-width parent-pixel-width)
        (setq pixel-width (float parent-pixel-width)
              pixel-height (/ pixel-width
                              vertico-buffer-frame--golden-ratio)))
      (cons (vertico-buffer-frame--round-positive
             (/ pixel-width (frame-char-width parent)))
            (vertico-buffer-frame--round-positive
              (/ pixel-height (frame-char-height parent)))))))

(defun vertico-buffer-frame--candidate-frame-size (parent)
  "Return candidate frame size parameters for PARENT."
  (if (vertico-buffer-frame--tty-child-frames-p parent)
      (vertico-buffer-frame--tty-golden-size parent)
    (vertico-buffer-frame--graphical-golden-size-ratio parent)))

(defun vertico-buffer-frame--preview-layout ()
  "Return the configured preview layout."
  (if (eq vertico-buffer-frame-preview-layout 'side-by-side)
      'side-by-side
    'overlay))

(defun vertico-buffer-frame--side-by-side-layout-p (&optional buffer)
  "Return non-nil when BUFFER should use side-by-side preview layout."
  (and (eq (vertico-buffer-frame--preview-layout) 'side-by-side)
       (vertico-buffer-frame--preview-enabled-p
        (or buffer (vertico-buffer-frame--minibuffer-buffer)))))

(defun vertico-buffer-frame--side-by-side-gap-columns (parent)
  "Return side-by-side gap columns for PARENT."
  (let ((width (max 20 (frame-width parent))))
    (min vertico-buffer-frame--preview-side-by-side-gap
         (max 0 (- width 20)))))

(defun vertico-buffer-frame--side-by-side-sizes (parent)
  "Return side-by-side candidate and preview sizes for PARENT."
  (let* ((scale (vertico-buffer-frame--golden-scale))
         (parent-width (max 20 (frame-width parent)))
         (parent-height (max 1 (frame-height parent)))
         (gap-columns (vertico-buffer-frame--side-by-side-gap-columns parent))
         (available-width (max 20 (- parent-width gap-columns)))
         (scaled-width (max 20
                            (min available-width
                                 (round (* available-width scale)))))
         (preview-width (vertico-buffer-frame--round-positive
                         (/ scaled-width
                            vertico-buffer-frame--golden-ratio)))
         (candidate-width (max 1 (- scaled-width preview-width)))
         (height (vertico-buffer-frame--round-positive
                  (min parent-height
                       (* (/ (float parent-height)
                             vertico-buffer-frame--golden-ratio)
                          scale)))))
    (list :candidate-size
          (cons candidate-width height)
          :preview-size
          (cons preview-width height)
          :gap-columns gap-columns)))

(defun vertico-buffer-frame--center-leading-margin (outer inner)
  "Return leading margin after centering INNER in OUTER."
  (let ((remaining (max 0 (- outer inner))))
    (round (/ (float remaining) 2))))

(defun vertico-buffer-frame--size-display-units (frame size)
  "Return SIZE on FRAME as a cons cell (WIDTH . HEIGHT) in display units."
  (cons (* (car size) (frame-char-width frame))
        (* (cdr size) (frame-char-height frame))))

(defun vertico-buffer-frame--candidate-position-from-size (parent size)
  "Return centered child-frame position for SIZE within PARENT.
SIZE is a cons cell (WIDTH . HEIGHT), in character columns and lines."
  (let ((units (vertico-buffer-frame--size-display-units parent size)))
    (cons (vertico-buffer-frame--center-leading-margin
           (frame-pixel-width parent)
           (car units))
          (vertico-buffer-frame--center-leading-margin
           (frame-pixel-height parent)
           (cdr units)))))

(defun vertico-buffer-frame--side-by-side-gap-display-units (parent)
  "Return side-by-side gap for PARENT in display units."
  (* (vertico-buffer-frame--side-by-side-gap-columns parent)
     (frame-char-width parent)))

(defun vertico-buffer-frame--side-by-side-top (parent total-height)
  "Return side-by-side top position in PARENT for TOTAL-HEIGHT."
  (max (frame-char-height parent)
       (round (/ (float (- (frame-pixel-height parent)
                           total-height
                           (* (frame-char-height parent) 2)))
                 2))))

(defun vertico-buffer-frame--side-by-side-layout (parent)
  "Return side-by-side layout positions and sizes for PARENT."
  (let* ((sizes (vertico-buffer-frame--side-by-side-sizes parent))
         (candidate-size (plist-get sizes :candidate-size))
         (preview-size (plist-get sizes :preview-size))
         (gap (vertico-buffer-frame--side-by-side-gap-display-units parent))
         (candidate-units (vertico-buffer-frame--size-display-units
                           parent candidate-size))
         (preview-units (vertico-buffer-frame--size-display-units
                         parent preview-size))
         (total-width (+ (car candidate-units) gap (car preview-units)))
         (total-height (max (cdr candidate-units) (cdr preview-units)))
         (left (vertico-buffer-frame--center-leading-margin
                (frame-pixel-width parent)
                total-width))
         (top (vertico-buffer-frame--side-by-side-top parent total-height)))
    (append
     sizes
     (list :candidate-left left
           :candidate-top top
           :preview-left (+ left (car candidate-units) gap)
           :preview-top top))))

(defun vertico-buffer-frame--frame-height-lines (frame)
  "Return FRAME height in lines."
  (or (frame-parameter frame 'height)
      (vertico-buffer-frame--round-positive
       (/ (float (frame-pixel-height frame))
          (frame-char-height frame)))))

(defun vertico-buffer-frame--golden-preview-top-lines (candidate-frame)
  "Return preview top inset for CANDIDATE-FRAME in lines.
The inset is the smaller segment after splitting the candidate frame height by
the golden ratio."
  (max 1
       (round (/ (float (vertico-buffer-frame--frame-height-lines
                         candidate-frame))
                 (expt vertico-buffer-frame--golden-ratio 2)))))

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

(defun vertico-buffer-frame--preview-parent (candidate-frame)
  "Return preview parent for live CANDIDATE-FRAME."
  (and (frame-live-p candidate-frame)
       (or (frame-parent candidate-frame) candidate-frame)))

(defun vertico-buffer-frame--preview-side-layout (parent)
  "Return side-by-side layout for PARENT when active."
  (and parent
       (vertico-buffer-frame--side-by-side-layout-p)
       (vertico-buffer-frame--side-by-side-layout parent)))

(defun vertico-buffer-frame--preview-size-for-frame
    (candidate-frame side-layout)
  "Return preview size for CANDIDATE-FRAME using SIDE-LAYOUT."
  (and (frame-live-p candidate-frame)
       (or (plist-get side-layout :preview-size)
           (vertico-buffer-frame--golden-preview-size candidate-frame))))

(defun vertico-buffer-frame--preview-top-offset (candidate-frame)
  "Return preview top offset for CANDIDATE-FRAME in display units."
  (* (frame-char-height candidate-frame)
     (vertico-buffer-frame--golden-preview-top-lines candidate-frame)))

(defun vertico-buffer-frame--preview-left
    (candidate-frame candidate-left preview-size)
  "Return overlay preview left position for CANDIDATE-FRAME.
CANDIDATE-LEFT is the candidate frame's left position in its parent.
PREVIEW-SIZE is the preview frame size in columns and lines."
  (max candidate-left
       (- (+ candidate-left (frame-pixel-width candidate-frame))
          (* (frame-char-width candidate-frame) (car preview-size)))))

(defun vertico-buffer-frame--preview-top (candidate-frame candidate-top)
  "Return overlay preview top position for CANDIDATE-FRAME.
CANDIDATE-TOP is the candidate frame's top position in its parent."
  (+ candidate-top
     (vertico-buffer-frame--preview-top-offset candidate-frame)))

(defun vertico-buffer-frame--current-candidate-share-key (&optional buffer)
  "Return the candidate child-frame sharing key for BUFFER."
  (let ((buffer (or buffer (vertico-buffer-frame--minibuffer-buffer))))
    (if (buffer-live-p buffer)
        (with-current-buffer
            (or (vertico-buffer-frame--ensure-minibuffer-session buffer)
                buffer)
          (or vertico-buffer-frame--candidate-share-key
              'vertico-buffer-frame))
      'vertico-buffer-frame)))

(defun vertico-buffer-frame--candidate-share-key-p (share &optional buffer)
  "Return non-nil when SHARE belongs to a candidate frame.
When BUFFER is non-nil, require SHARE to match BUFFER's session key."
  (if buffer
      (equal share (vertico-buffer-frame--current-candidate-share-key
                    buffer))
    (or (eq share 'vertico-buffer-frame)
        (and (consp share)
             (eq (car share) 'vertico-buffer-frame)))))

(defun vertico-buffer-frame--child-frame-parameters ()
  "Return frame parameters for the Vertico child frame."
  (let* ((parent (selected-frame))
         (golden-size (vertico-buffer-frame--candidate-frame-size parent))
         (side-layout (and (vertico-buffer-frame--side-by-side-layout-p)
                           (vertico-buffer-frame--side-by-side-layout
                            parent)))
         (candidate-size (or (plist-get side-layout :candidate-size)
                             golden-size))
         (position (if (and (display-graphic-p parent)
                            (not side-layout))
                       (cons 0.5 0.5)
                     (vertico-buffer-frame--candidate-position-from-size
                      parent candidate-size)))
         (left (or (plist-get side-layout :candidate-left)
                   (car position)))
         (top (or (plist-get side-layout :candidate-top)
                  (cdr position))))
    (vertico-buffer-frame--frame-parameters
     parent "vertico-buffer-frame" candidate-size left top
     (vertico-buffer-frame--current-candidate-share-key) t)))

(defun vertico-buffer-frame--preview-frame-parameters (&optional candidate-frame)
  "Return frame parameters for the preview child frame.
When CANDIDATE-FRAME is live, derive the preview size from it."
  (let* ((parent (vertico-buffer-frame--preview-parent candidate-frame))
         (side-layout (vertico-buffer-frame--preview-side-layout parent))
         (preview-size
          (vertico-buffer-frame--preview-size-for-frame
           candidate-frame side-layout))
         (candidate-left
          (and parent
               (not side-layout)
               (vertico-buffer-frame--candidate-left parent candidate-frame)))
         (candidate-top
          (and parent
               (not side-layout)
               (vertico-buffer-frame--candidate-top parent candidate-frame)))
         (preview-left
          (or (plist-get side-layout :preview-left)
              (and preview-size candidate-left
                   (vertico-buffer-frame--preview-left
                    candidate-frame candidate-left preview-size))))
         (preview-top
          (or (plist-get side-layout :preview-top)
              (and candidate-top
                   (vertico-buffer-frame--preview-top
                    candidate-frame candidate-top)))))
    (vertico-buffer-frame--frame-parameters
     parent vertico-buffer-frame--preview-frame-name preview-size
     preview-left preview-top
     'vertico-buffer-frame-preview)))

(defun vertico-buffer-frame--apply-frame-parameters (frame parameters)
  "Apply current child-frame PARAMETERS to FRAME when FRAME is live."
  (when (frame-live-p frame)
    (when-let* ((changed
                 (vertico-buffer-frame--changed-frame-parameters
                  frame
                  (vertico-buffer-frame--mutable-frame-parameters parameters))))
      (modify-frame-parameters frame changed))
    (vertico-buffer-frame--resize-frame-to-parameters frame parameters)
    (let ((border-color (vertico-buffer-frame--border-color)))
      (vertico-buffer-frame--set-face-background-if-changed
       'child-frame-border border-color frame)
      (vertico-buffer-frame--set-face-background-if-changed
       'internal-border border-color frame))))

(defun vertico-buffer-frame--set-face-background-if-changed
    (face color frame)
  "Set FACE background to COLOR on FRAME unless it is already current."
  (unless (equal (face-attribute face :background frame 'default) color)
    (set-face-background face color frame)))

(defun vertico-buffer-frame--hide-window-chrome (window)
  "Hide mode, header, and tab lines in WINDOW."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (setq-local truncate-lines t))
    (dolist (parameter vertico-buffer-frame--window-parameters)
      (set-window-parameter window (car parameter) (cdr parameter)))))

(defun vertico-buffer-frame--hide-frame-chrome (frame)
  "Hide mode, header, and tab lines in every live window of FRAME."
  (when (frame-live-p frame)
    (walk-windows #'vertico-buffer-frame--hide-window-chrome nil frame)))

(defun vertico-buffer-frame--ensure-frame-visible (frame)
  "Make child FRAME visible.
When `vertico-buffer-frame-redraw-after-show' is non-nil, also force a redraw."
  (when (frame-live-p frame)
    (let ((visible (frame-visible-p frame)))
      (unless visible
        (make-frame-visible frame))
      (when (and vertico-buffer-frame-redraw-after-show
                 (not visible))
        (redraw-frame frame)))))

(defun vertico-buffer-frame--set-frame-position (frame left top)
  "Set child FRAME position to LEFT and TOP."
  (when (frame-live-p frame)
    (if (vertico-buffer-frame--tty-child-frames-p frame)
        (modify-frame-parameters frame `((left . ,left) (top . ,top)))
      (set-frame-position frame left top))))

(defun vertico-buffer-frame--position-frame (frame)
  "Position child FRAME according to `vertico-buffer-frame' options."
  (unless (and (display-graphic-p frame)
               (not (vertico-buffer-frame--side-by-side-layout-p)))
    (let* ((parent (or (frame-parent frame) (selected-frame)))
           (side-layout (and (vertico-buffer-frame--side-by-side-layout-p)
                             (vertico-buffer-frame--side-by-side-layout
                              parent)))
           (left (or (plist-get side-layout :candidate-left)
                     (vertico-buffer-frame--candidate-left parent frame)))
           (top (or (plist-get side-layout :candidate-top)
                    (vertico-buffer-frame--candidate-top parent frame))))
      (when (and left top)
        (vertico-buffer-frame--set-frame-position frame left top)))))

(defun vertico-buffer-frame--candidate-left (parent candidate)
  "Return left display-unit position for CANDIDATE in PARENT."
  (vertico-buffer-frame--center-leading-margin
   (frame-pixel-width parent)
   (frame-pixel-width candidate)))

(defun vertico-buffer-frame--candidate-top (parent candidate)
  "Return top display-unit position for CANDIDATE in PARENT."
  (vertico-buffer-frame--center-leading-margin
   (frame-pixel-height parent)
   (frame-pixel-height candidate)))

(defun vertico-buffer-frame--position-pair (candidate-frame &optional preview-frame)
  "Position CANDIDATE-FRAME and PREVIEW-FRAME according to layout."
  (let* ((parent (or (frame-parent candidate-frame) (selected-frame)))
         (side-layout (and (vertico-buffer-frame--side-by-side-layout-p)
                           (vertico-buffer-frame--side-by-side-layout
                            parent)))
         (candidate-ratio-p (and (display-graphic-p candidate-frame)
                                 (not side-layout)))
         (top (or (plist-get side-layout :candidate-top)
                  (vertico-buffer-frame--candidate-top parent candidate-frame)))
         (left (or (plist-get side-layout :candidate-left)
                   (vertico-buffer-frame--candidate-left parent candidate-frame))))
    (when (and left top)
      (unless candidate-ratio-p
        (vertico-buffer-frame--set-frame-position candidate-frame left top))
      (when (frame-live-p preview-frame)
        (vertico-buffer-frame--set-frame-position
         preview-frame
         (if side-layout
             (+ left
                (frame-pixel-width candidate-frame)
                (vertico-buffer-frame--side-by-side-gap-display-units
                 parent))
           (max left
                (- (+ left (frame-pixel-width candidate-frame))
                   (frame-pixel-width preview-frame))))
         (if side-layout
             top
           (+ top
              (vertico-buffer-frame--preview-top-offset
               candidate-frame))))))))

(defun vertico-buffer-frame--preview-window (buffer parameters)
  "Return a preview child-frame window displaying BUFFER.
PARAMETERS are frame parameters for the preview frame."
  (when (frame-live-p vertico-buffer-frame--preview-frame)
    (vertico-buffer-frame--delete-frame
     vertico-buffer-frame--preview-frame))
  (setq vertico-buffer-frame--preview-frame nil)
  (when-let* ((window
               (vertico-buffer-frame--display-buffer-in-child-frame
                buffer
                `((inhibit-switch-frame . t)
                  (window-parameters
                   . ,vertico-buffer-frame--window-parameters)
                  (child-frame-parameters . ,parameters)))))
    (setq vertico-buffer-frame--preview-frame (window-frame window))
    window))

(defun vertico-buffer-frame--native-to-text-pixels
    (frame native-pixels horizontal)
  "Convert FRAME NATIVE-PIXELS to a text-area pixel size.
When HORIZONTAL is non-nil, convert width; otherwise convert height."
  (let* ((native-current (if horizontal
                             (frame-pixel-width frame)
                           (frame-pixel-height frame)))
         (text-current (if horizontal
                           (frame-text-width frame)
                         (frame-text-height frame)))
         (chrome (max 0 (- native-current text-current)))
         (minimum (if horizontal
                      (frame-char-width frame)
                    (frame-char-height frame))))
    (max minimum (- native-pixels chrome))))

(defun vertico-buffer-frame--size-parameter-text-pixels
    (frame parameter horizontal)
  "Return PARAMETER for FRAME as a text-area pixel size.
When HORIZONTAL is non-nil, interpret PARAMETER as width; otherwise interpret it
as height."
  (cond
   ((integerp parameter)
    (* parameter
       (if horizontal
           (frame-char-width frame)
         (frame-char-height frame))))
   ((floatp parameter)
    (when-let* ((parent (or (frame-parent frame) (selected-frame))))
      (vertico-buffer-frame--native-to-text-pixels
       frame
       (round (* parameter
                 (if horizontal
                     (frame-pixel-width parent)
                   (frame-pixel-height parent))))
       horizontal)))
   ((and (consp parameter)
         (eq (car parameter) 'text-pixels)
         (natnump (cdr parameter)))
    (cdr parameter))))

(defun vertico-buffer-frame--resize-frame-to-parameters (frame parameters)
  "Resize graphical FRAME to match width and height in PARAMETERS.
This uses `set-frame-size' with its pixelwise argument instead of temporarily
binding `frame-resize-pixelwise'."
  (when (and (frame-live-p frame)
             (display-graphic-p frame))
    (when-let* ((width-parameter (cdr (assq 'width parameters)))
                (height-parameter (cdr (assq 'height parameters)))
                (text-width
                 (vertico-buffer-frame--size-parameter-text-pixels
                  frame width-parameter t))
                (text-height
                 (vertico-buffer-frame--size-parameter-text-pixels
                  frame height-parameter nil)))
      (unless (and (= (frame-text-width frame) text-width)
                   (= (frame-text-height frame) text-height))
        (set-frame-size frame text-width text-height t)))))

(defun vertico-buffer-frame--display-buffer (buffer alist)
  "Display BUFFER in a child frame using ALIST.
This wraps `display-buffer-in-child-frame' so the reused child frame can be
repositioned after Emacs knows its actual size."
  (when (vertico-buffer-frame--child-frame-supported-p (selected-frame))
    (when-let* ((minibuffer-window (active-minibuffer-window))
                ((window-live-p minibuffer-window)))
      (vertico-buffer-frame--ensure-minibuffer-session
       (window-buffer minibuffer-window)))
    (let* ((parameters (vertico-buffer-frame--child-frame-parameters))
           (alist (vertico-buffer-frame--display-alist alist parameters)))
      (when-let* ((window
                   (vertico-buffer-frame--display-buffer-in-child-frame
                    buffer alist)))
        (let ((frame (window-frame window)))
          (vertico-buffer-frame--hide-window-chrome window)
          (vertico-buffer-frame--apply-frame-parameters
           frame parameters)
          (when-let* ((minibuffer-window (active-minibuffer-window)))
            (with-current-buffer (window-buffer minibuffer-window)
              (setq-local vertico-buffer-frame--candidate-frame
                          frame
                          vertico-buffer-frame--candidate-frame-chrome-hidden
                          t)))
          (vertico-buffer-frame--position-frame frame)
          (vertico-buffer-frame--ensure-frame-visible frame))
        window))))

(defun vertico-buffer-frame-display-action ()
  "Return a `display-buffer' action for showing Vertico in a child frame."
  `(vertico-buffer-frame--display-buffer
    (inhibit-switch-frame . t)))

(defun vertico-buffer-frame--refresh-display-action ()
  "Refresh `vertico-buffer-display-action' with current theme colors."
  (when (and (bound-and-true-p vertico-buffer-frame-mode)
             (boundp 'vertico-buffer-display-action))
    (setq vertico-buffer-display-action
          (vertico-buffer-frame-display-action))))

(defun vertico-buffer-frame--refresh-frames ()
  "Refresh live child frames when faces or themes change."
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
       vertico-buffer-frame--preview-frame))
    (when (frame-live-p candidate-frame)
      (if (frame-live-p vertico-buffer-frame--preview-frame)
          (vertico-buffer-frame--position-pair
           candidate-frame vertico-buffer-frame--preview-frame)
        (vertico-buffer-frame--position-frame candidate-frame)))))

(defun vertico-buffer-frame--theme-change-advice (&rest _)
  "Refresh child frames when theme commands update faces."
  (when (timerp vertico-buffer-frame--theme-timer)
    (cancel-timer vertico-buffer-frame--theme-timer))
  (setq vertico-buffer-frame--theme-timer
        (run-at-time 0 nil #'vertico-buffer-frame--refresh-frames)))

(defun vertico-buffer-frame--candidate-frame-p (frame &optional buffer)
  "Return non-nil when FRAME belongs to a candidate child frame.
When BUFFER is non-nil, require FRAME to belong to that minibuffer session."
  (and (frame-live-p frame)
       (frame-parent frame)
       (vertico-buffer-frame--candidate-share-key-p
        (frame-parameter frame 'share-child-frame)
        buffer)))

(defun vertico-buffer-frame--candidate-frame-from-buffer (buffer)
  "Return live candidate child frame tracked by minibuffer BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (and (frame-live-p vertico-buffer-frame--candidate-frame)
               (vertico-buffer-frame--candidate-frame-p
                vertico-buffer-frame--candidate-frame buffer)
               vertico-buffer-frame--candidate-frame)
          (when-let* (((boundp 'vertico--candidates-ov))
                      ((overlayp vertico--candidates-ov))
                      (window (overlay-get vertico--candidates-ov 'window))
                      ((window-live-p window))
                      (frame (window-frame window))
                      ((vertico-buffer-frame--candidate-frame-p
                        frame buffer)))
            (setq-local vertico-buffer-frame--candidate-frame frame)
            frame)))))

(defun vertico-buffer-frame--candidate-frame ()
  "Return the current Vertico candidate child frame."
  (or (vertico-buffer-frame--candidate-frame-from-buffer (current-buffer))
      (vertico-buffer-frame--candidate-frame-from-buffer
       (vertico-buffer-frame--active-minibuffer-buffer))))

(provide 'vertico-buffer-frame-display)
;;; vertico-buffer-frame-display.el ends here
