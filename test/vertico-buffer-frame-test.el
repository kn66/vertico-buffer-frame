;;; vertico-buffer-frame-test.el --- Tests for vertico-buffer-frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'bookmark)
(require 'imenu)
(require 'vertico-buffer-frame)

(defvar x-fast-protocol-requests)
(defvar x-gtk-resize-child-frames)

(defmacro vertico-buffer-frame-test--with-mode-cleanup (&rest body)
  "Run BODY while preserving global Vertico display state."
  (declare (indent 0) (debug t))
  `(let ((old-vbf-mode vertico-buffer-frame-mode)
         (old-vb-mode (bound-and-true-p vertico-buffer-mode))
         (old-display-action vertico-buffer-display-action)
         (old-saved-action vertico-buffer-frame--saved-display-action)
         (old-saved-buffer-mode vertico-buffer-frame--saved-buffer-mode))
     (unwind-protect
         (progn
           (when vertico-buffer-frame-mode
             (vertico-buffer-frame-mode -1))
           (setq vertico-buffer-frame--saved-display-action nil
                 vertico-buffer-frame--saved-buffer-mode nil)
           ,@body)
       (when vertico-buffer-frame-mode
         (vertico-buffer-frame-mode -1))
       (setq vertico-buffer-display-action old-display-action
             vertico-buffer-frame--saved-display-action old-saved-action
             vertico-buffer-frame--saved-buffer-mode old-saved-buffer-mode)
       (if old-vb-mode
           (vertico-buffer-mode 1)
         (vertico-buffer-mode -1))
       (when old-vbf-mode
         (vertico-buffer-frame-mode 1)))))

(ert-deftest vertico-buffer-frame-restores-disabled-vertico-buffer-mode ()
  (vertico-buffer-frame-test--with-mode-cleanup
    (vertico-buffer-mode -1)
    (setq vertico-buffer-display-action nil)
    (vertico-buffer-frame-mode 1)
    (should vertico-buffer-mode)
    (vertico-buffer-frame-mode -1)
    (should-not vertico-buffer-mode)
    (should-not vertico-buffer-display-action)))

(ert-deftest vertico-buffer-frame-restores-enabled-vertico-buffer-mode ()
  (vertico-buffer-frame-test--with-mode-cleanup
    (vertico-buffer-mode 1)
    (setq vertico-buffer-display-action '(old-display-action))
    (vertico-buffer-frame-mode 1)
    (should vertico-buffer-mode)
    (vertico-buffer-frame-mode -1)
    (should vertico-buffer-mode)
    (should (equal vertico-buffer-display-action '(old-display-action)))))

(ert-deftest vertico-buffer-frame-supports-tty-child-frames ()
  (let ((noninteractive nil)
        (emacs-basic-display nil))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) nil))
              ((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (eq feature 'tty-child-frames))))
      (should (vertico-buffer-frame--child-frame-supported-p)))))

(ert-deftest vertico-buffer-frame-display-buffer-skips-unsupported-tty ()
  (let ((noninteractive nil)
        (emacs-basic-display nil)
        called)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) nil))
              ((symbol-function 'featurep)
               (lambda (&rest _) nil))
              ((symbol-function 'display-buffer-in-child-frame)
               (lambda (&rest _)
                 (setq called t))))
      (should-not (vertico-buffer-frame--display-buffer (current-buffer) nil))
      (should-not called))))

(ert-deftest vertico-buffer-frame-tty-golden-size-compensates-cell-aspect ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0)
        (vertico-buffer-frame-tty-cell-height-ratio 2.0))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) nil))
              ((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (eq feature 'tty-child-frames)))
              ((symbol-function 'frame-width)
               (lambda (&optional _frame) 240))
              ((symbol-function 'frame-height)
               (lambda (&optional _frame) 60)))
      (should (equal (vertico-buffer-frame--golden-size 'frame)
                     '(120 . 37))))))

(ert-deftest vertico-buffer-frame-side-by-side-layout-splits-preview-beside-candidate ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) t))
              ((symbol-function 'featurep)
               (lambda (&rest _) nil))
              ((symbol-function 'frame-width)
               (lambda (&optional _frame) 240))
              ((symbol-function 'frame-height)
               (lambda (&optional _frame) 60))
              ((symbol-function 'frame-pixel-width)
               (lambda (&optional _frame) 1920))
              ((symbol-function 'frame-pixel-height)
               (lambda (&optional _frame) 1024))
              ((symbol-function 'frame-char-width)
               (lambda (&optional _frame) 8))
              ((symbol-function 'frame-char-height)
               (lambda (&optional _frame) 16)))
      (should (equal (vertico-buffer-frame--side-by-side-layout 'frame)
                     '(:candidate-size (91 . 37)
                       :preview-size (147 . 37)
                       :gap-columns 2
                       :candidate-left 0
                       :candidate-top 200
                       :preview-left 744
                       :preview-top 200))))))

(ert-deftest vertico-buffer-frame-side-by-side-child-parameters-shrink-candidates-only-with-preview ()
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _display) t))
            ((symbol-function 'featurep)
             (lambda (&rest _) nil))
            ((symbol-function 'selected-frame)
             (lambda () 'frame))
            ((symbol-function 'frame-width)
             (lambda (&optional _frame) 240))
            ((symbol-function 'frame-height)
             (lambda (&optional _frame) 60))
            ((symbol-function 'frame-pixel-width)
             (lambda (&optional _frame) 1920))
            ((symbol-function 'frame-pixel-height)
             (lambda (&optional _frame) 1024))
            ((symbol-function 'frame-char-width)
             (lambda (&optional _frame) 8))
            ((symbol-function 'frame-char-height)
             (lambda (&optional _frame) 16))
            ((symbol-function 'face-background)
             (lambda (&rest _) nil))
            ((symbol-function 'face-foreground)
             (lambda (&rest _) nil))
            ((symbol-function 'frame-parameter)
             (lambda (&rest _) nil))
            ((symbol-function 'active-minibuffer-window)
             (lambda () nil)))
    (let ((vertico-buffer-frame-preview-layout 'side-by-side)
          (vertico-buffer-frame-preview t))
      (let ((parameters (vertico-buffer-frame--child-frame-parameters)))
        (should (= (cdr (assq 'width parameters)) 91))
        (should (= (cdr (assq 'height parameters)) 37))
        (should (= (cdr (assq 'left parameters)) 0))
        (should (= (cdr (assq 'top parameters)) 200))))
    (let ((vertico-buffer-frame-preview-layout 'side-by-side)
          (vertico-buffer-frame-preview nil))
      (let ((parameters (vertico-buffer-frame--child-frame-parameters)))
        (should (< (abs (- (cdr (assq 'width parameters))
                           (/ 1024.0 1920.0)))
                   1e-6))
        (should (< (abs (- (cdr (assq 'height parameters))
                           (/ 1.0 vertico-buffer-frame--golden-ratio)))
                   1e-6))
        (should (= (cdr (assq 'left parameters)) 0.5))
        (should (= (cdr (assq 'top parameters)) 0.5))))))

(ert-deftest vertico-buffer-frame-side-by-side-position-pair-uses-actual-candidate-width ()
  (let ((vertico-buffer-frame-preview-layout 'side-by-side)
        (vertico-buffer-frame-preview t)
        positions)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) t))
              ((symbol-function 'featurep)
               (lambda (&rest _) nil))
              ((symbol-function 'frame-live-p)
               (lambda (frame) (memq frame '(candidate preview))))
              ((symbol-function 'frame-parent)
               (lambda (frame) (and (eq frame 'candidate) 'parent)))
              ((symbol-function 'selected-frame)
               (lambda () 'parent))
              ((symbol-function 'frame-width)
               (lambda (&optional _frame) 240))
              ((symbol-function 'frame-height)
               (lambda (&optional _frame) 60))
              ((symbol-function 'frame-pixel-width)
               (lambda (&optional frame)
                 (pcase frame
                   ('candidate 640)
                   ('preview 1024)
                   (_ 1920))))
              ((symbol-function 'frame-pixel-height)
               (lambda (&optional _frame) 1024))
              ((symbol-function 'frame-char-width)
               (lambda (&optional _frame) 8))
              ((symbol-function 'frame-char-height)
               (lambda (&optional _frame) 16))
              ((symbol-function 'vertico-buffer-frame--set-frame-position)
               (lambda (frame left top)
                 (push (list frame left top) positions))))
      (vertico-buffer-frame--position-pair 'candidate 'preview)
      (should (equal (nreverse positions)
                     '((candidate 0 200)
                       (preview 656 200)))))))

(ert-deftest vertico-buffer-frame-tty-parameters-use-terminal-decorations ()
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _display) nil))
            ((symbol-function 'featurep)
             (lambda (feature &optional _subfeature)
               (eq feature 'tty-child-frames))))
    (let ((candidate-parameters (vertico-buffer-frame--child-frame-parameters))
          (preview-parameters
           (vertico-buffer-frame--preview-frame-parameters (selected-frame))))
      (should (assq 'parent-frame candidate-parameters))
      (should (integerp (cdr (assq 'left candidate-parameters))))
      (should (integerp (cdr (assq 'top candidate-parameters))))
      (should (equal (cdr (assq 'internal-border-width
                                candidate-parameters))
                     0))
      (should (equal (cdr (assq 'child-frame-border-width
                                candidate-parameters))
                     0))
      (should (assq 'undecorated candidate-parameters))
      (should-not (cdr (assq 'undecorated candidate-parameters)))
      (should (eq (cdr (assq 'no-focus-on-map candidate-parameters)) t))
      (should (eq (cdr (assq 'tty-non-selected-cursor
                             candidate-parameters))
                  t))
      (should-not (assq 'tty-non-selected-cursor preview-parameters))
      (should-not (assq 'alpha candidate-parameters))
      (should-not (assq 'alpha preview-parameters)))))

(ert-deftest vertico-buffer-frame-parameters-avoid-focus-on-map ()
  (let ((candidate-parameters
         (vertico-buffer-frame--child-frame-parameters))
        (preview-parameters
         (vertico-buffer-frame--preview-frame-parameters (selected-frame))))
    (should (eq (cdr (assq 'no-accept-focus candidate-parameters)) t))
    (should (eq (cdr (assq 'no-focus-on-map candidate-parameters)) t))
    (should (eq (cdr (assq 'no-accept-focus preview-parameters)) t))
    (should (eq (cdr (assq 'no-focus-on-map preview-parameters)) t))))

(ert-deftest vertico-buffer-frame-parameters-use-built-in-minibuffer-cleanup ()
  (let ((candidate-parameters
         (vertico-buffer-frame--child-frame-parameters))
        (preview-parameters
         (vertico-buffer-frame--preview-frame-parameters (selected-frame))))
    (should (eq (cdr (assq 'auto-hide-function candidate-parameters))
                'delete-frame))
    (should (eq (cdr (assq 'auto-hide-function preview-parameters))
                'delete-frame))
    (should (eq (cdr (assq 'minibuffer-exit candidate-parameters))
                'delete-frame))
    (should (eq (cdr (assq 'minibuffer-exit preview-parameters))
                'delete-frame))
    (should (eq (cdr (assq 'keep-ratio candidate-parameters)) t))
    (should (eq (cdr (assq 'keep-ratio preview-parameters)) t))))

(ert-deftest vertico-buffer-frame-parameters-do-not-force-alpha ()
  (let ((candidate-parameters
         (vertico-buffer-frame--child-frame-parameters))
        (preview-parameters
         (vertico-buffer-frame--preview-frame-parameters (selected-frame))))
    (should-not (assq 'alpha candidate-parameters))
    (should-not (assq 'alpha-background candidate-parameters))
    (should-not (assq 'alpha preview-parameters))
    (should-not (assq 'alpha-background preview-parameters))))

(ert-deftest vertico-buffer-frame-parameters-include-stable-frame-settings ()
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame parameter)
               (and (eq parameter 'font) "Parent Font"))))
    (let ((parameters (vertico-buffer-frame--child-frame-parameters)))
      (should (equal (cdr (assq 'font parameters)) "Parent Font"))
      (should (assq 'fullscreen parameters))
      (should-not (cdr (assq 'fullscreen parameters)))
      (should (assq 'cursor-type parameters))
      (should-not (cdr (assq 'cursor-type parameters)))
      (should (eq (cdr (assq 'no-special-glyphs parameters)) t))
      (should (eq (cdr (assq 'desktop-dont-save parameters)) t))
      (should (eq (cdr (assq 'inhibit-double-buffering parameters)) t)))))

(ert-deftest vertico-buffer-frame-ensure-frame-visible-redraws-frame ()
  (let ((vertico-buffer-frame-redraw-after-show t)
        made-visible redrawn)
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (eq frame 'frame)))
              ((symbol-function 'frame-visible-p)
               (lambda (_frame) nil))
              ((symbol-function 'make-frame-visible)
               (lambda (frame)
                 (setq made-visible frame)))
              ((symbol-function 'redraw-frame)
               (lambda (frame)
                 (setq redrawn frame))))
      (vertico-buffer-frame--ensure-frame-visible 'frame)
      (should (eq made-visible 'frame))
      (should (eq redrawn 'frame)))))

(ert-deftest vertico-buffer-frame-ensure-frame-visible-can-skip-redraw ()
  (let ((vertico-buffer-frame-redraw-after-show nil)
        made-visible redrawn)
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (eq frame 'frame)))
              ((symbol-function 'frame-visible-p)
               (lambda (_frame) nil))
              ((symbol-function 'make-frame-visible)
               (lambda (frame)
                 (setq made-visible frame)))
              ((symbol-function 'redraw-frame)
               (lambda (frame)
                 (setq redrawn frame))))
      (vertico-buffer-frame--ensure-frame-visible 'frame)
      (should (eq made-visible 'frame))
      (should-not redrawn))))

(ert-deftest vertico-buffer-frame-ensure-frame-visible-skips-visible-redraw ()
  (let ((vertico-buffer-frame-redraw-after-show t)
        made-visible redrawn)
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (eq frame 'frame)))
              ((symbol-function 'frame-visible-p)
               (lambda (_frame) t))
              ((symbol-function 'make-frame-visible)
               (lambda (frame)
                 (setq made-visible frame)))
              ((symbol-function 'redraw-frame)
               (lambda (frame)
                 (setq redrawn frame))))
      (vertico-buffer-frame--ensure-frame-visible 'frame)
      (should-not made-visible)
      (should-not redrawn))))

(ert-deftest vertico-buffer-frame-display-buffer-in-child-frame-stabilizes-bindings ()
  (let ((vertico-buffer-frame--gtk-resize-child-frames 'resize-mode)
        captured)
    (cl-letf (((symbol-function 'display-buffer-in-child-frame)
               (lambda (_buffer _alist)
                 (setq captured
                       (list window-min-height
                             window-min-width
                             inhibit-redisplay
                             before-make-frame-hook
                             after-make-frame-functions
                             x-fast-protocol-requests
                             x-gtk-resize-child-frames))
                 'window)))
      (should (eq (vertico-buffer-frame--display-buffer-in-child-frame
                   'buffer nil)
                  'window))
      (should (equal captured '(1 1 t nil nil t resize-mode))))))

(ert-deftest vertico-buffer-frame-preview-window-reuses-live-frame ()
  (let ((vertico-buffer-frame--preview-frame (selected-frame))
        set-buffer displayed)
    (cl-letf (((symbol-function 'set-window-buffer)
               (lambda (window buffer &optional _keep-margins)
                 (setq set-buffer (list window buffer))))
              ((symbol-function 'display-buffer-in-child-frame)
               (lambda (&rest _)
                 (setq displayed t))))
      (should (eq (vertico-buffer-frame--preview-window
                   'buffer '((parent-frame . nil)))
                  (frame-root-window (selected-frame))))
      (should (equal set-buffer
                     (list (frame-root-window (selected-frame)) 'buffer)))
      (should-not displayed))))

(ert-deftest vertico-buffer-frame-preview-window-recreates-for-new-parent ()
  (let ((vertico-buffer-frame--preview-frame 'old-frame)
        deleted displayed)
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (memq frame '(old-frame new-frame))))
              ((symbol-function 'frame-parent)
               (lambda (frame) (and (eq frame 'old-frame) 'old-parent)))
              ((symbol-function 'delete-frame)
               (lambda (frame &optional force)
                 (setq deleted (list frame force))))
              ((symbol-function 'display-buffer-in-child-frame)
               (lambda (buffer alist)
                 (setq displayed (list buffer alist))
                 'window))
              ((symbol-function 'window-frame)
               (lambda (window) (and (eq window 'window) 'new-frame))))
      (should (eq (vertico-buffer-frame--preview-window
                   'buffer '((parent-frame . new-parent)))
                  'window))
      (should (equal deleted '(old-frame t)))
      (should (equal displayed
                     `(buffer ((inhibit-switch-frame . t)
                               (window-parameters
                                . ,vertico-buffer-frame--window-parameters)
                               (child-frame-parameters
                                (parent-frame . new-parent))))))
      (should (eq vertico-buffer-frame--preview-frame 'new-frame)))))

(ert-deftest vertico-buffer-frame-set-frame-position-uses-parameters-on-tty ()
  (let (modified moved)
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (eq frame 'frame)))
              ((symbol-function 'display-graphic-p)
               (lambda (&optional _display) nil))
              ((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (eq feature 'tty-child-frames)))
              ((symbol-function 'modify-frame-parameters)
               (lambda (frame parameters)
                 (setq modified (list frame parameters))))
              ((symbol-function 'set-frame-position)
               (lambda (&rest _)
                 (setq moved t))))
      (vertico-buffer-frame--set-frame-position 'frame 12 3)
      (should (equal modified '(frame ((left . 12) (top . 3)))))
      (should-not moved))))

(ert-deftest vertico-buffer-frame-mutable-parameters-drop-creation-only-keys ()
  (should (equal (vertico-buffer-frame--mutable-frame-parameters
                  '((parent-frame . parent)
                    (minibuffer . window)
                    (left . 1)
                    (top . 2)))
                 '((left . 1) (top . 2)))))

(ert-deftest vertico-buffer-frame-changed-parameters-keep-only-differences ()
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame parameter)
               (pcase parameter
                 ('left 1)
                 ('top 4)
                 (_ nil)))))
    (should (equal (vertico-buffer-frame--changed-frame-parameters
                    'frame '((left . 1) (top . 2) (width . 80)))
                   '((top . 2) (width . 80))))))

(ert-deftest vertico-buffer-frame-set-face-background-skips-current-color ()
  (let (set)
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (_face _attribute _frame _inherit)
                 "gray20"))
              ((symbol-function 'set-face-background)
               (lambda (&rest args)
                 (setq set args))))
      (vertico-buffer-frame--set-face-background-if-changed
       'child-frame-border "gray20" 'frame)
      (should-not set))))

(ert-deftest vertico-buffer-frame-set-face-background-updates-changed-color ()
  (let (set)
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (_face _attribute _frame _inherit)
                 "gray20"))
              ((symbol-function 'set-face-background)
               (lambda (&rest args)
                 (setq set args))))
      (vertico-buffer-frame--set-face-background-if-changed
       'child-frame-border "gray30" 'frame)
      (should (equal set '(child-frame-border "gray30" frame))))))

(ert-deftest vertico-buffer-frame-toggle-preview-is-session-local ()
  (let ((buffer (generate-new-buffer " *vbf-toggle*"))
        (vertico-buffer-frame-preview t)
        hidden-buffer refreshed frames-refreshed)
    (unwind-protect
        (cl-letf (((symbol-function 'vertico-buffer-frame--minibuffer-buffer)
                   (lambda () buffer))
                  ((symbol-function 'vertico-buffer-frame--live-minibuffer-buffer-p)
                   (lambda (candidate)
                     (eq candidate buffer)))
                  ((symbol-function 'vertico-buffer-frame--hide-preview)
                   (lambda (&optional candidate)
                     (setq hidden-buffer candidate)))
                  ((symbol-function 'vertico-buffer-frame--refresh-frames)
                   (lambda ()
                     (setq frames-refreshed t)))
                  ((symbol-function 'vertico-buffer-frame--refresh-active-preview)
                   (lambda ()
                     (setq refreshed t))))
          (with-current-buffer buffer
            (setq-local vertico-buffer-frame--preview-enabled t))
          (vertico-buffer-frame-toggle-preview)
          (should vertico-buffer-frame-preview)
          (should-not (buffer-local-value
                       'vertico-buffer-frame--preview-enabled buffer))
          (should (eq hidden-buffer buffer))
          (should frames-refreshed)
          (should-not refreshed))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-toggle-preview-updates-global-default-outside-minibuffer ()
  (let ((vertico-buffer-frame-preview nil)
        hidden-buffer refreshed frames-refreshed)
    (cl-letf (((symbol-function 'vertico-buffer-frame--minibuffer-buffer)
               (lambda () nil))
              ((symbol-function 'vertico-buffer-frame--hide-preview)
               (lambda (&optional candidate)
                 (setq hidden-buffer candidate)))
              ((symbol-function 'vertico-buffer-frame--refresh-frames)
               (lambda ()
                 (setq frames-refreshed t)))
              ((symbol-function 'vertico-buffer-frame--refresh-active-preview)
               (lambda ()
                 (setq refreshed t))))
      (vertico-buffer-frame-toggle-preview)
      (should vertico-buffer-frame-preview)
      (should frames-refreshed)
      (should refreshed)
      (should-not hidden-buffer))))

(ert-deftest vertico-buffer-frame-directory-entries-respects-limit ()
  (let ((directory (make-temp-file "vbf-directory-" t)))
    (unwind-protect
        (let ((vertico-buffer-frame-preview-directory-max-entries 2))
          (dolist (file '("a" "b" "c"))
            (with-temp-file (expand-file-name file directory)))
          (should (equal (vertico-buffer-frame--directory-entries directory)
                         '("a" "b" "..."))))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-directory-entries-allows-unlimited ()
  (let ((directory (make-temp-file "vbf-directory-all-" t)))
    (unwind-protect
        (let ((vertico-buffer-frame-preview-directory-max-entries nil))
          (dolist (file '("a" "b" "c"))
            (with-temp-file (expand-file-name file directory)))
          (should (equal (vertico-buffer-frame--directory-entries directory)
                         '("a" "b" "c"))))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-binary-file-p-detects-nul ()
  (let ((file (make-temp-file "vbf-binary-detect-")))
    (unwind-protect
        (let ((vertico-buffer-frame-preview-binary-detect-bytes 64))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert "abc\0def")
            (let ((coding-system-for-write 'binary))
              (write-region (point-min) (point-max) file)))
          (should (vertico-buffer-frame--binary-file-p file)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-binary-file-p-passes-text ()
  (let ((file (make-temp-file "vbf-text-detect-")))
    (unwind-protect
        (let ((vertico-buffer-frame-preview-binary-detect-bytes 64))
          (with-temp-file file
            (insert "hello\n"))
          (should-not (vertico-buffer-frame--binary-file-p file)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-binary-file-p-respects-disabled ()
  (let ((file (make-temp-file "vbf-binary-disabled-")))
    (unwind-protect
        (let ((vertico-buffer-frame-preview-binary-detect-bytes nil))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert "abc\0def")
            (let ((coding-system-for-write 'binary))
              (write-region (point-min) (point-max) file)))
          (should-not (vertico-buffer-frame--binary-file-p file)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-skips-binary ()
  (let ((file (make-temp-file "vbf-binary-")))
    (unwind-protect
        (let ((vertico-buffer-frame-preview-binary-detect-bytes 64)
              (vertico-buffer-frame-preview-io-timeout nil))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert "abc\0def")
            (let ((coding-system-for-write 'binary))
              (write-region (point-min) (point-max) file)))
          (should-not (vertico-buffer-frame--file-preview file)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-skips-remote ()
  (should-not (vertico-buffer-frame--file-preview "/ssh:example:/tmp/file")))

(ert-deftest vertico-buffer-frame-file-preview-uses-file-position ()
  (let ((file (make-temp-file "vbf-position-")))
    (unwind-protect
        (let ((vertico-buffer-frame-preview-binary-detect-bytes nil)
              (vertico-buffer-frame-preview-io-timeout nil))
          (with-temp-file file
            (insert "alpha\nbeta\ngamma\n"))
          (should (string-match-p
                   "beta"
                   (vertico-buffer-frame--file-preview file 8 "title\n\n"))))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-position-preview-uses-source-buffer-for-integers ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let ((source-buffer (current-buffer)))
      (with-temp-buffer
        (should (string-match-p
                 "two"
                 (vertico-buffer-frame--position-preview
                  5 nil nil source-buffer)))))))

(ert-deftest vertico-buffer-frame-insert-preview-content-truncates-strings ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-max-size 3))
      (vertico-buffer-frame--insert-preview-content "abcdef")
      (should (equal (buffer-string) "abc"))
      (should (= (point) (point-min))))))

(ert-deftest vertico-buffer-frame-insert-preview-content-truncates-buffers ()
  (let ((source (generate-new-buffer " *vbf-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "abcdef"))
          (with-temp-buffer
            (let ((vertico-buffer-frame-preview-max-size 4))
              (vertico-buffer-frame--insert-preview-content source)
              (should (equal (buffer-string) "abcd"))
              (should (= (point) (point-min))))))
      (kill-buffer source))))

(ert-deftest vertico-buffer-frame-bookmark-previews-file-position ()
  (let ((file (make-temp-file "vbf-bookmark-file-")))
    (unwind-protect
        (let ((bookmark-alist
               `(("file-bookmark"
                  (filename . ,file)
                  (position . 8)
                  (annotation . ""))))
              (vertico-buffer-frame-preview-binary-detect-bytes nil)
              (vertico-buffer-frame-preview-io-timeout nil))
          (with-temp-file file
            (insert "alpha\nbeta\ngamma\n"))
          (should (string-match-p
                   "beta"
                   (vertico-buffer-frame-preview-bookmark "file-bookmark"))))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-bookmark-previews-directory ()
  (let ((directory (make-temp-file "vbf-bookmark-dir-" t)))
    (unwind-protect
        (let ((bookmark-alist
               `(("dir-bookmark"
                  (filename . ,directory)
                  (position . nil)
                  (annotation . ""))))
              (vertico-buffer-frame-preview-directory-max-entries 2)
              (vertico-buffer-frame-preview-io-timeout nil))
          (dolist (file '("a" "b" "c"))
            (with-temp-file (expand-file-name file directory)))
          (should (equal (vertico-buffer-frame-preview-bookmark "dir-bookmark")
                         "a\nb\n...")))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-bookmark-falls-back-to-summary ()
  (let ((bookmark-alist
         '(("non-file-bookmark"
            (location . "custom location")
            (annotation . "bookmark note")))))
    (let ((preview (vertico-buffer-frame-preview-bookmark "non-file-bookmark")))
      (should (string-match-p "non-file-bookmark" preview))
      (should (string-match-p "custom location" preview))
      (should (string-match-p "bookmark note" preview)))))

(ert-deftest vertico-buffer-frame-location-uses-consult-location-property ()
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let* ((marker (copy-marker 8))
           (candidate (propertize "beta"
                                  'consult-location (cons marker nil))))
      (should (string-match-p
               "beta"
               (vertico-buffer-frame-preview-location candidate))))))

(ert-deftest vertico-buffer-frame-location-handles-cons-candidate ()
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((marker (copy-marker 8)))
      (should (string-match-p
               "beta"
               (vertico-buffer-frame-preview-location (cons marker "ignored")))))))

(ert-deftest vertico-buffer-frame-location-returns-nil-for-bare-string ()
  (should-not (vertico-buffer-frame-preview-location "no-property")))

(ert-deftest vertico-buffer-frame-completion-category-uses-emacs-metadata ()
  (let ((minibuffer-completion-table
         (lambda (_string _predicate action)
           (when (eq action 'metadata)
             '(metadata (category . command)))))
        (minibuffer-completion-predicate nil)
        (completion-extra-properties nil)
        (vertico-buffer-frame--preview-category nil))
    (should (eq (vertico-buffer-frame--completion-category) 'command))))

(ert-deftest vertico-buffer-frame-completion-category-uses-extra-properties ()
  (let ((minibuffer-completion-table '("alpha" "beta"))
        (minibuffer-completion-predicate nil)
        (completion-extra-properties '(:category custom-category))
        (vertico-buffer-frame--preview-category nil))
    (should (eq (vertico-buffer-frame--completion-category)
                'custom-category))))

(ert-deftest vertico-buffer-frame-metadata-preview-uses-affixation-and-group ()
  (let* ((affixation
          (lambda (candidates)
            (mapcar (lambda (candidate)
                      (list candidate "[prefix] " " -- suffix"))
                    candidates)))
         (group
          (lambda (_candidate transform)
            (unless transform "Built-in group")))
         (minibuffer-completion-table
          (lambda (_string _predicate action)
            (when (eq action 'metadata)
              `(metadata
                (affixation-function . ,affixation)
                (group-function . ,group)))))
         (minibuffer-completion-predicate nil)
         (completion-extra-properties nil))
    (let ((preview (vertico-buffer-frame-preview-metadata "alpha")))
      (should (string-match-p "alpha" preview))
      (should (string-match-p "Group: Built-in group" preview))
      (should (string-match-p "\\[prefix\\].*suffix" preview)))))

(ert-deftest vertico-buffer-frame-metadata-preview-uses-extra-annotation ()
  (let ((minibuffer-completion-table '("alpha"))
        (minibuffer-completion-predicate nil)
        (completion-extra-properties
         '(:annotation-function
           (lambda (_candidate) " annotation"))))
    (should (string-match-p
             "annotation"
             (vertico-buffer-frame-preview-metadata "alpha")))))

(ert-deftest vertico-buffer-frame-command-preview-includes-signature ()
  (should (string-match-p
           "Signature: (car list)"
           (vertico-buffer-frame-preview-function "car"))))

(ert-deftest vertico-buffer-frame-environment-variable-preview-shows-value ()
  (let ((process-environment
         (cons "VBF_TEST_ENV=present" process-environment)))
    (let ((preview
           (vertico-buffer-frame-preview-environment-variable
            "VBF_TEST_ENV")))
      (should (string-match-p "VBF_TEST_ENV" preview))
      (should (string-match-p "present" preview)))))

(ert-deftest vertico-buffer-frame-unicode-name-preview-shows-code-point ()
  (let ((preview
         (vertico-buffer-frame-preview-unicode-name
          "LATIN CAPITAL LETTER A")))
    (should (string-match-p "Character: A" preview))
    (should (string-match-p "Code point: U\\+0041" preview))))

(ert-deftest vertico-buffer-frame-color-preview-shows-rgb ()
  (let ((preview (vertico-buffer-frame-preview-color "red")))
    (should (string-match-p "red" preview))
    (should (string-match-p "#FF0000" preview))))

(ert-deftest vertico-buffer-frame-captures-completion-context ()
  (let ((predicate (lambda (_candidate) t))
        seen-predicate)
    (with-temp-buffer
      (let ((minibuffer-completion-table
             (lambda (_string pred action)
               (setq seen-predicate pred)
               (when (eq action 'metadata)
                 '(metadata (category . variable)))))
            (minibuffer-completion-predicate predicate)
            (completion-extra-properties '(:annotation-function ignore))
            (minibuffer-completing-file-name t))
        (vertico-buffer-frame--capture-preview-context)
        (should (eq vertico-buffer-frame--preview-category 'variable))
        (should (eq vertico-buffer-frame--preview-completion-table
                    minibuffer-completion-table))
        (should (eq vertico-buffer-frame--preview-completion-predicate
                    predicate))
        (should (equal vertico-buffer-frame--preview-completion-extra-properties
                       completion-extra-properties))
        (should (equal vertico-buffer-frame--preview-completion-input ""))
        (should (equal vertico-buffer-frame--preview-completion-metadata
                       '(metadata (category . variable))))
        (should vertico-buffer-frame--preview-completion-context-valid)
        (should vertico-buffer-frame--preview-completing-file-name)
        (should (eq seen-predicate predicate))))))

(ert-deftest vertico-buffer-frame-uses-captured-completion-metadata ()
  (let ((calls 0)
        (minibuffer-completion-predicate nil)
        (completion-extra-properties nil))
    (with-temp-buffer
      (let ((minibuffer-completion-table
             (lambda (_string _predicate action)
               (when (eq action 'metadata)
                 (cl-incf calls)
                 '(metadata (category . command))))))
        (vertico-buffer-frame--capture-preview-context)
        (should (= calls 1))
        (should (eq (vertico-buffer-frame--completion-category) 'command))
        (should (= calls 1))))))

(ert-deftest vertico-buffer-frame-capture-preview-context-reuses-same-state ()
  (let ((calls 0)
        (minibuffer-completion-predicate nil)
        (completion-extra-properties nil))
    (with-temp-buffer
      (let ((minibuffer-completion-table
             (lambda (_string _predicate action)
               (when (eq action 'metadata)
                 (cl-incf calls)
                 '(metadata (category . command))))))
        (vertico-buffer-frame--capture-preview-context)
        (vertico-buffer-frame--capture-preview-context)
        (should (= calls 1))))))

(ert-deftest vertico-buffer-frame-capture-preview-context-recomputes-new-input ()
  (let ((calls 0))
    (with-temp-buffer
      (let ((minibuffer-completion-table
             (lambda (_string _predicate action)
               (when (eq action 'metadata)
                 (cl-incf calls)
                 '(metadata (category . command)))))
            (minibuffer-completion-predicate nil)
            (completion-extra-properties nil))
        (cl-letf (((symbol-function 'vertico-buffer-frame--completion-input)
                   (lambda () "alpha")))
          (vertico-buffer-frame--capture-preview-context))
        (cl-letf (((symbol-function 'vertico-buffer-frame--completion-input)
                   (lambda () "beta")))
          (vertico-buffer-frame--capture-preview-context))
        (should (= calls 2))))))

(ert-deftest vertico-buffer-frame-imenu-uses-completion-table ()
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((marker (copy-marker 8))
          (minibuffer-completion-table nil)
          (vertico-buffer-frame--preview-completion-table nil))
      (setq minibuffer-completion-table `(("section" . ,marker)))
      (should (string-match-p
               "beta"
               (vertico-buffer-frame-preview-imenu "section"))))))

(ert-deftest vertico-buffer-frame-imenu-finds-recursive-item ()
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((marker (copy-marker 8))
          (imenu-space-replacement "."))
      (cl-letf (((symbol-function 'imenu--subalist-p)
                 (lambda (item)
                   (and (consp item) (listp (cdr item))))))
        (let ((found (vertico-buffer-frame--imenu-find-item
                      "Parent.Section.Child.Item"
                      `(("Parent Section" ("Child Item" . ,marker))))))
          (should (equal (car found) "Child Item"))
          (should (eq (cdr found) marker)))))))

(ert-deftest vertico-buffer-frame-top-minibuffer-buffer-prunes-stale-buffers ()
  (let ((older (generate-new-buffer " *vbf-older*"))
        (newer (generate-new-buffer " *vbf-newer*"))
        (stale (generate-new-buffer " *vbf-stale*")))
    (unwind-protect
        (let ((vertico-buffer-frame--minibuffer-buffers
               (list newer stale older)))
          (cl-letf (((symbol-function
                      'vertico-buffer-frame--live-minibuffer-buffer-p)
                     (lambda (buffer)
                       (memq buffer (list newer older)))))
            (should (eq (vertico-buffer-frame--top-minibuffer-buffer)
                        newer))
            (should (equal vertico-buffer-frame--minibuffer-buffers
                           (list newer older)))))
      (mapc #'kill-buffer (list older newer stale)))))

(ert-deftest vertico-buffer-frame-hide-preview-deletes-preview-frames ()
  (let ((frame-a 'frame-a)
        (frame-b 'frame-b)
        deleted redisplayed)
    (cl-letf (((symbol-function 'vertico-buffer-frame--preview-frames)
               (lambda () (list frame-a frame-b)))
              ((symbol-function 'frame-live-p)
               (lambda (frame) (memq frame (list frame-a frame-b))))
              ((symbol-function 'delete-frame)
               (lambda (frame &optional force)
                 (push (list frame force) deleted)))
              ((symbol-function 'redisplay)
               (lambda (&optional force)
                 (setq redisplayed force))))
      (setq vertico-buffer-frame--preview-frame frame-a)
      (vertico-buffer-frame--hide-preview)
      (should (equal (sort deleted
                           (lambda (a b)
                             (string< (symbol-name (car a))
                                      (symbol-name (car b)))))
                     '((frame-a t) (frame-b t))))
      (should-not vertico-buffer-frame--preview-frame)
      (should redisplayed))))

(ert-deftest vertico-buffer-frame-hide-preview-temporarily-keeps-frame ()
  (let ((vertico-buffer-frame--preview-frame 'frame)
        hidden deleted)
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (eq frame 'frame)))
              ((symbol-function 'make-frame-invisible)
               (lambda (frame &optional force)
                 (setq hidden (list frame force))))
              ((symbol-function 'delete-frame)
               (lambda (&rest _)
                 (setq deleted t))))
      (vertico-buffer-frame--hide-preview-temporarily)
      (should (equal hidden '(frame t)))
      (should-not deleted)
      (should (eq vertico-buffer-frame--preview-frame 'frame)))))

(ert-deftest vertico-buffer-frame-show-preview-if-current-skips-stale-candidate ()
  (let ((buffer (generate-new-buffer " *vbf-stale-preview*"))
        shown)
    (unwind-protect
        (cl-letf (((symbol-function 'vertico-buffer-frame--current-candidate)
                   (lambda () "current"))
                  ((symbol-function 'vertico-buffer-frame--show-preview)
                   (lambda (&optional _captured-context)
                     (setq shown t))))
          (with-current-buffer buffer
            (setq-local vertico-buffer-frame--exiting nil
                        vertico-buffer-frame--preview-enabled t
                        vertico-buffer-frame--preview-timer 'timer)
            (vertico-buffer-frame--show-preview-if-current buffer "old")
            (should-not vertico-buffer-frame--preview-timer))
          (should-not shown))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-show-preview-if-current-shows-current-candidate ()
  (let ((buffer (generate-new-buffer " *vbf-current-preview*"))
        shown)
    (unwind-protect
        (cl-letf (((symbol-function 'vertico-buffer-frame--current-candidate)
                   (lambda () "current"))
                  ((symbol-function 'vertico-buffer-frame--show-preview)
                   (lambda (&optional captured-context)
                     (setq shown captured-context))))
          (with-current-buffer buffer
            (setq-local vertico-buffer-frame--exiting nil
                        vertico-buffer-frame--preview-enabled t)
            (vertico-buffer-frame--show-preview-if-current
             buffer "current" 'captured))
          (should (eq shown 'captured)))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-schedule-preview-skips-disabled-context ()
  (let (captured hidden)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--exiting nil
                  vertico-buffer-frame--preview-enabled nil)
      (cl-letf (((symbol-function
                  'vertico-buffer-frame--capture-preview-context)
                 (lambda () (setq captured t)))
                ((symbol-function 'vertico-buffer-frame--hide-preview)
                 (lambda (&optional _buffer) (setq hidden t))))
        (vertico-buffer-frame--schedule-preview)
        (should-not captured)
        (should hidden)))))

(ert-deftest vertico-buffer-frame-schedule-preview-captures-once-immediate ()
  (let ((captures 0)
        shown)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--exiting nil
                  vertico-buffer-frame--preview-enabled t)
      (let ((vertico-buffer-frame-preview-delay 0))
        (cl-letf (((symbol-function 'vertico-buffer-frame--current-candidate)
                   (lambda () "candidate"))
                  ((symbol-function
                    'vertico-buffer-frame--capture-preview-context)
                   (lambda () (cl-incf captures)))
                  ((symbol-function
                    'vertico-buffer-frame--preview-content-while-no-input)
                   (lambda () (cons t "content")))
                  ((symbol-function
                    'vertico-buffer-frame--show-preview-content)
                   (lambda (content) (setq shown content))))
          (vertico-buffer-frame--schedule-preview)
          (should (= captures 1))
          (should (equal shown "content"))
          (should (equal vertico-buffer-frame--last-preview-candidate
                         "candidate"))
          (should (eq vertico-buffer-frame--last-preview-state
                      'content)))))))

(ert-deftest vertico-buffer-frame-schedule-preview-reuses-empty-preview ()
  (let (captured shown)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--exiting nil
                  vertico-buffer-frame--preview-enabled t
                  vertico-buffer-frame--last-preview-candidate "candidate"
                  vertico-buffer-frame--last-preview-state 'empty)
      (cl-letf (((symbol-function 'vertico-buffer-frame--current-candidate)
                 (lambda () "candidate"))
                ((symbol-function
                  'vertico-buffer-frame--capture-preview-context)
                 (lambda () (setq captured t)))
                ((symbol-function 'vertico-buffer-frame--show-preview)
                 (lambda (&optional _captured-context) (setq shown t))))
        (vertico-buffer-frame--schedule-preview)
        (should-not captured)
        (should-not shown)))))

(ert-deftest vertico-buffer-frame-grep-skips-remote-default-directory ()
  (let ((default-directory "/ssh:example:/tmp/"))
    (cl-letf (((symbol-function 'consult--grep-position)
               (lambda (&rest _)
                 (error "Consult--grep-position should not be called"))))
      (should-not (vertico-buffer-frame-preview-grep "candidate")))))

(ert-deftest vertico-buffer-frame-xref-skips-remote-locations ()
  (let ((marker-called nil))
    (cl-letf (((symbol-function 'xref-item-location)
              (lambda (_xref) 'remote-location))
             ((symbol-function 'xref-location-group)
              (lambda (_location) "/ssh:example:/tmp/file"))
              ((symbol-function 'xref-location-marker)
               (lambda (_location)
                 (setq marker-called t)
                 (error "Xref-location-marker should not be called"))))
      (should-not (vertico-buffer-frame-preview-xref 'xref-item))
      (should-not marker-called))))

(provide 'vertico-buffer-frame-test)
;;; vertico-buffer-frame-test.el ends here
