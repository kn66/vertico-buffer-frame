;;; vertico-buffer-frame-test.el --- Tests for vertico-buffer-frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;;; Code:

(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory
                (directory-file-name
                 (file-name-directory
                  (or load-file-name
                      (bound-and-true-p byte-compile-current-file)
                      buffer-file-name))))))

(require 'cl-lib)
(require 'ert)
(require 'bookmark)
(require 'imenu)
(require 'vertico-buffer-frame)

(defvar vertico--candidates-ov)
(defvar vertico--count-ov)
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

(defun vertico-buffer-frame-test--preview-string (content)
  "Return CONTENT as a string for preview assertions."
  (cond
   ((stringp content)
    content)
   ((bufferp content)
    (with-current-buffer content
      (buffer-substring-no-properties (point-min) (point-max))))))

(defun vertico-buffer-frame-test--marker-preview-marker (content)
  "Return CONTENT's marker after asserting it is a marker preview."
  (should (vertico-buffer-frame--marker-preview-p content))
  (vertico-buffer-frame--marker-preview-marker content))

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

(ert-deftest vertico-buffer-frame-current-source-skips-recursive-minibuffer-window ()
  (let ((source (selected-window))
        (minibuffer-window 'minibuffer-window))
    (with-temp-buffer
      (setq-local vertico-buffer-frame--source-window source)
      (let ((vertico-buffer-frame--minibuffer-buffers (list (current-buffer)))
            (orig-window-live-p (symbol-function 'window-live-p))
            (orig-window-minibuffer-p (symbol-function 'window-minibuffer-p)))
        (cl-letf (((symbol-function 'minibuffer-selected-window)
                   (lambda () minibuffer-window))
                  ((symbol-function 'window-live-p)
                   (lambda (window)
                     (or (eq window minibuffer-window)
                         (funcall orig-window-live-p window))))
                  ((symbol-function 'window-minibuffer-p)
                   (lambda (window)
                     (if (eq window minibuffer-window)
                         t
                       (funcall orig-window-minibuffer-p window)))))
          (should (eq (vertico-buffer-frame--current-source-window)
                      source)))))))

(ert-deftest vertico-buffer-frame-current-source-skips-child-frame-window ()
  (let ((source (selected-window))
        (child-window 'child-window)
        (child-buffer (generate-new-buffer " *vbf-child-window*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local vertico-buffer-frame--source-window source)
          (let ((vertico-buffer-frame--minibuffer-buffers
                 (list (current-buffer)))
                (orig-window-live-p (symbol-function 'window-live-p))
                (orig-window-minibuffer-p
                 (symbol-function 'window-minibuffer-p))
                (orig-window-frame (symbol-function 'window-frame))
                (orig-window-buffer (symbol-function 'window-buffer))
                (orig-frame-parent (symbol-function 'frame-parent)))
            (cl-letf (((symbol-function 'minibuffer-selected-window)
                       (lambda () child-window))
                      ((symbol-function 'window-live-p)
                       (lambda (window)
                         (or (eq window child-window)
                             (funcall orig-window-live-p window))))
                      ((symbol-function 'window-minibuffer-p)
                       (lambda (window)
                         (and (not (eq window child-window))
                              (funcall orig-window-minibuffer-p window))))
                      ((symbol-function 'window-frame)
                       (lambda (window)
                         (if (eq window child-window)
                             'child-frame
                           (funcall orig-window-frame window))))
                      ((symbol-function 'window-buffer)
                       (lambda (window)
                         (if (eq window child-window)
                             child-buffer
                           (funcall orig-window-buffer window))))
                      ((symbol-function 'frame-parent)
                       (lambda (frame)
                         (if (eq frame 'child-frame)
                             'parent-frame
                           (funcall orig-frame-parent frame)))))
              (should (eq (vertico-buffer-frame--current-source-window)
                          source)))))
      (kill-buffer child-buffer))))

(ert-deftest vertico-buffer-frame-source-window-rejects-minibuffer-buffer ()
  (let ((window (selected-window))
        (buffer (generate-new-buffer " *vbf-minibuffer-buffer*")))
    (unwind-protect
        (let ((orig-window-buffer (symbol-function 'window-buffer))
              (orig-minibufferp (symbol-function 'minibufferp)))
          (cl-letf (((symbol-function 'window-buffer)
                     (lambda (window-arg)
                       (if (eq window-arg window)
                           buffer
                         (funcall orig-window-buffer window-arg))))
                    ((symbol-function 'minibufferp)
                     (lambda (&optional buffer-arg &rest args)
                       (if (eq (or buffer-arg (current-buffer)) buffer)
                           t
                         (apply orig-minibufferp buffer-arg args)))))
            (should-not
             (vertico-buffer-frame--live-source-window-p window))))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-embark-deferred-action-selects-source-window ()
  (let ((vertico-buffer-frame-mode t)
        (vertico-buffer-frame-embark-select-source-window t)
        captured
        called-in
        reusable-frames
        post-command-select-window)
    (let* ((source (selected-window))
           (other (split-window-right)))
      (unwind-protect
          (progn
            (select-window source)
            (cl-letf (((symbol-function 'active-minibuffer-window)
                       (lambda () 'minibuffer-window)))
              (vertico-buffer-frame--embark-run-after-command-advice
               (lambda (fn &rest args)
                 (should-not args)
                 (setq captured fn))
               (lambda ()
                 (setq called-in (selected-window)
                       reusable-frames
                       (cdr (assq 'reusable-frames
                                  (cdr display-buffer-overriding-action)))
                       post-command-select-window
                       (cdr (assq 'post-command-select-window
                                  (cdr display-buffer-overriding-action)))))))
            (select-window other)
            (funcall captured)
            (should (eq called-in source))
            (should (eq reusable-frames (window-frame source)))
            (should (eq post-command-select-window t))
            (should (eq (selected-window) source)))
        (delete-other-windows)))))

(ert-deftest vertico-buffer-frame-embark-deferred-action-can-be-disabled ()
  (let ((vertico-buffer-frame-mode t)
        (vertico-buffer-frame-embark-select-source-window nil)
        captured
        called-in)
    (let* ((source (selected-window))
           (other (split-window-right)))
      (unwind-protect
          (progn
            (select-window source)
            (cl-letf (((symbol-function 'active-minibuffer-window)
                       (lambda () 'minibuffer-window)))
              (vertico-buffer-frame--embark-run-after-command-advice
               (lambda (fn &rest args)
                 (should-not args)
                 (setq captured fn))
               (lambda ()
                 (setq called-in (selected-window)))))
            (select-window other)
            (funcall captured)
            (should (eq called-in other)))
        (delete-other-windows)))))

(ert-deftest vertico-buffer-frame-embark-deferred-action-ignores-normal-buffer ()
  (let ((vertico-buffer-frame-mode t)
        (vertico-buffer-frame-embark-select-source-window t)
        captured
        called-in)
    (let* ((source (selected-window))
           (other (split-window-right)))
      (unwind-protect
          (progn
            (select-window source)
            (vertico-buffer-frame--embark-run-after-command-advice
             (lambda (fn &rest args)
               (should-not args)
               (setq captured fn))
             (lambda ()
               (setq called-in (selected-window))))
            (select-window other)
            (funcall captured)
            (should (eq called-in other)))
        (delete-other-windows)))))

(ert-deftest vertico-buffer-frame-embark-display-buffer-uses-source-frame ()
  (let ((vertico-buffer-frame-mode t)
        (vertico-buffer-frame-embark-select-source-window t)
        captured
        (display-buffer-overriding-action '(nil . nil)))
    (let* ((source (selected-window))
           (other (split-window-right))
           (buffer (generate-new-buffer "*Embark Collect: vbf*")))
      (unwind-protect
          (progn
            (select-window other)
            (cl-letf (((symbol-function 'active-minibuffer-window)
                       (lambda () 'minibuffer-window))
                      ((symbol-function 'minibuffer-selected-window)
                       (lambda () source)))
              (vertico-buffer-frame--display-buffer-advice
               (lambda (buffer-or-name &optional action frame)
                 (setq captured
                       (list buffer-or-name
                             action
                             frame
                             (selected-window)
                             (cdr (assq
                                   'reusable-frames
                                   (cdr display-buffer-overriding-action)))
                             (cdr (assq
                                   'post-command-select-window
                                   (cdr display-buffer-overriding-action)))))
                 source)
               buffer nil nil))
            (should (equal (list (nth 0 captured)
                                 (nth 1 captured)
                                 (nth 2 captured)
                                 (nth 3 captured)
                                 (nth 4 captured)
                                 (nth 5 captured))
                           (list buffer
                                 nil
                                 (window-frame source)
                                 source
                                 (window-frame source)
                                 t)))
            (should (eq (selected-window) other)))
        (kill-buffer buffer)
        (delete-other-windows)))))

(ert-deftest vertico-buffer-frame-embark-advice-skips-when-mode-disabled ()
  (let ((vertico-buffer-frame-mode nil)
        added)
    (cl-letf (((symbol-value 'features) (cons 'embark features))
              ((symbol-function 'fboundp)
               (lambda (symbol)
                 (memq symbol
                       '(display-buffer
                         embark--run-after-command))))
              ((symbol-function 'advice-member-p)
               (lambda (&rest _) nil))
              ((symbol-function 'advice-add)
               (lambda (&rest args)
                 (push args added))))
      (vertico-buffer-frame--install-embark-advice)
      (should-not added))))

(ert-deftest vertico-buffer-frame-embark-advice-installs-after-load ()
  (let ((vertico-buffer-frame-mode t)
        added)
    (cl-letf (((symbol-value 'features) (cons 'embark features))
              ((symbol-function 'fboundp)
               (lambda (symbol)
                 (memq symbol
                       '(display-buffer
                         embark--run-after-command))))
              ((symbol-function 'advice-member-p)
               (lambda (&rest _) nil))
              ((symbol-function 'advice-add)
               (lambda (symbol where function &rest _)
                 (push (list symbol where function) added))))
      (vertico-buffer-frame--install-embark-advice)
      (should (equal (nreverse added)
                     '((display-buffer
                        :around
                        vertico-buffer-frame--display-buffer-advice)
                       (embark--run-after-command
                        :around
                        vertico-buffer-frame--embark-run-after-command-advice)))))))

(ert-deftest vertico-buffer-frame-mode-registers-vertico-buffer-advice ()
  (vertico-buffer-frame-test--with-mode-cleanup
    (vertico-buffer-frame-mode 1)
    (should (advice-member-p
             #'vertico-buffer-frame--vertico-buffer-redisplay-advice
             'vertico-buffer--redisplay))
    (vertico-buffer-frame-mode -1)
    (should-not (advice-member-p
                 #'vertico-buffer-frame--vertico-buffer-redisplay-advice
                 'vertico-buffer--redisplay))))

(ert-deftest vertico-buffer-frame-redisplay-advice-clears-deleted-overlay-window ()
  (let ((vertico-buffer-frame-mode t)
        (called nil))
    (save-window-excursion
      (let ((dead-window (split-window-right))
            (vertico--candidates-ov (make-overlay (point-min) (point-min)))
            (vertico--count-ov (make-overlay (point-min) (point-min))))
        (unwind-protect
            (progn
              (delete-window dead-window)
              (overlay-put vertico--candidates-ov 'window dead-window)
              (overlay-put vertico--count-ov 'window dead-window)
              (vertico-buffer-frame--vertico-buffer-redisplay-advice
               (lambda (&rest _)
                 (setq called t))
               (selected-window))
              (should-not called)
              (should-not (overlay-get vertico--candidates-ov 'window))
              (should-not (overlay-get vertico--count-ov 'window)))
          (delete-overlay vertico--candidates-ov)
          (delete-overlay vertico--count-ov))))))

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

(ert-deftest vertico-buffer-frame-overlay-preview-uses-parent-frame-position ()
  (let ((vertico-buffer-frame-preview-layout 'overlay))
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (memq frame '(candidate parent))))
              ((symbol-function 'frame-parent)
               (lambda (frame) (and (eq frame 'candidate) 'parent)))
              ((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'frame-parameter)
               (lambda (frame parameter)
                 (pcase (list frame parameter)
                   (`(candidate height) 37)
                   (_ nil))))
              ((symbol-function 'frame-pixel-width)
               (lambda (frame)
                 (pcase frame
                   ('parent 1920)
                   ('candidate 744)
                   (_ 1024))))
              ((symbol-function 'frame-pixel-height)
               (lambda (frame)
                 (pcase frame
                   ('parent 1024)
                   ('candidate 592)
                   (_ 1024))))
              ((symbol-function 'frame-char-width)
               (lambda (&optional _frame) 8))
              ((symbol-function 'frame-char-height)
               (lambda (&optional _frame) 16))
              ((symbol-function 'face-background)
               (lambda (&rest _) nil))
              ((symbol-function 'face-foreground)
               (lambda (&rest _) nil))
              ((symbol-function 'active-minibuffer-window)
               (lambda () nil)))
      (let* ((parameters
              (vertico-buffer-frame--preview-frame-parameters 'candidate))
             (width (cdr (assq 'width parameters)))
             (height (cdr (assq 'height parameters)))
             (left (cdr (assq 'left parameters)))
             (top (cdr (assq 'top parameters)))
             (candidate-left (/ (- 1920 744) 2))
             (candidate-top (/ (- 1024 592) 2))
             (preview-pixel-width (* width 8))
             (expected-left
              (max candidate-left
                   (- (+ candidate-left 744) preview-pixel-width)))
             (expected-top
              (+ candidate-top
                 (* 16
                    (vertico-buffer-frame--golden-preview-top-lines
                     'candidate)))))
        (should (eq (cdr (assq 'parent-frame parameters)) 'parent))
        (should (= width 57))
        (should (= height 23))
        (should (= left expected-left))
        (should (= top expected-top))))))

(ert-deftest vertico-buffer-frame-candidate-parameters-use-session-share-key ()
  (let ((buffer (generate-new-buffer " *vbf-share-key*"))
        (share-key '(vertico-buffer-frame test-session)))
    (unwind-protect
        (cl-letf (((symbol-function 'vertico-buffer-frame--minibuffer-buffer)
                   (lambda () buffer)))
          (with-current-buffer buffer
            (setq-local vertico-buffer-frame--candidate-share-key share-key))
          (should (equal (cdr (assq 'share-child-frame
                                    (vertico-buffer-frame--child-frame-parameters)))
                         share-key)))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-candidate-parameters-initialize-session-early ()
  (let ((buffer (generate-new-buffer " *vbf-early-session*")))
    (unwind-protect
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda (&rest _) t))
                  ((symbol-function 'vertico-buffer-frame--minibuffer-buffer)
                   (lambda () buffer))
                  ((symbol-function 'vertico-buffer-frame--current-source-window)
                   (lambda () (selected-window))))
          (with-current-buffer buffer
            (should-not vertico-buffer-frame--candidate-share-key))
          (let ((share-key
                 (cdr (assq 'share-child-frame
                            (vertico-buffer-frame--child-frame-parameters)))))
            (should (consp share-key))
            (should (eq (car share-key) 'vertico-buffer-frame))
            (with-current-buffer buffer
              (should (equal vertico-buffer-frame--candidate-share-key
                             share-key))
              (should vertico-buffer-frame--preview-enabled)
              (should (memq #'vertico-buffer-frame--post-command
                            post-command-hook))
              (should (memq #'vertico-buffer-frame--minibuffer-exit
                            minibuffer-exit-hook)))))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-graphical-parameters-force-opaque-alpha ()
  (let ((vertico-buffer-frame-alpha 100)
        (vertico-buffer-frame-alpha-background 100))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t)))
      (let ((candidate-parameters
             (vertico-buffer-frame--child-frame-parameters))
            (preview-parameters
             (vertico-buffer-frame--preview-frame-parameters
              (selected-frame))))
        (should (= (cdr (assq 'alpha candidate-parameters)) 100))
        (should (= (cdr (assq 'alpha-background candidate-parameters)) 100))
        (should (= (cdr (assq 'alpha preview-parameters)) 100))
        (should (= (cdr (assq 'alpha-background preview-parameters))
                   100))))))

(ert-deftest vertico-buffer-frame-alpha-can-be-left-unset ()
  (let ((vertico-buffer-frame-alpha nil)
        (vertico-buffer-frame-alpha-background nil))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t)))
      (let ((parameters (vertico-buffer-frame--child-frame-parameters)))
        (should-not (assq 'alpha parameters))
        (should-not (assq 'alpha-background parameters))))))

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
      (should-not (cdr (assq 'inhibit-double-buffering parameters))))))

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

(ert-deftest vertico-buffer-frame-preview-window-creates-fresh-frame ()
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

(ert-deftest vertico-buffer-frame-preview-frames-use-share-parameter-only ()
  (cl-letf (((symbol-function 'frame-list)
             (lambda () '(shared named tracked unparented dead)))
            ((symbol-function 'frame-live-p)
             (lambda (frame)
               (memq frame '(shared named tracked unparented))))
            ((symbol-function 'frame-parent)
             (lambda (frame)
               (and (not (eq frame 'unparented)) 'parent)))
            ((symbol-function 'frame-parameter)
             (lambda (frame parameter)
               (pcase (list frame parameter)
                 (`(shared share-child-frame)
                  'vertico-buffer-frame-preview)
                 (`(named name)
                  vertico-buffer-frame--preview-frame-name)
                 (_ nil)))))
    (let ((vertico-buffer-frame--preview-frame 'tracked))
      (should (vertico-buffer-frame--preview-frame-p 'shared))
      (should-not (vertico-buffer-frame--preview-frame-p 'named))
      (should-not (vertico-buffer-frame--preview-frame-p 'tracked))
      (should-not (vertico-buffer-frame--preview-frame-p 'unparented))
      (should (equal (vertico-buffer-frame--preview-frames)
                     '(shared))))))

(ert-deftest vertico-buffer-frame-candidate-frame-matches-session-share-key ()
  (let ((buffer (generate-new-buffer " *vbf-candidate-share*"))
        (share-key '(vertico-buffer-frame recursive-session)))
    (unwind-protect
        (cl-letf (((symbol-function 'frame-live-p)
                   (lambda (frame)
                     (memq frame '(matching legacy other))))
                  ((symbol-function 'frame-parent)
                   (lambda (_frame) 'parent))
                  ((symbol-function 'frame-parameter)
                   (lambda (frame parameter)
                     (and (eq parameter 'share-child-frame)
                          (pcase frame
                            ('matching share-key)
                            ('legacy 'vertico-buffer-frame)
                            ('other '(other-session)))))))
          (with-current-buffer buffer
            (setq-local vertico-buffer-frame--candidate-share-key share-key))
          (should (vertico-buffer-frame--candidate-frame-p 'matching))
          (should (vertico-buffer-frame--candidate-frame-p 'legacy))
          (should (vertico-buffer-frame--candidate-frame-p 'matching buffer))
          (should-not (vertico-buffer-frame--candidate-frame-p 'legacy buffer))
          (should-not (vertico-buffer-frame--candidate-frame-p 'other buffer)))
      (kill-buffer buffer))))

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

(ert-deftest vertico-buffer-frame-apply-frame-parameters-resizes-pixelwise ()
  (let (modified resized)
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (eq frame 'frame)))
              ((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'frame-parent)
               (lambda (frame) (and (eq frame 'frame) 'parent)))
              ((symbol-function 'selected-frame)
               (lambda () 'parent))
              ((symbol-function 'frame-pixel-width)
               (lambda (&optional frame)
                 (if (eq frame 'parent) 1000 400)))
              ((symbol-function 'frame-pixel-height)
               (lambda (&optional frame)
                 (if (eq frame 'parent) 800 300)))
              ((symbol-function 'frame-text-width)
               (lambda (&optional _frame) 382))
              ((symbol-function 'frame-text-height)
               (lambda (&optional _frame) 282))
              ((symbol-function 'frame-char-width)
               (lambda (&optional _frame) 8))
              ((symbol-function 'frame-char-height)
               (lambda (&optional _frame) 16))
              ((symbol-function 'frame-parameter)
               (lambda (&rest _) nil))
              ((symbol-function 'modify-frame-parameters)
               (lambda (frame parameters)
                 (setq modified (list frame parameters))))
              ((symbol-function 'face-foreground)
               (lambda (&rest _) "black"))
              ((symbol-function 'face-attribute)
               (lambda (&rest _) "black"))
              ((symbol-function 'set-face-background)
               (lambda (&rest _) nil))
              ((symbol-function 'set-frame-size)
               (lambda (frame width height pixelwise)
                 (setq resized (list frame width height pixelwise)))))
      (vertico-buffer-frame--apply-frame-parameters
       'frame '((width . 0.5) (height . 0.25)))
      (should (equal modified
                     '(frame ((width . 0.5) (height . 0.25)))))
      (should (equal resized '(frame 482 182 t))))))

(ert-deftest vertico-buffer-frame-size-parameters-use-text-pixel-units ()
  (cl-letf (((symbol-function 'frame-char-width)
             (lambda (&optional _frame) 8))
            ((symbol-function 'frame-char-height)
             (lambda (&optional _frame) 16)))
    (should (= (vertico-buffer-frame--size-parameter-text-pixels
                'frame 80 t)
               640))
    (should (= (vertico-buffer-frame--size-parameter-text-pixels
                'frame 20 nil)
               320))
    (should (= (vertico-buffer-frame--size-parameter-text-pixels
                'frame '(text-pixels . 512) t)
               512))))

(ert-deftest vertico-buffer-frame-apply-frame-parameters-skips-current-size ()
  (let (resized)
    (cl-letf (((symbol-function 'frame-live-p)
               (lambda (frame) (eq frame 'frame)))
              ((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'frame-pixel-width)
               (lambda (&optional _frame) 400))
              ((symbol-function 'frame-pixel-height)
               (lambda (&optional _frame) 300))
              ((symbol-function 'frame-text-width)
               (lambda (&optional _frame) 382))
              ((symbol-function 'frame-text-height)
               (lambda (&optional _frame) 282))
              ((symbol-function 'frame-char-width)
               (lambda (&optional _frame) 8))
              ((symbol-function 'frame-char-height)
               (lambda (&optional _frame) 16))
              ((symbol-function 'frame-parameter)
               (lambda (&rest _) nil))
              ((symbol-function 'modify-frame-parameters)
               (lambda (&rest _) nil))
              ((symbol-function 'face-foreground)
               (lambda (&rest _) "black"))
              ((symbol-function 'face-attribute)
               (lambda (&rest _) "black"))
              ((symbol-function 'set-face-background)
               (lambda (&rest _) nil))
              ((symbol-function 'set-frame-size)
               (lambda (&rest _)
                 (setq resized t))))
      (vertico-buffer-frame--apply-frame-parameters
       'frame '((width . (text-pixels . 382))
                (height . (text-pixels . 282))))
      (should-not resized))))

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
          (let ((marker
                 (vertico-buffer-frame-test--marker-preview-marker
                  (vertico-buffer-frame--file-preview file 8 "title\n\n"))))
            (should (= (marker-position marker) 8))
            (with-current-buffer (marker-buffer marker)
              (save-excursion
                (goto-char marker)
                (should (string-match-p
                         "beta"
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))))))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-position-preview-uses-source-buffer-for-integers ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let ((source-buffer (current-buffer)))
      (with-temp-buffer
        (let ((marker
               (vertico-buffer-frame-test--marker-preview-marker
                (vertico-buffer-frame--position-preview
                 5 nil nil source-buffer))))
          (should (eq (marker-buffer marker) source-buffer))
          (should (= (marker-position marker) 5)))))))

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
          (let ((marker
                 (vertico-buffer-frame-test--marker-preview-marker
                  (vertico-buffer-frame-preview-bookmark "file-bookmark"))))
            (should (= (marker-position marker) 8))))
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
      (should (= (marker-position
                  (vertico-buffer-frame-test--marker-preview-marker
                   (vertico-buffer-frame-preview-location candidate)))
                 8)))))

(ert-deftest vertico-buffer-frame-location-handles-cons-candidate ()
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((marker (copy-marker 8)))
      (should (= (marker-position
                  (vertico-buffer-frame-test--marker-preview-marker
                   (vertico-buffer-frame-preview-location
                    (cons marker "ignored"))))
                 8)))))

(ert-deftest vertico-buffer-frame-location-returns-nil-for-bare-string ()
  (should-not (vertico-buffer-frame-preview-location "no-property")))

(ert-deftest vertico-buffer-frame-info-menu-preview-uses-target-node ()
  (require 'info)
  (let ((clone-buffer nil))
    (unwind-protect
        (with-temp-buffer
          (setq-local Info-current-file "emacs"
                      Info-current-node "Top")
          (cl-letf (((symbol-function 'Info-extract-menu-item)
                     (lambda (menu-item)
                       (should (equal menu-item "Intro"))
                       "Introduction"))
                    ((symbol-function 'clone-buffer)
                     (lambda (&optional _newname _display-flag &rest _)
                       (setq clone-buffer
                             (generate-new-buffer " *vbf-info-clone*"))
                       clone-buffer))
                    ((symbol-function 'Info-goto-node)
                     (lambda (target &rest _)
                       (should (equal target "Introduction"))
                       (insert "Target node body."))))
            (let ((preview (vertico-buffer-frame-preview-info-menu "Intro")))
              (should (string-match-p "Top -> Introduction" preview))
              (should (string-match-p "Target node body" preview)))
            (should-not (buffer-live-p clone-buffer))))
      (when (buffer-live-p clone-buffer)
        (kill-buffer clone-buffer)))))

(ert-deftest vertico-buffer-frame-completion-category-uses-emacs-metadata ()
  (let ((minibuffer-completion-table
         (lambda (_string _predicate action)
           (when (eq action 'metadata)
             '(metadata (category . command)))))
        (minibuffer-completion-predicate nil)
        (completion-extra-properties nil))
    (should (eq (vertico-buffer-frame--completion-category) 'command))))

(ert-deftest vertico-buffer-frame-completion-category-uses-extra-properties ()
  (let ((minibuffer-completion-table '("alpha" "beta"))
        (minibuffer-completion-predicate nil)
        (completion-extra-properties '(:category custom-category)))
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
           "(car LIST)"
           (vertico-buffer-frame-test--preview-string
            (vertico-buffer-frame-preview-function "car")))))

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

(ert-deftest vertico-buffer-frame-calendar-month-preview-shows-month-details ()
  (let ((preview (vertico-buffer-frame-preview-calendar-month "February")))
    (should (string-match-p "Month number: 2" preview))
    (should (string-match-p "Days: 28 or 29" preview))))

(ert-deftest vertico-buffer-frame-custom-group-preview-shows-documentation ()
  (let ((symbol 'vertico-buffer-frame-test-group))
    (unwind-protect
        (progn
          (put symbol 'group-documentation "Group documentation.")
          (put symbol 'custom-group
               '((vertico-buffer-frame-test-option custom-variable)
                 (vertico-buffer-frame-test-face custom-face)))
          (let ((preview
                 (vertico-buffer-frame-preview-custom-group
                  "vertico-buffer-frame-test-group")))
            (should (string-match-p "Group documentation" preview))
            (should (string-match-p "Options: .*test-option" preview))
            (should (string-match-p "Faces: .*test-face" preview))))
      (dolist (property '(group-documentation custom-group custom-loads
                          custom-autoload))
        (put symbol property nil)))))

(ert-deftest vertico-buffer-frame-customize-group-command-uses-custom-preview ()
  (let ((symbol 'vertico-buffer-frame-test-command-group)
        (this-command 'customize-group))
    (unwind-protect
        (progn
          (put symbol 'group-documentation "Command group documentation.")
          (put symbol 'custom-group
               '((vertico-buffer-frame-test-command-option custom-variable)))
          (let ((preview
                 (vertico-buffer-frame-preview-default
                  "vertico-buffer-frame-test-command-group")))
            (should (string-match-p "Command group documentation" preview))))
      (dolist (property '(group-documentation custom-group custom-loads
                          custom-autoload))
        (put symbol property nil)))))

(ert-deftest vertico-buffer-frame-command-preview-uses-captured-command ()
  (let ((symbol 'vertico-buffer-frame-test-captured-command-group)
        (this-command 'self-insert-command))
    (unwind-protect
        (with-temp-buffer
          (setq-local vertico-buffer-frame--completion-command 'customize-group)
          (put symbol 'group-documentation "Captured command documentation.")
          (put symbol 'custom-group
               '((vertico-buffer-frame-test-captured-command-option
                  custom-variable)))
          (let ((preview
                 (vertico-buffer-frame-preview-default
                  "vertico-buffer-frame-test-captured-command-group")))
            (should (string-match-p "Captured command documentation" preview))))
      (dolist (property '(group-documentation custom-group custom-loads
                          custom-autoload))
        (put symbol property nil)))))

(ert-deftest vertico-buffer-frame-completion-metadata-uses-current-state ()
  (let ((calls 0)
        (minibuffer-completion-predicate nil)
        (completion-extra-properties nil))
    (with-temp-buffer
      (let ((minibuffer-completion-table
             (lambda (_string _predicate action)
               (when (eq action 'metadata)
                 (cl-incf calls)
                 '(metadata (category . command))))))
        (should (eq (vertico-buffer-frame--completion-category) 'command))
        (should (= calls 1))))))

(ert-deftest vertico-buffer-frame-imenu-uses-completion-table ()
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((marker (copy-marker 8))
          (minibuffer-completion-table nil))
      (setq minibuffer-completion-table `(("section" . ,marker)))
      (should (= (marker-position
                  (vertico-buffer-frame-test--marker-preview-marker
                   (vertico-buffer-frame-preview-imenu "section")))
                 8)))))

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

(ert-deftest vertico-buffer-frame-recursive-exit-cleans-current-session-only ()
  (let ((parent (generate-new-buffer " *vbf-parent*"))
        (child (generate-new-buffer " *vbf-child*"))
        (parent-key '(vertico-buffer-frame parent-session))
        (child-key '(vertico-buffer-frame child-session))
        deleted hidden refreshed)
    (unwind-protect
        (let ((vertico-buffer-frame--minibuffer-buffers
               (list child parent)))
          (with-current-buffer parent
            (setq-local vertico-buffer-frame--candidate-share-key parent-key
                        vertico-buffer-frame--candidate-frame 'parent-frame))
          (with-current-buffer child
            (setq-local vertico-buffer-frame--candidate-share-key child-key
                        vertico-buffer-frame--candidate-frame 'child-frame))
          (cl-letf (((symbol-function
                      'vertico-buffer-frame--live-minibuffer-buffer-p)
                     (lambda (buffer)
                       (eq buffer parent)))
                    ((symbol-function 'vertico-buffer-frame--hide-preview)
                     (lambda (&optional buffer)
                       (setq hidden buffer)))
                    ((symbol-function
                      'vertico-buffer-frame--refresh-minibuffer-preview)
                     (lambda (&optional buffer)
                       (setq refreshed buffer)))
                    ((symbol-function 'delete-frame)
                     (lambda (frame &optional force)
                       (push (list frame force) deleted)))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat function &rest args)
                       (apply function args))))
            (with-current-buffer child
              (vertico-buffer-frame--minibuffer-exit))
            (should-not deleted)
            (should (eq hidden child))
            (should (eq refreshed parent))
            (should (equal vertico-buffer-frame--minibuffer-buffers
                           (list parent)))
            (with-current-buffer parent
              (should (eq vertico-buffer-frame--candidate-frame
                          'parent-frame)))))
      (mapc #'kill-buffer (list parent child)))))

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

(ert-deftest vertico-buffer-frame-show-preview-later-shows-current-candidate ()
  (let ((buffer (generate-new-buffer " *vbf-current-preview*"))
        shown)
    (unwind-protect
        (cl-letf (((symbol-function 'vertico-buffer-frame--current-candidate)
                   (lambda () "current"))
                  ((symbol-function 'vertico-buffer-frame--show-preview)
                   (lambda ()
                     (setq shown t))))
          (with-current-buffer buffer
            (setq-local vertico-buffer-frame--exiting nil
                        vertico-buffer-frame--preview-enabled t)
            (vertico-buffer-frame--show-preview-later buffer))
          (should shown))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-schedule-preview-hides-disabled-context ()
  (let (hidden)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--exiting nil
                  vertico-buffer-frame--preview-enabled nil)
      (cl-letf (((symbol-function 'vertico-buffer-frame--hide-preview)
                 (lambda (&optional _buffer) (setq hidden t))))
        (vertico-buffer-frame--schedule-preview)
        (should hidden)))))

(ert-deftest vertico-buffer-frame-schedule-preview-shows-immediate ()
  (let (shown)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--exiting nil
                  vertico-buffer-frame--preview-enabled t)
      (let ((vertico-buffer-frame-preview-delay 0))
        (cl-letf (((symbol-function 'vertico-buffer-frame--current-candidate)
                   (lambda () "candidate"))
                  ((symbol-function
                    'vertico-buffer-frame--preview-content-while-no-input)
                   (lambda () (cons t "content")))
                  ((symbol-function
                    'vertico-buffer-frame--show-preview-content)
                   (lambda (content) (setq shown content))))
          (vertico-buffer-frame--schedule-preview)
          (should (equal shown "content")))))))

(ert-deftest vertico-buffer-frame-schedule-preview-runs-each-time ()
  (let ((shown 0))
    (with-temp-buffer
      (setq-local vertico-buffer-frame--exiting nil
                  vertico-buffer-frame--preview-enabled t)
      (let ((vertico-buffer-frame-preview-delay 0))
        (cl-letf (((symbol-function 'vertico-buffer-frame--current-candidate)
                   (lambda () "candidate"))
                  ((symbol-function 'vertico-buffer-frame--show-preview)
                   (lambda () (cl-incf shown))))
          (vertico-buffer-frame--schedule-preview)
          (vertico-buffer-frame--schedule-preview)
          (should (= shown 2)))))))

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
