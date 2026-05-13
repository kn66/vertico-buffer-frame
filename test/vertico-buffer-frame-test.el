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

(require 'ert)
(require 'cl-lib)
(require 'xref)
(require 'vertico-buffer-frame)

(defmacro vertico-buffer-frame-test--with-clean-state (&rest body)
  "Run BODY with global mode state restored afterwards."
  (declare (indent 0) (debug t))
  `(let ((old-frame-mode vertico-buffer-frame-mode)
         (old-buffer-mode (bound-and-true-p vertico-buffer-mode))
         (old-action vertico-buffer-display-action)
         (old-saved-action vertico-buffer-frame--saved-display-action)
         (old-saved-buffer-mode vertico-buffer-frame--saved-buffer-mode)
         (old-saved-state vertico-buffer-frame--saved-state))
     (unwind-protect
         (progn
           (when vertico-buffer-frame-mode
             (vertico-buffer-frame-mode -1))
           (setq vertico-buffer-frame--saved-display-action nil
                 vertico-buffer-frame--saved-buffer-mode nil
                 vertico-buffer-frame--saved-state nil)
           ,@body)
       (when vertico-buffer-frame-mode
         (vertico-buffer-frame-mode -1))
       (vertico-buffer-frame-cleanup)
       (setq vertico-buffer-display-action old-action
             vertico-buffer-frame--saved-display-action old-saved-action
             vertico-buffer-frame--saved-buffer-mode old-saved-buffer-mode
             vertico-buffer-frame--saved-state old-saved-state)
       (if old-buffer-mode
           (vertico-buffer-mode 1)
         (vertico-buffer-mode -1))
       (when old-frame-mode
         (vertico-buffer-frame-mode 1)))))

(ert-deftest vertico-buffer-frame-mode-restores-state ()
  (vertico-buffer-frame-test--with-clean-state
   (vertico-buffer-mode -1)
   (setq vertico-buffer-display-action '(display-buffer-at-bottom))
   (vertico-buffer-frame-mode 1)
   (should vertico-buffer-mode)
   (should (equal vertico-buffer-display-action
                  (vertico-buffer-frame-display-action)))
   (vertico-buffer-frame-mode -1)
   (should-not vertico-buffer-mode)
   (should (equal vertico-buffer-display-action
                  '(display-buffer-at-bottom)))))

(ert-deftest vertico-buffer-frame-display-action-is-simple ()
  (should (equal (vertico-buffer-frame-display-action)
                 '(vertico-buffer-frame--display-buffer))))

(ert-deftest vertico-buffer-frame-base-parameters-strip-frame-chrome ()
  (let ((vertico-buffer-frame-border-width 1)
        (parameters (vertico-buffer-frame--base-parameters
                     'parent "name" 80 10)))
    (should (equal (alist-get 'tab-bar-lines parameters) 0))
    (should (equal (alist-get 'menu-bar-lines parameters) 0))
    (should (equal (alist-get 'tool-bar-lines parameters) 0))
    (should (equal (alist-get 'internal-border-width parameters) 0))
    (should (equal (alist-get 'child-frame-border-width parameters) 1))
    (should (equal (alist-get 'border-width parameters) 0))
    (should (equal (alist-get 'left-fringe parameters) 0))
    (should (equal (alist-get 'right-fringe parameters) 0))
    (should (equal (alist-get 'right-divider-width parameters) 0))
    (should (equal (alist-get 'bottom-divider-width parameters) 0))))

(ert-deftest vertico-buffer-frame-make-child-frame-colors-border-with-default-foreground ()
  (let (created-parameters face-background)
    (cl-letf (((symbol-function #'make-frame)
               (lambda (parameters)
                 (setq created-parameters parameters)
                 'frame))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (frame)
                 (should (eq frame 'frame))
                 "foreground"))
              ((symbol-function #'set-face-background)
               (lambda (face color frame)
                 (setq face-background (list face color frame)))))
      (should (eq (vertico-buffer-frame--make-child-frame 'parent "name" 80 10)
                  'frame))
      (should (equal (alist-get 'child-frame-border-width created-parameters)
                     vertico-buffer-frame-border-width))
      (should (equal face-background
                     '(child-frame-border "foreground" frame))))))

(ert-deftest vertico-buffer-frame-prepare-window-strips-window-chrome ()
  (let (parameters margins fringes scroll-bars)
    (cl-letf (((symbol-function #'set-window-parameter)
               (lambda (window parameter value)
                 (push (list window parameter value) parameters)))
              ((symbol-function #'set-window-margins)
               (lambda (window left right)
                 (setq margins (list window left right))))
              ((symbol-function #'set-window-fringes)
               (lambda (window left right outside-margins)
                 (setq fringes (list window left right outside-margins))))
              ((symbol-function #'set-window-scroll-bars)
               (lambda (window width vertical-type height horizontal-type)
                 (setq scroll-bars
                       (list window width vertical-type height
                             horizontal-type)))))
      (vertico-buffer-frame--prepare-window 'window)
      (should (equal parameters
                     '((window tab-line-format none)
                       (window header-line-format none)
                       (window mode-line-format none))))
      (should (equal margins '(window 0 0)))
      (should (equal fringes '(window 0 0 nil)))
      (should (equal scroll-bars '(window nil nil nil nil))))))

(ert-deftest vertico-buffer-frame-golden-size-uses-parent-frame-pixels ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 1080)))
      (should (equal (vertico-buffer-frame--golden-pixel-size 'parent)
                     '(1080 . 667))))))

(ert-deftest vertico-buffer-frame-preview-size-follows-candidate-frame ()
  (let ((vertico-buffer-frame-preview-width nil)
        (vertico-buffer-frame-preview-height nil))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1000))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 620)))
      (should (equal (vertico-buffer-frame--preview-frame-size 'candidate)
                     '((text-pixels . 618) . (text-pixels . 383)))))))

(ert-deftest vertico-buffer-frame-preview-size-options-cap-auto-size ()
  (let ((vertico-buffer-frame-preview-width 42)
        (vertico-buffer-frame-preview-height 11))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1000))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 620))
              ((symbol-function #'frame-char-width)
               (lambda (_frame) 10))
              ((symbol-function #'frame-char-height)
               (lambda (_frame) 20)))
      (should (equal (vertico-buffer-frame--preview-frame-size 'candidate)
                     '((text-pixels . 420) . (text-pixels . 220)))))))

(ert-deftest vertico-buffer-frame-place-candidate-frame-centers-both-axes ()
  (let (position)
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (frame)
                 (pcase frame
                   ('parent 1200)
                   ('child 400))))
              ((symbol-function #'frame-pixel-height)
               (lambda (frame)
                 (pcase frame
                   ('parent 800)
                   ('child 200))))
              ((symbol-function #'set-frame-position)
               (lambda (frame left top)
                 (setq position (list frame left top)))))
      (vertico-buffer-frame--place-candidate-frame 'child 'parent)
      (should (equal position '(child 400 300))))))

(ert-deftest vertico-buffer-frame-place-preview-frame-overlays-bottom-right ()
  (let (position)
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (frame)
                 (pcase frame
                   ('parent 1000)
                   ('preview 360))))
              ((symbol-function #'frame-pixel-height)
               (lambda (frame)
                 (pcase frame
                   ('parent 620)
                   ('preview 220))))
              ((symbol-function #'set-frame-position)
               (lambda (frame left top)
                 (setq position (list frame left top)))))
      (vertico-buffer-frame--place-preview-frame 'preview 'parent)
      (should (equal position '(preview 632 392))))))

(ert-deftest vertico-buffer-frame-minibuffer-setup-strips-buffer-chrome ()
  (with-temp-buffer
    (setq-local mode-line-format '("mode")
                header-line-format '("header")
                tab-line-format '("tab"))
    (let ((vertico-buffer-frame-mode t))
      (vertico-buffer-frame--minibuffer-setup)
      (should-not mode-line-format)
      (should-not header-line-format)
      (should-not tab-line-format)
      (should (memq #'vertico-buffer-frame--preview-post-command
                    post-command-hook)))))

(ert-deftest vertico-buffer-frame-display-buffer-creates-fresh-window ()
  (with-temp-buffer
    (let ((created nil)
          (deleted nil)
          (buffers nil))
      (cl-letf (((symbol-function #'vertico-buffer-frame--parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--make-child-frame)
                 (lambda (_parent _name _width _height)
                   (let ((frame (intern (format "frame-%s"
                                                (1+ (length created))))))
                     (push frame created)
                     frame)))
                ((symbol-function #'vertico-buffer-frame--golden-frame-size)
                 (lambda (_parent)
                   '(80 . 10)))
                ((symbol-function #'frame-root-window)
                 (lambda (frame)
                   (intern (format "window-%s"
                                   (substring (symbol-name frame) 6)))))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (when frame
                     (push frame deleted))))
                ((symbol-function #'set-window-dedicated-p)
                 (lambda (&rest _args)))
                ((symbol-function #'set-window-buffer)
                 (lambda (window buffer)
                   (push (list window buffer) buffers)))
                ((symbol-function #'vertico-buffer-frame--prepare-window)
                 (lambda (_window)))
                ((symbol-function #'vertico-buffer-frame--install-cleanup)
                 (lambda ()))
                ((symbol-function #'vertico-buffer-frame--place-candidate-frame)
                 (lambda (&rest _args))))
        (should (eq (vertico-buffer-frame--display-buffer 'buffer-a nil)
                    'window-1))
        (should (eq (vertico-buffer-frame--display-buffer 'buffer-b nil)
                    'window-2))
        (should (equal created '(frame-2 frame-1)))
        (should (equal deleted '(frame-1)))
        (should (equal buffers
                       '((window-2 buffer-b)
                         (window-1 buffer-a))))))))

(ert-deftest vertico-buffer-frame-reveal-candidate-frame-after-render ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'frame
                vertico-buffer-frame--candidate-window 'window)
    (let (shown forced placed resized refreshed)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'frame-visible-p)
                 (lambda (frame)
                   (and (eq frame 'frame) nil)))
                ((symbol-function #'make-frame-visible)
                 (lambda (frame)
                   (push frame shown)))
                ((symbol-function #'force-window-update)
                 (lambda (buffer)
                   (push buffer forced)))
                ((symbol-function #'window-buffer)
                 (lambda (window)
                   (and (eq window 'window) 'buffer)))
                ((symbol-function #'vertico-buffer-frame--parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--golden-frame-size)
                 (lambda (parent)
                   (should (eq parent 'parent))
                   'size))
                ((symbol-function #'vertico-buffer-frame--resize-frame-to-size)
                 (lambda (frame size)
                   (push (list frame size) resized)))
                ((symbol-function #'vertico-buffer-frame--place-candidate-frame)
                 (lambda (frame parent)
                   (push (list frame parent) placed)))
                ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                 (lambda ()
                   (setq refreshed t))))
        (vertico-buffer-frame--reveal-candidate-frame)
        (should (equal shown '(frame)))
        (should (equal forced '(buffer)))
        (should (equal resized '((frame size))))
        (should (equal placed '((frame parent))))
        (should refreshed)))))

(ert-deftest vertico-buffer-frame-toggle-preview-updates-default ()
  (let ((vertico-buffer-frame-preview t))
    (vertico-buffer-frame-toggle-preview)
    (should-not vertico-buffer-frame-preview)
    (vertico-buffer-frame-toggle-preview 1)
    (should vertico-buffer-frame-preview)))

(ert-deftest vertico-buffer-frame-cleanup-is-buffer-local ()
  (let ((buffer-a (generate-new-buffer " *vbf-a*"))
        (buffer-b (generate-new-buffer " *vbf-b*"))
        deleted)
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--delete-frame)
                   (lambda (frame)
                     (push frame deleted))))
          (with-current-buffer buffer-a
            (setq-local vertico-buffer-frame--candidate-frame 'a-candidate
                        vertico-buffer-frame--preview-frame 'a-preview))
          (with-current-buffer buffer-b
            (setq-local vertico-buffer-frame--candidate-frame 'b-candidate
                        vertico-buffer-frame--preview-frame 'b-preview))
          (setq vertico-buffer-frame--minibuffers
                (list buffer-a buffer-b))
          (vertico-buffer-frame--cleanup-minibuffer buffer-a)
          (should (equal deleted '(a-candidate a-preview)))
          (with-current-buffer buffer-b
            (should (eq vertico-buffer-frame--candidate-frame 'b-candidate))
            (should (eq vertico-buffer-frame--preview-frame 'b-preview))))
      (setq vertico-buffer-frame--minibuffers nil)
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list buffer-a buffer-b)))))

(ert-deftest vertico-buffer-frame-cleanup-hook-only-cleans-current-minibuffer ()
  "Regression: recursive minibuffer exit must not clean outer frames."
  (let ((outer (generate-new-buffer " *vbf-outer*"))
        (inner (generate-new-buffer " *vbf-inner*"))
        (old-hook minibuffer-exit-hook)
        deleted)
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil
                vertico-buffer-frame--minibuffers nil)
          (cl-letf (((symbol-function #'vertico-buffer-frame--delete-frame)
                     (lambda (frame)
                       (when frame
                         (push frame deleted)))))
            (with-current-buffer outer
              (setq-local vertico-buffer-frame--candidate-frame
                          'outer-candidate)
              (vertico-buffer-frame--install-cleanup))
            (with-current-buffer inner
              (setq-local vertico-buffer-frame--candidate-frame
                          'inner-candidate)
              (vertico-buffer-frame--install-cleanup))
            (with-current-buffer inner
              (run-hooks 'minibuffer-exit-hook))
            (should (equal deleted '(inner-candidate)))
            (with-current-buffer outer
              (should (eq vertico-buffer-frame--candidate-frame
                          'outer-candidate)))
            (with-current-buffer outer
              (run-hooks 'minibuffer-exit-hook))
            (should (equal deleted
                           '(outer-candidate inner-candidate)))))
      (setq minibuffer-exit-hook old-hook
            vertico-buffer-frame--minibuffers nil)
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list outer inner)))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-contains-full-file ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (setq-local vertico-buffer-frame--preview-buffer nil)
          (with-temp-file file
            (insert "abcdef"))
          (let ((preview (vertico-buffer-frame--file-preview-buffer file)))
            (with-current-buffer preview
              (should (equal (buffer-string) "abcdef"))))
          (when (buffer-live-p vertico-buffer-frame--preview-buffer)
            (kill-buffer vertico-buffer-frame--preview-buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-is-not-reused ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file file
            (insert "abcdef"))
          (let ((first (vertico-buffer-frame--file-preview-buffer file))
                (second (vertico-buffer-frame--file-preview-buffer file)))
            (should-not (eq first second))
            (mapc (lambda (buffer)
                    (when (buffer-live-p buffer)
                      (kill-buffer buffer)))
                  (list first second))))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-preview-post-command-uses-delay ()
  (with-temp-buffer
    (setq-local vertico--input "x")
    (let ((vertico-buffer-frame-mode t)
          (vertico-buffer-frame-preview t)
          (vertico-buffer-frame-preview-delay 0.2)
          scheduled
          canceled)
      (cl-letf (((symbol-function #'run-with-idle-timer)
                 (lambda (delay repeat function &rest args)
                   (setq scheduled (list delay repeat function args))
                   'preview-timer))
                ((symbol-function #'timerp)
                 (lambda (timer)
                   (eq timer 'preview-timer)))
                ((symbol-function #'cancel-timer)
                 (lambda (timer)
                   (push timer canceled))))
        (vertico-buffer-frame--preview-post-command)
        (should (equal scheduled
                       (list 0.2
                             nil
                             #'vertico-buffer-frame--show-preview-later
                             (list (current-buffer)))))
        (should (eq vertico-buffer-frame--preview-timer 'preview-timer))
        (setq scheduled nil)
        (vertico-buffer-frame--preview-post-command)
        (should (equal canceled '(preview-timer)))
        (should (equal (car scheduled) 0.2))))))

(ert-deftest vertico-buffer-frame-preview-later-does-not-cache-target ()
  (with-temp-buffer
    (setq-local vertico--input "x")
    (let ((vertico-buffer-frame-mode t)
          (vertico-buffer-frame-preview t)
          shown)
      (cl-letf (((symbol-function #'vertico-buffer-frame--preview-target)
                 (lambda ()
                   '(buffer "vbf-target")))
                ((symbol-function #'vertico-buffer-frame--show-preview)
                 (lambda (target)
                   (push target shown))))
        (vertico-buffer-frame--show-preview-later (current-buffer))
        (vertico-buffer-frame--show-preview-later (current-buffer))
        (should (equal shown
                       '((buffer "vbf-target")
                         (buffer "vbf-target"))))))))

(ert-deftest vertico-buffer-frame-buffer-target-detects-existing-buffer ()
  (let ((candidate-buffer (generate-new-buffer "vbf-target"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (setq-local vertico--input t
                      vertico--metadata '(metadata (category . buffer)))
          (cl-letf (((symbol-function #'vertico--candidate)
                     (lambda (&optional _hl)
                       (buffer-name candidate-buffer))))
            (should (equal (vertico-buffer-frame--preview-target)
                           (list 'buffer (buffer-name candidate-buffer))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list candidate-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-multi-category-buffer-target-uses-real-value ()
  (let ((candidate-buffer (generate-new-buffer "vbf-target"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (setq-local vertico--input t
                      vertico--metadata '(metadata (category . multi-category)))
          (let ((candidate (propertize
                            "displayed buffer"
                            'multi-category
                            (cons 'buffer (buffer-name candidate-buffer)))))
            (cl-letf (((symbol-function #'vertico--candidate)
                       (lambda (&optional _hl)
                         candidate)))
              (should (equal (vertico-buffer-frame--preview-target)
                             (list 'buffer
                                   (buffer-name candidate-buffer)))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list candidate-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-multi-category-buffer-target-uses-buffer-object ()
  (let ((candidate-buffer (generate-new-buffer "vbf-target"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (setq-local vertico--input t
                      vertico--metadata '(metadata (category . multi-category)))
          (let ((candidate (propertize
                            "displayed buffer"
                            'multi-category
                            (cons 'buffer candidate-buffer))))
            (cl-letf (((symbol-function #'vertico--candidate)
                       (lambda (&optional _hl)
                         candidate)))
              (should (equal (vertico-buffer-frame--preview-target)
                             (list 'buffer
                                   (buffer-name candidate-buffer)))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list candidate-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-multi-category-file-target-uses-real-value ()
  (let ((file (make-temp-file "vbf-preview"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (setq-local vertico--input t
                      vertico--metadata '(metadata (category . multi-category)))
          (let ((candidate (propertize
                            "displayed file"
                            'multi-category
                            (cons 'file file))))
            (cl-letf (((symbol-function #'vertico--candidate)
                       (lambda (&optional _hl)
                         candidate)))
              (should (equal (vertico-buffer-frame--preview-target)
                             (list 'file file))))))
      (delete-file file)
      (when (buffer-live-p minibuffer)
        (kill-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-multi-category-target-allows-prefix ()
  (let ((candidate-buffer (generate-new-buffer "vbf-target"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (setq-local vertico--input t
                      vertico--metadata '(metadata (category . multi-category)))
          (let ((candidate
                 (concat "prefix "
                         (propertize
                          "displayed buffer"
                          'multi-category
                          (cons 'buffer (buffer-name candidate-buffer))))))
            (cl-letf (((symbol-function #'vertico--candidate)
                       (lambda (&optional _hl)
                         candidate)))
              (should (equal (vertico-buffer-frame--preview-target)
                             (list 'buffer
                                   (buffer-name candidate-buffer)))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list candidate-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-file-target-does-not-skip-remote-files ()
  (with-temp-buffer
    (setq-local vertico--input t
                vertico--metadata '(metadata (category . file)))
    (cl-letf (((symbol-function #'vertico--candidate)
               (lambda (&optional _hl)
                 "/ssh:example:/tmp/file"))
              ((symbol-function #'file-readable-p)
               (lambda (_file)
                 t)))
      (should (equal (vertico-buffer-frame--preview-target)
                     '(file "/ssh:example:/tmp/file"))))))

(ert-deftest vertico-buffer-frame-preview-categories-include-builtins ()
  (dolist (category '(file
                      buffer
                      project-file
                      xref-location
                      info-menu
                      symbol-help
                      calendar-month
                      unicode-name
                      environment-variable
                      bookmark
                      command
                      function
                      variable
                      face
                      symbol
                      color
                      custom-theme
                      coding-system
                      charset
                      library
                      package
                      minor-mode
                      customization-group
                      custom-variable
                      register
                      imenu
                      dabbrev
                      input-method
                      process
                      email
                      ecomplete
                      bibtex-key
                      bibtex-string
                      kill-ring
                      apropos-symbol
                      theme
                      consult-location
                      consult-xref
                      consult-grep))
    (should (memq category vertico-buffer-frame-preview-categories))))

(ert-deftest vertico-buffer-frame-consult-location-target-uses-cheap-marker ()
  (let ((source (generate-new-buffer " *vbf-consult-location-source*"))
        (minibuffer (generate-new-buffer " *vbf-consult-location-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (setq-local vertico--input t
                      vertico--metadata '(metadata (category . consult-location)))
          (let ((candidate (propertize
                            "matching line"
                            'consult-location
                            (cons (cons source 7) 1))))
            (cl-letf (((symbol-function #'vertico--candidate)
                       (lambda (&optional _hl)
                         candidate)))
              (should (equal (vertico-buffer-frame--preview-target)
                             (list 'buffer-position source 7))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-consult-location-target-allows-prefix ()
  (let ((source (generate-new-buffer " *vbf-consult-location-source*"))
        (minibuffer (generate-new-buffer " *vbf-consult-location-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (setq-local vertico--input t
                      vertico--metadata '(metadata (category . consult-location)))
          (let ((candidate (concat "prefix "
                                   (propertize
                                    "matching line"
                                    'consult-location
                                    (cons (cons source 7) 1)))))
            (cl-letf (((symbol-function #'vertico--candidate)
                       (lambda (&optional _hl)
                         candidate)))
              (should (equal (vertico-buffer-frame--preview-target)
                             (list 'buffer-position source 7))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-consult-grep-target-uses-text-properties ()
  (let ((directory (make-temp-file "vbf-consult-grep" t))
        (minibuffer (generate-new-buffer " *vbf-consult-grep-minibuffer*")))
    (unwind-protect
        (let* ((default-directory (file-name-as-directory directory))
               (file (expand-file-name "target.el" directory))
               (candidate "target.el:2:match"))
          (with-temp-file file
            (insert "line 1\nline 2\n"))
          (add-face-text-property 0 9 'consult-file t candidate)
          (add-face-text-property 10 11 'consult-line-number t candidate)
          (with-current-buffer minibuffer
            (setq-local vertico--input t
                        vertico--metadata '(metadata (category . consult-grep))
                        default-directory (file-name-as-directory directory))
            (cl-letf (((symbol-function #'vertico--candidate)
                       (lambda (&optional _hl)
                         candidate)))
              (should (equal (vertico-buffer-frame--preview-target)
                             (list 'file-line file 2))))))
      (delete-directory directory t)
      (when (buffer-live-p minibuffer)
        (kill-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-consult-xref-target-uses-xref-property ()
  (let ((directory (make-temp-file "vbf-consult-xref" t)))
    (unwind-protect
        (let* ((file (expand-file-name "target.el" directory))
               (xref (xref-make "match"
                                (xref-make-file-location file 2 0)))
               (candidate (propertize "target.el:2:match"
                                      'consult-xref xref)))
          (with-temp-file file
            (insert "line 1\nline 2\n"))
          (should (equal (vertico-buffer-frame-consult--xref-target
                          candidate)
                         (list 'file-line file 2))))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-project-file-target-resolves-readable-file ()
  (let ((directory (make-temp-file "vbf-project" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory directory)))
          (with-temp-file (expand-file-name "project-file.el" directory)
            (insert "content"))
          (should (equal (vertico-buffer-frame--project-file-target
                          "project-file.el")
                         (list 'file
                               (expand-file-name "project-file.el"
                                                 directory)))))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-xref-target-uses-candidate-location ()
  (let ((directory (make-temp-file "vbf-xref" t)))
    (unwind-protect
        (let* ((default-directory (file-name-as-directory directory))
               (file (expand-file-name "xref-target.el" directory))
               (candidate (propertize "xref-target.el:2:match"
                                      'xref--group "xref-target.el")))
          (with-temp-file file
            (insert "line 1\nline 2\n"))
          (should (equal (vertico-buffer-frame--xref-location-target
                          (substring-no-properties candidate)
                          candidate)
                         (list 'file-line file 2))))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-xref-target-allows-prefix-before-group ()
  (let ((directory (make-temp-file "vbf-xref" t)))
    (unwind-protect
        (let* ((default-directory (file-name-as-directory directory))
               (file (expand-file-name "xref-target.el" directory))
               (candidate (concat "prefix "
                                  (propertize "xref-target.el"
                                              'xref--group "xref-target.el")
                                  ":2:match")))
          (with-temp-file file
            (insert "line 1\nline 2\n"))
          (should (equal (vertico-buffer-frame--xref-location-target
                          (substring-no-properties candidate)
                          candidate)
                         (list 'file-line file 2))))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-text-preview-buffer-is-owned-and-read-only ()
  (with-temp-buffer
    (let ((preview (vertico-buffer-frame--text-preview-buffer
                    "test"
                    (lambda ()
                      (insert "preview")))))
      (unwind-protect
          (progn
            (should (eq vertico-buffer-frame--preview-buffer preview))
            (with-current-buffer preview
              (should buffer-read-only)
              (should (equal (buffer-string) "preview"))))
        (when (buffer-live-p preview)
          (kill-buffer preview))))))

(ert-deftest vertico-buffer-frame-preview-window-buffer-set-before-dedication ()
  (let (events)
    (cl-letf (((symbol-function #'set-window-dedicated-p)
               (lambda (window dedicated)
                 (push (list 'dedicated window dedicated) events)))
              ((symbol-function #'set-window-buffer)
               (lambda (window buffer)
                 (push (list 'buffer window buffer) events))))
      (vertico-buffer-frame--set-preview-window-buffer 'window 'buffer)
      (should (equal (nreverse events)
                     '((dedicated window nil)
                       (buffer window buffer)
                       (dedicated window t)))))))

(ert-deftest vertico-buffer-frame-set-window-line-centers-point ()
  (let ((buffer (generate-new-buffer " *vbf-line-preview*"))
        (window (selected-window))
        (old-buffer (window-buffer (selected-window))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (dotimes (i 40)
              (insert (format "line %d\n" (1+ i)))))
          (set-window-buffer window buffer)
          (cl-letf (((symbol-function #'window-body-height)
                     (lambda (_window)
                       11)))
            (vertico-buffer-frame--set-window-line window 20))
          (with-current-buffer buffer
            (should (= (line-number-at-pos (window-point window)) 20))
            (should (= (line-number-at-pos (window-start window)) 15))))
      (set-window-buffer window old-buffer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-set-window-position-centers-point ()
  (let ((buffer (generate-new-buffer " *vbf-position-preview*"))
        (window (selected-window))
        (old-buffer (window-buffer (selected-window)))
        marker)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (dotimes (i 40)
              (insert (format "line %d\n" (1+ i))))
            (goto-char (point-min))
            (forward-line 19)
            (setq marker (point-marker)))
          (set-window-buffer window buffer)
          (cl-letf (((symbol-function #'window-body-height)
                     (lambda (_window)
                       11)))
            (vertico-buffer-frame--set-window-position window marker))
          (with-current-buffer buffer
            (should (= (line-number-at-pos (window-point window)) 20))
            (should (= (line-number-at-pos (window-start window)) 15))))
      (when (markerp marker)
        (set-marker marker nil))
      (set-window-buffer window old-buffer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-unicode-target-builds-text-preview ()
  (let ((target (vertico-buffer-frame--unicode-name-target "LATIN CAPITAL LETTER A")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "U\\+0041" (buffer-string))))))

(ert-deftest vertico-buffer-frame-color-target-builds-swatch-preview ()
  (let ((target (vertico-buffer-frame--color-target "red")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "red" (buffer-string)))
      (should (string-match-p "#FFFF00000000" (buffer-string))))))

(ert-deftest vertico-buffer-frame-coding-system-target-builds-text-preview ()
  (let ((target (vertico-buffer-frame--coding-system-target "utf-8")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "utf-8" (buffer-string)))
      (should (string-match-p "Base:" (buffer-string))))))

(ert-deftest vertico-buffer-frame-charset-target-builds-text-preview ()
  (let ((target (vertico-buffer-frame--charset-target "unicode")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "unicode" (buffer-string))))))

(ert-deftest vertico-buffer-frame-library-target-uses-locate-library ()
  (cl-letf (((symbol-function #'locate-library)
             (lambda (library &rest _args)
               (and (equal library "simple.el")
                    "/tmp/simple.el"))))
    (should (equal (vertico-buffer-frame--library-target "simple")
                   '(file "/tmp/simple.el")))))

(ert-deftest vertico-buffer-frame-custom-theme-target-prefers-theme-file ()
  (cl-letf (((symbol-function #'custom-available-themes)
             (lambda ()
               '(wombat)))
            ((symbol-function #'locate-library)
             (lambda (library &rest _args)
               (and (equal library "wombat-theme.el")
                    "/tmp/wombat-theme.el"))))
    (should (equal (vertico-buffer-frame--custom-theme-target "wombat")
                   '(file "/tmp/wombat-theme.el")))))

(ert-deftest vertico-buffer-frame-email-target-parses-address ()
  (let ((target (vertico-buffer-frame--email-target
                 "Ada Lovelace <ada@example.invalid>")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "Ada Lovelace" (buffer-string)))
      (should (string-match-p "ada@example.invalid" (buffer-string))))))

(ert-deftest vertico-buffer-frame-imenu-target-uses-text-property ()
  (let* ((source (generate-new-buffer " *vbf-imenu-source*"))
         (minibuffer (generate-new-buffer " *vbf-imenu-minibuffer*"))
         (candidate (propertize "target"
                                'imenu-choice
                                (cons "target" 7))))
    (unwind-protect
        (cl-letf (((symbol-function #'minibuffer-selected-window)
                   (lambda ()
                     'source-window))
                  ((symbol-function #'window-live-p)
                   (lambda (window)
                     (eq window 'source-window)))
                  ((symbol-function #'window-buffer)
                   (lambda (window)
                     (and (eq window 'source-window) source))))
          (with-current-buffer minibuffer
            (should (equal (vertico-buffer-frame--imenu-target
                            "target"
                            candidate)
                           (list 'buffer-position source 7)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-consult-imenu-target-uses-consult-items ()
  (let* ((source (generate-new-buffer " *vbf-consult-imenu-source*"))
         (minibuffer (generate-new-buffer " *vbf-consult-imenu-minibuffer*"))
         (marker (with-current-buffer source
                   (insert "target")
                   (copy-marker 3))))
    (unwind-protect
        (cl-letf (((symbol-function #'minibuffer-selected-window)
                   (lambda ()
                     'source-window))
                  ((symbol-function #'window-live-p)
                   (lambda (window)
                     (eq window 'source-window)))
                  ((symbol-function #'window-buffer)
                   (lambda (window)
                     (and (eq window 'source-window) source)))
                  ((symbol-function #'consult-imenu--items)
                   (lambda ()
                     (list (cons "Functions target" marker)))))
          (with-current-buffer minibuffer
            (should (equal (vertico-buffer-frame-consult--imenu-target
                            "Functions target")
                           (list 'buffer-position source marker)))))
      (set-marker marker nil)
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-dabbrev-target-finds-origin-buffer ()
  (let ((source (generate-new-buffer " *vbf-dabbrev-source*")))
    (unwind-protect
        (cl-letf (((symbol-function #'minibuffer-selected-window)
                   (lambda ()
                     'source-window))
                  ((symbol-function #'window-live-p)
                   (lambda (window)
                     (eq window 'source-window)))
                  ((symbol-function #'window-buffer)
                   (lambda (window)
                     (and (eq window 'source-window) source))))
          (with-current-buffer source
            (insert "prefix expansion-value suffix"))
          (should (equal (vertico-buffer-frame--dabbrev-target
                          "expansion-value")
                         (list 'buffer-position source 8))))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-bibtex-target-finds-entry ()
  (let ((source (generate-new-buffer " *vbf-bibtex-source*")))
    (unwind-protect
        (cl-letf (((symbol-function #'minibuffer-selected-window)
                   (lambda ()
                     'source-window))
                  ((symbol-function #'window-live-p)
                   (lambda (window)
                     (eq window 'source-window)))
                  ((symbol-function #'window-buffer)
                   (lambda (window)
                     (and (eq window 'source-window) source))))
          (with-current-buffer source
            (insert "@Article{knuth1984,\n  title = {Literate Programming}\n}\n"))
          (should (equal (vertico-buffer-frame--bibtex-target
                          "knuth1984"
                          nil)
                         (list 'buffer-position source 1))))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-buffer-position-target-displays-buffer ()
  (let ((source (generate-new-buffer " *vbf-position-source*"))
        events)
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--preview-window)
                   (lambda ()
                     'window))
                  ((symbol-function
                    #'vertico-buffer-frame--set-preview-window-buffer)
                   (lambda (window buffer)
                     (push (list 'buffer window buffer) events)))
                  ((symbol-function #'vertico-buffer-frame--set-window-position)
                   (lambda (window position)
                     (push (list 'position window position) events)))
                  ((symbol-function #'vertico-buffer-frame--show-frame)
                   (lambda (frame)
                     (push (list 'show frame) events))))
          (setq-local vertico-buffer-frame--preview-frame 'frame)
          (vertico-buffer-frame--show-preview
           (list 'buffer-position source 3))
          (should (equal (nreverse events)
                         (list (list 'buffer 'window source)
                               '(position window 3)
                               '(show frame)))))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(provide 'vertico-buffer-frame-test)
;;; vertico-buffer-frame-test.el ends here
