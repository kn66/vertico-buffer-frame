;;; vertico-buffer-frame-test.el --- Tests for vertico-buffer-frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'bookmark)
(require 'imenu)
(require 'vertico-buffer-frame)

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

(ert-deftest vertico-buffer-frame-toggle-preview-is-session-local ()
  (let ((buffer (generate-new-buffer " *vbf-toggle*"))
        (vertico-buffer-frame-preview t)
        hidden-buffer refreshed)
    (unwind-protect
        (cl-letf (((symbol-function 'vertico-buffer-frame--minibuffer-buffer)
                   (lambda () buffer))
                  ((symbol-function 'vertico-buffer-frame--live-minibuffer-buffer-p)
                   (lambda (candidate)
                     (eq candidate buffer)))
                  ((symbol-function 'vertico-buffer-frame--hide-preview)
                   (lambda (&optional candidate)
                     (setq hidden-buffer candidate)))
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
          (should-not refreshed))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-toggle-preview-updates-global-default-outside-minibuffer ()
  (let ((vertico-buffer-frame-preview nil)
        hidden-buffer refreshed)
    (cl-letf (((symbol-function 'vertico-buffer-frame--minibuffer-buffer)
               (lambda () nil))
              ((symbol-function 'vertico-buffer-frame--hide-preview)
               (lambda (&optional candidate)
                 (setq hidden-buffer candidate)))
              ((symbol-function 'vertico-buffer-frame--refresh-active-preview)
               (lambda ()
                 (setq refreshed t))))
      (vertico-buffer-frame-toggle-preview)
      (should vertico-buffer-frame-preview)
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

(ert-deftest vertico-buffer-frame-delete-candidate-frame-deletes-stale-frame ()
  (let ((deleted nil))
    (cl-letf (((symbol-function 'vertico-buffer-frame--candidate-frame-p)
               (lambda (frame) (eq frame 'candidate)))
              ((symbol-function 'vertico-buffer-frame--active-minibuffer-candidate-frame-p)
               (lambda (_frame) nil))
              ((symbol-function 'delete-frame)
               (lambda (frame &optional force)
                 (setq deleted (list frame force)))))
      (vertico-buffer-frame--delete-candidate-frame 'candidate)
      (should (equal deleted '(candidate t))))))

(ert-deftest vertico-buffer-frame-delete-candidate-frame-keeps-active-frame ()
  (let ((deleted nil))
    (cl-letf (((symbol-function 'vertico-buffer-frame--candidate-frame-p)
               (lambda (frame) (eq frame 'candidate)))
              ((symbol-function 'vertico-buffer-frame--active-minibuffer-candidate-frame-p)
               (lambda (_frame) t))
              ((symbol-function 'delete-frame)
               (lambda (&rest _)
                 (setq deleted t))))
      (vertico-buffer-frame--delete-candidate-frame 'candidate)
      (should-not deleted))))

(ert-deftest vertico-buffer-frame-show-preview-if-current-skips-stale-candidate ()
  (let ((buffer (generate-new-buffer " *vbf-stale-preview*"))
        shown)
    (unwind-protect
        (cl-letf (((symbol-function 'vertico-buffer-frame--current-candidate)
                   (lambda () "current"))
                  ((symbol-function 'vertico-buffer-frame--show-preview)
                   (lambda () (setq shown t))))
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
                   (lambda () (setq shown t))))
          (with-current-buffer buffer
            (setq-local vertico-buffer-frame--exiting nil
                        vertico-buffer-frame--preview-enabled t)
            (vertico-buffer-frame--show-preview-if-current buffer "current"))
          (should shown))
      (kill-buffer buffer))))

(ert-deftest vertico-buffer-frame-grep-skips-remote-default-directory ()
  (let ((default-directory "/ssh:example:/tmp/"))
    (cl-letf (((symbol-function 'consult--grep-position)
               (lambda (&rest _)
                 (error "consult--grep-position should not be called"))))
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
                 (error "xref-location-marker should not be called"))))
      (should-not (vertico-buffer-frame-preview-xref 'xref-item))
      (should-not marker-called))))

(provide 'vertico-buffer-frame-test)
;;; vertico-buffer-frame-test.el ends here
