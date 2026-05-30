;;; vertico-buffer-frame-test.el --- Tests for vertico-buffer-frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: Nobuyuki Kamimoto
;; Assisted-by: OpenAI Codex:GPT-5
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

;;; Code:

(eval-and-compile
  (defconst vertico-buffer-frame-test--root
    (file-name-directory
     (directory-file-name
      (file-name-directory
       (or load-file-name
           (bound-and-true-p byte-compile-current-file)
           buffer-file-name)))))
  (add-to-list 'load-path vertico-buffer-frame-test--root))

(eval-and-compile
  (require 'ert)
  (require 'cl-lib)
  (require 'xref)
  (load (expand-file-name "vertico-buffer-frame-preview.el"
                          vertico-buffer-frame-test--root)
        nil t)
  (load (expand-file-name "vertico-buffer-frame.el"
                          vertico-buffer-frame-test--root)
        nil t)
  (load (expand-file-name "vertico-buffer-frame-consult.el"
                          vertico-buffer-frame-test--root)
        nil t))

(defvar consult-imenu-config)
(defvar Info-complete-menu-buffer)

(declare-function calendar-generate-month "calendar")
(declare-function calendar-make-alist "calendar")
(declare-function bookmark-get-bookmark "bookmark")
(declare-function bookmark-get-filename "bookmark")
(declare-function bookmark-get-position "bookmark")

(defmacro vertico-buffer-frame-test--with-clean-state (&rest body)
  "Run BODY with global mode state restored afterwards."
  (declare (indent 0) (debug t))
  `(let ((old-frame-mode vertico-buffer-frame-mode)
         (old-buffer-mode (bound-and-true-p vertico-buffer-mode))
         (old-action vertico-buffer-display-action)
         (old-saved-action vertico-buffer-frame--saved-display-action)
         (old-saved-buffer-mode vertico-buffer-frame--saved-buffer-mode)
         (old-saved-embark-present
          vertico-buffer-frame--saved-embark-display-action-present)
         (old-saved-state vertico-buffer-frame--saved-state)
         (old-display-buffer-alist display-buffer-alist)
         (old-minibuffer-setup-hook minibuffer-setup-hook)
         (old-minibuffer-exit-hook minibuffer-exit-hook))
     (unwind-protect
         (progn
           (when vertico-buffer-frame-mode
             (vertico-buffer-frame-mode -1))
           (setq vertico-buffer-frame--saved-display-action nil
                 vertico-buffer-frame--saved-buffer-mode nil
                 vertico-buffer-frame--saved-embark-display-action-present
                 nil
                 vertico-buffer-frame--saved-state nil)
           ,@body)
       (when vertico-buffer-frame-mode
         (vertico-buffer-frame-mode -1))
       (vertico-buffer-frame-cleanup)
       (setq vertico-buffer-display-action old-action
             vertico-buffer-frame--saved-display-action old-saved-action
             vertico-buffer-frame--saved-buffer-mode old-saved-buffer-mode
             vertico-buffer-frame--saved-embark-display-action-present
             old-saved-embark-present
             vertico-buffer-frame--saved-state old-saved-state
             display-buffer-alist old-display-buffer-alist
             minibuffer-setup-hook old-minibuffer-setup-hook
             minibuffer-exit-hook old-minibuffer-exit-hook)
       (if old-buffer-mode
           (vertico-buffer-mode 1)
         (vertico-buffer-mode -1))
       (when old-frame-mode
         (vertico-buffer-frame-mode 1)))))

;;;; Mode lifecycle and display action state

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

(ert-deftest vertico-buffer-frame-mode-preserves-display-action-change ()
  (vertico-buffer-frame-test--with-clean-state
   (vertico-buffer-mode -1)
   (setq vertico-buffer-display-action '(display-buffer-at-bottom))
   (vertico-buffer-frame-mode 1)
   (setq vertico-buffer-display-action '(display-buffer-pop-up-window))
   (vertico-buffer-frame-mode -1)
   (should (equal vertico-buffer-display-action
                  '(display-buffer-pop-up-window)))))

(ert-deftest vertico-buffer-frame-mode-preserves-buffer-mode-on-action-change ()
  (vertico-buffer-frame-test--with-clean-state
   (vertico-buffer-mode -1)
   (setq vertico-buffer-display-action '(display-buffer-at-bottom))
   (vertico-buffer-frame-mode 1)
   (setq vertico-buffer-display-action '(display-buffer-pop-up-window))
   (vertico-buffer-frame-mode -1)
   (should vertico-buffer-mode)))

(ert-deftest vertico-buffer-frame-mode-registers-embark-display-action ()
  (vertico-buffer-frame-test--with-clean-state
   (setq display-buffer-alist nil)
   (vertico-buffer-frame-mode 1)
   (should (member vertico-buffer-frame--embark-display-action
                   display-buffer-alist))
   (vertico-buffer-frame-mode -1)
   (should-not (member vertico-buffer-frame--embark-display-action
                       display-buffer-alist))))

(ert-deftest vertico-buffer-frame-mode-preserves-user-embark-display-action ()
  (vertico-buffer-frame-test--with-clean-state
   (setq display-buffer-alist
         (list vertico-buffer-frame--embark-display-action))
   (vertico-buffer-frame-mode 1)
   (should (equal display-buffer-alist
                  (list vertico-buffer-frame--embark-display-action)))
   (vertico-buffer-frame-mode -1)
   (should (equal display-buffer-alist
                  (list vertico-buffer-frame--embark-display-action)))))

(ert-deftest vertico-buffer-frame-mode-preserves-user-embark-action-copy ()
  (vertico-buffer-frame-test--with-clean-state
   (setq display-buffer-alist nil)
   (vertico-buffer-frame-mode 1)
   (let ((user-action (copy-tree vertico-buffer-frame--embark-display-action)))
     (push user-action display-buffer-alist)
     (vertico-buffer-frame-mode -1)
     (should (equal display-buffer-alist
                    (list user-action)))
     (should (not (eq (car display-buffer-alist)
                      vertico-buffer-frame--embark-display-action))))))

(ert-deftest vertico-buffer-frame-mode-tolerates-improper-display-buffer-alist ()
  (vertico-buffer-frame-test--with-clean-state
   (vertico-buffer-mode -1)
   (setq display-buffer-alist '(user-rule . bad))
   (vertico-buffer-frame-mode 1)
   (should vertico-buffer-mode)
   (should (equal display-buffer-alist '(user-rule . bad)))
   (vertico-buffer-frame-mode -1)
   (should-not vertico-buffer-mode)
   (should (equal display-buffer-alist '(user-rule . bad)))))

(ert-deftest vertico-buffer-frame-mode-tolerates-improper-minibuffer-setup-hook ()
  (vertico-buffer-frame-test--with-clean-state
   (vertico-buffer-mode -1)
   (setq minibuffer-setup-hook '(ignore . bad))
   (vertico-buffer-frame-mode 1)
   (should (proper-list-p minibuffer-setup-hook))
   (should (memq #'ignore minibuffer-setup-hook))
   (should (memq #'vertico-buffer-frame--minibuffer-setup
                 minibuffer-setup-hook))
   (vertico-buffer-frame-mode -1)
   (should (proper-list-p minibuffer-setup-hook))
   (should (memq #'ignore minibuffer-setup-hook))
   (should-not (memq #'vertico-buffer-frame--minibuffer-setup
                     minibuffer-setup-hook))))

;;;; Embark display routing

(ert-deftest vertico-buffer-frame-embark-display-uses-minibuffer-origin-window ()
  (let ((old-hook minibuffer-exit-hook)
        (buffer (generate-new-buffer " *vbf-embark*"))
        (origin (selected-window))
        same-window
        popped)
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil)
          (cl-letf (((symbol-function #'minibufferp)
                     (lambda (&optional _buffer)
                       t))
                    ((symbol-function #'minibuffer-selected-window)
                     (lambda ()
                       origin))
                    ((symbol-function #'display-buffer-same-window)
                     (lambda (buffer alist)
                       (setq same-window (list buffer alist))
                       origin))
                    ((symbol-function #'run-at-time)
                     (lambda (_time _repeat function)
                       (funcall function)
                       'timer))
                    ((symbol-function #'pop-to-buffer)
                     (lambda (buffer action)
                       (setq popped (list buffer action))
                       origin)))
            (should
             (eq (vertico-buffer-frame--display-minibuffer-selected-window
                  buffer
                  '((inhibit-same-window . t)))
                 origin))
            (run-hooks 'minibuffer-exit-hook)
            (should (equal same-window
                           (list buffer
                                 nil)))
            (should (equal popped
                           (list buffer
                                 '(display-buffer-same-window))))))
      (setq minibuffer-exit-hook old-hook)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-embark-display-overrides-inhibit-same-window ()
  (let* ((origin (selected-window))
         (old-buffer (window-buffer origin))
         (owner (generate-new-buffer " *vbf-embark-owner*"))
         (buffer (generate-new-buffer " *vbf-embark-real*")))
    (unwind-protect
        (cl-letf (((symbol-function #'minibufferp)
                   (lambda (&optional _buffer)
                     t))
                  ((symbol-function #'minibuffer-selected-window)
                   (lambda ()
                     origin)))
          (with-current-buffer owner
            (should
             (eq (vertico-buffer-frame--display-minibuffer-selected-window
                  buffer
                  '((inhibit-same-window . t)))
                 origin))
            (should (eq (window-buffer origin) buffer))))
      (when (buffer-live-p old-buffer)
        (set-window-buffer origin old-buffer))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list owner buffer)))))

(ert-deftest vertico-buffer-frame-embark-display-exit-hook-is-local ()
  (let ((old-hook minibuffer-exit-hook)
        (owner (generate-new-buffer " *vbf-embark-owner*"))
        (other (generate-new-buffer " *vbf-embark-other*"))
        (buffer (generate-new-buffer " *vbf-embark-buffer*"))
        (origin (selected-window))
        popped)
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil)
          (cl-letf (((symbol-function #'minibufferp)
                     (lambda (&optional _buffer)
                       t))
                    ((symbol-function #'minibuffer-selected-window)
                     (lambda ()
                       origin))
                    ((symbol-function #'display-buffer-same-window)
                     (lambda (_buffer _alist)
                       origin))
                    ((symbol-function #'run-at-time)
                     (lambda (_time _repeat function)
                       (funcall function)
                       'timer))
                    ((symbol-function #'pop-to-buffer)
                     (lambda (buffer action)
                       (setq popped (list buffer action))
                       origin)))
            (with-current-buffer owner
              (should
               (eq (vertico-buffer-frame--display-minibuffer-selected-window
                    buffer nil)
                   origin))
              (should (local-variable-p 'minibuffer-exit-hook owner))
              (should-not (default-value 'minibuffer-exit-hook)))
            (with-current-buffer other
              (run-hooks 'minibuffer-exit-hook))
            (should-not popped)
            (with-current-buffer owner
              (run-hooks 'minibuffer-exit-hook)
              (should-not minibuffer-exit-hook)
              (should-not
               (local-variable-p 'minibuffer-exit-hook owner)))
            (should (equal popped
                           (list buffer
                                 '(display-buffer-same-window))))))
      (setq minibuffer-exit-hook old-hook)
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list owner other buffer)))))

(ert-deftest vertico-buffer-frame-embark-display-skips-dead-origin-window ()
  (let ((old-hook minibuffer-exit-hook)
        display-called)
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil)
          (cl-letf (((symbol-function #'minibufferp)
                     (lambda (&optional _buffer)
                       t))
                    ((symbol-function #'minibuffer-selected-window)
                     (lambda ()
                       'dead-window))
                    ((symbol-function #'window-live-p)
                     (lambda (_window)
                       nil))
                    ((symbol-function #'display-buffer-same-window)
                     (lambda (&rest _args)
                       (setq display-called t))))
            (should-not
             (vertico-buffer-frame--display-minibuffer-selected-window
              (current-buffer)
              nil))
            (should-not display-called)
            (should-not minibuffer-exit-hook)))
      (setq minibuffer-exit-hook old-hook))))

(ert-deftest vertico-buffer-frame-embark-display-ignores-immediate-window-error ()
  (let ((old-hook minibuffer-exit-hook)
        (buffer (generate-new-buffer " *vbf-embark-error*"))
        (origin (selected-window)))
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil)
          (cl-letf (((symbol-function #'minibufferp)
                     (lambda (&optional _buffer)
                       t))
                    ((symbol-function #'minibuffer-selected-window)
                     (lambda ()
                       origin))
                    ((symbol-function #'display-buffer-same-window)
                     (lambda (&rest _args)
                       (error "Stale origin"))))
            (should-not
             (vertico-buffer-frame--display-minibuffer-selected-window
              buffer
              nil))
            (should-not minibuffer-exit-hook)))
      (setq minibuffer-exit-hook old-hook)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-embark-display-ignores-deferred-window-error ()
  (let ((old-hook minibuffer-exit-hook)
        (buffer (generate-new-buffer " *vbf-embark-deferred-error*"))
        (origin (selected-window))
        popped)
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil)
          (cl-letf (((symbol-function #'minibufferp)
                     (lambda (&optional _buffer)
                       t))
                    ((symbol-function #'minibuffer-selected-window)
                     (lambda ()
                       origin))
                    ((symbol-function #'display-buffer-same-window)
                     (lambda (_buffer _alist)
                       origin))
                    ((symbol-function #'run-at-time)
                     (lambda (_time _repeat function)
                       (funcall function)
                       'timer))
                    ((symbol-function #'pop-to-buffer)
                     (lambda (&rest _args)
                       (setq popped t)
                       (error "Stale origin"))))
            (should
             (eq (vertico-buffer-frame--display-minibuffer-selected-window
                  buffer
                  nil)
                 origin))
            (run-hooks 'minibuffer-exit-hook)
            (should popped)
            (should-not minibuffer-exit-hook)))
      (setq minibuffer-exit-hook old-hook)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-embark-display-ignores-deferred-timer-error ()
  (let ((old-hook minibuffer-exit-hook)
        (buffer (generate-new-buffer " *vbf-embark-timer-error*"))
        (origin (selected-window)))
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil)
          (cl-letf (((symbol-function #'minibufferp)
                     (lambda (&optional _buffer)
                       t))
                    ((symbol-function #'minibuffer-selected-window)
                     (lambda ()
                       origin))
                    ((symbol-function #'display-buffer-same-window)
                     (lambda (_buffer _alist)
                       origin))
                    ((symbol-function #'run-at-time)
                     (lambda (&rest _args)
                       (error "Timer failed"))))
            (should
             (eq (vertico-buffer-frame--display-minibuffer-selected-window
                  buffer
                  nil)
                 origin))
            (run-hooks 'minibuffer-exit-hook)
            (should-not minibuffer-exit-hook)))
      (setq minibuffer-exit-hook old-hook)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-action-is-simple ()
  (should (equal (vertico-buffer-frame-display-action)
                 '(vertico-buffer-frame--display-buffer))))

;;;; Child-frame parameters and backend compatibility

(ert-deftest vertico-buffer-frame-base-parameters-strip-frame-chrome ()
  (let ((vertico-buffer-frame-border-width 1))
    (cl-letf (((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil)))
      (let ((parameters (vertico-buffer-frame--base-parameters
                         'parent "name" 80 10)))
        (should (equal (alist-get 'tab-bar-lines parameters) 0))
        (should (equal (alist-get 'menu-bar-lines parameters) 0))
        (should (equal (alist-get 'tool-bar-lines parameters) 0))
        (should (equal (alist-get 'internal-border-width parameters) 0))
        (should (equal (alist-get 'child-frame-border-width parameters) 1))
        (should (equal (alist-get 'border-width parameters) 0))
        (should (equal (alist-get 'title parameters) ""))
        (should-not (alist-get 'minibuffer-exit parameters))
        (should (equal (alist-get 'left-fringe parameters) 0))
        (should (equal (alist-get 'right-fringe parameters) 0))
        (should (equal (alist-get 'right-divider-width parameters) 0))
        (should (equal (alist-get 'bottom-divider-width parameters) 0))
        (should-not (alist-get 'visibility parameters))
        (should (equal (alist-get 'no-accept-focus parameters) t))
        (should (equal (alist-get 'no-focus-on-map parameters) t))
        (should (equal (alist-get 'alpha parameters) 100))
        (should (equal (alist-get 'alpha-background parameters) 100))))))

(ert-deftest vertico-buffer-frame-base-parameters-make-candidate-interactive ()
  (cl-letf (((symbol-function #'vertico-buffer-frame--default-background)
             (lambda (_frame)
               nil))
            ((symbol-function #'vertico-buffer-frame--default-foreground)
             (lambda (_frame)
               nil)))
    (let ((candidate-parameters
           (vertico-buffer-frame--base-parameters
            'parent "name" 80 10 'candidate))
          (preview-parameters
           (vertico-buffer-frame--base-parameters
            'parent "name" 80 10 'preview)))
      (should-not (alist-get 'visibility candidate-parameters))
      (should-not (alist-get 'no-accept-focus candidate-parameters))
      (should (equal (alist-get 'no-focus-on-map candidate-parameters) t))
      (should-not (alist-get 'visibility preview-parameters))
      (should (equal (alist-get 'no-accept-focus preview-parameters)
                     t)))))

(ert-deftest vertico-buffer-frame-base-parameters-use-parent-minibuffer-window ()
  (cl-letf (((symbol-function #'vertico-buffer-frame--default-background)
             (lambda (_frame)
               nil))
            ((symbol-function #'vertico-buffer-frame--default-foreground)
             (lambda (_frame)
               nil))
            ((symbol-function #'vertico-buffer-frame--minibuffer-window)
             (lambda (frame)
               (and (eq frame 'parent) 'minibuffer-window))))
    (let ((parameters (vertico-buffer-frame--base-parameters
                       'parent "name" 80 10 'candidate)))
      (should (eq (alist-get 'minibuffer parameters)
                  'minibuffer-window)))))

(ert-deftest vertico-buffer-frame-base-parameters-inherit-parent-font ()
  (cl-letf (((symbol-function #'frame-parameter)
             (lambda (frame parameter)
               (and (eq frame 'parent)
                    (eq parameter 'font)
                    "Parent Font 12")))
            ((symbol-function #'vertico-buffer-frame--default-background)
             (lambda (_frame)
               nil))
            ((symbol-function #'vertico-buffer-frame--default-foreground)
             (lambda (_frame)
               nil)))
    (let ((parameters (vertico-buffer-frame--base-parameters
                       'parent "name" 80 10 'candidate)))
      (should (equal (alist-get 'font parameters)
                     "Parent Font 12")))))

(ert-deftest vertico-buffer-frame-base-parameters-uses-extra-parameters ()
  (let ((vertico-buffer-frame-parameters '((user-parameter . value))))
    (cl-letf (((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil)))
      (let ((parameters (vertico-buffer-frame--base-parameters
                         'parent "name" 80 10)))
        (should (eq (alist-get 'user-parameter parameters) 'value))))))

(ert-deftest vertico-buffer-frame-base-parameters-sanitize-invalid-options ()
  (let ((vertico-buffer-frame-border-width 'bad)
        (vertico-buffer-frame-parameters 'bad))
    (cl-letf (((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil)))
      (let ((parameters (vertico-buffer-frame--base-parameters
                         'parent "name" 80 10)))
        (should (equal (alist-get 'child-frame-border-width parameters)
                       vertico-buffer-frame--default-border-width))
        (should (null (cdr (last parameters))))))))

(ert-deftest vertico-buffer-frame-base-parameters-ignore-improper-extra-parameters ()
  (let ((vertico-buffer-frame-parameters '((user-parameter . value) . bad)))
    (cl-letf (((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil)))
      (let ((parameters (vertico-buffer-frame--base-parameters
                         'parent "name" 80 10)))
        (should-not (alist-get 'user-parameter parameters))
        (should (null (cdr (last parameters))))))))

(ert-deftest vertico-buffer-frame-base-parameters-filter-malformed-extra-parameters ()
  (dolist (case '(((bad) . nil)
                  (((42 . value)) . nil)
                  (((user-parameter . value) bad) . value)))
    (let ((vertico-buffer-frame-parameters (car case))
          (expected (cdr case)))
      (cl-letf (((symbol-function #'vertico-buffer-frame--default-background)
                 (lambda (_frame)
                   nil))
                ((symbol-function #'vertico-buffer-frame--default-foreground)
                 (lambda (_frame)
                   nil)))
        (let ((parameters (vertico-buffer-frame--base-parameters
                           'parent "name" 80 10)))
          (should (eq (alist-get 'user-parameter parameters)
                      expected))
          (should (proper-list-p parameters)))))))

(ert-deftest vertico-buffer-frame-border-width-allows-zero ()
  (let ((vertico-buffer-frame-border-width 0))
    (should (= (vertico-buffer-frame--border-width) 0))))

(ert-deftest vertico-buffer-frame-default-colors-ignore-unspecified-values ()
  (should-not
   (vertico-buffer-frame--specified-color "unspecified-bg"))
  (should-not
   (vertico-buffer-frame--specified-color "unspecified-fg"))
  (should (equal (vertico-buffer-frame--specified-color "#112233")
                 "#112233"))
  (cl-letf (((symbol-function #'face-background)
             (lambda (&rest _args)
               "unspecified-bg"))
            ((symbol-function #'face-foreground)
             (lambda (&rest _args)
               "unspecified-fg")))
    (should-not (vertico-buffer-frame--default-background 'frame))
    (should-not (vertico-buffer-frame--default-foreground 'frame))))

(ert-deftest vertico-buffer-frame-default-colors-ignore-face-errors ()
  (cl-letf (((symbol-function #'face-background)
             (lambda (&rest _args)
               (error "Background failed")))
            ((symbol-function #'face-foreground)
             (lambda (&rest _args)
               (error "Foreground failed"))))
    (should-not (vertico-buffer-frame--default-background 'frame))
    (should-not (vertico-buffer-frame--default-foreground 'frame))))

(ert-deftest vertico-buffer-frame-default-colors-fall-back-to-frame-parameters ()
  (cl-letf (((symbol-function #'face-background)
             (lambda (&rest _args)
               "unspecified-bg"))
            ((symbol-function #'face-foreground)
             (lambda (&rest _args)
               "unspecified-fg"))
            ((symbol-function #'frame-parameter)
             (lambda (_frame parameter)
               (pcase parameter
                 ('background-color "background")
                 ('foreground-color "foreground")))))
    (should (equal (vertico-buffer-frame--default-background 'frame)
                   "background"))
    (should (equal (vertico-buffer-frame--default-foreground 'frame)
                   "foreground"))))

(ert-deftest vertico-buffer-frame-preview-colors-use-outer-parent-frame ()
  (let (background-source
        foreground-source)
    (cl-letf (((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (and (eq frame 'candidate-frame)
                      (eq parameter 'parent-frame)
                      'root-frame)))
              ((symbol-function #'frame-live-p)
               (lambda (frame)
                 (eq frame 'root-frame)))
              ((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (frame)
                 (setq background-source frame)
                 (and (eq frame 'root-frame)
                      "background")))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (frame)
                 (setq foreground-source frame)
                 (and (eq frame 'root-frame)
                      "foreground"))))
      (let ((parameters (vertico-buffer-frame--base-parameters
                         'candidate-frame "preview" 80 10 'preview)))
        (should (equal (alist-get 'background-color parameters)
                       "background"))
        (should (equal (alist-get 'foreground-color parameters)
                       "foreground"))
        (should (eq background-source 'root-frame))
        (should (eq foreground-source 'root-frame))))))

(ert-deftest vertico-buffer-frame-apply-border-face-ignores-face-errors ()
  (let (backgrounds foregrounds)
    (cl-letf (((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (frame)
                 (should (eq frame 'source))
                 "background"))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (frame)
                 (should (eq frame 'source))
                 "foreground"))
              ((symbol-function #'set-face-background)
               (lambda (face color frame)
                 (push (list face color frame) backgrounds)
                 (error "Face failed")))
              ((symbol-function #'set-face-foreground)
               (lambda (face color frame)
                 (push (list face color frame) foregrounds)
                 (error "Face failed"))))
      (should (eq (vertico-buffer-frame--apply-border-face 'frame 'source)
                  'frame))
      (should (equal backgrounds
                     '((child-frame-border "foreground" frame)
                       (default "background" frame))))
      (should (equal foregrounds
                     '((default "foreground" frame)))))))

(ert-deftest vertico-buffer-frame-parent-frame-falls-back-on-window-error ()
  (cl-letf (((symbol-function #'minibuffer-selected-window)
             (lambda ()
               (error "No minibuffer window")))
            ((symbol-function #'selected-window)
             (lambda ()
               'selected-window))
            ((symbol-function #'window-live-p)
             (lambda (window)
               (eq window 'selected-window)))
            ((symbol-function #'window-frame)
             (lambda (window)
               (and (eq window 'selected-window) 'selected-frame))))
    (should (eq (vertico-buffer-frame--parent-frame)
                'selected-frame))))

(ert-deftest vertico-buffer-frame-window-frame-ignores-stale-window ()
  (cl-letf (((symbol-function #'window-live-p)
             (lambda (window)
               (eq window 'stale-window)))
            ((symbol-function #'window-frame)
             (lambda (_window)
               (error "Stale window"))))
    (should-not (vertico-buffer-frame--window-frame 'stale-window))))

(ert-deftest vertico-buffer-frame-parent-frame-falls-back-on-window-frame-error ()
  (cl-letf (((symbol-function #'minibuffer-selected-window)
             (lambda ()
               'stale-window))
            ((symbol-function #'window-live-p)
             (lambda (window)
               (eq window 'stale-window)))
            ((symbol-function #'window-frame)
             (lambda (_window)
               (error "Stale window")))
            ((symbol-function #'selected-frame)
             (lambda ()
               'fallback-frame)))
    (should (eq (vertico-buffer-frame--parent-frame)
                'fallback-frame))))

(ert-deftest vertico-buffer-frame-live-checks-ignore-errors ()
  (cl-letf (((symbol-function #'frame-live-p)
             (lambda (_frame)
               (error "Stale frame")))
            ((symbol-function #'window-live-p)
             (lambda (_window)
               (error "Stale window"))))
    (should-not (vertico-buffer-frame--frame-live-p 'stale-frame))
    (should-not (vertico-buffer-frame--window-live-p 'stale-window))))

(ert-deftest vertico-buffer-frame-child-frames-supported-p-checks-backend ()
  (let (graphic tty)
    (cl-letf (((symbol-function #'framep)
               (lambda (frame)
                 (eq frame 'frame)))
              ((symbol-function #'display-graphic-p)
               (lambda (frame)
                 (and (eq frame 'frame) graphic)))
              ((symbol-function #'featurep)
               (lambda (feature)
                 (and (eq feature 'tty-child-frames) tty))))
      (setq graphic t
            tty nil)
      (should (vertico-buffer-frame--child-frames-supported-p 'frame))
      (setq graphic nil
            tty t)
      (should (vertico-buffer-frame--child-frames-supported-p 'frame))
      (setq tty nil)
      (should-not (vertico-buffer-frame--child-frames-supported-p 'frame))
      (should (vertico-buffer-frame--child-frames-supported-p 'mock-frame)))))

;;;; Child-frame creation and fallback display

(ert-deftest vertico-buffer-frame-display-child-frame-uses-opaque-default-colors ()
  (let (display-alist face-backgrounds face-foregrounds prepared)
    (cl-letf (((symbol-function #'display-buffer-in-child-frame)
               (lambda (_buffer alist)
                 (setq display-alist alist)
                 'window))
              ((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'frame)))
              ((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (frame)
                 (should (eq frame 'parent))
                 "background"))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (frame)
                 (should (eq frame 'parent))
                 "foreground"))
              ((symbol-function #'set-face-background)
               (lambda (face color frame)
                 (push (list face color frame) face-backgrounds)))
              ((symbol-function #'set-face-foreground)
               (lambda (face color frame)
                 (push (list face color frame) face-foregrounds)))
              ((symbol-function #'vertico-buffer-frame--prepare-window)
               (lambda (window)
                 (push window prepared))))
      (should (eq (vertico-buffer-frame--display-buffer-in-child-frame
                   'buffer 'parent "name" '(80 . 10) 'candidate)
                  'window))
      (let ((created-parameters
             (alist-get 'child-frame-parameters display-alist)))
        (should (equal (alist-get 'width created-parameters) 80))
        (should (equal (alist-get 'height created-parameters) 10))
        (should (equal (alist-get 'title created-parameters) ""))
        (should (equal (alist-get 'child-frame-border-width created-parameters)
                       vertico-buffer-frame-border-width))
        (should (equal (alist-get 'background-color created-parameters)
                       "background"))
        (should (equal (alist-get 'foreground-color created-parameters)
                       "foreground"))
        (should (equal (alist-get 'alpha created-parameters) 100))
        (should (equal (alist-get 'alpha-background created-parameters) 100))
        (should (equal (alist-get 'vertico-buffer-frame-owner
                                  created-parameters)
                       t))
        (should (equal (alist-get 'vertico-buffer-frame-role
                                  created-parameters)
                       'candidate))
        (let ((share (alist-get 'share-child-frame created-parameters)))
          (should (symbolp (car-safe share)))
          (should (equal (cdr share) '(candidate "name")))))
      (should (equal prepared '(window)))
      (should (equal face-backgrounds
                     '((child-frame-border "foreground" frame)
                       (default "background" frame))))
      (should (equal face-foregrounds
                     '((default "foreground" frame)))))))

(ert-deftest vertico-buffer-frame-display-preview-applies-outer-parent-colors ()
  (let (display-alist face-backgrounds face-foregrounds prepared)
    (cl-letf (((symbol-function #'display-buffer-in-child-frame)
               (lambda (_buffer alist)
                 (setq display-alist alist)
                 'window))
              ((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'preview-frame)))
              ((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(root-frame preview-frame))))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (and (eq frame 'candidate-frame)
                      (eq parameter 'parent-frame)
                      'root-frame)))
              ((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (frame)
                 (and (eq frame 'root-frame)
                      "background")))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (frame)
                 (and (eq frame 'root-frame)
                      "foreground")))
              ((symbol-function #'set-face-background)
               (lambda (face color frame)
                 (push (list face color frame) face-backgrounds)))
              ((symbol-function #'set-face-foreground)
               (lambda (face color frame)
                 (push (list face color frame) face-foregrounds)))
              ((symbol-function #'vertico-buffer-frame--prepare-window)
               (lambda (window)
                 (push window prepared))))
      (should (eq (vertico-buffer-frame--display-buffer-in-child-frame
                   'buffer 'candidate-frame "preview" '(80 . 10) 'preview)
                  'window))
      (let ((created-parameters
             (alist-get 'child-frame-parameters display-alist)))
        (should (equal (alist-get 'parent-frame created-parameters)
                       'candidate-frame))
        (should (equal (alist-get 'background-color created-parameters)
                       "background"))
        (should (equal (alist-get 'foreground-color created-parameters)
                       "foreground")))
      (should (equal prepared '(window)))
      (should (equal face-backgrounds
                     '((child-frame-border "foreground" preview-frame)
                       (default "background" preview-frame))))
      (should (equal face-foregrounds
                     '((default "foreground" preview-frame)))))))

(ert-deftest vertico-buffer-frame-child-frame-session-is-buffer-local ()
  (let ((buffer-a (generate-new-buffer " *vbf-session-a*"))
        (buffer-b (generate-new-buffer " *vbf-session-b*")))
    (unwind-protect
        (let (session-a session-b)
          (with-current-buffer buffer-a
            (setq session-a (vertico-buffer-frame--child-frame-session))
            (should (eq session-a
                        (vertico-buffer-frame--child-frame-session))))
          (with-current-buffer buffer-b
            (setq session-b (vertico-buffer-frame--child-frame-session)))
          (should-not (eq session-a session-b)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list buffer-a buffer-b)))))

(ert-deftest vertico-buffer-frame-display-child-frame-sanitizes-extra-parameters ()
  (let ((vertico-buffer-frame-parameters '((user-parameter . value)))
        display-alist)
    (cl-letf (((symbol-function #'display-buffer-in-child-frame)
               (lambda (_buffer alist)
                 (setq display-alist alist)
                 'window))
              ((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'frame)))
              ((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--prepare-window)
               (lambda (_window)))
              ((symbol-function #'vertico-buffer-frame--apply-border-face)
               (lambda (&rest _args))))
      (vertico-buffer-frame--display-buffer-in-child-frame
       'buffer 'parent "name" '(80 . 10) 'candidate)
      (let ((created-parameters
             (alist-get 'child-frame-parameters display-alist)))
        (should (proper-list-p created-parameters))
        (should (equal (alist-get 'user-parameter created-parameters)
                       'value))
        (should (= (cl-count 'user-parameter created-parameters
                             :key #'car)
                   1))))))

(ert-deftest vertico-buffer-frame-display-child-frame-ignores-improper-extra-parameters ()
  (let ((vertico-buffer-frame-parameters '((user-parameter . value) . bad))
        display-alist)
    (cl-letf (((symbol-function #'display-buffer-in-child-frame)
               (lambda (_buffer alist)
                 (setq display-alist alist)
                 'window))
              ((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'frame)))
              ((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--prepare-window)
               (lambda (_window)))
              ((symbol-function #'vertico-buffer-frame--apply-border-face)
               (lambda (&rest _args))))
      (vertico-buffer-frame--display-buffer-in-child-frame
       'buffer 'parent "name" '(80 . 10) 'candidate)
      (let ((created-parameters
             (alist-get 'child-frame-parameters display-alist)))
        (should (proper-list-p created-parameters))
        (should-not (alist-get 'user-parameter created-parameters))))))

(ert-deftest vertico-buffer-frame-display-child-frame-filters-malformed-extra-parameters ()
  (let ((vertico-buffer-frame-parameters '((user-parameter . value) bad))
        display-alist)
    (cl-letf (((symbol-function #'display-buffer-in-child-frame)
               (lambda (_buffer alist)
                 (setq display-alist alist)
                 'window))
              ((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'frame)))
              ((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--prepare-window)
               (lambda (_window)))
              ((symbol-function #'vertico-buffer-frame--apply-border-face)
               (lambda (&rest _args))))
      (vertico-buffer-frame--display-buffer-in-child-frame
       'buffer 'parent "name" '(80 . 10) 'candidate)
      (let ((created-parameters
             (alist-get 'child-frame-parameters display-alist)))
        (should (proper-list-p created-parameters))
        (should (equal (alist-get 'user-parameter created-parameters)
                       'value))
        (should (= (cl-count 'user-parameter created-parameters
                             :key #'car)
                   1))))))

(ert-deftest vertico-buffer-frame-display-child-frame-tags-owner-buffer ()
  (with-temp-buffer
    (let (owner)
      (cl-letf (((symbol-function #'display-buffer-in-child-frame)
                 (lambda (&rest _args)
                   'window))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'window-frame)
                 (lambda (window)
                   (and (eq window 'window) 'frame)))
                ((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'set-frame-parameter)
                 (lambda (frame parameter value)
                   (setq owner (list frame parameter value))))
                ((symbol-function #'vertico-buffer-frame--default-background)
                 (lambda (_frame)
                   nil))
                ((symbol-function #'vertico-buffer-frame--default-foreground)
                 (lambda (_frame)
                   nil))
                ((symbol-function #'vertico-buffer-frame--prepare-window)
                 (lambda (_window)))
                ((symbol-function #'vertico-buffer-frame--apply-border-face)
                 (lambda (&rest _args))))
        (should (eq (vertico-buffer-frame--display-buffer-in-child-frame
                     'buffer 'parent "name" '(80 . 10))
                    'window))
        (should (equal owner
                       (list 'frame
                             vertico-buffer-frame--owner-buffer-parameter
                             (current-buffer))))))))

(ert-deftest vertico-buffer-frame-display-child-frame-deletes-on-setup-error ()
  (let (deleted)
    (cl-letf (((symbol-function #'display-buffer-in-child-frame)
               (lambda (&rest _args)
                 'window))
              ((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'frame)))
              ((symbol-function #'frame-live-p)
               (lambda (frame)
                 (eq frame 'frame)))
              ((symbol-function #'delete-frame)
               (lambda (frame force)
                 (push (list frame force) deleted)))
              ((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--prepare-window)
               (lambda (_window)
                 (error "Setup failed"))))
      (should-error
       (vertico-buffer-frame--display-buffer-in-child-frame
        'buffer 'parent "name" '(80 . 10)))
      (should (equal deleted '((frame t)))))))

(ert-deftest vertico-buffer-frame-display-child-frame-errors-on-stale-window-frame ()
  (let (prepared)
    (cl-letf (((symbol-function #'display-buffer-in-child-frame)
               (lambda (&rest _args)
                 'window))
              ((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (_window)
                 (error "Stale window")))
              ((symbol-function #'vertico-buffer-frame--default-background)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--default-foreground)
               (lambda (_frame)
                 nil))
              ((symbol-function #'vertico-buffer-frame--prepare-window)
               (lambda (_window)
                 (setq prepared t))))
      (should-error
       (vertico-buffer-frame--display-buffer-in-child-frame
        'buffer 'parent "name" '(80 . 10)))
      (should-not prepared))))

(ert-deftest vertico-buffer-frame-display-buffer-falls-back-when-unsupported ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-unsupported*"))
        (fallback-window (selected-window))
        fallback
        displayed
        sized)
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--parent-frame)
                   (lambda ()
                     'parent))
                  ((symbol-function
                    #'vertico-buffer-frame--child-frames-supported-p)
                   (lambda (frame)
                     (should (eq frame 'parent))
                     nil))
                  ((symbol-function #'vertico-buffer-frame--candidate-frame-size)
                   (lambda (_parent)
                     (setq sized t)
                     '(80 . 10)))
                  ((symbol-function #'display-buffer-in-child-frame)
                   (lambda (&rest _args)
                     (setq displayed t)
                     'window))
                  ((symbol-function #'display-buffer-use-least-recent-window)
                   (lambda (buffer alist)
                     (setq fallback (list buffer alist))
                     fallback-window)))
          (should (eq (vertico-buffer-frame--display-buffer
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (should (equal fallback
                         (list buffer '((inhibit-same-window . t)))))
          (should-not sized)
          (should-not displayed))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-buffer-fallback-uses-saved-action ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-saved*"))
        (fallback-window (selected-window))
        (vertico-buffer-frame--saved-state t)
        (vertico-buffer-frame--saved-display-action
         '(display-buffer-at-bottom (window-height . 7)))
        called)
    (unwind-protect
        (cl-letf (((symbol-function #'display-buffer-at-bottom)
                   (lambda (buffer alist)
                     (setq called (list buffer alist))
                     fallback-window)))
          (should (eq (vertico-buffer-frame--display-buffer-fallback
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (should (equal called
                         (list buffer
                               '((window-height . 7)
                                 (inhibit-same-window . t))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-buffer-fallback-uses-saved-nil-action ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-nil-action*"))
        (fallback-window (selected-window))
        (vertico-buffer-frame--saved-state t)
        (vertico-buffer-frame--saved-display-action nil)
        (display-buffer-fallback-action '((display-buffer-use-some-window)))
        called)
    (unwind-protect
        (cl-letf (((symbol-function #'display-buffer-use-some-window)
                   (lambda (buffer alist)
                     (setq called (list buffer alist))
                     fallback-window)))
          (should (eq (vertico-buffer-frame--display-buffer-fallback
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (should (equal called
                         (list buffer
                               '((inhibit-same-window . t))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-buffer-fallback-uses-saved-function-list ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-function-list*"))
        (fallback-window (selected-window))
        (vertico-buffer-frame--saved-state t)
        (vertico-buffer-frame--saved-display-action
         '((display-buffer-reuse-window display-buffer-pop-up-window)
           (window-height . 7)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function #'display-buffer-reuse-window)
                   (lambda (buffer alist)
                     (push (list 'reuse buffer alist) calls)
                     nil))
                  ((symbol-function #'display-buffer-pop-up-window)
                   (lambda (buffer alist)
                     (push (list 'pop-up buffer alist) calls)
                     fallback-window)))
          (should (eq (vertico-buffer-frame--display-buffer-fallback
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (setq calls (nreverse calls))
          (should (equal (mapcar #'car calls)
                         '(reuse pop-up)))
          (should (equal (mapcar #'cadr calls)
                         (list buffer buffer)))
          (should (equal (mapcar #'caddr calls)
                         '(((window-height . 7)
                            (inhibit-same-window . t))
                           ((window-height . 7)
                            (inhibit-same-window . t))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-buffer-fallback-skips-frame-action ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-frame-action*"))
        (fallback-window (selected-window))
        (vertico-buffer-frame--saved-state t)
        (vertico-buffer-frame--saved-display-action
         (vertico-buffer-frame-display-action))
        fallback)
    (unwind-protect
        (cl-letf (((symbol-function #'display-buffer-use-least-recent-window)
                   (lambda (buffer alist)
                     (setq fallback (list buffer alist))
                     fallback-window)))
          (should (eq (vertico-buffer-frame--display-buffer-fallback
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (should (equal fallback
                         (list buffer
                               '((inhibit-same-window . t))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-buffer-fallback-keeps-display-buffer-fallbacks ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-reuse*"))
        (fallback-window (selected-window))
        (vertico-buffer-frame--saved-state t)
        (vertico-buffer-frame--saved-display-action
         '(display-buffer-reuse-window))
        (display-buffer-fallback-action '((display-buffer-use-some-window)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function #'display-buffer-reuse-window)
                   (lambda (buffer alist)
                     (push (list 'reuse buffer alist) calls)
                     nil))
                  ((symbol-function #'display-buffer-use-some-window)
                   (lambda (buffer alist)
                     (push (list 'some buffer alist) calls)
                     fallback-window)))
          (should (eq (vertico-buffer-frame--display-buffer-fallback
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (setq calls (nreverse calls))
          (should (equal (mapcar #'car calls)
                         '(reuse some)))
          (should (equal (mapcar #'cadr calls)
                         (list buffer buffer)))
          (should (equal (mapcar #'caddr calls)
                         '(((inhibit-same-window . t))
                           ((inhibit-same-window . t))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-buffer-falls-back-on-nil-window ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-nil-window*"))
        (fallback-window (selected-window))
        fallback)
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--parent-frame)
                   (lambda ()
                     'parent))
                  ((symbol-function #'vertico-buffer-frame--candidate-frame-size)
                   (lambda (_parent)
                     '(80 . 10)))
                  ((symbol-function #'vertico-buffer-frame--default-background)
                   (lambda (_frame)
                     nil))
                  ((symbol-function #'vertico-buffer-frame--default-foreground)
                   (lambda (_frame)
                     nil))
                  ((symbol-function #'display-buffer-in-child-frame)
                   (lambda (&rest _args)
                     nil))
                  ((symbol-function #'display-buffer-use-least-recent-window)
                   (lambda (buffer alist)
                     (setq fallback (list buffer alist))
                     fallback-window)))
          (should (eq (vertico-buffer-frame--display-buffer
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (should (equal fallback
                         (list buffer '((inhibit-same-window . t))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-buffer-falls-back-on-stale-window-frame ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-stale-window*"))
        (fallback-window (selected-window))
        fallback
        prepared)
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--parent-frame)
                   (lambda ()
                     'parent))
                  ((symbol-function #'vertico-buffer-frame--candidate-frame-size)
                   (lambda (_parent)
                     '(80 . 10)))
                  ((symbol-function
                    #'vertico-buffer-frame--display-buffer-in-child-frame)
                   (lambda (&rest _args)
                     'candidate-window))
                  ((symbol-function #'window-live-p)
                   (lambda (window)
                     (eq window 'candidate-window)))
                  ((symbol-function #'window-frame)
                   (lambda (_window)
                     (error "Stale window")))
                  ((symbol-function #'set-window-buffer)
                   (lambda (&rest _args)
                     (setq prepared t)))
                  ((symbol-function #'display-buffer-use-least-recent-window)
                   (lambda (buffer alist)
                     (setq fallback (list buffer alist))
                     fallback-window)))
          (should (eq (vertico-buffer-frame--display-buffer
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (should (equal fallback
                         (list buffer '((inhibit-same-window . t)))))
          (should-not prepared))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-display-buffer-cleans-up-on-late-error ()
  (with-temp-buffer
    (let ((target-buffer (generate-new-buffer " *vbf-fallback-late-error*"))
          (fallback-window (selected-window))
          deleted
          fallback
          killed
          orphan-cleanup
          timer-cancelled)
      (unwind-protect
          (progn
            (setq-local vertico-buffer-frame--file-preview-cache '(key . buffer)
                        vertico-buffer-frame--imenu-cache '(key . entries)
                        vertico-buffer-frame--consult-imenu-entry-table-cache
                        '(key . table))
            (cl-letf (((symbol-function #'vertico-buffer-frame--parent-frame)
                       (lambda ()
                         'parent))
                      ((symbol-function #'vertico-buffer-frame--candidate-frame-size)
                       (lambda (_parent)
                         '(80 . 10)))
                      ((symbol-function
                        #'vertico-buffer-frame--display-buffer-in-child-frame)
                       (lambda (&rest _args)
                         'candidate-window))
                      ((symbol-function #'window-live-p)
                       (lambda (window)
                         (eq window 'candidate-window)))
                      ((symbol-function #'frame-live-p)
                       (lambda (frame)
                         (eq frame 'candidate-frame)))
                      ((symbol-function #'window-frame)
                       (lambda (window)
                         (and (eq window 'candidate-window)
                              'candidate-frame)))
                      ((symbol-function #'window-buffer)
                       (lambda (window)
                         (and (eq window 'candidate-window)
                              'old-buffer)))
                      ((symbol-function #'set-window-dedicated-p)
                       (lambda (&rest _args)))
                      ((symbol-function #'set-window-buffer)
                       (lambda (&rest _args)
                         (error "Late display failure")))
                      ((symbol-function #'vertico-buffer-frame--delete-frame)
                       (lambda (frame)
                         (when frame
                           (push frame deleted))))
                      ((symbol-function
                        #'vertico-buffer-frame--delete-frames-owned-by-buffer)
                       (lambda (owner)
                         (setq orphan-cleanup owner)))
                      ((symbol-function
                        #'vertico-buffer-frame--cancel-preview-timer)
                       (lambda ()
                         (setq timer-cancelled t)))
                      ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                       (lambda (&optional _keep-buffer)
                         (setq killed t)))
                      ((symbol-function #'display-buffer-use-least-recent-window)
                       (lambda (buffer alist)
                         (setq fallback (list buffer alist))
                         fallback-window)))
              (should (eq (vertico-buffer-frame--display-buffer
                           target-buffer
                           '((inhibit-same-window . t)))
                          fallback-window))
              (should (equal fallback
                             (list target-buffer
                                   '((inhibit-same-window . t)))))
              (should (equal deleted '(candidate-frame)))
              (should (eq orphan-cleanup (current-buffer)))
              (should killed)
              (should timer-cancelled)
              (should-not vertico-buffer-frame--candidate-frame)
              (should-not vertico-buffer-frame--candidate-window)
              (should-not vertico-buffer-frame--file-preview-cache)
              (should-not vertico-buffer-frame--imenu-cache)
              (should-not
               vertico-buffer-frame--consult-imenu-entry-table-cache)))
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer))))))

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

;;;; Candidate and preview frame layout

(ert-deftest vertico-buffer-frame-golden-size-uses-parent-frame-pixels ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 1080)))
      (should (equal (vertico-buffer-frame--golden-pixel-size 'parent)
                     '(1080 . 667))))))

(ert-deftest vertico-buffer-frame-golden-size-tolerates-invalid-scale ()
  (let ((vertico-buffer-frame-golden-ratio-scale "bad"))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 1080)))
      (should (equal (vertico-buffer-frame--golden-pixel-size 'parent)
                     '(1080 . 667))))))

(ert-deftest vertico-buffer-frame-golden-frame-size-rounds-to-characters ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 1080))
              ((symbol-function #'frame-char-width)
               (lambda (_frame) 10))
              ((symbol-function #'frame-char-height)
               (lambda (_frame) 20)))
      (should (equal (vertico-buffer-frame--golden-frame-size 'parent)
                     '(108 . 33))))))

(ert-deftest vertico-buffer-frame-candidate-size-can-use-fixed-options ()
  (let ((vertico-buffer-frame-size-method 'fixed)
        (vertico-buffer-frame-width 42)
        (vertico-buffer-frame-height 7))
    (should (equal (vertico-buffer-frame--candidate-frame-size 'parent)
                   '(42 . 7)))))

(ert-deftest vertico-buffer-frame-candidate-size-to-fit-disabled-keeps-size ()
  (let ((vertico-buffer-frame-resize-to-fit-candidates nil))
    (should (equal (vertico-buffer-frame--candidate-frame-size-to-fit
                    'parent 'window '(80 . 10))
                   '(80 . 10)))))

(ert-deftest vertico-buffer-frame-candidate-size-to-fit-expands-width ()
  (let ((vertico-buffer-frame-resize-to-fit-candidates t))
    (cl-letf (((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'frame)))
              ((symbol-function #'frame-char-width)
               (lambda (_frame)
                 10))
              ((symbol-function #'frame-pixel-width)
               (lambda (_frame)
                 1200))
              ((symbol-function
                #'vertico-buffer-frame--candidate-text-pixel-width)
               (lambda (window parent)
                 (should (eq window 'window))
                 (should (eq parent 'parent))
                 950)))
      (should (equal (vertico-buffer-frame--candidate-frame-size-to-fit
                      'parent 'window '(80 . 10))
                     '(95 . 10))))))

(ert-deftest vertico-buffer-frame-candidate-size-to-fit-caps-at-parent-width ()
  (let ((vertico-buffer-frame-resize-to-fit-candidates t)
        (vertico-buffer-frame-resize-to-fit-candidates-margin 0))
    (cl-letf (((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'frame)))
              ((symbol-function #'frame-char-width)
               (lambda (_frame)
                 10))
              ((symbol-function #'frame-pixel-width)
               (lambda (_frame)
                 1200))
              ((symbol-function
                #'vertico-buffer-frame--candidate-text-pixel-width)
               (lambda (_window _parent)
                 1800)))
      (should (equal (vertico-buffer-frame--candidate-frame-size-to-fit
                      'parent 'window '(80 . 10))
                     '(120 . 10))))))

(ert-deftest vertico-buffer-frame-candidate-size-to-fit-leaves-margin ()
  (let ((vertico-buffer-frame-resize-to-fit-candidates t)
        (vertico-buffer-frame-resize-to-fit-candidates-margin 16))
    (cl-letf (((symbol-function #'window-live-p)
               (lambda (window)
                 (eq window 'window)))
              ((symbol-function #'window-frame)
               (lambda (window)
                 (and (eq window 'window) 'frame)))
              ((symbol-function #'frame-char-width)
               (lambda (_frame)
                 10))
              ((symbol-function #'frame-pixel-width)
               (lambda (_frame)
                 1200))
              ((symbol-function
                #'vertico-buffer-frame--candidate-text-pixel-width)
               (lambda (_window _parent)
                 1800)))
      (should (equal (vertico-buffer-frame--candidate-frame-size-to-fit
                      'parent 'window '(80 . 10))
                     '(116 . 10))))))

(ert-deftest vertico-buffer-frame-candidate-size-to-fit-tolerates-bad-margin ()
  (let ((vertico-buffer-frame-resize-to-fit-candidates t)
        (vertico-buffer-frame-resize-to-fit-candidates-margin "bad"))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame)
                 1200))
              ((symbol-function #'frame-char-width)
               (lambda (_frame)
                 10)))
      (should (equal (vertico-buffer-frame--candidate-frame-max-width
                      'parent 'frame)
                     113)))))

(ert-deftest vertico-buffer-frame-candidate-text-pixel-width-measures-overlay ()
  (let ((old-buffer (window-buffer (selected-window)))
        (vertico-buffer-frame-resize-to-fit-candidates-margin 0))
    (with-temp-buffer
      (unwind-protect
          (progn
            (switch-to-buffer (current-buffer))
            (setq-local vertico--candidates-ov
                        (make-overlay (point-max) (point-max)))
            (overlay-put vertico--candidates-ov
                         'before-string
                         "aa\nabcdef\nbbbb")
            (cl-letf (((symbol-function #'frame-pixel-width)
                       (lambda (_frame)
                         100))
                      ((symbol-function #'string-pixel-width)
                       (lambda (string)
                         (* 10 (length string)))))
              (should (= (vertico-buffer-frame--candidate-text-pixel-width
                          (selected-window) 'parent)
                         60))))
        (when (buffer-live-p old-buffer)
          (set-window-buffer (selected-window) old-buffer))))))

(ert-deftest vertico-buffer-frame-preview-size-follows-candidate-frame ()
  (let ((vertico-buffer-frame-preview-width nil)
        (vertico-buffer-frame-preview-height nil))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1000))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 620))
              ((symbol-function #'frame-char-width)
               (lambda (_frame) 10))
              ((symbol-function #'frame-char-height)
               (lambda (_frame) 20)))
      (should (equal (vertico-buffer-frame--preview-frame-size 'candidate)
                     '(62 . 19))))))

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
                     '(42 . 11))))))

(ert-deftest vertico-buffer-frame-resize-frame-to-size-ignores-frame-errors ()
  (let (resized)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (eq frame 'frame)))
              ((symbol-function #'set-frame-size)
               (lambda (frame width height)
                 (setq resized (list frame width height))
                 (error "Resize failed"))))
      (should-not
       (vertico-buffer-frame--resize-frame-to-size 'frame '(42 . 11)))
      (should (equal resized '(frame 42 11))))))

(ert-deftest vertico-buffer-frame-resize-frame-to-size-skips-unchanged-size ()
  (let (resized)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (eq frame 'frame)))
              ((symbol-function #'frame-width)
               (lambda (frame)
                 (and (eq frame 'frame) 42)))
              ((symbol-function #'frame-height)
               (lambda (frame)
                 (and (eq frame 'frame) 11)))
              ((symbol-function #'set-frame-size)
               (lambda (&rest args)
                 (setq resized args))))
      (should
       (vertico-buffer-frame--resize-frame-to-size 'frame '(42 . 11)))
      (should-not resized))))

(ert-deftest vertico-buffer-frame-resize-frame-to-size-preserves-gtk-setting ()
  (let ((vertico-buffer-frame--gtk-resize-child-frames nil)
        (x-gtk-resize-child-frames 'resize-mode)
        observed)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (eq frame 'frame)))
              ((symbol-function #'frame-width)
               (lambda (frame)
                 (and (eq frame 'frame) 41)))
              ((symbol-function #'frame-height)
               (lambda (frame)
                 (and (eq frame 'frame) 10)))
              ((symbol-function #'set-frame-size)
               (lambda (&rest _args)
                 (setq observed x-gtk-resize-child-frames))))
      (should
       (vertico-buffer-frame--resize-frame-to-size 'frame '(42 . 11)))
      (should (eq observed 'resize-mode)))))

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

(ert-deftest vertico-buffer-frame-place-candidate-frame-ignores-errors ()
  (cl-letf (((symbol-function #'frame-pixel-width)
             (lambda (_frame)
               100))
            ((symbol-function #'frame-pixel-height)
             (lambda (_frame)
               50))
            ((symbol-function #'set-frame-position)
             (lambda (&rest _args)
               (error "Place failed"))))
    (should-not
     (vertico-buffer-frame--place-candidate-frame 'child 'parent))))

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

(ert-deftest vertico-buffer-frame-place-preview-frame-follows-candidate ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'candidate)
    (let (position)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame '(candidate parent))))
                ((symbol-function #'frame-parameter)
                 (lambda (frame parameter)
                   (and (eq frame 'candidate)
                        (eq parameter 'parent-frame)
                        'parent)))
                ((symbol-function #'frame-position)
                 (lambda (frame)
                   (and (eq frame 'candidate)
                        '(300 . 200))))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (pcase frame
                     ('parent 1000)
                     ('candidate 500)
                     ('preview 200))))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (pcase frame
                     ('parent 620)
                     ('candidate 300)
                     ('preview 120))))
                ((symbol-function #'set-frame-position)
                 (lambda (frame left top)
                   (setq position (list frame left top)))))
        (vertico-buffer-frame--place-preview-frame 'preview 'parent)
        (should (equal position '(preview 592 372)))))))

(ert-deftest vertico-buffer-frame-place-preview-frame-ignores-errors ()
  (cl-letf (((symbol-function #'frame-pixel-width)
             (lambda (_frame)
               100))
            ((symbol-function #'frame-pixel-height)
             (lambda (_frame)
               50))
            ((symbol-function #'set-frame-position)
             (lambda (&rest _args)
               (error "Place failed"))))
    (should-not
     (vertico-buffer-frame--place-preview-frame 'preview 'parent))))

;;;; Hook setup and minibuffer cleanup

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
      (should (memq #'vertico-buffer-frame--candidate-post-command
                    post-command-hook))
      (should (memq #'vertico-buffer-frame--preview-post-command
                    post-command-hook)))))

(ert-deftest vertico-buffer-frame-minibuffer-setup-tolerates-improper-local-hooks ()
  (with-temp-buffer
    (setq-local post-command-hook '(ignore . bad))
    (let ((vertico-buffer-frame-mode t))
      (vertico-buffer-frame--minibuffer-setup)
      (should (proper-list-p post-command-hook))
      (should (memq #'ignore post-command-hook))
      (should (memq #'vertico-buffer-frame--candidate-post-command
                    post-command-hook))
      (should (memq #'vertico-buffer-frame--preview-post-command
                    post-command-hook)))))

(ert-deftest vertico-buffer-frame-minibuffer-setup-installs-cleanup ()
  (let ((buffer (generate-new-buffer " *vbf-setup-cleanup*"))
        (old-hook minibuffer-exit-hook))
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil
                vertico-buffer-frame--minibuffers nil)
          (cl-letf (((symbol-function #'minibufferp)
                     (lambda (&optional _buffer)
                       t)))
            (with-current-buffer buffer
              (let ((vertico-buffer-frame-mode t))
                (vertico-buffer-frame--minibuffer-setup)
                (should vertico-buffer-frame--cleanup-function)
                (should (memq buffer vertico-buffer-frame--minibuffers))
                (should (memq #'vertico-buffer-frame--minibuffer-exit
                              minibuffer-exit-hook))
                (run-hooks 'minibuffer-exit-hook)
                (should-not vertico-buffer-frame--cleanup-function)
                (should-not (memq buffer
                                  vertico-buffer-frame--minibuffers))))))
      (setq minibuffer-exit-hook old-hook
            vertico-buffer-frame--minibuffers nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-install-cleanup-tolerates-improper-exit-hook ()
  (let ((buffer (generate-new-buffer " *vbf-setup-improper-cleanup*"))
        (old-hook minibuffer-exit-hook))
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook '(ignore . bad)
                vertico-buffer-frame--minibuffers nil)
          (with-current-buffer buffer
            (vertico-buffer-frame--install-cleanup)
            (should (proper-list-p minibuffer-exit-hook))
            (should (memq #'ignore minibuffer-exit-hook))
            (should (memq #'vertico-buffer-frame--minibuffer-exit
                          minibuffer-exit-hook))))
      (setq minibuffer-exit-hook old-hook
            vertico-buffer-frame--minibuffers nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-remove-hook-tolerates-improper-hook-list ()
  (let ((old-hook minibuffer-setup-hook))
    (unwind-protect
        (progn
          (setq minibuffer-setup-hook
                '(vertico-buffer-frame--minibuffer-setup . bad))
          (vertico-buffer-frame--remove-hook
           'minibuffer-setup-hook
           #'vertico-buffer-frame--minibuffer-setup)
          (should (proper-list-p minibuffer-setup-hook))
          (should-not minibuffer-setup-hook))
      (setq minibuffer-setup-hook old-hook))))

(ert-deftest vertico-buffer-frame-proper-list-prefix-handles-malformed-lists ()
  (let ((hook (list #'ignore #'identity)))
    (should (equal (vertico-buffer-frame--proper-list-prefix
                    '(ignore identity . bad))
                   '(ignore identity)))
    (setcdr (cdr hook) hook)
    (should (equal (vertico-buffer-frame--proper-list-prefix hook)
                   (list #'ignore #'identity)))))

(ert-deftest vertico-buffer-frame-add-hook-tolerates-circular-hook-list ()
  (let ((old-hook minibuffer-setup-hook)
        (hook (list #'ignore)))
    (unwind-protect
        (progn
          (setcdr hook hook)
          (setq minibuffer-setup-hook hook)
          (vertico-buffer-frame--add-hook
           'minibuffer-setup-hook
           #'vertico-buffer-frame--minibuffer-setup)
          (should (proper-list-p minibuffer-setup-hook))
          (should (memq #'ignore minibuffer-setup-hook))
          (should (memq #'vertico-buffer-frame--minibuffer-setup
                        minibuffer-setup-hook)))
      (setq minibuffer-setup-hook old-hook))))

(ert-deftest vertico-buffer-frame-add-hook-tolerates-unbound-local-hook ()
  (with-temp-buffer
    (let ((hook 'vertico-buffer-frame-test-unbound-hook))
      (when (boundp hook)
        (makunbound hook))
      (vertico-buffer-frame--add-hook hook #'ignore nil t)
      (should (equal (symbol-value hook) '(ignore t))))))

(ert-deftest vertico-buffer-frame-display-buffer-reuses-live-window ()
  (with-temp-buffer
    (let ((created nil)
          (deleted nil)
          (buffers nil)
          window-buffer
          resized
          placed)
      (cl-letf (((symbol-function #'vertico-buffer-frame--parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--display-buffer-in-child-frame)
                 (lambda (buffer _parent _name _size role &optional _alist)
                   (should (eq role 'candidate))
                   (let ((frame (intern (format "frame-%s"
                                                (1+ (length created))))))
                     (push frame created)
                     (setq window-buffer buffer)
                     (intern (format "window-%s"
                                     (substring (symbol-name frame) 6))))))
                ((symbol-function #'vertico-buffer-frame--candidate-frame-size)
                 (lambda (_parent)
                   '(80 . 10)))
                ((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame created)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window-1)))
                ((symbol-function #'window-frame)
                 (lambda (window)
                   (and (eq window 'window-1) 'frame-1)))
                ((symbol-function #'window-buffer)
                 (lambda (window)
                   (and (eq window 'window-1) window-buffer)))
                ((symbol-function #'frame-parameter)
                 (lambda (_frame parameter)
                   (and (eq parameter 'parent-frame)
                        'parent)))
                ((symbol-function #'frame-pixel-width)
                 (lambda (_frame)
                   100))
                ((symbol-function #'frame-pixel-height)
                 (lambda (_frame)
                   50))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (when frame
                     (push frame deleted))))
                ((symbol-function #'vertico-buffer-frame--resize-frame-to-size)
                 (lambda (frame size)
                   (push (list frame size) resized)))
                ((symbol-function #'set-window-dedicated-p)
                 (lambda (&rest _args)))
                ((symbol-function #'set-window-buffer)
                 (lambda (window buffer)
                   (push (list window buffer) buffers)
                   (setq window-buffer buffer)))
                ((symbol-function #'vertico-buffer-frame--install-cleanup)
                 (lambda ()))
                ((symbol-function #'vertico-buffer-frame--place-candidate-frame)
                 (lambda (frame parent)
                   (push (list frame parent) placed))))
        (should (eq (vertico-buffer-frame--display-buffer 'buffer-a nil)
                    'window-1))
        (should (eq (vertico-buffer-frame--display-buffer 'buffer-b nil)
                    'window-1))
        (should (eq (vertico-buffer-frame--display-buffer 'buffer-b nil)
                    'window-1))
        (should (equal created '(frame-1)))
        (should-not deleted)
        (should (equal buffers '((window-1 buffer-b))))
        (should (equal resized '((frame-1 (80 . 10)))))
        (should (equal placed '((frame-1 parent))))))))

(ert-deftest vertico-buffer-frame-display-buffer-reports-fallback-error ()
  (let ((buffer (generate-new-buffer " *vbf-fallback-report*"))
        (fallback-window (selected-window))
        (vertico-buffer-frame-report-display-errors t)
        (vertico-buffer-frame--last-display-error-message nil)
        fallback
        reported)
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--parent-frame)
                   (lambda ()
                     (error "Boom")))
                  ((symbol-function #'display-buffer-use-least-recent-window)
                   (lambda (buffer alist)
                     (setq fallback (list buffer alist))
                     fallback-window))
                  ((symbol-function #'message)
                   (lambda (format-string &rest args)
                     (setq reported (apply #'format format-string args)))))
          (should (eq (vertico-buffer-frame--display-buffer
                       buffer
                       '((inhibit-same-window . t)))
                      fallback-window))
          (should (equal fallback
                         (list buffer '((inhibit-same-window . t)))))
          (should (equal reported
                         "vertico-buffer-frame display error: Boom")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-show-frame-ignores-frame-errors ()
  (let (shown)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (eq frame 'frame)))
              ((symbol-function #'frame-visible-p)
               (lambda (_frame)
                 nil))
              ((symbol-function #'make-frame-visible)
               (lambda (frame)
                 (push frame shown)
                 (error "Show failed"))))
      (should-not (vertico-buffer-frame--show-frame 'frame))
      (should (equal shown '(frame))))))

(ert-deftest vertico-buffer-frame-hide-frame-ignores-frame-errors ()
  (let (hidden)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (eq frame 'frame)))
              ((symbol-function #'frame-visible-p)
               (lambda (_frame)
                 t))
              ((symbol-function #'make-frame-invisible)
               (lambda (frame)
                 (push frame hidden)
                 (error "Hide failed"))))
      (should-not (vertico-buffer-frame--hide-frame 'frame))
      (should (equal hidden '(frame))))))

(ert-deftest vertico-buffer-frame-hide-frame-restores-parent-focus ()
  (let (selected hidden)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(child parent))))
              ((symbol-function #'selected-frame)
               (lambda ()
                 'child))
              ((symbol-function #'frame-visible-p)
               (lambda (frame)
                 (eq frame 'child)))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (and (eq frame 'child)
                      (eq parameter 'parent-frame)
                      'parent)))
              ((symbol-function #'select-frame-set-input-focus)
               (lambda (frame)
                 (setq selected frame)))
              ((symbol-function #'make-frame-invisible)
               (lambda (frame)
                 (push frame hidden))))
      (should (vertico-buffer-frame--hide-frame 'child))
      (should (eq selected 'parent))
      (should (equal hidden '(child))))))

(ert-deftest vertico-buffer-frame-hide-frame-restores-parent-focus-when-hidden ()
  (let (selected hidden)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(child parent))))
              ((symbol-function #'selected-frame)
               (lambda ()
                 'child))
              ((symbol-function #'frame-visible-p)
               (lambda (_frame)
                 nil))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (and (eq frame 'child)
                      (eq parameter 'parent-frame)
                      'parent)))
              ((symbol-function #'select-frame-set-input-focus)
               (lambda (frame)
                 (setq selected frame)))
              ((symbol-function #'make-frame-invisible)
               (lambda (frame)
                 (push frame hidden))))
      (should-not (vertico-buffer-frame--hide-frame 'child))
      (should (eq selected 'parent))
      (should-not hidden))))

(ert-deftest vertico-buffer-frame-delete-frame-restores-parent-focus ()
  (let (selected deleted)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(child parent))))
              ((symbol-function #'selected-frame)
               (lambda ()
                 'child))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (and (eq frame 'child)
                      (eq parameter 'parent-frame)
                      'parent)))
              ((symbol-function #'select-frame-set-input-focus)
               (lambda (frame)
                 (setq selected frame)))
              ((symbol-function #'delete-frame)
               (lambda (frame force)
                 (setq deleted (list frame force)))))
      (vertico-buffer-frame--delete-frame 'child)
      (should (eq selected 'parent))
      (should (equal deleted '(child t))))))

(ert-deftest vertico-buffer-frame-sync-layout-keeps-state-on-place-error ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'frame
                vertico-buffer-frame--candidate-window 'window
                vertico-buffer-frame--candidate-layout-state nil)
    (let (resized placed)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (pcase frame
                     ('parent 100)
                     ('frame 80))))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (pcase frame
                     ('parent 50)
                     ('frame 20))))
                ((symbol-function #'vertico-buffer-frame--resize-frame-to-size)
                 (lambda (frame size)
                   (push (list frame size) resized)
                   t))
                ((symbol-function #'vertico-buffer-frame--place-candidate-frame)
                 (lambda (frame parent)
                   (push (list frame parent) placed)
                   nil)))
        (should-not
         (vertico-buffer-frame--sync-candidate-frame-layout 'parent 'size))
        (should (equal resized '((frame size))))
        (should (equal placed '((frame parent))))
        (should-not vertico-buffer-frame--candidate-layout-state)))))

(ert-deftest vertico-buffer-frame-refresh-candidate-layout-uses-fit-size ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'frame
                vertico-buffer-frame--candidate-window 'window)
    (let (synced)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'frame-parameter)
                 (lambda (_frame parameter)
                   (and (eq parameter 'parent-frame)
                        'parent)))
                ((symbol-function #'vertico-buffer-frame--candidate-frame-size)
                 (lambda (parent)
                   (should (eq parent 'parent))
                   '(80 . 10)))
                ((symbol-function
                  #'vertico-buffer-frame--candidate-frame-size-to-fit)
                 (lambda (parent window size)
                   (should (eq parent 'parent))
                   (should (eq window 'window))
                   (should (equal size '(80 . 10)))
                   '(95 . 10)))
                ((symbol-function #'vertico-buffer-frame--sync-candidate-frame-layout)
                 (lambda (parent size)
                   (setq synced (list parent size)))))
        (vertico-buffer-frame--refresh-candidate-frame-layout)
        (should (equal synced '(parent (95 . 10))))))))

(ert-deftest vertico-buffer-frame-candidate-post-command-shows-live-frame ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'frame
                vertico-buffer-frame--candidate-window 'window)
    (let (events)
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--refresh-candidate-frame-layout)
                 (lambda ()
                   (push 'refresh events)))
                ((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'vertico-buffer-frame--show-frame)
                 (lambda (frame)
                   (push (list 'show frame) events))))
        (vertico-buffer-frame--candidate-post-command)
        (should (equal (nreverse events)
                       '(refresh (show frame))))))))

(ert-deftest vertico-buffer-frame-delete-owned-frames-deletes-package-frames ()
  (let (deleted)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(candidate-frame preview-frame))))
              ((symbol-function #'frame-list)
               (lambda ()
                 '(candidate-frame other-frame preview-frame)))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (cond
                  ((eq parameter 'vertico-buffer-frame-owner)
                   (memq frame '(candidate-frame preview-frame)))
                  ((eq parameter 'vertico-buffer-frame-role)
                   (pcase frame
                     ('candidate-frame 'candidate)
                     ('preview-frame 'preview))))))
              ((symbol-function #'delete-frame)
               (lambda (frame force)
                 (push (list frame force) deleted))))
      (vertico-buffer-frame--delete-owned-frames)
      (should (equal deleted
                     '((candidate-frame t)
                       (preview-frame t)))))))

(ert-deftest vertico-buffer-frame-delete-frame-ignores-delete-errors ()
  (let (deleted)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (eq frame 'bad-frame)))
              ((symbol-function #'delete-frame)
               (lambda (frame force)
                 (push (list frame force) deleted)
                 (error "Cannot delete frame"))))
      (should-not (vertico-buffer-frame--delete-frame 'bad-frame))
      (should (equal deleted '((bad-frame t)))))))

(ert-deftest vertico-buffer-frame-delete-owned-frames-uses-owner-buffer ()
  (let (deleted)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(candidate-frame preview-frame))))
              ((symbol-function #'frame-list)
               (lambda ()
                 '(candidate-frame other-frame preview-frame)))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (cond
                  ((eq parameter
                       vertico-buffer-frame--owner-buffer-parameter)
                   (memq frame '(candidate-frame preview-frame)))
                  ((eq parameter 'vertico-buffer-frame-role)
                   (pcase frame
                     ('candidate-frame 'candidate)
                     ('preview-frame 'preview))))))
              ((symbol-function #'delete-frame)
               (lambda (frame force)
                 (push (list frame force) deleted))))
      (vertico-buffer-frame--delete-owned-frames)
      (should (equal deleted
                     '((candidate-frame t)
                       (preview-frame t)))))))

(ert-deftest vertico-buffer-frame-delete-frames-owned-by-buffer-ignores-nil ()
  (let (deleted)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(main-frame child-frame))))
              ((symbol-function #'frame-list)
               (lambda ()
                 '(main-frame child-frame)))
              ((symbol-function #'frame-parameter)
               (lambda (_frame _parameter)
                 nil))
              ((symbol-function #'delete-frame)
               (lambda (frame force)
                 (push (list frame force) deleted))))
      (vertico-buffer-frame--delete-frames-owned-by-buffer nil)
      (should-not deleted))))

(ert-deftest vertico-buffer-frame-delete-owned-frames-ignores-stale-frames ()
  (let (deleted)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(stale-frame owned-frame))))
              ((symbol-function #'frame-list)
               (lambda ()
                 '(stale-frame owned-frame)))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (cond
                  ((eq frame 'stale-frame)
                   (error "Stale frame"))
                  ((and (eq frame 'owned-frame)
                        (eq parameter
                            vertico-buffer-frame--owner-buffer-parameter))
                   'owner))))
              ((symbol-function #'delete-frame)
               (lambda (frame force)
                 (push (list frame force) deleted))))
      (vertico-buffer-frame--delete-owned-frames)
      (should (equal deleted '((owned-frame t)))))))

(ert-deftest vertico-buffer-frame-candidate-window-live-p-ignores-live-check-errors ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'stale-frame
                vertico-buffer-frame--candidate-window 'stale-window)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (_frame)
                 (error "Stale frame")))
              ((symbol-function #'window-live-p)
               (lambda (_window)
                 (error "Stale window"))))
      (should-not (vertico-buffer-frame--candidate-window-live-p)))))

(ert-deftest vertico-buffer-frame-mode-disable-deletes-owned-frames ()
  (vertico-buffer-frame-test--with-clean-state
   (let (deleted)
     (cl-letf (((symbol-function #'vertico-buffer-frame--delete-owned-frames)
                (lambda ()
                  (setq deleted t))))
       (vertico-buffer-frame-mode 1)
       (vertico-buffer-frame-mode -1)
       (should deleted)))))

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
                     (when frame
                       (push frame deleted)))))
          (with-current-buffer buffer-a
            (setq-local vertico-buffer-frame--candidate-frame 'a-candidate
                        vertico-buffer-frame--preview-frame 'a-preview
                        vertico-buffer-frame--consult-imenu-entry-table-cache
                        '(entries . table)))
          (with-current-buffer buffer-b
            (setq-local vertico-buffer-frame--candidate-frame 'b-candidate
                        vertico-buffer-frame--preview-frame 'b-preview))
          (setq vertico-buffer-frame--minibuffers
                (list buffer-a buffer-b))
          (vertico-buffer-frame--cleanup-minibuffer buffer-a)
          (should (equal deleted
                         '(a-candidate a-preview)))
          (with-current-buffer buffer-a
            (should-not
             vertico-buffer-frame--consult-imenu-entry-table-cache))
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

(ert-deftest vertico-buffer-frame-cleanup-hook-uses-active-minibuffer ()
  "Regression: abnormal minibuffer exit may run outside the minibuffer buffer."
  (let ((outer (generate-new-buffer " *vbf-active-outer*"))
        (inner (generate-new-buffer " *vbf-active-inner*"))
        (old-hook minibuffer-exit-hook)
        active-buffer
        deleted)
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil
                vertico-buffer-frame--minibuffers nil)
          (cl-letf (((symbol-function #'active-minibuffer-window)
                     (lambda ()
                       'active-minibuffer-window))
                    ((symbol-function #'window-live-p)
                     (lambda (window)
                       (eq window 'active-minibuffer-window)))
                    ((symbol-function #'window-buffer)
                     (lambda (window)
                       (and (eq window 'active-minibuffer-window)
                            active-buffer)))
                    ((symbol-function #'vertico-buffer-frame--delete-frame)
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
            (setq active-buffer inner)
            (with-temp-buffer
              (run-hooks 'minibuffer-exit-hook))
            (should (equal deleted '(inner-candidate)))
            (with-current-buffer outer
              (should (eq vertico-buffer-frame--candidate-frame
                          'outer-candidate)))
            (setq active-buffer outer)
            (with-temp-buffer
              (run-hooks 'minibuffer-exit-hook))
            (should (equal deleted
                           '(outer-candidate inner-candidate)))))
      (setq minibuffer-exit-hook old-hook
            vertico-buffer-frame--minibuffers nil)
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list outer inner)))))

(ert-deftest vertico-buffer-frame-cleanup-hook-uses-selected-owned-frame ()
  "Regression: mouse exit may leave the candidate child frame selected."
  (let ((outer (generate-new-buffer " *vbf-selected-outer*"))
        (inner (generate-new-buffer " *vbf-selected-inner*"))
        (old-hook minibuffer-exit-hook)
        selected-frame
        deleted)
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil
                vertico-buffer-frame--minibuffers nil)
          (cl-letf (((symbol-function #'selected-frame)
                     (lambda ()
                       selected-frame))
                    ((symbol-function #'frame-live-p)
                     (lambda (frame)
                       (memq frame '(outer-frame inner-frame))))
                    ((symbol-function #'frame-parameter)
                     (lambda (frame parameter)
                       (and (eq parameter
                                vertico-buffer-frame--owner-buffer-parameter)
                            (pcase frame
                              ('outer-frame outer)
                              ('inner-frame inner)))))
                    ((symbol-function #'vertico-buffer-frame--delete-frame)
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
            (setq selected-frame 'inner-frame)
            (with-temp-buffer
              (run-hooks 'minibuffer-exit-hook))
            (should (equal deleted '(inner-candidate)))
            (with-current-buffer outer
              (should (eq vertico-buffer-frame--candidate-frame
                          'outer-candidate)))
            (setq selected-frame 'outer-frame)
            (with-temp-buffer
              (run-hooks 'minibuffer-exit-hook))
            (should (equal deleted
                           '(outer-candidate inner-candidate)))))
      (setq minibuffer-exit-hook old-hook
            vertico-buffer-frame--minibuffers nil)
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list outer inner)))))

(ert-deftest vertico-buffer-frame-active-minibuffer-buffer-skips-dead-window ()
  (let (window-buffer-called)
    (cl-letf (((symbol-function #'active-minibuffer-window)
               (lambda ()
                 'dead-window))
              ((symbol-function #'window-live-p)
               (lambda (_window)
                 nil))
              ((symbol-function #'window-buffer)
               (lambda (_window)
                 (setq window-buffer-called t)
                 'buffer)))
      (should-not (vertico-buffer-frame--active-minibuffer-buffer))
      (should-not window-buffer-called))))

(ert-deftest vertico-buffer-frame-cleanup-hook-drops-dead-minibuffer ()
  "Dead minibuffer buffers must not remain in the tracked buffer list."
  (let ((buffer (generate-new-buffer " *vbf-dead-minibuffer*"))
        (old-hook minibuffer-exit-hook))
    (unwind-protect
        (progn
          (setq minibuffer-exit-hook nil
                vertico-buffer-frame--minibuffers nil)
          (with-current-buffer buffer
            (vertico-buffer-frame--install-cleanup))
          (should (memq buffer vertico-buffer-frame--minibuffers))
          (kill-buffer buffer)
          (with-temp-buffer
            (run-hooks 'minibuffer-exit-hook))
          (should-not (memq buffer vertico-buffer-frame--minibuffers)))
      (setq minibuffer-exit-hook old-hook
            vertico-buffer-frame--minibuffers nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-cleanup-dead-minibuffer-deletes-owned-frames ()
  "Dead minibuffer buffers can still have live child frames to reclaim."
  (let ((buffer (generate-new-buffer " *vbf-dead-owner*"))
        (preview-buffer (generate-new-buffer " *vbf-dead-owner-preview*"))
        deleted)
    (unwind-protect
        (progn
          (setq vertico-buffer-frame--minibuffers (list buffer))
          (with-current-buffer preview-buffer
            (setq-local vertico-buffer-frame--temporary-preview-buffer t
                        vertico-buffer-frame--preview-owner-buffer buffer)
            (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (cl-letf (((symbol-function #'frame-list)
                     (lambda ()
                       '(owned-candidate owned-preview other-frame)))
                    ((symbol-function #'frame-parameter)
                     (lambda (frame parameter)
                       (and (memq frame '(owned-candidate owned-preview))
                            (cond
                             ((eq parameter
                                  vertico-buffer-frame--owner-buffer-parameter)
                              buffer)
                             ((eq parameter 'vertico-buffer-frame-role)
                              (pcase frame
                                ('owned-candidate 'candidate)
                                ('owned-preview 'preview)))))))
                    ((symbol-function #'frame-live-p)
                     (lambda (frame)
                       (memq frame '(owned-candidate owned-preview
                                                     other-frame))))
                    ((symbol-function #'delete-frame)
                     (lambda (frame force)
                       (push (list frame force) deleted))))
            (vertico-buffer-frame--cleanup-minibuffer buffer)
            (should (equal deleted
                           '((owned-candidate t)
                             (owned-preview t))))
            (should-not (buffer-live-p preview-buffer))
            (should-not (memq buffer vertico-buffer-frame--minibuffers))))
      (setq vertico-buffer-frame--minibuffers nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p preview-buffer)
        (kill-buffer preview-buffer)))))

(ert-deftest vertico-buffer-frame-cleanup-live-minibuffer-deletes-owned-orphans ()
  "Live minibuffer cleanup also reclaims owner-tagged orphan frames."
  (let ((buffer (generate-new-buffer " *vbf-live-owner*"))
        deleted)
    (unwind-protect
        (progn
          (setq vertico-buffer-frame--minibuffers (list buffer))
          (cl-letf (((symbol-function #'frame-list)
                     (lambda ()
                       '(stray-frame other-frame)))
                    ((symbol-function #'frame-parameter)
                     (lambda (frame parameter)
                       (and (eq frame 'stray-frame)
                            (eq parameter
                                vertico-buffer-frame--owner-buffer-parameter)
                            buffer)))
                    ((symbol-function #'frame-live-p)
                     (lambda (frame)
                       (eq frame 'stray-frame)))
                    ((symbol-function #'delete-frame)
                     (lambda (frame force)
                       (push (list frame force) deleted))))
            (vertico-buffer-frame--cleanup-minibuffer buffer)
            (should (equal deleted '((stray-frame t))))
            (should-not (memq buffer vertico-buffer-frame--minibuffers))))
      (setq vertico-buffer-frame--minibuffers nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-cleanup-deletes-without-scheduling-timer ()
  "Regression: minibuffer exit must not schedule delayed frame cleanup."
  (let ((buffer (generate-new-buffer " *vbf-cleanup*"))
        scheduled
        events)
    (unwind-protect
        (cl-letf (((symbol-function #'frame-live-p)
                   (lambda (frame)
                     (memq frame '(candidate-frame preview-frame))))
                  ((symbol-function #'run-at-time)
                   (lambda (&rest args)
                     (push args scheduled)
                     'timer))
                  ((symbol-function #'delete-frame)
                   (lambda (frame &rest _args)
                     (push (list 'delete frame) events)))
                  ((symbol-function #'vertico-buffer-frame--hide-frame)
                   (lambda (frame)
                     (push (list 'hide frame) events))))
          (with-current-buffer buffer
            (setq-local vertico-buffer-frame--candidate-frame
                        'candidate-frame
                        vertico-buffer-frame--preview-frame
                        'preview-frame))
          (vertico-buffer-frame--cleanup-minibuffer buffer)
          (should-not scheduled)
          (should (equal (nreverse events)
                         '((hide preview-frame)
                           (delete preview-frame)
                           (delete candidate-frame)))))
      (setq vertico-buffer-frame--minibuffers nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-cleanup-clears-improper-minibuffer-list ()
  (let ((vertico-buffer-frame--minibuffers '(minibuffer . bad)))
    (vertico-buffer-frame-cleanup)
    (should-not vertico-buffer-frame--minibuffers)))

(ert-deftest vertico-buffer-frame-cleanup-minibuffer-clears-improper-minibuffer-list ()
  (let ((buffer (generate-new-buffer " *vbf-direct-cleanup*"))
        (vertico-buffer-frame--minibuffers '(minibuffer . bad)))
    (unwind-protect
        (progn
          (vertico-buffer-frame--cleanup-minibuffer buffer)
          (should-not vertico-buffer-frame--minibuffers))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;;; File preview rendering and caching

(ert-deftest vertico-buffer-frame-file-preview-buffer-contains-full-file ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (setq-local vertico-buffer-frame--preview-buffer nil)
          (with-temp-file file
            (insert "abcdef"))
          (let ((preview (vertico-buffer-frame--file-preview-buffer file)))
            (with-current-buffer preview
              (should-not (buffer-modified-p))
              (should (equal (buffer-string) "abcdef"))))
          (when (buffer-live-p vertico-buffer-frame--preview-buffer)
            (kill-buffer vertico-buffer-frame--preview-buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-truncates-large-file ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (setq-local vertico-buffer-frame--preview-buffer nil)
          (with-temp-file file
            (insert "abcdef"))
          (let ((vertico-buffer-frame-preview-max-file-size 3))
            (let ((preview (vertico-buffer-frame--file-preview-buffer file)))
              (with-current-buffer preview
                (should (string-prefix-p "abc" (buffer-string)))
                (should (string-match-p
                         "Preview truncated at 3 of 6 bytes"
                         (buffer-string))))))
          (when (buffer-live-p vertico-buffer-frame--preview-buffer)
            (kill-buffer vertico-buffer-frame--preview-buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-line-preview-starts-at-large-target ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (unwind-protect
              (progn
                (setq-local vertico-buffer-frame--preview-buffer nil)
                (with-temp-file file
                  (insert "one\n")
                  (insert "two\n")
                  (insert "three\n")
                  (insert "target\n")
                  (insert "five\n"))
                (let ((vertico-buffer-frame-preview-max-file-size 8))
                  (pcase-let ((`(,preview ,location ,external)
                               (vertico-buffer-frame--preview-target-display-spec
                                (list 'file-line file 4))))
                    (should-not external)
                    (should (equal location '(line . 1)))
                    (with-current-buffer preview
                      (should (string-prefix-p "target\n"
                                               (buffer-string)))
                      (should (string-match-p
                               "Preview starts at byte"
                               (buffer-string)))))))
            (vertico-buffer-frame--kill-preview-buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-position-preview-starts-at-large-target ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (unwind-protect
              (progn
                (setq-local vertico-buffer-frame--preview-buffer nil)
                (with-temp-file file
                  (insert "aaaaTARGETbbbb"))
                (let ((vertico-buffer-frame-preview-max-file-size 6))
                  (pcase-let ((`(,preview ,location ,external)
                               (vertico-buffer-frame--preview-target-display-spec
                                (list 'file-position file 5))))
                    (should-not external)
                    (should (equal location '(position . 1)))
                    (with-current-buffer preview
                      (should (string-prefix-p "TARGET"
                                               (buffer-string)))
                      (should (string-match-p
                               "Preview starts at byte"
                               (buffer-string)))))))
            (vertico-buffer-frame--kill-preview-buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-position-preview-handles-multibyte-prefix ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (unwind-protect
              (progn
                (setq-local vertico-buffer-frame--preview-buffer nil)
                (let ((coding-system-for-write 'utf-8-unix))
                  (with-temp-file file
                    (insert (string #x3042 #x3042) "TARGETbbbb")))
                (let ((vertico-buffer-frame-preview-max-file-size 6))
                  (pcase-let ((`(,preview ,location ,external)
                               (vertico-buffer-frame--preview-target-display-spec
                                (list 'file-position file 3))))
                    (should-not external)
                    (should (equal location '(position . 1)))
                    (with-current-buffer preview
                      (should (string-prefix-p "TARGET"
                                               (buffer-string)))
                      (should (string-match-p
                               "Preview starts at byte 6"
                               (buffer-string)))))))
            (vertico-buffer-frame--kill-preview-buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-position-preview-handles-dos-eol-prefix ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (unwind-protect
              (progn
                (setq-local vertico-buffer-frame--preview-buffer nil)
                (let ((coding-system-for-write 'utf-8-dos))
                  (with-temp-file file
                    (insert "one\ntarget\nextra")))
                (let ((vertico-buffer-frame-preview-max-file-size 8))
                  (pcase-let ((`(,preview ,location ,external)
                               (vertico-buffer-frame--preview-target-display-spec
                                (list 'file-position file 5))))
                    (should-not external)
                    (should (equal location '(position . 1)))
                    (with-current-buffer preview
                      (should (string-prefix-p "target"
                                               (buffer-string)))
                      (should (string-match-p
                               "Preview starts at byte 5"
                               (buffer-string)))))))
            (vertico-buffer-frame--kill-preview-buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-preview-limits-preserve-nil-and-sanitize-invalid ()
  (let ((vertico-buffer-frame-preview-max-file-size nil)
        (vertico-buffer-frame-preview-directory-entry-limit nil)
        (vertico-buffer-frame-preview-search-buffer-limit nil))
    (should-not (vertico-buffer-frame--preview-max-file-size))
    (should-not (vertico-buffer-frame--preview-directory-entry-limit))
    (should-not (vertico-buffer-frame--preview-search-buffer-limit))
    (should-not (vertico-buffer-frame--file-preview-limit 100)))
  (let ((vertico-buffer-frame-preview-max-file-size -1)
        (vertico-buffer-frame-preview-directory-entry-limit -2)
        (vertico-buffer-frame-preview-search-buffer-limit -3))
    (should (= (vertico-buffer-frame--preview-max-file-size) 0))
    (should (= (vertico-buffer-frame--preview-directory-entry-limit) 0))
    (should (= (vertico-buffer-frame--preview-search-buffer-limit) 0))
    (should (= (vertico-buffer-frame--file-preview-limit 100) 0)))
  (let ((vertico-buffer-frame-preview-max-file-size 'bad)
        (vertico-buffer-frame-preview-directory-entry-limit 'bad)
        (vertico-buffer-frame-preview-search-buffer-limit 'bad))
    (should (= (vertico-buffer-frame--preview-max-file-size)
               vertico-buffer-frame--default-preview-max-file-size))
    (should (= (vertico-buffer-frame--preview-directory-entry-limit)
               vertico-buffer-frame--default-preview-directory-entry-limit))
    (should (= (vertico-buffer-frame--preview-search-buffer-limit)
               vertico-buffer-frame--default-preview-search-buffer-limit))
    (should (= (vertico-buffer-frame--file-preview-limit 2000000)
               vertico-buffer-frame--default-preview-max-file-size))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-skips-binary-file ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (setq-local vertico-buffer-frame--preview-buffer nil)
          (with-temp-file file
            (insert "a" (string 0) "b"))
          (let ((preview (vertico-buffer-frame--file-preview-buffer file)))
            (with-current-buffer preview
              (should (string-match-p
                       "Binary file preview skipped"
                       (buffer-string)))))
          (when (buffer-live-p vertico-buffer-frame--preview-buffer)
            (kill-buffer vertico-buffer-frame--preview-buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-reports-read-error ()
  (let ((file (make-temp-file "vbf-preview"))
        preview)
    (unwind-protect
        (with-temp-buffer
          (setq-local vertico-buffer-frame--preview-buffer nil)
          (with-temp-file file
            (insert "abcdef"))
          (cl-letf (((symbol-function #'insert-file-contents)
                     (lambda (&rest _args)
                       (error "Read failed"))))
            (setq preview (vertico-buffer-frame--file-preview-buffer file))
            (with-current-buffer preview
              (should (string-match-p "File preview failed"
                                      (buffer-string)))
              (should (string-match-p "Read failed"
                                      (buffer-string))))))
      (when (buffer-live-p preview)
        (kill-buffer preview))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-does-not-cache-read-error ()
  (let ((file (make-temp-file "vbf-preview"))
        first
        second
        fail
        (original-insert-file-contents
         (symbol-function #'insert-file-contents)))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file file
            (insert "abcdef"))
          (cl-letf (((symbol-function #'insert-file-contents)
                     (lambda (&rest args)
                       (if fail
                           (error "Read failed")
                         (apply original-insert-file-contents args)))))
            (setq fail t
                  first (vertico-buffer-frame--file-preview-buffer file))
            (with-current-buffer first
              (should (string-match-p "File preview failed"
                                      (buffer-string))))
            (should-not vertico-buffer-frame--file-preview-cache)
            (setq fail nil
                  second (vertico-buffer-frame--file-preview-buffer file))
            (should (buffer-live-p second))
            (with-current-buffer second
              (should (equal (buffer-string) "abcdef")))
            (should vertico-buffer-frame--file-preview-cache)))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-cache-key-ignores-attribute-errors ()
  (cl-letf (((symbol-function #'file-attributes)
             (lambda (&rest _args)
               'bad-attributes)))
    (should-not
     (vertico-buffer-frame--file-preview-cache-key "/tmp/vbf-bad"))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-reuses-unchanged-file ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file file
            (insert "abcdef"))
          (let ((first (vertico-buffer-frame--file-preview-buffer file))
                (second (vertico-buffer-frame--file-preview-buffer file)))
            (should (eq first second))
            (mapc (lambda (buffer)
                    (when (buffer-live-p buffer)
                      (kill-buffer buffer)))
                  (list first second))))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-skips-foreign-cache ()
  (let ((file (make-temp-file "vbf-preview"))
        (foreign-owner (generate-new-buffer " *vbf-foreign-owner*"))
        (foreign (generate-new-buffer " *vbf-foreign-file-preview*"))
        new)
    (unwind-protect
        (with-temp-buffer
          (let ((owner (current-buffer)))
            (with-temp-file file
              (insert "abcdef"))
            (with-current-buffer foreign
              (setq-local vertico-buffer-frame--temporary-preview-buffer t
                          vertico-buffer-frame--preview-owner-buffer
                          foreign-owner)
              (set-buffer-modified-p nil))
            (setq-local vertico-buffer-frame--file-preview-cache
                        (cons (vertico-buffer-frame--file-preview-cache-key
                               file)
                              foreign))
            (setq new (vertico-buffer-frame--file-preview-buffer file))
            (should-not (eq new foreign))
            (should (buffer-live-p foreign))
            (should (eq vertico-buffer-frame--preview-buffer new))
            (with-current-buffer new
              (should (eq vertico-buffer-frame--preview-owner-buffer
                          owner))
              (should (equal (buffer-string) "abcdef")))))
      (dolist (buffer (list foreign-owner foreign new))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-file file))))

(ert-deftest vertico-buffer-frame-file-preview-buffer-refreshes-changed-file ()
  (let ((file (make-temp-file "vbf-preview")))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file file
            (insert "abcdef"))
          (let ((first (vertico-buffer-frame--file-preview-buffer file)))
            (with-temp-file file
              (insert "changed"))
            (let ((second (vertico-buffer-frame--file-preview-buffer file)))
              (should-not (eq first second))
              (with-current-buffer second
                (should (equal (buffer-string) "changed")))
              (mapc (lambda (buffer)
                      (when (buffer-live-p buffer)
                        (kill-buffer buffer)))
                    (list first second)))))
      (delete-file file))))

;;;; Preview refresh scheduling

(ert-deftest vertico-buffer-frame-preview-post-command-uses-delay ()
  (with-temp-buffer
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
                   (push timer canceled)))
                ((symbol-function
                  #'vertico-buffer-frame--completion-active-p)
                 (lambda ()
                   t)))
        (vertico-buffer-frame--preview-post-command)
        (should (equal scheduled
                       (list 0.2
                             nil
                             #'vertico-buffer-frame--show-preview-later
                             (list (current-buffer)))))
        (should (eq vertico-buffer-frame--preview-timer 'preview-timer))
        (setq scheduled nil)
        (vertico-buffer-frame--preview-post-command)
        (should-not canceled)
        (should-not scheduled)))))

(ert-deftest vertico-buffer-frame-preview-post-command-tolerates-invalid-delay ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-mode t)
          (vertico-buffer-frame-preview t)
          (vertico-buffer-frame-preview-delay "bad")
          scheduled)
      (cl-letf (((symbol-function #'run-with-idle-timer)
                 (lambda (delay repeat function &rest args)
                   (setq scheduled (list delay repeat function args))
                   'preview-timer))
                ((symbol-function
                  #'vertico-buffer-frame--completion-active-p)
                 (lambda ()
                   t)))
        (vertico-buffer-frame--preview-post-command)
        (should (equal scheduled
                       (list 0.0
                             nil
                             #'vertico-buffer-frame--show-preview-later
                             (list (current-buffer)))))))))

(ert-deftest vertico-buffer-frame-preview-post-command-schedules-input-changes ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-mode t)
          (vertico-buffer-frame-preview t)
          (vertico-buffer-frame-preview-delay 0.2)
          (input "ab")
          scheduled
          hidden)
      (cl-letf (((symbol-function #'vertico-buffer-frame--minibuffer-input)
                 (lambda ()
                   input))
                ((symbol-function #'vertico-buffer-frame--candidate)
                 (lambda ()
                   "candidate"))
                ((symbol-function #'vertico-buffer-frame--completion-active-p)
                 (lambda ()
                   t))
                ((symbol-function #'vertico-buffer-frame--hide-preview)
                 (lambda ()
                   (setq hidden t)))
                ((symbol-function #'run-with-idle-timer)
                 (lambda (delay repeat function &rest args)
                   (setq scheduled (list delay repeat function args))
                   'preview-timer)))
        (vertico-buffer-frame--preview-post-command)
        (should-not hidden)
        (should (equal scheduled
                       (list 0.2
                             nil
                             #'vertico-buffer-frame--show-preview-later
                             (list (current-buffer)))))))))

(ert-deftest vertico-buffer-frame-cancel-preview-timer-ignores-timer-errors ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-timer 'bad-timer
                vertico-buffer-frame--preview-scheduled-state 'state)
    (cl-letf (((symbol-function #'timerp)
               (lambda (&rest _args)
                 (error "Timerp failed"))))
      (vertico-buffer-frame--cancel-preview-timer))
    (should-not vertico-buffer-frame--preview-timer)
    (should-not vertico-buffer-frame--preview-scheduled-state))
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-timer 'bad-timer
                vertico-buffer-frame--preview-scheduled-state 'state)
    (cl-letf (((symbol-function #'timerp)
               (lambda (_timer)
                 t))
              ((symbol-function #'cancel-timer)
               (lambda (&rest _args)
                 (error "Cancel failed"))))
      (vertico-buffer-frame--cancel-preview-timer))
    (should-not vertico-buffer-frame--preview-timer)
    (should-not vertico-buffer-frame--preview-scheduled-state)))

(ert-deftest vertico-buffer-frame-schedule-preview-reports-timer-errors ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-report-errors t)
          messages
          hidden)
      (cl-letf (((symbol-function #'run-with-idle-timer)
                 (lambda (&rest _args)
                   (error "Timer failed")))
                ((symbol-function #'vertico-buffer-frame--hide-preview)
                 (lambda ()
                   (setq hidden t)))
                ((symbol-function #'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (vertico-buffer-frame--schedule-preview 'state)
        (should hidden)
        (should-not vertico-buffer-frame--preview-timer)
        (should-not vertico-buffer-frame--preview-scheduled-state)
        (should (equal messages
                       '("vertico-buffer-frame preview error: Timer failed")))))))

(ert-deftest vertico-buffer-frame-schedule-preview-ignores-timerp-errors ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-timer 'bad-timer
                vertico-buffer-frame--preview-scheduled-state 'state)
    (let ((vertico-buffer-frame-preview-delay "bad")
          scheduled)
      (cl-letf (((symbol-function #'timerp)
                 (lambda (&rest _args)
                   (error "Timerp failed")))
                ((symbol-function #'run-with-idle-timer)
                 (lambda (delay repeat function &rest args)
                   (setq scheduled (list delay repeat function args))
                   'new-timer)))
        (vertico-buffer-frame--schedule-preview 'state)
        (should (equal scheduled
                       (list 0.0
                             nil
                             #'vertico-buffer-frame--show-preview-later
                             (list (current-buffer)))))
        (should (eq vertico-buffer-frame--preview-timer 'new-timer))
        (should (eq vertico-buffer-frame--preview-scheduled-state 'state))))))

(ert-deftest vertico-buffer-frame-completion-state-helpers-ignore-errors ()
  (cl-letf (((symbol-function #'minibufferp)
             (lambda (&rest _args)
               (error "Minibuffer failed"))))
    (should-not (vertico-buffer-frame--completion-active-p))
    (should-not (vertico-buffer-frame--minibuffer-input)))
  (with-temp-buffer
    (let ((minibuffer-completion-table 'table)
          (minibuffer-completion-predicate nil))
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--completion-active-p)
                 (lambda ()
                   t))
                ((symbol-function #'completion-metadata)
                 (lambda (&rest _args)
                   (error "Metadata failed"))))
        (should-not (vertico-buffer-frame--category))))))

(ert-deftest vertico-buffer-frame-preview-post-command-reports-state-errors ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-mode t)
          (vertico-buffer-frame-preview t)
          (vertico-buffer-frame-preview-report-errors t)
          messages
          hidden)
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--completion-active-p)
                 (lambda ()
                   t))
                ((symbol-function #'vertico-buffer-frame--preview-state)
                 (lambda ()
                   (error "State failed")))
                ((symbol-function #'vertico-buffer-frame--hide-preview)
                 (lambda ()
                   (setq hidden t)))
                ((symbol-function #'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (vertico-buffer-frame--preview-post-command)
        (should hidden)
        (should (equal messages
                       '("vertico-buffer-frame preview error: State failed")))))))

(ert-deftest vertico-buffer-frame-preview-state-is-cheap ()
  (with-temp-buffer
    (cl-letf (((symbol-function #'vertico-buffer-frame--minibuffer-input)
               (lambda ()
                 "input"))
              ((symbol-function #'vertico-buffer-frame--candidate)
               (lambda ()
                 "candidate"))
              ((symbol-function #'vertico-buffer-frame--category)
               (lambda ()
                 (error "Category lookup should be delayed"))))
      (should (equal (vertico-buffer-frame--preview-state)
                     (list "input" (point) "candidate"))))))

(ert-deftest vertico-buffer-frame-preview-later-shows-target-each-time ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-mode t)
          (vertico-buffer-frame-preview t)
          shown)
      (cl-letf (((symbol-function #'vertico-buffer-frame--preview-target)
                 (lambda ()
                   '(buffer "vbf-target")))
                ((symbol-function
                  #'vertico-buffer-frame--completion-active-p)
                 (lambda ()
                   t))
                ((symbol-function
                  #'vertico-buffer-frame--child-frames-supported-p)
                 (lambda (_frame)
                   t))
                ((symbol-function #'vertico-buffer-frame--show-preview)
                 (lambda (target)
                   (push target shown))))
        (vertico-buffer-frame--show-preview-later (current-buffer))
        (vertico-buffer-frame--show-preview-later (current-buffer))
        (should (equal shown
                       '((buffer "vbf-target")
                         (buffer "vbf-target"))))))))

(ert-deftest vertico-buffer-frame-preview-later-hides-when-unsupported ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-mode t)
          (vertico-buffer-frame-preview t)
          hidden)
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--completion-active-p)
                 (lambda ()
                   t))
                ((symbol-function
                  #'vertico-buffer-frame--child-frames-supported-p)
                 (lambda (_frame)
                   nil))
                ((symbol-function #'vertico-buffer-frame--preview-target)
                 (lambda ()
                   (error "Target should not be resolved")))
                ((symbol-function #'vertico-buffer-frame--hide-preview)
                 (lambda ()
                   (setq hidden t))))
        (vertico-buffer-frame--show-preview-later (current-buffer))
        (should hidden)))))

(ert-deftest vertico-buffer-frame-preview-later-reports-error-once ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-mode t)
          (vertico-buffer-frame-preview t)
          (vertico-buffer-frame-preview-report-errors t)
          (debug-on-error nil)
          hidden
          messages)
      (cl-letf (((symbol-function #'vertico-buffer-frame--preview-target)
                 (lambda ()
                   (error "Boom")))
                ((symbol-function
                  #'vertico-buffer-frame--completion-active-p)
                 (lambda ()
                   t))
                ((symbol-function
                  #'vertico-buffer-frame--child-frames-supported-p)
                 (lambda (_frame)
                   t))
                ((symbol-function #'vertico-buffer-frame--hide-preview)
                 (lambda ()
                   (push 'hide hidden)))
                ((symbol-function #'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (vertico-buffer-frame--show-preview-later (current-buffer))
        (vertico-buffer-frame--show-preview-later (current-buffer))
        (should (equal hidden '(hide hide)))
        (should (equal messages
                       '("vertico-buffer-frame preview error: Boom")))))))

(ert-deftest vertico-buffer-frame-hide-preview-keeps-frame-for-reuse ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'preview-frame
                vertico-buffer-frame--preview-window 'preview-window
                vertico-buffer-frame--preview-layout-state 'layout
                vertico-buffer-frame--preview-buffer 'preview-buffer)
    (let (hidden deleted killed)
      (cl-letf (((symbol-function #'vertico-buffer-frame--cancel-preview-timer)
                 (lambda ()))
                ((symbol-function #'vertico-buffer-frame--hide-frame)
                 (lambda (frame)
                   (push frame hidden)))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (push frame deleted)))
                ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                 (lambda ()
                   (push vertico-buffer-frame--preview-buffer killed)
                   (setq-local vertico-buffer-frame--preview-buffer nil))))
        (vertico-buffer-frame--hide-preview)
        (should (equal hidden '(preview-frame)))
        (should-not deleted)
        (should-not killed)
        (should (eq vertico-buffer-frame--preview-frame 'preview-frame))
        (should (eq vertico-buffer-frame--preview-window 'preview-window))
        (should (eq vertico-buffer-frame--preview-layout-state 'layout))
        (should (eq vertico-buffer-frame--preview-buffer 'preview-buffer))))))

;;;; Preview frame lifecycle

(ert-deftest vertico-buffer-frame-preview-parent-frame-ignores-stale-candidate-frame ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'stale-frame)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (when (eq frame 'stale-frame)
                   (error "Stale frame"))
                 nil))
              ((symbol-function #'vertico-buffer-frame--parent-frame)
               (lambda ()
                 'parent-frame)))
      (should (eq (vertico-buffer-frame--preview-parent-frame)
                  'parent-frame)))))

(ert-deftest vertico-buffer-frame-preview-parent-frame-uses-candidate-parent ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'candidate-frame)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(candidate-frame parent-frame))))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (and (eq frame 'candidate-frame)
                      (eq parameter 'parent-frame)
                      'parent-frame)))
              ((symbol-function #'vertico-buffer-frame--parent-frame)
               (lambda ()
                 'fallback-frame)))
      (should (eq (vertico-buffer-frame--preview-parent-frame)
                  'parent-frame)))))

(ert-deftest vertico-buffer-frame-preview-size-source-uses-candidate ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--candidate-frame 'candidate-frame)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (frame)
                 (memq frame '(candidate-frame parent-frame))))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (and (eq frame 'candidate-frame)
                      (eq parameter 'parent-frame)
                      'parent-frame))))
      (should (eq (vertico-buffer-frame--preview-size-source-frame
                   'parent-frame)
                  'candidate-frame))
      (should (eq (vertico-buffer-frame--preview-size-source-frame
                   'other-parent)
                  'other-parent)))))

(ert-deftest vertico-buffer-frame-preview-window-reuses-live-frame ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'frame
                vertico-buffer-frame--preview-window 'window)
    (let (created
          (refreshed 0))
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'frame-parameter)
                 (lambda (_frame parameter)
                   (and (eq parameter 'parent-frame)
                        'parent)))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (pcase frame
                     ('frame 100)
                     ('window 80))))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (pcase frame
                     ('frame 50)
                     ('window 20))))
                ((symbol-function #'vertico-buffer-frame--display-buffer-in-child-frame)
                 (lambda (&rest _args)
                   (setq created t)
                   'new-window))
                ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                 (lambda ()
                   (cl-incf refreshed))))
        (should (eq (vertico-buffer-frame--preview-window) 'window))
        (should-not created)
        (should (= refreshed 1))))))

(ert-deftest vertico-buffer-frame-preview-window-displays-supplied-buffer ()
  (let ((preview (generate-new-buffer " *vbf-preview-initial*"))
        displayed
        killed)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function #'frame-live-p)
                     (lambda (_frame)
                       nil))
                    ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                     (lambda ()
                       'parent))
                    ((symbol-function #'vertico-buffer-frame--delete-frame)
                     (lambda (&rest _args)))
                    ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                     (lambda (&optional keep-buffer)
                       (setq killed keep-buffer)))
                    ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                     (lambda (_parent)
                       '(42 . 11)))
                    ((symbol-function
                      #'vertico-buffer-frame--display-buffer-in-child-frame)
                     (lambda (buffer parent _name size role)
                       (setq displayed (list buffer parent size role))
                       'new-window))
                    ((symbol-function #'vertico-buffer-frame--window-frame)
                     (lambda (_window)
                       'new-frame))
                    ((symbol-function #'set-window-parameter)
                     (lambda (&rest _args)))
                    ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                     (lambda (&rest _args)
                       t))
                    ((symbol-function #'vertico-buffer-frame--frame-layout-state)
                     (lambda (frame parent size)
                       (list frame parent size)))
                    ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                     (lambda ())))
            (should (eq (vertico-buffer-frame--preview-window preview)
                        'new-window))
            (should (eq killed preview))
            (should (eq (car displayed) preview))
            (should (equal (cdr displayed)
                           '(parent (42 . 11) preview)))))
      (when (buffer-live-p preview)
        (kill-buffer preview)))))

(ert-deftest vertico-buffer-frame-preview-window-recreates-for-new-parent ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'old-frame
                vertico-buffer-frame--preview-window 'old-window
                vertico-buffer-frame--preview-buffer 'old-buffer)
    (let (deleted killed created placed refreshed)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame '(old-frame new-frame))))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (memq window '(old-window new-window))))
                ((symbol-function #'frame-parameter)
                 (lambda (frame parameter)
                   (and (eq frame 'old-frame)
                        (eq parameter 'parent-frame)
                        'old-parent)))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'new-parent))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (push frame deleted)))
                ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                 (lambda (&optional keep-buffer)
                   (push (list vertico-buffer-frame--preview-buffer
                               keep-buffer)
                         killed)
                   (setq-local vertico-buffer-frame--preview-buffer nil)))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (parent)
                   (should (eq parent 'new-parent))
                   '(42 . 11)))
                ((symbol-function #'vertico-buffer-frame--display-buffer-in-child-frame)
                 (lambda (buffer parent _name size role)
                   (setq created (list buffer parent size role))
                   'new-window))
                ((symbol-function #'window-frame)
                 (lambda (window)
                   (and (eq window 'new-window) 'new-frame)))
                ((symbol-function #'set-window-parameter)
                 (lambda (&rest _args)))
                ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                 (lambda (frame parent)
                   (setq placed (list frame parent))))
                ((symbol-function #'vertico-buffer-frame--frame-layout-state)
                 (lambda (frame parent size)
                   (list frame parent size)))
                ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                 (lambda ()
                   (setq refreshed t))))
        (should (eq (vertico-buffer-frame--preview-window) 'new-window))
        (should (equal deleted '(old-frame)))
        (should (equal killed '((old-buffer nil))))
        (should (buffer-live-p (car created)))
        (should (equal (cdr created) '(new-parent (42 . 11) preview)))
        (should (equal placed '(new-frame new-parent)))
        (should (equal vertico-buffer-frame--preview-layout-state
                       '(new-frame new-parent (42 . 11) nil)))
        (should refreshed)))))

(ert-deftest vertico-buffer-frame-preview-window-keeps-layout-open-on-place-error ()
  (with-temp-buffer
    (let (placed refreshed)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'new-frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'new-window)))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (&rest _args)))
                ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                 (lambda (&optional _keep-buffer)))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (_parent)
                   '(42 . 11)))
                ((symbol-function #'vertico-buffer-frame--display-buffer-in-child-frame)
                 (lambda (_buffer _parent _name _size _role)
                   'new-window))
                ((symbol-function #'window-frame)
                 (lambda (window)
                   (and (eq window 'new-window) 'new-frame)))
                ((symbol-function #'set-window-parameter)
                 (lambda (&rest _args)))
                ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                 (lambda (frame parent)
                   (setq placed (list frame parent))
                   nil))
                ((symbol-function #'vertico-buffer-frame--frame-layout-state)
                 (lambda (&rest _args)
                   (error "Layout state should not be recorded")))
                ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                 (lambda ()
                   (setq refreshed t))))
        (should (eq (vertico-buffer-frame--preview-window) 'new-window))
        (should (equal placed '(new-frame parent)))
        (should-not vertico-buffer-frame--preview-layout-state)
        (should refreshed)))))

(ert-deftest vertico-buffer-frame-preview-window-recreates-on-stale-parent ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'old-frame
                vertico-buffer-frame--preview-window 'old-window)
    (let (deleted killed created refreshed)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame '(old-frame new-frame))))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (memq window '(old-window new-window))))
                ((symbol-function #'frame-parameter)
                 (lambda (frame _parameter)
                   (when (eq frame 'old-frame)
                     (error "Stale frame"))))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (push frame deleted)))
                ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                 (lambda (&optional keep-buffer)
                   (push keep-buffer killed)
                   (setq-local vertico-buffer-frame--preview-buffer nil)))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (parent)
                   (should (eq parent 'parent))
                   '(42 . 11)))
                ((symbol-function #'vertico-buffer-frame--display-buffer-in-child-frame)
                 (lambda (buffer parent _name size role)
                   (setq created (list buffer parent size role))
                   'new-window))
                ((symbol-function #'window-frame)
                 (lambda (window)
                   (and (eq window 'new-window) 'new-frame)))
                ((symbol-function #'set-window-parameter)
                 (lambda (&rest _args)))
                ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                 (lambda (&rest _args)
                   t))
                ((symbol-function #'vertico-buffer-frame--frame-layout-state)
                 (lambda (frame parent size)
                   (list frame parent size)))
                ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                 (lambda ()
                   (setq refreshed t))))
        (should (eq (vertico-buffer-frame--preview-window) 'new-window))
        (should (equal deleted '(old-frame)))
        (should (equal killed '(nil)))
        (should (buffer-live-p (car created)))
        (should (equal (cdr created) '(parent (42 . 11) preview)))
        (should refreshed)))))

(ert-deftest vertico-buffer-frame-preview-window-recreates-on-stale-live-check ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'old-frame
                vertico-buffer-frame--preview-window 'old-window)
    (let (deleted killed created refreshed)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame '(old-frame new-frame))))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (cond
                    ((eq window 'old-window)
                     (error "Stale window"))
                    ((eq window 'new-window)
                     t))))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (push frame deleted)))
                ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                 (lambda (&optional keep-buffer)
                   (push keep-buffer killed)
                   (setq-local vertico-buffer-frame--preview-buffer nil)))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (_parent)
                   '(42 . 11)))
                ((symbol-function #'vertico-buffer-frame--display-buffer-in-child-frame)
                 (lambda (buffer parent _name size role)
                   (setq created (list buffer parent size role))
                   'new-window))
                ((symbol-function #'window-frame)
                 (lambda (window)
                   (and (eq window 'new-window) 'new-frame)))
                ((symbol-function #'set-window-parameter)
                 (lambda (&rest _args)))
                ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                 (lambda (&rest _args)
                   t))
                ((symbol-function #'vertico-buffer-frame--frame-layout-state)
                 (lambda (frame parent size)
                   (list frame parent size)))
                ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                 (lambda ()
                   (setq refreshed t))))
        (should (eq (vertico-buffer-frame--preview-window) 'new-window))
        (should (equal deleted '(old-frame)))
        (should (equal killed '(nil)))
        (should (buffer-live-p (car created)))
        (should (equal (cdr created) '(parent (42 . 11) preview)))
        (should refreshed)))))

(ert-deftest vertico-buffer-frame-preview-window-clears-layout-on-size-error ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'old-frame
                vertico-buffer-frame--preview-window 'old-window
                vertico-buffer-frame--preview-layout-state 'old-layout)
    (let (deleted killed)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'old-frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'old-window)))
                ((symbol-function #'frame-parameter)
                 (lambda (_frame _parameter)
                   'old-parent))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'new-parent))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (push frame deleted)))
                ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                 (lambda (&optional keep-buffer)
                   (push keep-buffer killed)
                   (setq-local vertico-buffer-frame--preview-buffer nil)))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (_parent)
                   (error "Size failed"))))
        (should-error
         (vertico-buffer-frame--preview-window)
         :type 'error)
        (should (equal deleted '(old-frame)))
        (should (equal killed '(nil)))
        (should-not vertico-buffer-frame--preview-frame)
        (should-not vertico-buffer-frame--preview-window)
        (should-not vertico-buffer-frame--preview-layout-state)))))

(ert-deftest vertico-buffer-frame-preview-window-deletes-frame-on-setup-error ()
  (with-temp-buffer
    (let (deleted)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (_frame)
                   nil))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'new-window)))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (when frame
                     (push frame deleted))))
                ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                 (lambda (&optional _keep-buffer)
                   (setq-local vertico-buffer-frame--preview-buffer nil)))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (parent)
                   (should (eq parent 'parent))
                   '(42 . 11)))
                ((symbol-function #'vertico-buffer-frame--display-buffer-in-child-frame)
                 (lambda (_buffer _parent _name _size _role)
                   'new-window))
                ((symbol-function #'window-frame)
                 (lambda (window)
                   (and (eq window 'new-window) 'new-frame)))
                ((symbol-function #'set-window-parameter)
                 (lambda (&rest _args)
                   (error "Parameter failed")))
                ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                 (lambda ()
                   (error "Refresh should not run"))))
        (should-error
         (vertico-buffer-frame--preview-window)
         :type 'error)
        (should (equal deleted '(new-frame)))
        (should-not vertico-buffer-frame--preview-frame)
        (should-not vertico-buffer-frame--preview-window)
        (should-not vertico-buffer-frame--preview-layout-state)))))

(ert-deftest vertico-buffer-frame-preview-window-errors-on-stale-window-frame ()
  (with-temp-buffer
    (let (deleted parameterized)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (_frame)
                   nil))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'new-window)))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--delete-frame)
                 (lambda (frame)
                   (when frame
                     (push frame deleted))))
                ((symbol-function #'vertico-buffer-frame--kill-preview-buffer)
                 (lambda (&optional _keep-buffer)
                   (setq-local vertico-buffer-frame--preview-buffer nil)))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (_parent)
                   '(42 . 11)))
                ((symbol-function #'vertico-buffer-frame--display-buffer-in-child-frame)
                 (lambda (_buffer _parent _name _size _role)
                   'new-window))
                ((symbol-function #'window-frame)
                 (lambda (_window)
                   (error "Stale window")))
                ((symbol-function #'set-window-parameter)
                 (lambda (&rest _args)
                   (setq parameterized t)))
                ((symbol-function #'vertico-buffer-frame--refresh-preview-frame)
                 (lambda ()
                   (error "Refresh should not run"))))
        (should-error
         (vertico-buffer-frame--preview-window)
         :type 'error)
        (should-not deleted)
        (should-not parameterized)
        (should-not vertico-buffer-frame--preview-frame)
        (should-not vertico-buffer-frame--preview-window)
        (should-not vertico-buffer-frame--preview-layout-state)))))

(ert-deftest vertico-buffer-frame-refresh-preview-frame-updates-changed-layout ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'frame
                vertico-buffer-frame--preview-window 'window)
    (let (forced placed resized)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'window-buffer)
                 (lambda (window)
                   (and (eq window 'window) 'buffer)))
                ((symbol-function #'force-window-update)
                 (lambda (buffer)
                   (push buffer forced)))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (pcase frame
                     ('parent 100)
                     ('frame 80))))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (pcase frame
                     ('parent 50)
                     ('frame 20))))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (parent)
                   (should (eq parent 'parent))
                   'size))
                ((symbol-function #'vertico-buffer-frame--resize-frame-to-size)
                 (lambda (frame size)
                   (push (list frame size) resized)))
                ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                 (lambda (frame parent)
                   (push (list frame parent) placed))))
        (vertico-buffer-frame--refresh-preview-frame)
        (should (equal forced '(buffer)))
        (should (equal resized '((frame size))))
        (should (equal placed '((frame parent))))))))

(ert-deftest vertico-buffer-frame-refresh-preview-keeps-state-on-place-error ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'frame
                vertico-buffer-frame--preview-window 'window
                vertico-buffer-frame--preview-layout-state nil)
    (let (forced placed resized)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'window-buffer)
                 (lambda (window)
                   (and (eq window 'window) 'buffer)))
                ((symbol-function #'force-window-update)
                 (lambda (buffer)
                   (push buffer forced)))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (pcase frame
                     ('parent 100)
                     ('frame 80))))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (pcase frame
                     ('parent 50)
                     ('frame 20))))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (_parent)
                   'size))
                ((symbol-function #'vertico-buffer-frame--resize-frame-to-size)
                 (lambda (frame size)
                   (push (list frame size) resized)
                   t))
                ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                 (lambda (frame parent)
                   (push (list frame parent) placed)
                   nil)))
        (should-not (vertico-buffer-frame--refresh-preview-frame))
        (should-not forced)
        (should (equal resized '((frame size))))
        (should (equal placed '((frame parent))))
        (should-not vertico-buffer-frame--preview-layout-state)))))

(ert-deftest vertico-buffer-frame-refresh-preview-frame-skips-unchanged-layout ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'frame
                vertico-buffer-frame--preview-window 'window
                vertico-buffer-frame--preview-layout-state
                (list 'parent 100 50 80 20 'size nil))
    (let (forced placed resized)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (eq frame 'frame)))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'window-buffer)
                 (lambda (window)
                   (and (eq window 'window) 'buffer)))
                ((symbol-function #'force-window-update)
                 (lambda (buffer)
                   (push buffer forced)))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (pcase frame
                     ('parent 100)
                     ('frame 80))))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (pcase frame
                     ('parent 50)
                     ('frame 20))))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (_parent)
                   'size))
                ((symbol-function #'vertico-buffer-frame--resize-frame-to-size)
                 (lambda (frame size)
                   (push (list frame size) resized)))
                ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                 (lambda (frame parent)
                   (push (list frame parent) placed))))
        (vertico-buffer-frame--refresh-preview-frame)
        (should-not forced)
        (should-not resized)
        (should-not placed)))))

(ert-deftest vertico-buffer-frame-refresh-preview-frame-follows-candidate-position ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--preview-frame 'frame
                vertico-buffer-frame--preview-window 'window
                vertico-buffer-frame--candidate-frame 'candidate
                vertico-buffer-frame--preview-layout-state
                (list 'parent 100 50 80 20 'size '(10 10 40 20)))
    (let (forced placed resized)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame '(frame candidate parent))))
                ((symbol-function #'window-live-p)
                 (lambda (window)
                   (eq window 'window)))
                ((symbol-function #'window-buffer)
                 (lambda (window)
                   (and (eq window 'window) 'buffer)))
                ((symbol-function #'force-window-update)
                 (lambda (buffer)
                   (push buffer forced)))
                ((symbol-function #'frame-parameter)
                 (lambda (frame parameter)
                   (and (eq frame 'candidate)
                        (eq parameter 'parent-frame)
                        'parent)))
                ((symbol-function #'frame-position)
                 (lambda (frame)
                   (and (eq frame 'candidate)
                        '(20 . 10))))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (pcase frame
                     ('parent 100)
                     ('frame 80)
                     ('candidate 40))))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (pcase frame
                     ('parent 50)
                     ('frame 20)
                     ('candidate 20))))
                ((symbol-function #'vertico-buffer-frame--preview-parent-frame)
                 (lambda ()
                   'parent))
                ((symbol-function #'vertico-buffer-frame--preview-frame-size)
                 (lambda (_parent)
                   'size))
                ((symbol-function #'vertico-buffer-frame--resize-frame-to-size)
                 (lambda (frame size)
                   (push (list frame size) resized)
                   t))
                ((symbol-function #'vertico-buffer-frame--place-preview-frame)
                 (lambda (frame parent)
                   (push (list frame parent) placed)
                   t)))
        (vertico-buffer-frame--refresh-preview-frame)
        (should (equal forced '(buffer)))
        (should (equal resized '((frame size))))
        (should (equal placed '((frame parent))))
        (should (equal vertico-buffer-frame--preview-layout-state
                       (list 'parent 100 50 80 20 'size
                             '(20 10 40 20))))))))

;;;; Preview target resolution

(ert-deftest vertico-buffer-frame-find-buffer-position-respects-buffer-limit ()
  (let ((first (generate-new-buffer " *vbf-search-first*"))
        (second (generate-new-buffer " *vbf-search-second*")))
    (unwind-protect
        (progn
          (with-current-buffer second
            (insert "needle"))
          (cl-letf (((symbol-function #'vertico-buffer-frame--origin-buffer)
                     (lambda ()
                       nil))
                    ((symbol-function #'buffer-list)
                     (lambda ()
                       (list first second))))
            (let ((vertico-buffer-frame-preview-search-buffer-limit 1))
              (should-not
               (vertico-buffer-frame--find-buffer-position "needle")))
            (let ((vertico-buffer-frame-preview-search-buffer-limit 2))
              (should (equal
                       (vertico-buffer-frame--find-buffer-position "needle")
                       (list 'buffer-position second 1))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list first second)))))

(ert-deftest vertico-buffer-frame-find-buffer-position-skips-empty-candidate ()
  (let ((buffer (generate-new-buffer " *vbf-search-empty*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "content"))
          (should-not
           (vertico-buffer-frame--find-buffer-position "" (list buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-find-buffer-position-ignores-improper-buffer-list ()
  (should-not
   (vertico-buffer-frame--find-buffer-position
    "needle"
    '(not-a-buffer . bad))))

(ert-deftest vertico-buffer-frame-find-buffer-position-continues-after-buffer-error ()
  (let ((first (generate-new-buffer " *vbf-search-error*"))
        (second (generate-new-buffer " *vbf-search-after-error*")))
    (unwind-protect
        (progn
          (with-current-buffer first
            (insert "needle"))
          (with-current-buffer second
            (insert "needle"))
          (let ((original-widen (symbol-function #'widen)))
            (cl-letf (((symbol-function #'widen)
                       (lambda ()
                         (if (eq (current-buffer) first)
                             (error "Widen failed")
                           (funcall original-widen)))))
              (should (equal
                       (vertico-buffer-frame--find-buffer-position
                        "needle"
                        (list first second))
                       (list 'buffer-position second 1))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list first second)))))

(ert-deftest vertico-buffer-frame-directory-preview-limits-entry-output ()
  (let ((directory (make-temp-file "vbf-directory" t)))
    (unwind-protect
        (progn
          (dotimes (index 5)
            (with-temp-file (expand-file-name
                             (format "entry-%d" index)
                             directory)
              (insert "")))
          (let ((vertico-buffer-frame-preview-directory-entry-limit 2))
            (with-temp-buffer
              (vertico-buffer-frame--insert-directory-preview directory)
              (should (string-match-p "More entries not shown"
                                      (buffer-string))))))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-directory-preview-reports-directory-error ()
  (with-temp-buffer
    (cl-letf (((symbol-function #'directory-files)
               (lambda (&rest _args)
                 (error "Directory failed"))))
      (vertico-buffer-frame--insert-directory-preview "/tmp/vbf-directory")
      (should (string-match-p "Directory preview failed"
                              (buffer-string)))
      (should (string-match-p "directory failed"
                              (buffer-string))))))

(ert-deftest vertico-buffer-frame-buffer-target-detects-existing-buffer ()
  (let ((candidate-buffer (generate-new-buffer "vbf-target"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (should (equal (vertico-buffer-frame--preview-target
                          (buffer-name candidate-buffer)
                          'buffer)
                         (list 'buffer (buffer-name candidate-buffer)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list candidate-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-text-property-value-ignores-property-errors ()
  (cl-letf (((symbol-function #'text-property-not-all)
             (lambda (&rest _args)
               (error "Property failed"))))
    (should-not
     (vertico-buffer-frame--text-property-value
      'multi-category
      "candidate"))))

(ert-deftest vertico-buffer-frame-multi-category-buffer-target-uses-real-value ()
  (let ((candidate-buffer (generate-new-buffer "vbf-target"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (let ((candidate (propertize
                            "displayed buffer"
                            'multi-category
                            (cons 'buffer (buffer-name candidate-buffer)))))
            (should (equal (vertico-buffer-frame--preview-target
                            candidate
                            'multi-category)
                           (list 'buffer
                                 (buffer-name candidate-buffer))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list candidate-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-multi-category-buffer-target-uses-buffer-object ()
  (let ((candidate-buffer (generate-new-buffer "vbf-target"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (let ((candidate (propertize
                            "displayed buffer"
                            'multi-category
                            (cons 'buffer candidate-buffer))))
            (should (equal (vertico-buffer-frame--preview-target
                            candidate
                            'multi-category)
                           (list 'buffer
                                 (buffer-name candidate-buffer))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list candidate-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-multi-category-file-target-uses-real-value ()
  (let ((file (make-temp-file "vbf-preview"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (let ((candidate (propertize
                            "displayed file"
                            'multi-category
                            (cons 'file file))))
            (should (equal (vertico-buffer-frame--preview-target
                            candidate
                            'multi-category)
                           (list 'file file)))))
      (delete-file file)
      (when (buffer-live-p minibuffer)
        (kill-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-multi-category-target-allows-prefix ()
  (let ((candidate-buffer (generate-new-buffer "vbf-target"))
        (minibuffer (generate-new-buffer " *vbf-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (let ((candidate
                 (concat "prefix "
                         (propertize
                          "displayed buffer"
                          'multi-category
                          (cons 'buffer (buffer-name candidate-buffer))))))
            (should (equal (vertico-buffer-frame--preview-target
                            candidate
                            'multi-category)
                           (list 'buffer
                                 (buffer-name candidate-buffer))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list candidate-buffer minibuffer)))))

(ert-deftest vertico-buffer-frame-file-target-skips-remote-files-by-default ()
  (with-temp-buffer
    (let (readable-called)
      (cl-letf (((symbol-function #'file-readable-p)
                 (lambda (_file)
                   (setq readable-called t)
                   t)))
        (should-not (vertico-buffer-frame--preview-target
                     "/ssh:example:/tmp/file"
                     'file))
        (should-not readable-called)))))

(ert-deftest vertico-buffer-frame-file-target-ignores-file-api-errors ()
  (with-temp-buffer
    (cl-letf (((symbol-function #'expand-file-name)
               (lambda (&rest _args)
                 (error "Expand failed"))))
      (should-not
       (vertico-buffer-frame--preview-target "broken" 'file)))))

(ert-deftest vertico-buffer-frame-file-target-allows-remote-files-when-enabled ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-remote-files t))
      (cl-letf (((symbol-function #'file-readable-p)
                 (lambda (_file)
                   t)))
        (should (equal (vertico-buffer-frame--preview-target
                        "/ssh:example:/tmp/file"
                        'file)
                       '(file "/ssh:example:/tmp/file")))))))

(ert-deftest vertico-buffer-frame-preview-target-skips-failing-hook ()
  (let ((candidate-buffer (generate-new-buffer "vbf-hook-target"))
        messages)
    (unwind-protect
        (with-temp-buffer
          (let ((vertico-buffer-frame-preview-target-functions
                 (list (lambda (&rest _args)
                         (error "Hook failed")))))
            (cl-letf (((symbol-function #'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args)
                               messages))))
              (should (equal (vertico-buffer-frame--preview-target
                              (buffer-name candidate-buffer)
                              'buffer)
                             (list 'buffer
                                   (buffer-name candidate-buffer))))
              (should (equal messages
                             '("vertico-buffer-frame preview target error: Hook failed"))))))
      (when (buffer-live-p candidate-buffer)
        (kill-buffer candidate-buffer)))))

(ert-deftest vertico-buffer-frame-preview-target-continues-after-hook-error ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-target-functions
           (list (lambda (&rest _args)
                   (error "Hook failed"))
                 (lambda (_category _candidate _raw-candidate)
                   '(text "Hook" ignore))))
          messages)
      (cl-letf (((symbol-function #'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args)
                         messages))))
        (should (equal (vertico-buffer-frame--preview-target
                        "candidate"
                        'buffer)
                       '(text "Hook" ignore)))
        (should (equal messages
                       '("vertico-buffer-frame preview target error: Hook failed")))))))

(ert-deftest vertico-buffer-frame-preview-target-reports-repeated-hook-error-once ()
  (let ((candidate-buffer (generate-new-buffer "vbf-hook-target"))
        messages)
    (unwind-protect
        (with-temp-buffer
          (let ((vertico-buffer-frame-preview-target-functions
                 (list (lambda (&rest _args)
                         (error "Hook failed")))))
            (cl-letf (((symbol-function #'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args)
                               messages))))
              (dotimes (_ 2)
                (should (equal (vertico-buffer-frame--preview-target
                                (buffer-name candidate-buffer)
                                'buffer)
                               (list 'buffer
                                     (buffer-name candidate-buffer))))
                (setq-local
                 vertico-buffer-frame--preview-last-error-message nil))
              (should (equal messages
                             '("vertico-buffer-frame preview target error: Hook failed"))))))
      (when (buffer-live-p candidate-buffer)
        (kill-buffer candidate-buffer)))))

(ert-deftest vertico-buffer-frame-preview-target-accepts-single-hook-function ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-target-functions
           (lambda (_category _candidate _raw-candidate)
             '(text "Hook" ignore))))
      (should (equal (vertico-buffer-frame--preview-target
                      "candidate"
                      'buffer)
                     '(text "Hook" ignore))))))

(ert-deftest vertico-buffer-frame-preview-target-skips-non-function-hook-values ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-target-functions
           (list 'not-a-function
                 (lambda (_category _candidate _raw-candidate)
                   '(text "Hook" ignore))))
          messages)
      (cl-letf (((symbol-function #'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args)
                         messages))))
        (should (equal (vertico-buffer-frame--preview-target
                        "candidate"
                        'buffer)
                       '(text "Hook" ignore)))
        (should-not messages)))))

(ert-deftest vertico-buffer-frame-preview-target-ignores-non-function-hook-values ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-target-functions '(not-a-function 42))
          messages)
      (cl-letf (((symbol-function #'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args)
                         messages))))
        (let ((target (vertico-buffer-frame--preview-target
                       "candidate"
                       'kill-ring)))
          (should (eq (car target) 'text))
          (should (equal (cadr target) "Kill Ring"))
          (should (functionp (caddr target))))
        (should-not messages)))))

(ert-deftest vertico-buffer-frame-preview-target-ignores-invalid-category-list ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-categories 'not-a-list))
      (should-not
       (vertico-buffer-frame--preview-target "candidate" 'buffer)))))

(ert-deftest vertico-buffer-frame-preview-target-ignores-improper-category-list ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-categories '(buffer . bad)))
      (should-not
       (vertico-buffer-frame--preview-target "candidate" 'buffer)))))

(ert-deftest vertico-buffer-frame-preview-target-ignores-improper-hook-list ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-target-functions
           (cons (lambda (&rest _args)
                   '(text "Hook" ignore))
                 'bad)))
      (should (equal (vertico-buffer-frame--preview-target
                      "candidate"
                      'buffer)
                     nil)))))

(ert-deftest vertico-buffer-frame-preview-target-reports-builtin-error ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-preview-target-functions nil)
          messages)
      (cl-letf (((symbol-function #'vertico-buffer-frame--buffer-target)
                 (lambda (&rest _args)
                   (error "Builtin failed")))
                ((symbol-function #'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args)
                         messages))))
        (should-not
         (vertico-buffer-frame--preview-target "candidate" 'buffer))
        (should (equal messages
                       '("vertico-buffer-frame preview target error: Builtin failed")))))))

(ert-deftest vertico-buffer-frame-preview-categories-include-builtins ()
  (dolist (category '(file
                      buffer
                      project-file
                      xref-location
                      info-menu
                      symbol-help
                      calendar-month
                      unicode-name
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

;;;; Consult adapters

(ert-deftest vertico-buffer-frame-consult-location-target-uses-cheap-marker ()
  (let ((source (generate-new-buffer " *vbf-consult-location-source*"))
        (minibuffer (generate-new-buffer " *vbf-consult-location-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (let ((candidate (propertize
                            "matching line"
                            'consult-location
                            (cons (cons source 7) 1))))
            (should (equal (vertico-buffer-frame--preview-target
                            candidate
                            'consult-location)
                           (list 'buffer-position source 7)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-consult-location-target-allows-prefix ()
  (let ((source (generate-new-buffer " *vbf-consult-location-source*"))
        (minibuffer (generate-new-buffer " *vbf-consult-location-minibuffer*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (let ((candidate (concat "prefix "
                                   (propertize
                                    "matching line"
                                    'consult-location
                                    (cons (cons source 7) 1)))))
            (should (equal (vertico-buffer-frame--preview-target
                            candidate
                            'consult-location)
                           (list 'buffer-position source 7)))))
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
               (candidate (copy-sequence "target.el:2:match")))
          (with-temp-file file
            (insert "line 1\nline 2\n"))
          (add-face-text-property 0 9 'consult-file t candidate)
          (add-face-text-property 10 11 'consult-line-number t candidate)
          (with-current-buffer minibuffer
            (setq-local default-directory (file-name-as-directory directory))
            (should (equal (vertico-buffer-frame--preview-target
                            candidate
                            'consult-grep)
                           (list 'file-line file 2)))))
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

(ert-deftest vertico-buffer-frame-consult-xref-target-ignores-location-errors ()
  (let ((candidate (propertize "broken"
                               'consult-xref 'broken-xref)))
    (cl-letf (((symbol-function #'xref-item-location)
               (lambda (&rest _args)
                 (error "Xref failed"))))
      (should-not
       (vertico-buffer-frame-consult--xref-target candidate)))))

(ert-deftest vertico-buffer-frame-consult-xref-target-falls-back-to-marker ()
  (let ((buffer (generate-new-buffer " *vbf-consult-xref-marker*"))
        marker)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "line 1\nline 2\n")
            (goto-char (point-min))
            (forward-line 1)
            (setq marker (point-marker)))
          (let ((candidate (propertize "target"
                                       'consult-xref 'xref)))
            (cl-letf (((symbol-function #'xref-item-location)
                       (lambda (_xref)
                         'location))
                      ((symbol-function #'xref-location-group)
                       (lambda (_location)
                         (error "Group failed")))
                      ((symbol-function #'xref-location-marker)
                       (lambda (_location)
                         marker)))
              (should (equal
                       (vertico-buffer-frame-consult--xref-target candidate)
                       (list 'buffer-position buffer marker))))))
      (when (markerp marker)
        (set-marker marker nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-consult-install-preserves-categories ()
  (let ((vertico-buffer-frame-preview-categories '(file buffer))
        (vertico-buffer-frame-preview-target-functions nil))
    (vertico-buffer-frame-consult--install)
    (should (equal vertico-buffer-frame-preview-categories '(file buffer)))
    (should (memq #'vertico-buffer-frame-consult-preview-target
                  vertico-buffer-frame-preview-target-functions))))

(ert-deftest vertico-buffer-frame-consult-install-preserves-user-hook-priority ()
  (let ((vertico-buffer-frame-preview-target-functions
         '(vertico-buffer-frame-test-user-preview-target)))
    (vertico-buffer-frame-consult--install)
    (should (equal vertico-buffer-frame-preview-target-functions
                   '(vertico-buffer-frame-test-user-preview-target
                     vertico-buffer-frame-consult-preview-target)))))

(ert-deftest vertico-buffer-frame-consult-install-tolerates-improper-hook-list ()
  (let ((vertico-buffer-frame-preview-target-functions
         '(vertico-buffer-frame-test-user-preview-target . bad)))
    (vertico-buffer-frame-consult--install)
    (should (equal vertico-buffer-frame-preview-target-functions
                   '(vertico-buffer-frame-test-user-preview-target
                     vertico-buffer-frame-consult-preview-target)))))

(ert-deftest vertico-buffer-frame-consult-install-tolerates-invalid-hook-atom ()
  (let ((vertico-buffer-frame-preview-target-functions
         'not-a-function))
    (vertico-buffer-frame-consult--install)
    (should (equal vertico-buffer-frame-preview-target-functions
                   '(not-a-function
                     vertico-buffer-frame-consult-preview-target)))))

(ert-deftest vertico-buffer-frame-consult-install-tolerates-circular-hook-list ()
  (let* ((hook (list 'vertico-buffer-frame-test-user-preview-target))
         (vertico-buffer-frame-preview-target-functions hook))
    (setcdr hook hook)
    (vertico-buffer-frame-consult--install)
    (should (proper-list-p vertico-buffer-frame-preview-target-functions))
    (should (equal vertico-buffer-frame-preview-target-functions
                   '(vertico-buffer-frame-test-user-preview-target
                     vertico-buffer-frame-consult-preview-target)))))

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

(ert-deftest vertico-buffer-frame-project-root-ignores-project-api-errors ()
  (let ((directory (make-temp-file "vbf-project-error" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory directory)))
          (cl-letf (((symbol-function #'project-current)
                     (lambda (&rest _args)
                       'project))
                    ((symbol-function #'project-root)
                     (lambda (&rest _args)
                       (error "Project failed"))))
            (should-not (vertico-buffer-frame--project-root))
            (should (equal (car vertico-buffer-frame--project-root-cache)
                           default-directory))))
      (delete-directory directory t))))

(ert-deftest vertico-buffer-frame-xref-target-uses-candidate-location ()
  (let ((directory (make-temp-file "vbf-xref" t)))
    (unwind-protect
        (let* ((default-directory (file-name-as-directory directory))
               (file (expand-file-name "xref-target.el" directory))
               (candidate "xref-target.el:2:match"))
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
               (candidate "prefix xref-target.el:2:match"))
          (with-temp-file file
            (insert "line 1\nline 2\n"))
          (should (equal (vertico-buffer-frame--xref-location-target
                          (substring-no-properties candidate)
                          candidate)
                         (list 'file-line file 2))))
      (delete-directory directory t))))

;;;; Preview buffer ownership and display

(ert-deftest vertico-buffer-frame-text-preview-buffer-is-owned-and-read-only ()
  (with-temp-buffer
    (let ((owner (current-buffer))
          (preview (vertico-buffer-frame--text-preview-buffer
                    "test"
                    (lambda ()
                      (insert "preview")))))
      (unwind-protect
          (progn
            (should (eq vertico-buffer-frame--preview-buffer preview))
            (with-current-buffer preview
              (should buffer-read-only)
              (should-not (buffer-modified-p))
              (should (eq vertico-buffer-frame--preview-owner-buffer
                          owner))
              (should (equal (buffer-string) "preview"))))
        (when (buffer-live-p preview)
          (kill-buffer preview))))))

(ert-deftest vertico-buffer-frame-text-preview-buffer-cleans-up-after-error ()
  (with-temp-buffer
    (let (preview)
      (should-error
       (vertico-buffer-frame--text-preview-buffer
        "test"
        (lambda ()
          (setq preview (current-buffer))
          (insert "partial")
          (error "Preview failed"))))
      (should-not vertico-buffer-frame--preview-buffer)
      (should-not (buffer-live-p preview)))))

(ert-deftest vertico-buffer-frame-kills-preview-buffers-by-owner ()
  (let ((owner (generate-new-buffer " *vbf-preview-owner*"))
        (other-owner (generate-new-buffer " *vbf-preview-other-owner*"))
        (owned (generate-new-buffer " *vbf-owned-preview*"))
        (other-owned (generate-new-buffer " *vbf-other-owned-preview*"))
        (unowned (generate-new-buffer " *vbf-unowned-preview*")))
    (unwind-protect
        (progn
          (with-current-buffer owned
            (setq-local vertico-buffer-frame--temporary-preview-buffer t
                        vertico-buffer-frame--preview-owner-buffer owner)
            (set-buffer-modified-p nil))
          (with-current-buffer other-owned
            (setq-local vertico-buffer-frame--temporary-preview-buffer t
                        vertico-buffer-frame--preview-owner-buffer other-owner)
            (set-buffer-modified-p nil))
          (with-current-buffer unowned
            (setq-local vertico-buffer-frame--temporary-preview-buffer t)
            (set-buffer-modified-p nil))
          (vertico-buffer-frame--kill-preview-buffers-owned-by-buffer owner)
          (should-not (buffer-live-p owned))
          (should (buffer-live-p other-owned))
          (should (buffer-live-p unowned)))
      (dolist (buffer (list owner other-owner owned other-owned unowned))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-kill-temporary-preview-buffer-skips-user-buffer ()
  (let ((temporary (generate-new-buffer " *vbf-temporary-preview*"))
        (user-buffer (generate-new-buffer " *vbf-user-buffer*")))
    (unwind-protect
        (progn
          (with-current-buffer temporary
            (setq-local vertico-buffer-frame--temporary-preview-buffer t)
            (set-buffer-modified-p nil))
          (vertico-buffer-frame--kill-temporary-preview-buffer user-buffer)
          (should (buffer-live-p user-buffer))
          (vertico-buffer-frame--kill-temporary-preview-buffer temporary)
          (should-not (buffer-live-p temporary)))
      (dolist (buffer (list temporary user-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-kill-temporary-preview-buffer-ignores-local-errors ()
  (let (killed)
    (cl-letf (((symbol-function #'buffer-live-p)
               (lambda (buffer)
                 (eq buffer 'stale-buffer)))
              ((symbol-function #'buffer-local-value)
               (lambda (&rest _args)
                 (error "Stale buffer")))
              ((symbol-function #'kill-buffer)
               (lambda (buffer)
                 (push buffer killed))))
      (should-not
       (vertico-buffer-frame--kill-temporary-preview-buffer 'stale-buffer))
      (should-not killed))))

(ert-deftest vertico-buffer-frame-kill-temporary-preview-buffer-ignores-kill-errors ()
  (let ((temporary (generate-new-buffer " *vbf-temporary-preview*")))
    (unwind-protect
        (progn
          (with-current-buffer temporary
            (setq-local vertico-buffer-frame--temporary-preview-buffer t)
            (set-buffer-modified-p nil))
          (cl-letf (((symbol-function #'kill-buffer)
                     (lambda (_buffer)
                       (error "Kill failed"))))
            (should-not
             (vertico-buffer-frame--kill-temporary-preview-buffer temporary)))
          (should (buffer-live-p temporary)))
      (when (buffer-live-p temporary)
        (kill-buffer temporary)))))

(ert-deftest vertico-buffer-frame-preview-buffer-owned-by-p-ignores-local-errors ()
  (cl-letf (((symbol-function #'buffer-live-p)
             (lambda (buffer)
               (eq buffer 'stale-buffer)))
            ((symbol-function #'buffer-local-value)
             (lambda (&rest _args)
               (error "Stale buffer"))))
    (should-not
     (vertico-buffer-frame--preview-buffer-owned-by-p
      'stale-buffer
      'owner))))

(ert-deftest vertico-buffer-frame-kill-preview-buffer-skips-foreign-owner ()
  (let ((owner (generate-new-buffer " *vbf-preview-owner*"))
        (foreign-owner (generate-new-buffer " *vbf-foreign-owner*"))
        (foreign (generate-new-buffer " *vbf-foreign-preview*")))
    (unwind-protect
        (with-current-buffer owner
          (setq-local vertico-buffer-frame--preview-buffer foreign)
          (with-current-buffer foreign
            (setq-local vertico-buffer-frame--temporary-preview-buffer t
                        vertico-buffer-frame--preview-owner-buffer
                        foreign-owner)
            (set-buffer-modified-p nil))
          (vertico-buffer-frame--kill-preview-buffer)
          (should (buffer-live-p foreign))
          (should-not vertico-buffer-frame--preview-buffer))
      (dolist (buffer (list owner foreign-owner foreign))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-kill-preview-buffer-preserves-keep-buffer ()
  (let ((owner (generate-new-buffer " *vbf-preview-owner*"))
        (old (generate-new-buffer " *vbf-old-preview*"))
        (keep (generate-new-buffer " *vbf-keep-preview*")))
    (unwind-protect
        (with-current-buffer owner
          (setq-local vertico-buffer-frame--preview-buffer old)
          (dolist (buffer (list old keep))
            (with-current-buffer buffer
              (setq-local vertico-buffer-frame--temporary-preview-buffer t
                          vertico-buffer-frame--preview-owner-buffer
                          owner)
              (set-buffer-modified-p nil)))
          (vertico-buffer-frame--kill-preview-buffer keep)
          (should-not (buffer-live-p old))
          (should (buffer-live-p keep))
          (should-not vertico-buffer-frame--preview-buffer))
      (dolist (buffer (list owner old keep))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-kill-old-preview-buffer-skips-foreign-owner ()
  (let ((owner (generate-new-buffer " *vbf-preview-owner*"))
        (foreign-owner (generate-new-buffer " *vbf-foreign-owner*"))
        (old (generate-new-buffer " *vbf-foreign-preview*"))
        (new (generate-new-buffer " *vbf-new-preview*")))
    (unwind-protect
        (with-current-buffer owner
          (setq-local vertico-buffer-frame--file-preview-cache
                      (cons 'key old))
          (with-current-buffer old
            (setq-local vertico-buffer-frame--temporary-preview-buffer t
                        vertico-buffer-frame--preview-owner-buffer
                        foreign-owner)
            (set-buffer-modified-p nil))
          (vertico-buffer-frame--kill-old-preview-buffer old new)
          (should (buffer-live-p old))
          (should (buffer-live-p new))
          (should-not vertico-buffer-frame--file-preview-cache))
      (dolist (buffer (list owner foreign-owner old new))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-text-preview-buffer-reuses-owned-buffer ()
  (with-temp-buffer
    (let (first second)
      (unwind-protect
          (progn
            (setq first
                  (vertico-buffer-frame--text-preview-buffer
                   "test"
                   (lambda ()
                     (insert "first"))))
            (setq second
                  (vertico-buffer-frame--text-preview-buffer
                   "test"
                   (lambda ()
                     (insert "second"))))
            (should (eq first second))
            (with-current-buffer second
              (should buffer-read-only)
              (should (equal (buffer-string) "second"))))
        (when (buffer-live-p second)
          (kill-buffer second))
        (when (and (buffer-live-p first)
                   (not (eq first second)))
          (kill-buffer first))))))

(ert-deftest vertico-buffer-frame-text-preview-buffer-skips-foreign-owner ()
  (let ((owner (generate-new-buffer " *vbf-preview-owner*"))
        (foreign-owner (generate-new-buffer " *vbf-foreign-owner*"))
        (foreign (generate-new-buffer " *vbf-foreign-preview*"))
        new)
    (unwind-protect
        (with-current-buffer owner
          (setq-local vertico-buffer-frame--preview-buffer foreign)
          (with-current-buffer foreign
            (setq-local vertico-buffer-frame--temporary-preview-buffer t
                        vertico-buffer-frame--preview-owner-buffer
                        foreign-owner)
            (set-buffer-modified-p nil))
          (setq new
                (vertico-buffer-frame--text-preview-buffer
                 "test"
                 (lambda ()
                   (insert "new"))))
          (should-not (eq new foreign))
          (should (eq vertico-buffer-frame--preview-buffer new))
          (should (buffer-live-p foreign))
          (with-current-buffer new
            (should (eq vertico-buffer-frame--preview-owner-buffer
                        owner))
            (should (equal (buffer-string) "new"))))
      (dolist (buffer (list owner foreign-owner foreign new))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-preview-window-buffer-set-before-dedication ()
  (let (events)
    (cl-letf (((symbol-function #'set-window-dedicated-p)
               (lambda (window dedicated)
                 (push (list 'dedicated window dedicated) events)))
              ((symbol-function #'set-window-buffer)
               (lambda (window buffer)
                 (push (list 'buffer window buffer) events)))
              ((symbol-function #'window-buffer)
               (lambda (_window)
                 'old-buffer)))
      (vertico-buffer-frame--set-preview-window-buffer 'window 'buffer)
      (should (equal (nreverse events)
                     '((dedicated window nil)
                       (buffer window buffer)
                       (dedicated window t)))))))

(ert-deftest vertico-buffer-frame-preview-window-skips-same-buffer ()
  (let (events)
    (cl-letf (((symbol-function #'set-window-dedicated-p)
               (lambda (window dedicated)
                 (push (list 'dedicated window dedicated) events)))
              ((symbol-function #'set-window-buffer)
               (lambda (window buffer)
                 (push (list 'buffer window buffer) events)))
              ((symbol-function #'window-buffer)
               (lambda (_window)
                 'buffer))
              ((symbol-function #'window-dedicated-p)
               (lambda (_window)
                 t)))
      (vertico-buffer-frame--set-preview-window-buffer 'window 'buffer)
      (should-not events))))

(ert-deftest vertico-buffer-frame-preview-window-dedicates-same-buffer ()
  (let (events)
    (cl-letf (((symbol-function #'set-window-dedicated-p)
               (lambda (window dedicated)
                 (push (list 'dedicated window dedicated) events)))
              ((symbol-function #'set-window-buffer)
               (lambda (window buffer)
                 (push (list 'buffer window buffer) events)))
              ((symbol-function #'window-buffer)
               (lambda (_window)
                 'buffer))
              ((symbol-function #'window-dedicated-p)
               (lambda (_window)
                 nil)))
      (vertico-buffer-frame--set-preview-window-buffer 'window 'buffer)
      (should (equal events '((dedicated window t)))))))

(ert-deftest vertico-buffer-frame-preview-window-buffer-ignores-window-errors ()
  (let (killed)
    (cl-letf (((symbol-function #'window-buffer)
               (lambda (_window)
                 (error "Stale window")))
              ((symbol-function #'vertico-buffer-frame--kill-old-preview-buffer)
               (lambda (&rest _args)
                 (setq killed t))))
      (should-not
       (vertico-buffer-frame--set-preview-window-buffer
        'stale-window 'buffer 'old-buffer))
      (should-not killed))))

(ert-deftest vertico-buffer-frame-display-preview-buffer-skips-nil-buffer ()
  (let ((old-buffer (generate-new-buffer " *vbf-old-preview*"))
        preview-window-called
        hidden)
    (unwind-protect
        (with-temp-buffer
          (let ((owner (current-buffer)))
            (setq-local vertico-buffer-frame--preview-buffer old-buffer
                        vertico-buffer-frame--preview-frame 'frame)
            (with-current-buffer old-buffer
              (setq-local vertico-buffer-frame--temporary-preview-buffer t
                          vertico-buffer-frame--preview-owner-buffer
                          owner)
              (set-buffer-modified-p nil))
            (cl-letf (((symbol-function #'vertico-buffer-frame--preview-window)
                       (lambda (&optional _keep-buffer)
                         (setq preview-window-called t)
                         'window))
                      ((symbol-function #'vertico-buffer-frame--hide-preview)
                       (lambda ()
                         (setq hidden t))))
              (vertico-buffer-frame--display-preview-buffer
               nil nil nil old-buffer)
              (should-not preview-window-called)
              (should hidden)
              (should-not (buffer-live-p old-buffer))
              (should-not vertico-buffer-frame--preview-buffer))))
      (when (buffer-live-p old-buffer)
        (kill-buffer old-buffer)))))

(ert-deftest vertico-buffer-frame-display-preview-buffer-preserves-new-buffer ()
  (let ((old-buffer (generate-new-buffer " *vbf-old-preview*"))
        new-buffer
        preview-buffer-during-window
        events)
    (unwind-protect
        (with-temp-buffer
          (let ((owner (current-buffer)))
            (setq-local vertico-buffer-frame--preview-buffer old-buffer
                        vertico-buffer-frame--file-preview-cache
                        (cons 'key old-buffer)
                        vertico-buffer-frame--preview-frame 'frame)
            (with-current-buffer old-buffer
              (setq-local vertico-buffer-frame--temporary-preview-buffer t
                          vertico-buffer-frame--preview-owner-buffer
                          owner)
              (set-buffer-modified-p nil))
            (cl-letf (((symbol-function #'vertico-buffer-frame--preview-window)
                       (lambda (&optional _keep-buffer)
                         (setq preview-buffer-during-window
                               vertico-buffer-frame--preview-buffer)
                         'window))
                      ((symbol-function
                        #'vertico-buffer-frame--set-preview-window-buffer)
                       (lambda (window buffer old-preview-buffer)
                         (push (list 'set window buffer old-preview-buffer)
                               events)))
                      ((symbol-function #'vertico-buffer-frame--show-frame)
                       (lambda (frame)
                         (push (list 'show frame) events))))
              (setq new-buffer
                    (vertico-buffer-frame--text-preview-buffer
                     "test"
                     (lambda ()
                       (insert "new"))))
              (vertico-buffer-frame--display-preview-buffer
               new-buffer nil nil old-buffer)
              (setq new-buffer vertico-buffer-frame--preview-buffer)
              (should (eq preview-buffer-during-window old-buffer))
              (should (buffer-live-p new-buffer))
              (should-not (eq new-buffer old-buffer))
              (with-current-buffer new-buffer
                (should (equal (buffer-string) "new")))
              (should (equal (nreverse events)
                             (list (list 'set 'window new-buffer old-buffer)
                                   '(show frame)))))))
      (dolist (buffer (list old-buffer new-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-preview-target-display-spec-builds-buffer ()
  (let (preview)
    (unwind-protect
        (with-temp-buffer
          (let ((spec
                 (vertico-buffer-frame--preview-target-display-spec
                  (list 'text "test" (lambda ()
                                       (insert "preview"))))))
            (setq preview (car spec))
            (should (buffer-live-p preview))
            (should-not (cadr spec))
            (should-not (caddr spec))
            (with-current-buffer preview
              (should (equal (buffer-string) "preview")))))
      (when (buffer-live-p preview)
        (kill-buffer preview)))))

(ert-deftest vertico-buffer-frame-show-preview-displays-resolved-buffer ()
  (let ((source (generate-new-buffer " *vbf-display-spec-source*"))
        displayed)
    (unwind-protect
        (with-temp-buffer
          (setq-local vertico-buffer-frame--preview-buffer 'old-preview)
          (cl-letf (((symbol-function
                      #'vertico-buffer-frame--display-preview-buffer)
                     (lambda (buffer location external-buffer old-buffer)
                       (setq displayed
                             (list buffer location external-buffer
                                   old-buffer)))))
            (vertico-buffer-frame--show-preview
             (list 'buffer-position source 3))
            (should (equal displayed
                           (list source '(position . 3) t
                                 'old-preview)))))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-show-preview-hides-invalid-target ()
  (with-temp-buffer
    (let (displayed hidden)
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--display-preview-buffer)
                 (lambda (&rest _args)
                   (setq displayed t)))
                ((symbol-function #'vertico-buffer-frame--hide-preview)
                 (lambda ()
                   (setq hidden t))))
        (vertico-buffer-frame--show-preview '(unsupported target))
        (should hidden)
        (should-not displayed)))))

(ert-deftest vertico-buffer-frame-display-preview-buffer-kills-new-buffer-on-error ()
  (let (new-buffer)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function #'vertico-buffer-frame--preview-window)
                     (lambda (&optional _keep-buffer)
                       (error "Preview window failed")))
                    ((symbol-function
                      #'vertico-buffer-frame--set-preview-window-buffer)
                     (lambda (&rest _args)
                       (error "Set should not run"))))
            (setq new-buffer
                  (vertico-buffer-frame--text-preview-buffer
                   "test"
                   (lambda ()
                     (insert "new"))))
            (should-error
             (vertico-buffer-frame--display-preview-buffer
              new-buffer nil nil nil)
             :type 'error)
            (should-not (buffer-live-p new-buffer))
            (should-not vertico-buffer-frame--preview-buffer)))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer)))))

(ert-deftest vertico-buffer-frame-display-preview-buffer-kills-new-buffer-on-window-set-error ()
  (let (new-buffer shown)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function #'vertico-buffer-frame--preview-window)
                     (lambda (&optional _keep-buffer)
                       'stale-window))
                    ((symbol-function #'window-buffer)
                     (lambda (_window)
                       (error "Stale window")))
                    ((symbol-function #'vertico-buffer-frame--show-frame)
                     (lambda (_frame)
                       (setq shown t))))
            (setq new-buffer
                  (vertico-buffer-frame--text-preview-buffer
                   "test"
                   (lambda ()
                     (insert "new"))))
            (should-error
             (vertico-buffer-frame--display-preview-buffer
              new-buffer nil nil nil)
             :type 'error)
            (should-not shown)
            (should-not (buffer-live-p new-buffer))
            (should-not vertico-buffer-frame--preview-buffer)))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer)))))

(ert-deftest vertico-buffer-frame-preview-replaces-buffer-before-kill ()
  (let ((old-buffer (generate-new-buffer " *vbf-old-preview*"))
        new-buffer
        events)
    (unwind-protect
        (with-temp-buffer
          (let ((owner (current-buffer)))
            (setq-local vertico-buffer-frame--preview-buffer old-buffer
                        vertico-buffer-frame--file-preview-cache
                        (cons 'file-cache-key old-buffer)
                        vertico-buffer-frame--preview-frame 'frame)
            (with-current-buffer old-buffer
              (setq-local vertico-buffer-frame--temporary-preview-buffer t
                          vertico-buffer-frame--preview-owner-buffer
                          owner)
              (set-buffer-modified-p nil))
            (cl-letf (((symbol-function #'vertico-buffer-frame--preview-window)
                       (lambda (&optional _keep-buffer)
                         'window))
                      ((symbol-function #'set-window-dedicated-p)
                       (lambda (window dedicated)
                         (push (list 'dedicated window dedicated) events)))
                      ((symbol-function #'set-window-buffer)
                       (lambda (window buffer)
                         (setq new-buffer buffer)
                         (push (list 'buffer window buffer) events)))
                      ((symbol-function #'window-buffer)
                       (lambda (_window)
                         old-buffer))
                      ((symbol-function #'kill-buffer)
                       (lambda (buffer)
                         (push (list 'kill buffer) events)))
                      ((symbol-function #'vertico-buffer-frame--show-frame)
                       (lambda (frame)
                         (push (list 'show frame) events))))
              (vertico-buffer-frame--show-preview
               (list 'text "Symbol" (lambda ()
                                      (insert "preview"))))
              (should (buffer-live-p new-buffer))
              (should (equal (nreverse events)
                             (list '(dedicated window nil)
                                   (list 'buffer 'window new-buffer)
                                   '(dedicated window t)
                                   (list 'kill old-buffer)
                                   '(show frame)))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list old-buffer new-buffer)))))

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

(ert-deftest vertico-buffer-frame-center-window-point-ignores-window-errors ()
  (cl-letf (((symbol-function #'set-window-point)
             (lambda (&rest _args)
               (error "Stale window"))))
    (should-not
     (vertico-buffer-frame--center-window-point 'stale-window))))

(ert-deftest vertico-buffer-frame-set-window-line-ignores-window-errors ()
  (cl-letf (((symbol-function #'window-buffer)
             (lambda (&rest _args)
               (error "Stale window"))))
    (should-not
     (vertico-buffer-frame--set-window-line 'stale-window 10))))

(ert-deftest vertico-buffer-frame-set-window-position-ignores-window-errors ()
  (cl-letf (((symbol-function #'window-buffer)
             (lambda (&rest _args)
               (error "Stale window"))))
    (should-not
     (vertico-buffer-frame--set-window-position 'stale-window 10))))

(ert-deftest vertico-buffer-frame-display-preview-buffer-keeps-preview-on-location-error ()
  (let ((new-buffer (generate-new-buffer " *vbf-location-preview*"))
        events)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function #'vertico-buffer-frame--preview-window)
                     (lambda (&optional _keep-buffer)
                       'window))
                    ((symbol-function
                      #'vertico-buffer-frame--set-preview-window-buffer)
                     (lambda (window buffer old-preview-buffer)
                       (push (list 'set window buffer old-preview-buffer)
                             events)))
                    ((symbol-function #'vertico-buffer-frame--show-frame)
                     (lambda (frame)
                       (push (list 'show frame) events))))
            (setq-local vertico-buffer-frame--preview-frame 'frame)
            (vertico-buffer-frame--display-preview-buffer
             new-buffer '(line . 10) t nil)
            (should-not vertico-buffer-frame--preview-buffer)
            (should (buffer-live-p new-buffer))
            (should (equal (nreverse events)
                           (list (list 'set 'window new-buffer nil)
                                 '(show frame))))))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer)))))

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

(ert-deftest vertico-buffer-frame-color-target-ignores-color-api-errors ()
  (cl-letf (((symbol-function #'color-defined-p)
             (lambda (&rest _args)
               (error "Color failed"))))
    (should-not (vertico-buffer-frame--color-target "red")))
  (cl-letf (((symbol-function #'color-defined-p)
             (lambda (&rest _args)
               t))
            ((symbol-function #'color-values)
             (lambda (&rest _args)
               '(1 2))))
    (should-not (vertico-buffer-frame--color-target "red"))))

(ert-deftest vertico-buffer-frame-coding-system-target-builds-text-preview ()
  (let ((target (vertico-buffer-frame--coding-system-target "utf-8")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "utf-8" (buffer-string)))
      (should (string-match-p "Base:" (buffer-string))))))

(ert-deftest vertico-buffer-frame-coding-system-target-ignores-accessor-errors ()
  (cl-letf (((symbol-function #'coding-system-doc-string)
             (lambda (&rest _args)
               (error "Doc failed")))
            ((symbol-function #'coding-system-mnemonic)
             (lambda (&rest _args)
               (error "Mnemonic failed"))))
    (let ((target (vertico-buffer-frame--coding-system-target "utf-8")))
      (with-temp-buffer
        (funcall (nth 2 target))
        (should (string-match-p "utf-8" (buffer-string)))
        (should (string-match-p "EOL:" (buffer-string)))))))

(ert-deftest vertico-buffer-frame-charset-target-builds-text-preview ()
  (let ((target (vertico-buffer-frame--charset-target "unicode")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "unicode" (buffer-string))))))

(ert-deftest vertico-buffer-frame-charset-target-ignores-accessor-errors ()
  (cl-letf (((symbol-function #'charset-description)
             (lambda (&rest _args)
               (error "Description failed")))
            ((symbol-function #'get-charset-property)
             (lambda (&rest _args)
               (error "Property failed"))))
    (let ((target (vertico-buffer-frame--charset-target "unicode")))
      (with-temp-buffer
        (funcall (nth 2 target))
        (should (equal (buffer-string) "unicode\n\n"))))))

(ert-deftest vertico-buffer-frame-library-target-uses-locate-library ()
  (cl-letf (((symbol-function #'locate-library)
             (lambda (library &rest _args)
               (and (equal library "simple.el")
                    "/tmp/simple.el"))))
    (should (equal (vertico-buffer-frame--library-target "simple")
                   '(file "/tmp/simple.el")))))

(ert-deftest vertico-buffer-frame-library-target-ignores-locate-errors ()
  (cl-letf (((symbol-function #'locate-library)
             (lambda (&rest _args)
               (error "Locate failed"))))
    (should-not (vertico-buffer-frame--library-target "simple"))))

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

(ert-deftest vertico-buffer-frame-custom-theme-target-ignores-invalid-theme-lists ()
  (let* ((theme (intern "vbf-test-theme"))
         (candidate (symbol-name theme)))
    (cl-letf (((symbol-function #'custom-available-themes)
               (lambda ()
                 'not-a-list)))
      (should-not
       (vertico-buffer-frame--custom-theme-target candidate)))
    (let ((custom-enabled-themes 'not-a-list))
      (cl-letf (((symbol-function #'custom-available-themes)
                 (lambda ()
                   (list theme)))
                ((symbol-function #'locate-library)
                 (lambda (&rest _args)
                   nil)))
        (let ((target (vertico-buffer-frame--custom-theme-target
                       candidate)))
          (should (eq (car target) 'text))
          (with-temp-buffer
            (funcall (nth 2 target))
            (should (string-match-p "Enabled: no" (buffer-string)))))))))

(ert-deftest vertico-buffer-frame-email-target-parses-address ()
  (let ((target (vertico-buffer-frame--email-target
                 "Ada Lovelace <ada@example.invalid>")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "Ada Lovelace" (buffer-string)))
      (should (string-match-p "ada@example.invalid" (buffer-string))))))

(ert-deftest vertico-buffer-frame-email-target-ignores-parser-errors ()
  (require 'mail-extr)
  (let ((target (vertico-buffer-frame--email-target
                 "broken@example.invalid")))
    (cl-letf (((symbol-function #'mail-extract-address-components)
               (lambda (&rest _args)
                 (error "Parse failed"))))
      (with-temp-buffer
        (funcall (nth 2 target))
        (should (equal (buffer-string) "broken@example.invalid\n"))))))

(ert-deftest vertico-buffer-frame-package-target-ignores-invalid-metadata ()
  (require 'package)
  (let* ((package (intern "vbf-test-package"))
         (package-alist 'not-a-list)
         (package-archive-contents '(bad . worse))
         (target (vertico-buffer-frame--package-target
                  (symbol-name package))))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "Installed: no" (buffer-string)))
      (should (string-match-p "Built-in: no" (buffer-string))))))

(ert-deftest vertico-buffer-frame-package-requirements-ignore-malformed-entries ()
  (require 'package)
  (should (equal
           (vertico-buffer-frame--package-requirements-string
            '((emacs (29 1)) bad (foo . "1.0")))
           "emacs 29.1, foo 1.0"))
  (should-not
   (vertico-buffer-frame--package-requirements-string
    '((emacs (29 1)) . bad))))

(ert-deftest vertico-buffer-frame-input-method-target-uses-existing-alist ()
  (let* ((input-method-alist
          '(("test-input-method" "Test Language" nil "TT" "Description")))
         (target (vertico-buffer-frame--input-method-target
                  "test-input-method")))
    (should (eq (car target) 'text))
    (with-temp-buffer
      (funcall (nth 2 target))
      (should (string-match-p "Test Language" (buffer-string)))
      (should (string-match-p "TT" (buffer-string)))
      (should (string-match-p "Description" (buffer-string))))))

(ert-deftest vertico-buffer-frame-input-method-target-ignores-improper-alist ()
  (let ((input-method-alist '(("test-input-method") . bad)))
    (should-not
     (vertico-buffer-frame--input-method-target "test-input-method"))))

(ert-deftest vertico-buffer-frame-register-target-ignores-register-errors ()
  (cl-letf (((symbol-function #'get-register)
             (lambda (&rest _args)
               (error "Register failed"))))
    (should-not (vertico-buffer-frame--register-target "a"))))

(ert-deftest vertico-buffer-frame-register-target-ignores-display-errors ()
  (cl-letf (((symbol-function #'get-register)
             (lambda (&rest _args)
               "value"))
            ((symbol-function #'single-key-description)
             (lambda (&rest _args)
               (error "Name failed"))))
    (let ((target (vertico-buffer-frame--register-target "a")))
      (with-temp-buffer
        (funcall (nth 2 target))
        (should (string-match-p "value" (buffer-string)))))))

(ert-deftest vertico-buffer-frame-bookmark-target-ignores-bookmark-errors ()
  (require 'bookmark)
  (cl-letf (((symbol-function #'bookmark-get-bookmark)
             (lambda (&rest _args)
               (error "Bookmark failed"))))
    (should-not (vertico-buffer-frame--bookmark-target "broken"))))

(ert-deftest vertico-buffer-frame-bookmark-target-uses-text-fallback-safely ()
  (require 'bookmark)
  (cl-letf (((symbol-function #'bookmark-get-bookmark)
             (lambda (&rest _args)
               'bookmark))
            ((symbol-function #'bookmark-get-filename)
             (lambda (&rest _args)
               "/not/readable"))
            ((symbol-function #'bookmark-get-position)
             (lambda (&rest _args)
               'bad-position))
            ((symbol-function #'file-readable-p)
             (lambda (&rest _args)
               nil)))
    (let ((target (vertico-buffer-frame--bookmark-target "bookmark")))
      (should (eq (car target) 'text))
      (with-temp-buffer
        (funcall (nth 2 target))
        (should (string-match-p "bad-position" (buffer-string)))))))

(ert-deftest vertico-buffer-frame-text-targets-ignore-non-strings ()
  (dolist (function '(vertico-buffer-frame--symbol-preview-target
                      vertico-buffer-frame--color-target
                      vertico-buffer-frame--custom-theme-target
                      vertico-buffer-frame--coding-system-target
                      vertico-buffer-frame--charset-target
                      vertico-buffer-frame--library-target
                      vertico-buffer-frame--package-target
                      vertico-buffer-frame--email-target
                      vertico-buffer-frame--dabbrev-target
                      vertico-buffer-frame--unicode-name-target
                      vertico-buffer-frame--bookmark-target
                      vertico-buffer-frame--info-menu-target
                      vertico-buffer-frame--calendar-month-target))
    (should-not (funcall function 'not-a-string)))
  (let ((input-method-alist 'not-a-list))
    (should-not (vertico-buffer-frame--input-method-target
                 "test-input-method")))
  (should-not (vertico-buffer-frame--bibtex-target 'not-a-string nil))
  (should-not (vertico-buffer-frame--imenu-target
               'not-a-string
               'not-a-string))
  (should-not (vertico-buffer-frame--register-target 'not-a-string))
  (should-not (vertico-buffer-frame--simple-text-target
               'not-a-string
               "Text")))

(ert-deftest vertico-buffer-frame-symbol-preview-ignores-documentation-errors ()
  (cl-letf (((symbol-function #'where-is-internal)
             (lambda (&rest _args)
               (error "Keys failed")))
            ((symbol-function #'documentation-property)
             (lambda (&rest _args)
               (error "Doc failed")))
            ((symbol-function #'facep)
             (lambda (&rest _args)
               (error "Face failed"))))
    (with-temp-buffer
      (vertico-buffer-frame--insert-symbol-preview 'ignore)
      (should (string-match-p "ignore" (buffer-string))))))

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

(ert-deftest vertico-buffer-frame-imenu-target-caches-generated-index ()
  (let* ((source (generate-new-buffer " *vbf-imenu-cache-source*"))
         (minibuffer (generate-new-buffer " *vbf-imenu-cache-minibuffer*"))
         (calls 0))
    (unwind-protect
        (progn
          (require 'imenu)
          (cl-letf (((symbol-function #'minibuffer-selected-window)
                     (lambda ()
                       'source-window))
                    ((symbol-function #'window-live-p)
                     (lambda (window)
                       (eq window 'source-window)))
                    ((symbol-function #'window-buffer)
                     (lambda (window)
                       (and (eq window 'source-window) source)))
                    ((symbol-function
                      #'vertico-buffer-frame--imenu-index-entries)
                     (lambda ()
                       (cl-incf calls)
                       (list (cons "target" 7)))))
            (with-current-buffer minibuffer
              (should (equal (vertico-buffer-frame--imenu-target
                              "target"
                              "target")
                             (list 'buffer-position source 7)))
              (should (equal (vertico-buffer-frame--imenu-target
                              "target"
                              "target")
                             (list 'buffer-position source 7)))
              (should (= calls 1)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-imenu-cache-key-includes-mode-and-indexer ()
  (let ((source (generate-new-buffer " *vbf-imenu-key-source*")))
    (unwind-protect
        (with-current-buffer source
          (let ((imenu-create-index-function #'ignore))
            (fundamental-mode)
            (let ((first (vertico-buffer-frame--imenu-cache-key source)))
              (emacs-lisp-mode)
              (let ((second (vertico-buffer-frame--imenu-cache-key source)))
                (should-not (equal first second)))
              (let ((third (let ((imenu-create-index-function #'identity))
                             (vertico-buffer-frame--imenu-cache-key
                              source))))
                (should-not (equal first third))))))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-imenu-target-ignores-non-list-index ()
  (let ((source (generate-new-buffer " *vbf-imenu-bad-index-source*"))
        (minibuffer (generate-new-buffer " *vbf-imenu-bad-index-minibuffer*")))
    (unwind-protect
        (progn
          (require 'imenu)
          (cl-letf (((symbol-function #'vertico-buffer-frame--origin-buffer)
                     (lambda ()
                       source))
                    ((symbol-function
                      #'vertico-buffer-frame--imenu-index-entries)
                     (lambda ()
                       'not-a-list)))
            (with-current-buffer minibuffer
              (should-not
               (vertico-buffer-frame--imenu-target "target" "target")))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-imenu-target-ignores-improper-index ()
  (let ((source (generate-new-buffer " *vbf-imenu-improper-index-source*"))
        (minibuffer (generate-new-buffer " *vbf-imenu-improper-index-minibuffer*")))
    (unwind-protect
        (progn
          (require 'imenu)
          (cl-letf (((symbol-function #'vertico-buffer-frame--origin-buffer)
                     (lambda ()
                       source))
                    ((symbol-function
                      #'vertico-buffer-frame--imenu-index-entries)
                     (lambda ()
                       '(("target" . 7) . bad))))
            (with-current-buffer minibuffer
              (should-not
               (vertico-buffer-frame--imenu-target "target" "target")))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-imenu-target-ignores-cache-key-errors ()
  (let ((source (generate-new-buffer " *vbf-imenu-cache-error-source*"))
        (minibuffer (generate-new-buffer " *vbf-imenu-cache-error-minibuffer*")))
    (unwind-protect
        (progn
          (require 'imenu)
          (cl-letf (((symbol-function #'vertico-buffer-frame--origin-buffer)
                     (lambda ()
                       source))
                    ((symbol-function #'vertico-buffer-frame--imenu-cache-key)
                     (lambda (&rest _args)
                       (error "Cache failed"))))
            (with-current-buffer minibuffer
              (should-not
               (vertico-buffer-frame--imenu-target "target" "target")))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-imenu-find-entry-skips-non-string-names ()
  (should (equal (vertico-buffer-frame--imenu-find-entry
                  "target"
                  '((42 . 1)
                    ("group" ("target" . 7))))
                 '("target" . 7))))

(ert-deftest vertico-buffer-frame-consult-imenu-target-uses-consult-prefix ()
  (let ((source (generate-new-buffer " *vbf-consult-imenu-source*"))
        (minibuffer (generate-new-buffer " *vbf-consult-imenu-minibuffer*"))
        (consult-imenu-config
         '((emacs-lisp-mode :toplevel "Functions"))))
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
                  ((symbol-function
                    #'vertico-buffer-frame--imenu-index-entries)
                   (lambda ()
                     (list (cons "target" 7)))))
          (with-current-buffer source
            (emacs-lisp-mode))
          (with-current-buffer minibuffer
            (should (equal (vertico-buffer-frame--imenu-target
                            "Functions target"
                            "Functions target")
                           (list 'buffer-position source 7)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-consult-imenu-target-handles-deduplicated-name ()
  (let ((source (generate-new-buffer " *vbf-consult-imenu-dupe-source*"))
        (minibuffer (generate-new-buffer " *vbf-consult-imenu-dupe-minibuffer*"))
        (consult-imenu-config
         '((emacs-lisp-mode :toplevel "Functions"))))
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
                  ((symbol-function
                    #'vertico-buffer-frame--imenu-index-entries)
                   (lambda ()
                     (list (cons "target" 7)
                           (cons "target" 11)))))
          (with-current-buffer source
            (emacs-lisp-mode))
          (with-current-buffer minibuffer
            (should (equal (vertico-buffer-frame--imenu-target
                            "Functions target (1)"
                            "Functions target (1)")
                           (list 'buffer-position source 11)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list source minibuffer)))))

(ert-deftest vertico-buffer-frame-consult-imenu-table-cache-reuses-entries ()
  (with-temp-buffer
    (let ((entries-a (list (cons "a" 1)))
          (entries-b (list (cons "b" 2)))
          (calls 0))
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--consult-imenu-entry-table)
                 (lambda (entries _buffer)
                   (cl-incf calls)
                   (let ((table (make-hash-table :test #'equal)))
                     (puthash (caar entries) (cdar entries) table)
                     table))))
        (should (equal
                 (gethash "a"
                          (vertico-buffer-frame--consult-imenu-entry-table-cached
                           entries-a
                           (current-buffer)))
                 1))
        (should (equal
                 (gethash "a"
                          (vertico-buffer-frame--consult-imenu-entry-table-cached
                           entries-a
                           (current-buffer)))
                 1))
        (should (= calls 1))
        (should (equal
                 (gethash "b"
                          (vertico-buffer-frame--consult-imenu-entry-table-cached
                           entries-b
                           (current-buffer)))
                 2))
        (should (= calls 2))))))

(ert-deftest vertico-buffer-frame-consult-imenu-config-ignores-invalid-config ()
  (let ((source (generate-new-buffer " *vbf-consult-imenu-bad-config*"))
        (consult-imenu-config 'not-a-list))
    (unwind-protect
        (with-current-buffer source
          (emacs-lisp-mode)
          (should-not
           (vertico-buffer-frame--consult-imenu-config source)))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-consult-imenu-config-ignores-improper-config ()
  (let ((source (generate-new-buffer " *vbf-consult-imenu-improper-config*"))
        (consult-imenu-config '((emacs-lisp-mode :toplevel "Functions") . bad)))
    (unwind-protect
        (with-current-buffer source
          (emacs-lisp-mode)
          (should-not
           (vertico-buffer-frame--consult-imenu-config source)))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-consult-imenu-config-ignores-malformed-entry ()
  (let ((source (generate-new-buffer " *vbf-consult-imenu-malformed-config*"))
        (consult-imenu-config '(("bad-mode" :toplevel "Bad"))))
    (unwind-protect
        (with-current-buffer source
          (emacs-lisp-mode)
          (should-not
           (vertico-buffer-frame--consult-imenu-config source)))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-consult-imenu-items-ignores-atoms ()
  (with-temp-buffer
    (let ((consult-imenu-config '((fundamental-mode :toplevel "Top"))))
      (fundamental-mode)
      (should (equal
               (vertico-buffer-frame--consult-imenu-items
                '(bad ("target" . 1))
                (current-buffer))
               '(("Top" ("target" . 1))))))))

(ert-deftest vertico-buffer-frame-consult-imenu-flatten-ignores-improper-entries ()
  (should-not
   (vertico-buffer-frame--consult-imenu-flatten
    nil
    '(("target" . 1) . bad))))

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

(ert-deftest vertico-buffer-frame-info-menu-target-ignores-invalid-buffer ()
  (let ((Info-complete-menu-buffer 'not-a-buffer))
    (should-not
     (vertico-buffer-frame--info-menu-target "Target"))))

(ert-deftest vertico-buffer-frame-info-menu-target-falls-back-after-buffer-death ()
  (let ((menu-buffer (generate-new-buffer " *vbf-info-menu*")))
    (unwind-protect
        (let (target)
          (with-current-buffer menu-buffer
            (insert "* Menu:\n\n* Target:: Entry text\n"))
          (let ((Info-complete-menu-buffer menu-buffer))
            (setq target (vertico-buffer-frame--info-menu-target "Target")))
          (kill-buffer menu-buffer)
          (with-temp-buffer
            (funcall (nth 2 target))
            (should (equal (buffer-string) "Target\n"))))
      (when (buffer-live-p menu-buffer)
        (kill-buffer menu-buffer)))))

(ert-deftest vertico-buffer-frame-calendar-month-target-ignores-calendar-errors ()
  (require 'calendar)
  (cl-letf (((symbol-function #'calendar-make-alist)
             (lambda (&rest _args)
               (error "Calendar failed"))))
    (should-not
     (vertico-buffer-frame--calendar-month-target "January")))
  (cl-letf (((symbol-function #'calendar-generate-month)
             (lambda (&rest _args)
               (error "Generate failed"))))
    (let ((target (vertico-buffer-frame--calendar-month-target "January")))
      (with-temp-buffer
        (funcall (nth 2 target))
        (should (equal (buffer-string) "January\n"))))))

(ert-deftest vertico-buffer-frame-origin-buffer-skips-dead-window ()
  (let (window-buffer-called)
    (cl-letf (((symbol-function #'minibuffer-selected-window)
               (lambda ()
                 'dead-window))
              ((symbol-function #'window-live-p)
               (lambda (_window)
                 nil))
              ((symbol-function #'window-buffer)
               (lambda (_window)
                 (setq window-buffer-called t)
                 'buffer)))
      (should-not (vertico-buffer-frame--origin-buffer))
      (should-not window-buffer-called))))

(ert-deftest vertico-buffer-frame-origin-buffer-ignores-live-check-errors ()
  (let (window-buffer-called)
    (cl-letf (((symbol-function #'minibuffer-selected-window)
               (lambda ()
                 'stale-window))
              ((symbol-function #'window-live-p)
               (lambda (_window)
                 (error "Stale window")))
              ((symbol-function #'window-buffer)
               (lambda (_window)
                 (setq window-buffer-called t)
                 'buffer)))
      (should-not (vertico-buffer-frame--origin-buffer))
      (should-not window-buffer-called))))

(ert-deftest vertico-buffer-frame-bibtex-target-finds-entry ()
  (let ((source (generate-new-buffer " *vbf-bibtex-source*")))
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--origin-buffer)
                   (lambda ()
                     source)))
          (with-current-buffer source
            (insert "@Article{knuth1984,\n  title = {Literate Programming}\n}\n"))
          (should (equal (vertico-buffer-frame--bibtex-target
                          "knuth1984"
                          nil)
                         (list 'buffer-position source 1))))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-bibtex-target-ignores-buffer-errors ()
  (let ((source (generate-new-buffer " *vbf-bibtex-error-source*")))
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--origin-buffer)
                   (lambda ()
                     source))
                  ((symbol-function #'widen)
                   (lambda ()
                     (error "Widen failed"))))
          (should-not
           (vertico-buffer-frame--bibtex-target "knuth1984" nil)))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest vertico-buffer-frame-buffer-position-target-displays-buffer ()
  (let ((source (generate-new-buffer " *vbf-position-source*"))
        events)
    (unwind-protect
        (cl-letf (((symbol-function #'vertico-buffer-frame--preview-window)
                   (lambda (&optional _keep-buffer)
                     'window))
                  ((symbol-function
                    #'vertico-buffer-frame--set-preview-window-buffer)
                   (lambda (window buffer &optional _old-preview-buffer)
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
