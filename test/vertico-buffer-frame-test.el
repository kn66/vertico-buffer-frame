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
  (load (expand-file-name "vertico-buffer-frame.el"
                          vertico-buffer-frame-test--root)
        nil t))

(defmacro vertico-buffer-frame-test--with-clean-state (&rest body)
  "Run BODY with global mode state restored afterwards."
  (declare (indent 0) (debug t))
  `(let ((old-frame-mode vertico-buffer-frame-mode)
         (old-buffer-mode (bound-and-true-p vertico-buffer-mode))
         (old-action vertico-buffer-display-action)
         (old-saved-action vertico-buffer-frame--saved-display-action)
         (old-saved-buffer-mode vertico-buffer-frame--saved-buffer-mode)
         (old-saved-state vertico-buffer-frame--saved-state)
         (old-minibuffers vertico-buffer-frame--minibuffers)
         (old-minibuffer-setup-hook minibuffer-setup-hook))
     (unwind-protect
         (progn
           (when vertico-buffer-frame-mode
             (vertico-buffer-frame-mode -1))
           (setq vertico-buffer-frame--saved-display-action nil
                 vertico-buffer-frame--saved-buffer-mode nil
                 vertico-buffer-frame--saved-state nil
                 vertico-buffer-frame--minibuffers nil)
           ,@body)
       (when vertico-buffer-frame-mode
         (vertico-buffer-frame-mode -1))
       (vertico-buffer-frame-cleanup)
       (setq vertico-buffer-display-action old-action
             vertico-buffer-frame--saved-display-action old-saved-action
             vertico-buffer-frame--saved-buffer-mode old-saved-buffer-mode
             vertico-buffer-frame--saved-state old-saved-state
             vertico-buffer-frame--minibuffers old-minibuffers
             minibuffer-setup-hook old-minibuffer-setup-hook)
       (if old-buffer-mode
           (vertico-buffer-mode 1)
         (vertico-buffer-mode -1))
       (when old-frame-mode
         (vertico-buffer-frame-mode 1)))))

(ert-deftest vertico-buffer-frame-display-action-is-simple ()
  (should (equal (vertico-buffer-frame-display-action)
                 '(vertico-buffer-frame--display-buffer))))

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
   (should vertico-buffer-mode)
   (should (equal vertico-buffer-display-action
                  '(display-buffer-pop-up-window)))))

(ert-deftest vertico-buffer-frame-mode-adds-setup-hook ()
  (vertico-buffer-frame-test--with-clean-state
   (setq minibuffer-setup-hook nil)
   (vertico-buffer-frame-mode 1)
   (should (memq #'vertico-buffer-frame--setup-minibuffer
                 minibuffer-setup-hook))
   (vertico-buffer-frame-mode -1)
   (should-not (memq #'vertico-buffer-frame--setup-minibuffer
                     minibuffer-setup-hook))))

(ert-deftest vertico-buffer-frame-base-parameters-use-fixed-size ()
  (let ((vertico-buffer-frame-width 72)
        (vertico-buffer-frame-height 11)
        (vertico-buffer-frame-border-width 2)
        (vertico-buffer-frame-candidate-accept-focus nil)
        (vertico-buffer-frame-parameters '((alpha . 95))))
    (let ((parameters
           (vertico-buffer-frame--base-parameters
            (selected-frame) "Vertico test")))
      (should (equal (alist-get 'width parameters) 72))
      (should (equal (alist-get 'height parameters) 11))
      (should (equal (alist-get 'child-frame-border-width parameters) 2))
      (should (eq (alist-get 'no-accept-focus parameters) t))
      (should (equal (alist-get 'alpha parameters) 95)))))

(ert-deftest vertico-buffer-frame-base-parameters-honor-focus-option ()
  (let ((vertico-buffer-frame-candidate-accept-focus t))
    (should-not
     (alist-get 'no-accept-focus
                (vertico-buffer-frame--base-parameters
                 (selected-frame) "Vertico test")))))

(ert-deftest vertico-buffer-frame-minibuffer-setup-installs-hooks ()
  (vertico-buffer-frame-test--with-clean-state
   (with-temp-buffer
     (let ((vertico-buffer-frame-mode t)
           (vertico-buffer-frame--minibuffers nil))
       (vertico-buffer-frame--setup-minibuffer)
       (should-not mode-line-format)
       (should-not header-line-format)
       (should-not tab-line-format)
       (should (memq #'vertico-buffer-frame--minibuffer-exit
                     minibuffer-exit-hook))
       (should (memq (current-buffer)
                     vertico-buffer-frame--minibuffers))))))

(ert-deftest vertico-buffer-frame-display-buffer-falls-back-when-unsupported ()
  (vertico-buffer-frame-test--with-clean-state
   (let ((buffer (generate-new-buffer " *vbf-fallback*"))
         displayed)
     (unwind-protect
         (cl-letf (((symbol-function #'display-graphic-p)
                    (lambda (&optional _frame) nil))
                   ((symbol-function #'display-buffer)
                    (lambda (buffer action)
                      (setq displayed (list buffer action))
                      'fallback-window)))
           (setq vertico-buffer-frame--saved-state t
                 vertico-buffer-frame--saved-display-action
                 '(display-buffer-at-bottom))
           (should (eq (vertico-buffer-frame--display-buffer
                        buffer '((reusable-frames . visible)))
                       'fallback-window))
           (should (equal displayed
                          (list buffer
                                '(display-buffer-at-bottom
                                  (reusable-frames . visible))))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-display-buffer-creates-child-frame ()
  (vertico-buffer-frame-test--with-clean-state
   (let ((buffer (generate-new-buffer " *vbf-child*"))
         captured-action
         owner-recorded
         synced)
     (unwind-protect
         (with-temp-buffer
           (let ((owner (current-buffer)))
             (cl-letf (((symbol-function #'display-graphic-p)
                        (lambda (&optional _frame) t))
                       ((symbol-function #'display-buffer-in-child-frame)
                        (lambda (_buffer action)
                          (setq captured-action action)
                          (selected-window)))
                       ((symbol-function
                         #'vertico-buffer-frame--set-frame-owner-buffer)
                        (lambda (_frame buffer)
                          (setq owner-recorded buffer)))
                       ((symbol-function #'vertico-buffer-frame--sync-frame)
                        (lambda ()
                          (setq synced t))))
               (should (eq (vertico-buffer-frame--display-buffer
                            buffer nil)
                           (selected-window)))
               (should (eq owner-recorded owner))
               (should synced)
               (should (assq 'child-frame-parameters captured-action))
               (should (memq owner vertico-buffer-frame--minibuffers)))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-display-buffer-falls-back-on-error ()
  (vertico-buffer-frame-test--with-clean-state
   (let ((buffer (generate-new-buffer " *vbf-error*"))
         displayed)
     (unwind-protect
         (cl-letf (((symbol-function #'display-graphic-p)
                    (lambda (&optional _frame) t))
                   ((symbol-function #'display-buffer-in-child-frame)
                    (lambda (&rest _args)
                      (error "child frame failed")))
                   ((symbol-function #'display-buffer)
                    (lambda (buffer action)
                      (setq displayed (list buffer action))
                      'fallback-window)))
           (setq vertico-buffer-frame--saved-state t
                 vertico-buffer-frame--saved-display-action
                 '(display-buffer-pop-up-window))
           (should (eq (vertico-buffer-frame--display-buffer buffer nil)
                       'fallback-window))
           (should (equal displayed
                          (list buffer
                                '(display-buffer-pop-up-window)))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-cleanup-minibuffer-deletes-frame ()
  (let ((buffer (generate-new-buffer " *vbf-cleanup*"))
        deleted
        deleted-owner)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local vertico-buffer-frame--frame 'frame
                      vertico-buffer-frame--window 'window
                      vertico-buffer-frame--parent 'parent
                      vertico-buffer-frame--cleanup-installed t)
          (setq vertico-buffer-frame--minibuffers (list buffer))
          (cl-letf (((symbol-function #'vertico-buffer-frame--delete-frame)
                     (lambda (frame)
                       (setq deleted frame)))
                    ((symbol-function
                      #'vertico-buffer-frame--delete-frames-owned-by-buffer)
                     (lambda (owner)
                       (setq deleted-owner owner))))
            (vertico-buffer-frame--cleanup-minibuffer buffer)
            (should (eq deleted 'frame))
            (should (eq deleted-owner buffer))
            (should-not vertico-buffer-frame--frame)
            (should-not vertico-buffer-frame--window)
            (should-not vertico-buffer-frame--parent)
            (should-not vertico-buffer-frame--cleanup-installed)
            (should-not (memq buffer vertico-buffer-frame--minibuffers))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-cleanup-dead-minibuffer-deletes-owned-frames ()
  (let ((buffer (generate-new-buffer " *vbf-dead-cleanup*"))
        deleted-owner)
    (kill-buffer buffer)
    (cl-letf (((symbol-function
                #'vertico-buffer-frame--delete-frames-owned-by-buffer)
               (lambda (owner)
                 (setq deleted-owner owner))))
      (setq vertico-buffer-frame--minibuffers (list buffer))
      (vertico-buffer-frame--cleanup-minibuffer buffer)
      (should (eq deleted-owner buffer))
      (should-not (memq buffer vertico-buffer-frame--minibuffers)))))

(ert-deftest vertico-buffer-frame-mode-disable-deletes-owned-frames ()
  (vertico-buffer-frame-test--with-clean-state
   (let (deleted)
     (cl-letf (((symbol-function
                 #'vertico-buffer-frame--delete-owned-frames)
                (lambda ()
                  (setq deleted t))))
       (vertico-buffer-frame-mode 1)
       (vertico-buffer-frame-mode -1)
       (should deleted)))))

(provide 'vertico-buffer-frame-test)
;;; vertico-buffer-frame-test.el ends here
