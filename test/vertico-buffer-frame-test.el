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
         (old-saved-state vertico-buffer-frame--saved-state)
         (old-minibuffers vertico-buffer-frame--minibuffers)
         (old-minibuffer-setup-hook minibuffer-setup-hook))
     (unwind-protect
         (progn
           (when vertico-buffer-frame-mode
             (vertico-buffer-frame-mode -1))
           (setq vertico-buffer-frame--saved-state nil
                 vertico-buffer-frame--minibuffers nil)
           ,@body)
       (when vertico-buffer-frame-mode
         (vertico-buffer-frame-mode -1))
       (vertico-buffer-frame-cleanup)
       (setq vertico-buffer-display-action old-action
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

(ert-deftest vertico-buffer-frame-candidate-size-rounds-to-characters ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 1080))
              ((symbol-function #'frame-char-width)
               (lambda (_frame) 10))
              ((symbol-function #'frame-char-height)
               (lambda (_frame) 20)))
      (should (equal (vertico-buffer-frame--candidate-frame-size 'parent)
                     '(108 . 33))))))

(ert-deftest vertico-buffer-frame-preview-size-uses-candidate-frame ()
  (cl-letf (((symbol-function #'frame-pixel-width)
             (lambda (frame)
               (if (eq frame 'candidate) 1080 1920)))
            ((symbol-function #'frame-pixel-height)
             (lambda (frame)
               (if (eq frame 'candidate) 667 1080)))
            ((symbol-function #'frame-char-width)
             (lambda (_frame) 10))
            ((symbol-function #'frame-char-height)
             (lambda (_frame) 20)))
    (should (equal (vertico-buffer-frame--preview-frame-size
                    'parent 'candidate)
                   '(67 . 21)))))

(ert-deftest vertico-buffer-frame-base-parameters-use-golden-size ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0)
        (vertico-buffer-frame-border-width 2)
        (vertico-buffer-frame-candidate-accept-focus nil)
        (vertico-buffer-frame-parameters '((alpha . 95))))
    (cl-letf (((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920))
              ((symbol-function #'frame-pixel-height)
               (lambda (_frame) 1080))
              ((symbol-function #'frame-char-width)
               (lambda (_frame) 10))
              ((symbol-function #'frame-char-height)
               (lambda (_frame) 20)))
      (let ((parameters
             (vertico-buffer-frame--base-parameters
              (selected-frame) "Vertico test")))
        (should (equal (alist-get 'width parameters) 108))
        (should (equal (alist-get 'height parameters) 33))
        (should (equal (alist-get 'child-frame-border-width parameters) 2))
        (should (eq (alist-get 'no-accept-focus parameters) t))
        (should-not (assq 'minibuffer-exit parameters))
        (should (equal (alist-get 'alpha parameters) 95))))))

(ert-deftest vertico-buffer-frame-base-parameters-honor-focus-option ()
  (let ((vertico-buffer-frame-candidate-accept-focus t))
    (should-not
     (alist-get 'no-accept-focus
                (vertico-buffer-frame--base-parameters
                 (selected-frame) "Vertico test")))))

(ert-deftest vertico-buffer-frame-sync-frame-resizes-from-parent-size ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0)
        set-size
        set-position
        visible)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--frame 'child
                  vertico-buffer-frame--parent 'parent)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame '(child parent))))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (if (eq frame 'parent) 1920 1080)))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (if (eq frame 'parent) 1080 667)))
                ((symbol-function #'frame-char-width)
                 (lambda (_frame) 10))
                ((symbol-function #'frame-char-height)
                 (lambda (_frame) 20))
                ((symbol-function #'frame-width)
                 (lambda (_frame) 1))
                ((symbol-function #'frame-height)
                 (lambda (_frame) 1))
                ((symbol-function #'set-frame-size)
                 (lambda (frame width height)
                   (setq set-size (list frame width height))))
                ((symbol-function #'set-frame-position)
                 (lambda (frame left top)
                   (setq set-position (list frame left top))))
                ((symbol-function #'frame-visible-p)
                 (lambda (_frame) nil))
                ((symbol-function #'make-frame-visible)
                 (lambda (frame)
                   (setq visible frame))))
        (vertico-buffer-frame--sync-frame)
        (should (equal set-size '(child 108 33)))
        (should (equal set-position '(child 420 206)))
        (should (eq visible 'child))))))

(ert-deftest vertico-buffer-frame-sync-preview-overlays-candidate-lower-right ()
  (let (set-size
        set-position
        visible)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--frame 'candidate
                  vertico-buffer-frame--preview-frame 'preview
                  vertico-buffer-frame--parent 'parent)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame '(candidate preview parent))))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame)
                   (pcase frame
                     ('candidate 1080)
                     ('preview 667)
                     (_ 1920))))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame)
                   (pcase frame
                     ('candidate 667)
                     ('preview 412)
                     (_ 1080))))
                ((symbol-function #'frame-char-width)
                 (lambda (_frame) 10))
                ((symbol-function #'frame-char-height)
                 (lambda (_frame) 20))
                ((symbol-function #'frame-width)
                 (lambda (_frame) 1))
                ((symbol-function #'frame-height)
                 (lambda (_frame) 1))
                ((symbol-function #'frame-position)
                 (lambda (_frame) '(420 . 206)))
                ((symbol-function #'set-frame-size)
                 (lambda (frame width height)
                   (setq set-size (list frame width height))))
                ((symbol-function #'set-frame-position)
                 (lambda (frame left top)
                   (setq set-position (list frame left top))))
                ((symbol-function #'frame-visible-p)
                 (lambda (_frame) nil))
                ((symbol-function #'make-frame-visible)
                 (lambda (frame)
                   (setq visible frame))))
        (vertico-buffer-frame--sync-preview-frame)
        (should (equal set-size '(preview 67 21)))
        (should (equal set-position '(preview 833 461)))
        (should (eq visible 'preview))))))

(ert-deftest vertico-buffer-frame-owner-disables-minibuffer-exit-parameter ()
  (let (parameters)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (_frame) t))
              ((symbol-function #'modify-frame-parameters)
               (lambda (_frame params)
                 (setq parameters params))))
      (vertico-buffer-frame--set-frame-owner-buffer 'frame 'owner)
      (should (eq (alist-get vertico-buffer-frame--owner-buffer-parameter
                             parameters)
                  'owner))
      (should-not (alist-get 'minibuffer-exit parameters))
      (should (eq (alist-get 'share-child-frame parameters) 'owner)))))

(ert-deftest vertico-buffer-frame-owner-honors-share-parameter ()
  (let (parameters)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (_frame) t))
              ((symbol-function #'modify-frame-parameters)
               (lambda (_frame params)
                 (setq parameters params))))
      (vertico-buffer-frame--set-frame-owner-buffer
       'frame 'owner '(owner preview))
      (should (eq (alist-get vertico-buffer-frame--owner-buffer-parameter
                             parameters)
                  'owner))
      (should (equal (alist-get 'share-child-frame parameters)
                     '(owner preview))))))

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
           (setq vertico-buffer-frame--saved-state
                 '(:display-action (display-buffer-at-bottom)
                   :buffer-mode nil))
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
               (should (eq (alist-get
                            'share-child-frame
                            (alist-get 'child-frame-parameters
                                       captured-action))
                           owner))
               (should (memq owner vertico-buffer-frame--minibuffers)))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-display-buffer-creates-preview-role-frame ()
  (vertico-buffer-frame-test--with-clean-state
   (let ((buffer (generate-new-buffer " *vbf-preview-role*"))
         captured-action
         owner-recorded
         share-recorded)
     (unwind-protect
         (with-temp-buffer
           (let ((owner (current-buffer)))
             (cl-letf (((symbol-function #'display-buffer-in-child-frame)
                        (lambda (_buffer action)
                          (setq captured-action action)
                          (selected-window)))
                       ((symbol-function
                         #'vertico-buffer-frame--set-frame-owner-buffer)
                        (lambda (_frame buffer &optional share)
                          (setq owner-recorded buffer
                                share-recorded share))))
               (should (eq (vertico-buffer-frame--display-buffer-in-child-frame
                            buffer
                            (selected-frame)
                            "Vertico preview"
                            nil
                            '(20 . 10)
                            'preview)
                           (selected-window)))
               (let ((parameters
                      (alist-get 'child-frame-parameters captured-action)))
                 (should (eq owner-recorded owner))
                 (should (equal share-recorded (list owner 'preview)))
                 (should (equal (alist-get 'share-child-frame parameters)
                                (list owner 'preview)))
                 (should (eq (alist-get 'vertico-buffer-frame-role parameters)
                             'preview))
                 (should (eq (alist-get 'no-accept-focus parameters) t))
                 (should (equal (alist-get 'width parameters) 20))
                 (should (equal (alist-get 'height parameters) 10))))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-recursive-minibuffers-use-distinct-frames ()
  (vertico-buffer-frame-test--with-clean-state
   (let ((outer-owner (generate-new-buffer " *vbf-outer-owner*"))
         (inner-owner (generate-new-buffer " *vbf-inner-owner*"))
         (candidate (generate-new-buffer " *vbf-recursive-candidate*"))
         (parent (selected-frame))
         actions
         owners)
     (unwind-protect
         (cl-letf (((symbol-function #'display-buffer-in-child-frame)
                    (lambda (_buffer action)
                      (push action actions)
                      (selected-window)))
                   ((symbol-function
                     #'vertico-buffer-frame--set-frame-owner-buffer)
                    (lambda (_frame owner)
                      (push owner owners))))
           (dolist (owner (list outer-owner inner-owner))
             (with-current-buffer owner
               (vertico-buffer-frame--display-buffer-in-child-frame
                candidate parent "Vertico recursive" nil)))
           (setq actions (nreverse actions)
                 owners (nreverse owners))
           (should (equal owners (list outer-owner inner-owner)))
           (should (eq (alist-get
                        'share-child-frame
                        (alist-get 'child-frame-parameters
                                   (nth 0 actions)))
                       outer-owner))
           (should (eq (alist-get
                        'share-child-frame
                        (alist-get 'child-frame-parameters
                                   (nth 1 actions)))
                       inner-owner)))
       (dolist (buffer (list outer-owner inner-owner candidate))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

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
           (setq vertico-buffer-frame--saved-state
                 '(:display-action (display-buffer-pop-up-window)
                   :buffer-mode nil))
           (should (eq (vertico-buffer-frame--display-buffer buffer nil)
                       'fallback-window))
           (should (equal displayed
                          (list buffer
                                '(display-buffer-pop-up-window)))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-cleanup-minibuffer-deletes-owned-frames ()
  (let ((buffer (generate-new-buffer " *vbf-cleanup*"))
        deleted-owner)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local vertico-buffer-frame--frame 'frame
                      vertico-buffer-frame--window 'window
                      vertico-buffer-frame--parent 'parent
                      vertico-buffer-frame--preview-frame 'preview-frame
                      vertico-buffer-frame--preview-window 'preview-window)
          (add-hook 'minibuffer-exit-hook
                    #'vertico-buffer-frame--minibuffer-exit nil t)
          (setq vertico-buffer-frame--minibuffers (list buffer))
          (cl-letf (((symbol-function
                      #'vertico-buffer-frame--delete-frames-owned-by-buffer)
                     (lambda (owner)
                       (setq deleted-owner owner))))
            (vertico-buffer-frame--cleanup-minibuffer buffer)
            (should (eq deleted-owner buffer))
            (should-not vertico-buffer-frame--frame)
            (should-not vertico-buffer-frame--window)
            (should-not vertico-buffer-frame--parent)
            (should-not vertico-buffer-frame--preview-frame)
            (should-not vertico-buffer-frame--preview-window)
            (should-not (memq #'vertico-buffer-frame--minibuffer-exit
                              minibuffer-exit-hook))
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

(ert-deftest vertico-buffer-frame-consult-mirror-window-uses-preview-frame ()
  (let ((owner (generate-new-buffer " *vbf-consult-owner*"))
        (source-buffer (generate-new-buffer " *vbf-consult-source*"))
        set-buffer
        synced
        copied
        highlighted)
    (unwind-protect
        (with-current-buffer owner
          (let ((vertico-buffer-frame-mode t)
                (vertico-buffer-frame-consult-preview t))
            (setq-local vertico-buffer-frame--frame 'candidate
                        vertico-buffer-frame--parent 'parent)
            (cl-letf (((symbol-function #'active-minibuffer-window)
                       (lambda () 'minibuffer-window))
                      ((symbol-function #'window-live-p)
                       (lambda (window)
                         (memq window
                               '(minibuffer-window source-window
                                                   preview-window))))
                      ((symbol-function #'window-buffer)
                       (lambda (window)
                         (pcase window
                           ('minibuffer-window owner)
                           ('source-window source-buffer)
                           ('preview-window source-buffer))))
                      ((symbol-function #'frame-live-p)
                       (lambda (frame)
                         (memq frame '(candidate parent))))
                      ((symbol-function #'display-graphic-p)
                       (lambda (&optional _frame) t))
                      ((symbol-function
                        #'vertico-buffer-frame--ensure-preview-window)
                       (lambda (buffer parent)
                         (should (eq buffer source-buffer))
                         (should (eq parent 'parent))
                         'preview-window))
                      ((symbol-function
                        #'vertico-buffer-frame--set-preview-window-buffer)
                       (lambda (window buffer)
                         (setq set-buffer (list window buffer))))
                      ((symbol-function
                        #'vertico-buffer-frame--sync-preview-frame)
                       (lambda ()
                         (setq synced t)))
                      ((symbol-function
                        #'vertico-buffer-frame--copy-window-view)
                       (lambda (source target)
                         (setq copied (list source target))))
                      ((symbol-function
                        #'vertico-buffer-frame--highlight-preview-line)
                       (lambda (window)
                         (setq highlighted window))))
              (should (eq (vertico-buffer-frame-consult-preview-mirror-window
                           'source-window)
                          'preview-window))
              (should (equal set-buffer
                             (list 'preview-window source-buffer)))
              (should synced)
              (should (equal copied '(source-window preview-window)))
              (should (eq highlighted 'preview-window)))))
      (dolist (buffer (list owner source-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-consult-state-wrapper-mirrors-preview ()
  (let (events)
    (cl-letf (((symbol-function
                #'vertico-buffer-frame-consult-preview-mirror-window)
               (lambda (_window)
                 (push 'mirror events)))
              ((symbol-function #'vertico-buffer-frame-consult-preview-hide)
               (lambda ()
                 (push 'hide events))))
      (let ((state (vertico-buffer-frame-consult--wrap-state
                    (lambda (action cand)
                      (push (list 'state action cand) events)
                      'state-result))))
        (should (eq (funcall state 'preview 'candidate)
                    'state-result))
        (funcall state 'preview nil)
        (funcall state 'exit nil)
        (should (equal (nreverse events)
                       '((state preview candidate)
                         mirror
                         (state preview nil)
                         hide
                         (state exit nil)
                         hide)))))))

(provide 'vertico-buffer-frame-test)
;;; vertico-buffer-frame-test.el ends here
