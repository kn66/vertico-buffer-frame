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
         (old-consult-preview
          (default-value 'vertico-buffer-frame-consult-preview))
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
             vertico-buffer-frame-consult-preview old-consult-preview
             vertico-buffer-frame--saved-state old-saved-state
             vertico-buffer-frame--minibuffers old-minibuffers
             minibuffer-setup-hook old-minibuffer-setup-hook)
       (setq-default vertico-buffer-frame-consult-preview old-consult-preview)
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

(ert-deftest vertico-buffer-frame-mode-preserves-display-buffer-alist ()
  (vertico-buffer-frame-test--with-clean-state
   (let ((display-buffer-alist
          '(("\\`\\*Example\\*" display-buffer-pop-up-frame))))
     (vertico-buffer-frame-mode 1)
     (vertico-buffer-frame-mode -1)
     (should (equal display-buffer-alist
                    '(("\\`\\*Example\\*" display-buffer-pop-up-frame)))))))

(ert-deftest vertico-buffer-frame-local-mode-restores-state ()
  (vertico-buffer-frame-test--with-clean-state
   (vertico-buffer-mode -1)
   (setq vertico-buffer-display-action '(display-buffer-at-bottom))
   (with-temp-buffer
     (let ((buffer (current-buffer)))
       (cl-letf (((symbol-function #'minibufferp)
                  (lambda (&optional _buffer) t)))
         (vertico-buffer-frame-local-mode 1)
         (should vertico-buffer-frame-local-mode)
         (should vertico-buffer-mode)
         (should (local-variable-p 'vertico-buffer-mode))
         (should (equal vertico-buffer-display-action
                        (vertico-buffer-frame-display-action)))
         (should (local-variable-p 'vertico-buffer-display-action))
         (should (memq buffer vertico-buffer-frame--minibuffers))
         (vertico-buffer-frame-local-mode -1)
         (should-not vertico-buffer-frame-local-mode)
         (should-not (local-variable-p 'vertico-buffer-mode))
         (should-not vertico-buffer-mode)
         (should-not (local-variable-p 'vertico-buffer-display-action))
         (should (equal vertico-buffer-display-action
                        '(display-buffer-at-bottom)))
         (should-not (memq buffer vertico-buffer-frame--minibuffers)))))))

(ert-deftest vertico-buffer-frame-local-mode-requires-minibuffer ()
  (vertico-buffer-frame-test--with-clean-state
   (with-temp-buffer
     (should-error (vertico-buffer-frame-local-mode 1)
                   :type 'user-error)
     (should-not vertico-buffer-frame-local-mode))))

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

(ert-deftest vertico-buffer-frame-target-width-disabled-keeps-golden ()
  (let ((vertico-buffer-frame-auto-width nil))
    (cl-letf (((symbol-function #'vertico-buffer-frame--content-pixel-width)
               (lambda () 2000)))
      (should (= (vertico-buffer-frame--candidate-target-width 'parent 80)
                 80)))))

(ert-deftest vertico-buffer-frame-target-width-grows-to-content ()
  (let ((vertico-buffer-frame-auto-width t))
    (cl-letf (((symbol-function #'vertico-buffer-frame--content-pixel-width)
               (lambda () 900))
              ((symbol-function #'frame-char-width)
               (lambda (_frame) 10))
              ((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920)))
      ;; 900px / 10px = 90 columns, plus the 6-column margin.
      (should (= (vertico-buffer-frame--candidate-target-width 'parent 80)
                 96)))))

(ert-deftest vertico-buffer-frame-target-width-never-below-golden ()
  (let ((vertico-buffer-frame-auto-width t))
    (cl-letf (((symbol-function #'vertico-buffer-frame--content-pixel-width)
               (lambda () 200))
              ((symbol-function #'frame-char-width)
               (lambda (_frame) 10))
              ((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920)))
      (should (= (vertico-buffer-frame--candidate-target-width 'parent 80)
                 80)))))

(ert-deftest vertico-buffer-frame-target-width-caps-below-parent-with-margin ()
  (let ((vertico-buffer-frame-auto-width t))
    (cl-letf (((symbol-function #'vertico-buffer-frame--content-pixel-width)
               (lambda () 5000))
              ((symbol-function #'frame-char-width)
               (lambda (_frame) 10))
              ((symbol-function #'frame-pixel-width)
               (lambda (_frame) 1920)))
      ;; Parent is 1920px / 10px = 192 columns; the cap keeps a centered margin
      ;; at `vertico-buffer-frame--auto-width-max-ratio' (0.9), so 172 columns.
      (should (= (vertico-buffer-frame--candidate-target-width 'parent 80)
                 172)))))

(ert-deftest vertico-buffer-frame-target-width-without-content-keeps-golden ()
  (let ((vertico-buffer-frame-auto-width t))
    (cl-letf (((symbol-function #'vertico-buffer-frame--content-pixel-width)
               (lambda () nil)))
      (should (= (vertico-buffer-frame--candidate-target-width 'parent 80)
                 80)))))

(ert-deftest vertico-buffer-frame-effective-width-grows-but-never-shrinks ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-auto-width t))
      (setq-local vertico-buffer-frame--auto-width-floor nil)
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--candidate-target-width)
                 (lambda (_parent _golden) 80)))
        ;; First grows to the content width.
        (should (= (vertico-buffer-frame--effective-target-width 'parent 40)
                   80)))
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--candidate-target-width)
                 (lambda (_parent _golden) 50)))
        ;; Shorter candidates do not shrink the frame below the floor.
        (should (= (vertico-buffer-frame--effective-target-width 'parent 40)
                   80)))
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--candidate-target-width)
                 (lambda (_parent _golden) 100)))
        ;; A wider candidate still grows the frame further.
        (should (= (vertico-buffer-frame--effective-target-width 'parent 40)
                   100))))))

(ert-deftest vertico-buffer-frame-effective-width-disabled-ignores-floor ()
  (with-temp-buffer
    (let ((vertico-buffer-frame-auto-width nil))
      (setq-local vertico-buffer-frame--auto-width-floor 80)
      (cl-letf (((symbol-function
                  #'vertico-buffer-frame--candidate-target-width)
                 (lambda (_parent golden) golden)))
        (should (= (vertico-buffer-frame--effective-target-width 'parent 40)
                   40))))))

(ert-deftest vertico-buffer-frame-clear-frame-state-resets-width-floor ()
  (with-temp-buffer
    (setq-local vertico-buffer-frame--auto-width-floor 120)
    (vertico-buffer-frame--clear-frame-state)
    (should-not vertico-buffer-frame--auto-width-floor)))

(ert-deftest vertico-buffer-frame-visible-candidates-returns-scrolled-slice ()
  (with-temp-buffer
    (setq-local vertico--candidates '("a" "b" "c" "d" "e" "f"))
    (setq-local vertico--scroll 2)
    (setq-local vertico-count 3)
    (should (equal (vertico-buffer-frame--visible-candidates)
                   '("c" "d" "e")))))

(ert-deftest vertico-buffer-frame-preview-size-uses-candidate-frame ()
  ;; The candidate pixel size is derived from its character dimensions
  ;; (108*10 = 1080 wide, 33*20 = 660 tall).  Scaling by 1/golden-ratio gives
  ;; 1080/1.618 = 667px -> 67 cols and 660/1.618 = 408px -> 20 rows.
  (cl-letf (((symbol-function #'frame-width)
             (lambda (frame) (if (eq frame 'candidate) 108 1)))
            ((symbol-function #'frame-height)
             (lambda (frame) (if (eq frame 'candidate) 33 1)))
            ((symbol-function #'frame-char-width)
             (lambda (_frame) 10))
            ((symbol-function #'frame-char-height)
             (lambda (_frame) 20)))
    (should (equal (vertico-buffer-frame--preview-frame-size
                    'parent 'candidate)
                   '(67 . 20)))))

(ert-deftest vertico-buffer-frame-preview-width-tracks-candidate-width ()
  ;; The candidate height is fixed (auto-width only changes width); the preview
  ;; width must grow with the candidate width while the height stays constant.
  (cl-letf (((symbol-function #'frame-height)
             (lambda (frame) (if (eq frame 'candidate) 33 1)))
            ((symbol-function #'frame-char-width)
             (lambda (_frame) 10))
            ((symbol-function #'frame-char-height)
             (lambda (_frame) 20)))
    (let (narrow wide)
      (cl-letf (((symbol-function #'frame-width)
                 (lambda (frame) (if (eq frame 'candidate) 108 1))))
        (setq narrow (vertico-buffer-frame--preview-frame-size
                      'parent 'candidate)))
      (cl-letf (((symbol-function #'frame-width)
                 (lambda (frame) (if (eq frame 'candidate) 156 1))))
        (setq wide (vertico-buffer-frame--preview-frame-size
                    'parent 'candidate)))
      (should (> (car wide) (car narrow)))
      (should (= (cdr wide) (cdr narrow))))))

(ert-deftest vertico-buffer-frame-base-parameters-use-golden-size ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0)
        (vertico-buffer-frame-border-width 2)
        (vertico-buffer-frame-candidate-accept-focus nil)
        (vertico-buffer-frame-parameters '((alpha . 95)
                                           (alpha-background . 90))))
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
        (should (equal (alist-get 'alpha parameters) 95))
        (should (equal (alist-get 'alpha-background parameters) 90))))))

(ert-deftest vertico-buffer-frame-base-parameters-are-opaque-by-default ()
  (let ((parameters
         (vertico-buffer-frame--base-parameters
          (selected-frame) "Vertico test" '(20 . 10))))
    (should (equal (alist-get 'alpha parameters) 100))
    (should (equal (alist-get 'alpha-background parameters) 100))))

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
        ;; Centered from the character size being set: width 108*10 = 1080px
        ;; gives left (1920-1080)/2 = 420; height 33*20 = 660px gives top
        ;; (1080-660)/2 = 210.
        (should (equal set-position '(child 420 210)))
        (should (eq visible 'child))))))

(ert-deftest vertico-buffer-frame-sync-frame-centers-auto-width ()
  (let ((vertico-buffer-frame-golden-ratio-scale 1.0)
        (vertico-buffer-frame-auto-width t)
        set-size
        set-position)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--frame 'child
                  vertico-buffer-frame--parent 'parent)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame) (memq frame '(child parent))))
                ((symbol-function #'vertico-buffer-frame--content-pixel-width)
                 (lambda () 1500))
                ((symbol-function #'frame-pixel-width)
                 (lambda (frame) (if (eq frame 'parent) 1920 1080)))
                ((symbol-function #'frame-pixel-height)
                 (lambda (frame) (if (eq frame 'parent) 1080 667)))
                ((symbol-function #'frame-char-width) (lambda (_frame) 10))
                ((symbol-function #'frame-char-height) (lambda (_frame) 20))
                ((symbol-function #'frame-width) (lambda (_frame) 1))
                ((symbol-function #'frame-height) (lambda (_frame) 1))
                ((symbol-function #'set-frame-size)
                 (lambda (frame width height)
                   (setq set-size (list frame width height))))
                ((symbol-function #'set-frame-position)
                 (lambda (frame left top)
                   (setq set-position (list frame left top))))
                ((symbol-function #'frame-visible-p) (lambda (_frame) t))
                ((symbol-function #'make-frame-visible) #'ignore))
        (vertico-buffer-frame--sync-frame)
        ;; 1500px / 10px = 150 columns plus the 6-column slack = 156, under the
        ;; 0.9 cap (172).  Centered: 156*10 = 1560px gives left
        ;; (1920-1560)/2 = 180.
        (should (equal set-size '(child 156 33)))
        (should (equal set-position '(child 180 210)))))))

(ert-deftest vertico-buffer-frame-auto-width-resyncs-visible-preview ()
  (let ((vertico-buffer-frame-mode t)
        (vertico-buffer-frame-auto-width t)
        synced-frame
        synced-preview)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--frame 'child
                  vertico-buffer-frame--parent 'parent
                  vertico-buffer-frame--preview-frame 'preview)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame) (memq frame '(child parent preview))))
                ((symbol-function #'vertico-buffer-frame--candidate-frame-size)
                 (lambda (_parent) '(80 . 33)))
                ((symbol-function #'vertico-buffer-frame--candidate-target-width)
                 (lambda (_parent _golden) 120))
                ((symbol-function #'frame-width) (lambda (_frame) 80))
                ((symbol-function #'frame-visible-p) (lambda (_frame) t))
                ((symbol-function #'vertico-buffer-frame--sync-frame)
                 (lambda () (setq synced-frame t)))
                ((symbol-function #'vertico-buffer-frame--sync-preview-frame)
                 (lambda () (setq synced-preview t))))
        (vertico-buffer-frame--auto-width-redisplay nil)
        (should synced-frame)
        (should synced-preview)))))

(ert-deftest vertico-buffer-frame-auto-width-skips-hidden-preview ()
  (let ((vertico-buffer-frame-mode t)
        (vertico-buffer-frame-auto-width t)
        synced-frame
        synced-preview)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--frame 'child
                  vertico-buffer-frame--parent 'parent
                  vertico-buffer-frame--preview-frame 'preview)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame) (memq frame '(child parent preview))))
                ((symbol-function #'vertico-buffer-frame--candidate-frame-size)
                 (lambda (_parent) '(80 . 33)))
                ((symbol-function #'vertico-buffer-frame--candidate-target-width)
                 (lambda (_parent _golden) 120))
                ((symbol-function #'frame-width) (lambda (_frame) 80))
                ((symbol-function #'frame-visible-p) (lambda (_frame) nil))
                ((symbol-function #'vertico-buffer-frame--sync-frame)
                 (lambda () (setq synced-frame t)))
                ((symbol-function #'vertico-buffer-frame--sync-preview-frame)
                 (lambda () (setq synced-preview t))))
        (vertico-buffer-frame--auto-width-redisplay nil)
        (should synced-frame)
        (should-not synced-preview)))))

(ert-deftest vertico-buffer-frame-sync-preview-overlays-candidate-lower-right ()
  (let (set-size
        set-position
        visible
        (preview-w 1)
        (preview-h 1))
    (with-temp-buffer
      (setq-local vertico-buffer-frame--frame 'candidate
                  vertico-buffer-frame--preview-frame 'preview
                  vertico-buffer-frame--parent 'parent)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (frame)
                   (memq frame '(candidate preview parent))))
                ((symbol-function #'frame-char-width)
                 (lambda (_frame) 10))
                ((symbol-function #'frame-char-height)
                 (lambda (_frame) 20))
                ;; Candidate is 108x33 chars (1080x660px).  The preview frame
                ;; size tracks `set-frame-size' so the position step reads the
                ;; freshly-set 66x20 chars (660x400px).
                ((symbol-function #'frame-width)
                 (lambda (frame)
                   (pcase frame ('candidate 108) ('preview preview-w) (_ 1))))
                ((symbol-function #'frame-height)
                 (lambda (frame)
                   (pcase frame ('candidate 33) ('preview preview-h) (_ 1))))
                ((symbol-function #'frame-position)
                 (lambda (_frame) '(420 . 210)))
                ((symbol-function #'set-frame-size)
                 (lambda (frame width height)
                   (setq set-size (list frame width height)
                         preview-w width
                         preview-h height)))
                ((symbol-function #'set-frame-position)
                 (lambda (frame left top)
                   (setq set-position (list frame left top))))
                ((symbol-function #'frame-visible-p)
                 (lambda (_frame) nil))
                ((symbol-function #'make-frame-visible)
                 (lambda (frame)
                   (setq visible frame)))
                ((symbol-function #'raise-frame) #'ignore))
        (vertico-buffer-frame--sync-preview-frame)
        (should (equal set-size '(preview 67 20)))
        ;; Preview is 67x20 chars (670x400px).  Lower-right of the candidate:
        ;; left 420 + (1080-670) = 830; top 210 + (660-400) = 470.
        (should (equal set-position '(preview 830 470)))
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

(ert-deftest vertico-buffer-frame-owner-records-current-session ()
  (let (parameters)
    (with-temp-buffer
      (setq-local vertico-buffer-frame--session 'session)
      (cl-letf (((symbol-function #'frame-live-p)
                 (lambda (_frame) t))
                ((symbol-function #'modify-frame-parameters)
                 (lambda (_frame params)
                   (setq parameters params))))
        (vertico-buffer-frame--set-frame-owner-buffer
         'frame (current-buffer))
        (should (eq (alist-get vertico-buffer-frame--owner-session-parameter
                               parameters)
                    'session))))))

(ert-deftest vertico-buffer-frame-delete-frame-clears-owner-first ()
  (let (cleared-owner
        cleared-session
        deleted)
    (cl-letf (((symbol-function #'frame-live-p)
               (lambda (_frame) t))
              ((symbol-function #'modify-frame-parameters)
               (lambda (_frame parameters)
                 (setq cleared-owner
                       (assq vertico-buffer-frame--owner-buffer-parameter
                             parameters)
                       cleared-session
                       (assq vertico-buffer-frame--owner-session-parameter
                             parameters))))
              ((symbol-function #'delete-frame)
               (lambda (_frame &optional force)
                 (should (equal cleared-owner
                                (cons
                                 vertico-buffer-frame--owner-buffer-parameter
                                 nil)))
                 (should (equal cleared-session
                                (cons
                                 vertico-buffer-frame--owner-session-parameter
                                 nil)))
                 (should force)
                 (setq deleted t))))
      (vertico-buffer-frame--delete-frame 'frame)
      (should deleted))))

(ert-deftest vertico-buffer-frame-delete-frame-clears-vertico-overlays-first ()
  (let ((owner (generate-new-buffer " *vbf-overlay-cleanup*"))
        (window (selected-window))
        deleted)
    (unwind-protect
        (with-current-buffer owner
          (setq-local vertico--candidates-ov
                      (make-overlay (point-min) (point-min))
                      vertico--count-ov
                      (make-overlay (point-min) (point-min)))
          (overlay-put vertico--candidates-ov 'window window)
          (overlay-put vertico--count-ov 'window window)
          (cl-letf (((symbol-function #'frame-live-p)
                     (lambda (_frame) t))
                    ((symbol-function #'frame-parameter)
                     (lambda (_frame parameter)
                       (when (eq parameter
                                 vertico-buffer-frame--owner-buffer-parameter)
                         owner)))
                    ((symbol-function #'frame-root-window)
                     (lambda (_frame) window))
                    ((symbol-function #'modify-frame-parameters)
                     #'ignore)
                    ((symbol-function #'delete-frame)
                     (lambda (_frame &optional _force)
                       (should-not
                        (overlay-get vertico--candidates-ov 'window))
                       (should-not
                        (overlay-get vertico--count-ov 'window))
                       (setq deleted t))))
            (vertico-buffer-frame--delete-frame 'frame)
            (should deleted)))
      (when (buffer-live-p owner)
        (kill-buffer owner)))))

(ert-deftest vertico-buffer-frame-delete-frames-owned-by-buffer-respects-session ()
  (let ((owner (generate-new-buffer " *vbf-session-owner*"))
        deleted)
    (unwind-protect
        (cl-letf (((symbol-function #'frame-list)
                   (lambda ()
                     '(old new legacy other)))
                  ((symbol-function #'frame-parameter)
                   (lambda (frame parameter)
                     (cond
                      ((and (eq parameter
                                vertico-buffer-frame--owner-buffer-parameter)
                            (memq frame '(old new legacy)))
                       owner)
                      ((eq parameter
                           vertico-buffer-frame--owner-buffer-parameter)
                       'other-owner)
                      ((and (eq frame 'old)
                            (eq parameter
                                vertico-buffer-frame--owner-session-parameter))
                       'old-session)
                      ((and (eq frame 'new)
                            (eq parameter
                                vertico-buffer-frame--owner-session-parameter))
                       'new-session))))
                  ((symbol-function #'vertico-buffer-frame--delete-frame)
                   (lambda (frame)
                     (push frame deleted))))
          (vertico-buffer-frame--delete-frames-owned-by-buffer
           owner 'old-session)
          (should (equal deleted '(legacy old))))
      (when (buffer-live-p owner)
        (kill-buffer owner)))))

(ert-deftest vertico-buffer-frame-delete-frame-avoids-reentrant-owner-delete ()
  (let ((owner 'owner)
        (owner-parameter 'owner)
        (live t)
        (delete-calls 0))
    (cl-letf (((symbol-function #'frame-list)
               (lambda ()
                 '(child)))
              ((symbol-function #'frame-live-p)
               (lambda (frame)
                 (and (eq frame 'child) live)))
              ((symbol-function #'frame-parameter)
               (lambda (frame parameter)
                 (when (and (eq frame 'child)
                            (eq parameter
                                vertico-buffer-frame--owner-buffer-parameter))
                   owner-parameter)))
              ((symbol-function #'modify-frame-parameters)
               (lambda (frame parameters)
                 (when (eq frame 'child)
                   (setq owner-parameter
                         (alist-get
                          vertico-buffer-frame--owner-buffer-parameter
                          parameters)))))
              ((symbol-function #'delete-frame)
               (lambda (_frame &optional _force)
                 (setq delete-calls (1+ delete-calls))
                 (vertico-buffer-frame--delete-frames-owned-by-buffer owner)
                 (setq live nil))))
      (vertico-buffer-frame--delete-frame 'child)
      (should (= delete-calls 1)))))

(ert-deftest vertico-buffer-frame-minibuffer-setup-installs-hooks ()
  (vertico-buffer-frame-test--with-clean-state
   (with-temp-buffer
     (let ((vertico-buffer-frame-mode t)
           (vertico-buffer-frame--minibuffers nil))
       (vertico-buffer-frame--setup-minibuffer)
       (should-not mode-line-format)
       (should-not header-line-format)
       (should-not tab-line-format)
       (should vertico-buffer-frame--session)
       (should (memq #'vertico-buffer-frame--minibuffer-exit
                     minibuffer-exit-hook))
       (should (memq #'vertico-buffer-frame--auto-width-redisplay
                     pre-redisplay-functions))
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

(ert-deftest vertico-buffer-frame-display-buffer-uses-local-fallback ()
  (vertico-buffer-frame-test--with-clean-state
   (let ((buffer (generate-new-buffer " *vbf-local-fallback*"))
         displayed)
     (unwind-protect
         (with-temp-buffer
           (setq-local vertico-buffer-frame--local-saved-state
                       '(:display-action nil
                                         :display-action-local-p nil
                                         :buffer-mode nil
                                         :buffer-mode-local-p nil))
           (cl-letf (((symbol-function #'display-graphic-p)
                      (lambda (&optional _frame) nil))
                     ((symbol-function #'display-buffer)
                      (lambda (buffer action)
                        (setq displayed (list buffer action))
                        'fallback-window)))
             (should (eq (vertico-buffer-frame--display-buffer
                          buffer '((reusable-frames . visible)))
                         'fallback-window))
             (should (equal displayed
                            (list buffer
                                  '(nil
                                    (reusable-frames . visible)))))))
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
                      vertico-buffer-frame--preview-window 'preview-window
                      vertico-buffer-frame--session 'session)
          (add-hook 'minibuffer-exit-hook
                    #'vertico-buffer-frame--minibuffer-exit nil t)
          (setq vertico-buffer-frame--minibuffers (list buffer))
          (cl-letf (((symbol-function
                      #'vertico-buffer-frame--delete-frames-owned-by-buffer)
                     (lambda (owner &optional session)
                       (setq deleted-owner (list owner session)))))
            (vertico-buffer-frame--cleanup-minibuffer buffer)
            (should (equal deleted-owner (list buffer 'session)))
            (should-not vertico-buffer-frame--frame)
            (should-not vertico-buffer-frame--window)
            (should-not vertico-buffer-frame--parent)
            (should-not vertico-buffer-frame--preview-frame)
            (should-not vertico-buffer-frame--preview-window)
            (should-not vertico-buffer-frame--session)
            (should-not (memq #'vertico-buffer-frame--minibuffer-exit
                              minibuffer-exit-hook))
            (should-not (memq buffer vertico-buffer-frame--minibuffers))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-minibuffer-exit-delays-frame-deletion ()
  (let ((buffer (generate-new-buffer " *vbf-exit-cleanup*"))
        scheduled
        deleted-owner
        hidden)
    (unwind-protect
        (with-current-buffer buffer
          (setq vertico-buffer-frame--minibuffers (list buffer))
          (setq-local vertico-buffer-frame--frame 'frame
                      vertico-buffer-frame--window 'window
                      vertico-buffer-frame--preview-frame 'preview-frame
                      vertico-buffer-frame--session 'session)
          (add-hook 'minibuffer-exit-hook
                    #'vertico-buffer-frame--minibuffer-exit nil t)
          (cl-letf (((symbol-function #'run-at-time)
                     (lambda (time repeat function &rest args)
                       (setq scheduled (list time repeat function args))
                       'timer))
                    ((symbol-function #'frame-live-p)
                     (lambda (frame)
                       (memq frame '(frame preview-frame))))
                    ((symbol-function #'make-frame-invisible)
                     (lambda (frame &optional force)
                       (push (list frame force) hidden)))
                    ((symbol-function
                      #'vertico-buffer-frame--delete-frames-owned-by-buffer)
                     (lambda (owner &optional session)
                       (setq deleted-owner (list owner session)))))
            (vertico-buffer-frame--minibuffer-exit)
            (should-not deleted-owner)
            (should (= (nth 0 scheduled) 0))
            (should-not (nth 1 scheduled))
            (should (eq (nth 2 scheduled)
                        #'vertico-buffer-frame--delete-frames-owned-by-buffer))
            (should (equal (nth 3 scheduled) (list buffer 'session)))
            (should-not vertico-buffer-frame--frame)
            (should-not vertico-buffer-frame--window)
            (should-not vertico-buffer-frame--preview-frame)
            (should-not vertico-buffer-frame--session)
            (should (equal hidden
                           '((frame t) (preview-frame t))))
            (should-not (memq #'vertico-buffer-frame--minibuffer-exit
                              minibuffer-exit-hook))
            (should-not (memq buffer vertico-buffer-frame--minibuffers))
            (apply (nth 2 scheduled) (nth 3 scheduled))
            (should (equal deleted-owner (list buffer 'session)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vertico-buffer-frame-cleanup-dead-minibuffer-deletes-owned-frames ()
  (let ((buffer (generate-new-buffer " *vbf-dead-cleanup*"))
        deleted-owner)
    (kill-buffer buffer)
    (cl-letf (((symbol-function
                #'vertico-buffer-frame--delete-frames-owned-by-buffer)
               (lambda (owner &optional session)
                 (setq deleted-owner (list owner session)))))
      (setq vertico-buffer-frame--minibuffers (list buffer))
      (vertico-buffer-frame--cleanup-minibuffer buffer)
      (should (equal deleted-owner (list buffer nil)))
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
        highlighted
        mirrored)
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
                        #'vertico-buffer-frame--mirror-preview-overlays)
                       (lambda (source target)
                         (setq mirrored (list source target))))
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
              (should (equal mirrored '(source-window preview-window)))
              (should (eq highlighted 'preview-window)))))
      (dolist (buffer (list owner source-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-consult-mirror-window-supports-local-mode ()
  (let ((owner (generate-new-buffer " *vbf-consult-local-owner*"))
        (source-buffer (generate-new-buffer " *vbf-consult-local-source*"))
        mirrored)
    (unwind-protect
        (with-current-buffer owner
          (let ((vertico-buffer-frame-mode nil)
                (vertico-buffer-frame-consult-preview t))
            (setq-local vertico-buffer-frame-local-mode t
                        vertico-buffer-frame--frame 'candidate
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
                           (_ source-buffer))))
                      ((symbol-function #'frame-live-p)
                       (lambda (frame)
                         (memq frame '(candidate parent))))
                      ((symbol-function #'display-graphic-p)
                       (lambda (&optional _frame) t))
                      ((symbol-function
                        #'vertico-buffer-frame--ensure-preview-window)
                       (lambda (_buffer _parent)
                         'preview-window))
                      ((symbol-function
                        #'vertico-buffer-frame--set-preview-window-buffer)
                       #'ignore)
                      ((symbol-function
                        #'vertico-buffer-frame--copy-window-view)
                       #'ignore)
                      ((symbol-function
                        #'vertico-buffer-frame--mirror-preview-overlays)
                       (lambda (source target)
                         (setq mirrored (list source target))))
                      ((symbol-function
                        #'vertico-buffer-frame--highlight-preview-line)
                       #'ignore)
                      ((symbol-function
                        #'vertico-buffer-frame--sync-preview-frame)
                       #'ignore))
              (should (eq (vertico-buffer-frame-consult-preview-mirror-window
                           'source-window)
                          'preview-window))
              (should (equal mirrored '(source-window preview-window))))))
      (dolist (buffer (list owner source-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-consult-mirrors-insertion-overlays ()
  (let ((owner (generate-new-buffer " *vbf-consult-owner*"))
        (source-buffer (generate-new-buffer " *vbf-consult-source*"))
        source-overlay
        unrelated-overlay)
    (unwind-protect
        (progn
          (with-current-buffer source-buffer
            (insert "alpha beta gamma")
            (setq source-overlay (make-overlay 7 7 source-buffer))
            (overlay-put source-overlay 'window 'source-window)
            (overlay-put source-overlay 'before-string "inserted")
            (overlay-put source-overlay 'invisible t)
            (setq unrelated-overlay (make-overlay 1 6 source-buffer))
            (overlay-put unrelated-overlay 'window 'source-window)
            (overlay-put unrelated-overlay 'face 'bold))
          (with-current-buffer owner
            (setq-local vertico-buffer-frame--preview-overlays nil)
            (cl-letf (((symbol-function #'window-start)
                       (lambda (_window) 1))
                      ((symbol-function #'window-end)
                       (lambda (_window &optional _update) 18))
                      ((symbol-function #'window-point)
                       (lambda (_window) 7))
                      ((symbol-function #'window-buffer)
                       (lambda (_window) source-buffer)))
              (vertico-buffer-frame--mirror-preview-overlays
               'source-window 'preview-window))
            (should (= (length vertico-buffer-frame--preview-overlays) 1))
            (let ((copy (car vertico-buffer-frame--preview-overlays)))
              (should (= (overlay-start copy) 7))
              (should (= (overlay-end copy) 7))
              (should (eq (overlay-buffer copy) source-buffer))
              (should (eq (overlay-get copy 'window) 'preview-window))
              (should (equal (overlay-get copy 'before-string) "inserted"))
              (should (overlay-get copy 'invisible))
              (should-not (eq copy source-overlay))
              (should-not (memq unrelated-overlay
                                vertico-buffer-frame--preview-overlays)))))
      (dolist (buffer (list owner source-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest vertico-buffer-frame-toggle-preview-is-minibuffer-local ()
  (vertico-buffer-frame-test--with-clean-state
   (let (hidden
         refreshed)
     (with-temp-buffer
       (let ((vertico-buffer-frame-consult-preview t))
         (setq-local consult--preview-function
                     (lambda ()
                       (setq refreshed t)))
         (cl-letf (((symbol-function #'minibufferp)
                    (lambda (&optional _buffer) t))
                   ((symbol-function
                     #'vertico-buffer-frame-consult-preview-hide)
                    (lambda ()
                      (setq hidden t))))
           (vertico-buffer-frame-toggle-preview)
           (should-not vertico-buffer-frame-consult-preview)
           (should (local-variable-p
                    'vertico-buffer-frame-consult-preview))
           (should hidden)
           (setq hidden nil)
           (vertico-buffer-frame-toggle-preview 1)
           (should vertico-buffer-frame-consult-preview)
           (should refreshed)
           (should-not hidden)))))))

(ert-deftest vertico-buffer-frame-toggle-preview-outside-minibuffer-sets-default ()
  (vertico-buffer-frame-test--with-clean-state
   (let (hidden)
     (setq-default vertico-buffer-frame-consult-preview t)
     (cl-letf (((symbol-function #'minibufferp)
                (lambda (&optional _buffer) nil))
               ((symbol-function #'vertico-buffer-frame-consult-preview-hide)
                (lambda ()
                  (setq hidden t))))
       (vertico-buffer-frame-toggle-preview)
       (should-not (default-value 'vertico-buffer-frame-consult-preview))
       (should hidden)
       (vertico-buffer-frame-toggle-preview 1)
       (should (default-value 'vertico-buffer-frame-consult-preview))))))

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
