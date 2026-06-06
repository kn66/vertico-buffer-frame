;;; vertico-buffer-frame-consult.el --- Consult preview mirror -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: Nobuyuki Kamimoto
;; Assisted-by: OpenAI Codex:GPT-5
;; Keywords: convenience, frames, minibuffer
;; URL: https://github.com/kn66/vertico-buffer-frame
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

;;; Commentary:

;; Mirror Consult window previews in `vertico-buffer-frame' child frames.
;;
;; Consult still owns preview resolution and cleanup.  This adapter wraps
;; Consult preview state functions and mirrors the resulting original-window
;; view into the preview child frame managed by `vertico-buffer-frame'.

;;; Code:

(require 'vertico-buffer-frame)

(defvar vertico-buffer-frame-consult--installed nil
  "Non-nil means the Consult preview advice is installed.")

(declare-function consult--with-preview-f "consult")
(declare-function vertico-buffer-frame-consult-preview-hide
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame-consult-preview-mirror-window
                  "vertico-buffer-frame")

(defun vertico-buffer-frame-consult--wrap-state (state)
  "Return a Consult preview STATE wrapper."
  (lambda (action cand)
    (prog1 (funcall state action cand)
      (pcase action
        ('preview
         (if cand
             (vertico-buffer-frame-consult-preview-mirror-window
              (selected-window))
           (vertico-buffer-frame-consult-preview-hide)))
        ('exit
         (vertico-buffer-frame-consult-preview-hide))))))

(defun vertico-buffer-frame-consult--with-preview-f
    (orig preview-key state transform candidate save-input body)
  "Advice ORIG around Consult preview setup.
PREVIEW-KEY, STATE, TRANSFORM, CANDIDATE, SAVE-INPUT and BODY are the
arguments of `consult--with-preview-f'."
  (funcall orig
           preview-key
           (and state
                (vertico-buffer-frame-consult--wrap-state state))
           transform
           candidate
           save-input
           body))

(defun vertico-buffer-frame-consult--install ()
  "Install Consult preview mirroring."
  (when (and (fboundp 'consult--with-preview-f)
             (not vertico-buffer-frame-consult--installed))
    (advice-add #'consult--with-preview-f
                :around
                #'vertico-buffer-frame-consult--with-preview-f)
    (setq vertico-buffer-frame-consult--installed t)))

(vertico-buffer-frame-consult--install)

(with-eval-after-load 'consult
  (vertico-buffer-frame-consult--install))

(provide 'vertico-buffer-frame-consult)
;;; vertico-buffer-frame-consult.el ends here
