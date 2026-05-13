;;; vertico-buffer-frame-consult.el --- Consult preview support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: Nobuyuki Kamimoto
;; Keywords: convenience, completion

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Preview target adapters for Consult completion categories.

;;; Code:

(require 'subr-x)

(defvar vertico-buffer-frame-preview-categories)
(defvar vertico-buffer-frame-preview-target-functions)
(defvar xref-file-name-display)

(declare-function vertico-buffer-frame--buffer-position-target
                  "vertico-buffer-frame-preview" (buffer position))
(declare-function vertico-buffer-frame--file-target
                  "vertico-buffer-frame-preview" (candidate &optional directory))
(declare-function vertico-buffer-frame--origin-buffer
                  "vertico-buffer-frame-preview" ())
(declare-function vertico-buffer-frame--text-property-value
                  "vertico-buffer-frame-preview" (property string))
(declare-function xref-item-location "xref" (xref))
(declare-function xref-location-group "xref" (location))
(declare-function xref-location-line "xref" (location))
(declare-function xref-location-marker "xref" (location))
(declare-function consult-imenu--deduplicate "consult-imenu" (items))
(declare-function consult-imenu--items "consult-imenu" ())

(defun vertico-buffer-frame-consult--location-value (raw-candidate)
  "Return Consult location metadata from RAW-CANDIDATE."
  (vertico-buffer-frame--text-property-value
   'consult-location raw-candidate))

(defun vertico-buffer-frame-consult--location-target (raw-candidate)
  "Return a preview target for a Consult location RAW-CANDIDATE."
  (when-let* ((location
               (vertico-buffer-frame-consult--location-value raw-candidate))
              (position (car-safe location)))
    (cond
     ((markerp position)
      (when-let* ((buffer (marker-buffer position)))
        (vertico-buffer-frame--buffer-position-target buffer position)))
     ((and (consp position)
           (buffer-live-p (car position))
           (integer-or-marker-p (cdr position)))
      (vertico-buffer-frame--buffer-position-target
       (car position)
       (cdr position))))))

(defun vertico-buffer-frame-consult--grep-location-from-properties
    (raw-candidate)
  "Return (FILE . LINE) from Consult grep RAW-CANDIDATE text properties."
  (when (and (stringp raw-candidate)
             (> (length raw-candidate) 0))
    (let* ((length (length raw-candidate))
           (file-end (next-single-property-change
                      0 'face raw-candidate length))
           (line-start (and file-end (1+ file-end)))
           (line-end (and line-start
                          (< line-start length)
                          (next-single-property-change
                           line-start 'face raw-candidate length))))
      (when (and file-end
                 line-end
                 (< line-start line-end))
        (let ((line (string-to-number
                     (substring-no-properties raw-candidate
                                              line-start
                                              line-end))))
          (when (> line 0)
            (cons (substring-no-properties raw-candidate 0 file-end)
                  line)))))))

(defun vertico-buffer-frame-consult--grep-location-from-string (candidate)
  "Return (FILE . LINE) by parsing Consult grep CANDIDATE."
  (when (and (stringp candidate)
             (string-match "\\`\\(.+?\\)[:-]\\([0-9]+\\)[:-]" candidate))
    (cons (match-string 1 candidate)
          (string-to-number (match-string 2 candidate)))))

(defun vertico-buffer-frame-consult--file-line-target (file line)
  "Return a file-line preview target for FILE at LINE."
  (when (and (stringp file)
             (integerp line)
             (> line 0))
    (when-let* ((target (vertico-buffer-frame--file-target file)))
      (list 'file-line (cadr target) line))))

(defun vertico-buffer-frame-consult--grep-target
    (candidate raw-candidate)
  "Return a preview target for Consult grep CANDIDATE and RAW-CANDIDATE."
  (when-let* ((location
               (or (vertico-buffer-frame-consult--grep-location-from-properties
                    raw-candidate)
                   (vertico-buffer-frame-consult--grep-location-from-string
                    candidate))))
    (vertico-buffer-frame-consult--file-line-target
     (car location)
     (cdr location))))

(defun vertico-buffer-frame-consult--xref-target (raw-candidate)
  "Return a preview target for a Consult Xref RAW-CANDIDATE."
  (when-let* ((xref (vertico-buffer-frame--text-property-value
                     'consult-xref raw-candidate))
              ((fboundp 'xref-item-location))
              (location (xref-item-location xref)))
    (or (when-let* ((file (let ((xref-file-name-display 'abs))
                            (xref-location-group location)))
                    (line (xref-location-line location)))
          (vertico-buffer-frame-consult--file-line-target file line))
        (when-let* ((marker (ignore-errors
                              (xref-location-marker location)))
                    ((markerp marker))
                    (buffer (marker-buffer marker)))
          (vertico-buffer-frame--buffer-position-target buffer marker)))))

(defun vertico-buffer-frame-consult--imenu-position-target
    (position fallback-buffer)
  "Return a preview target for Consult Imenu POSITION.
FALLBACK-BUFFER is used for integer positions which do not carry a buffer."
  (pcase position
    ((pred markerp)
     (when-let* ((buffer (marker-buffer position)))
       (vertico-buffer-frame--buffer-position-target buffer position)))
    ((pred integerp)
     (vertico-buffer-frame--buffer-position-target fallback-buffer position))
    (`(,pos ,_function ,buffer . ,_args)
     (when (buffer-live-p buffer)
       (vertico-buffer-frame-consult--imenu-position-target pos buffer)))))

(defun vertico-buffer-frame-consult--imenu-target (candidate)
  "Return a preview target for a Consult Imenu CANDIDATE."
  (when-let* ((buffer (vertico-buffer-frame--origin-buffer))
              ((fboundp 'consult-imenu--items)))
    (with-current-buffer buffer
      (when-let* ((items (ignore-errors
                           (copy-tree (consult-imenu--items)))))
        (when (fboundp 'consult-imenu--deduplicate)
          (consult-imenu--deduplicate items))
        (when-let* ((item (assoc candidate items)))
          (vertico-buffer-frame-consult--imenu-position-target
           (cdr item)
           buffer))))))

(defun vertico-buffer-frame-consult-preview-target
    (category candidate raw-candidate)
  "Return a Consult preview target for CATEGORY and CANDIDATE."
  (pcase category
    ('consult-location
     (vertico-buffer-frame-consult--location-target raw-candidate))
    ('consult-grep
     (vertico-buffer-frame-consult--grep-target candidate raw-candidate))
    ('consult-xref
     (vertico-buffer-frame-consult--xref-target raw-candidate))
    ('imenu
     (vertico-buffer-frame-consult--imenu-target candidate))))

(defun vertico-buffer-frame-consult--install ()
  "Install Consult preview target support."
  (dolist (category '(consult-location consult-grep consult-xref))
    (add-to-list 'vertico-buffer-frame-preview-categories category t))
  (add-hook 'vertico-buffer-frame-preview-target-functions
            #'vertico-buffer-frame-consult-preview-target))

(if (boundp 'vertico-buffer-frame-preview-target-functions)
    (vertico-buffer-frame-consult--install)
  (with-eval-after-load 'vertico-buffer-frame-preview
    (vertico-buffer-frame-consult--install)))

(provide 'vertico-buffer-frame-consult)
;;; vertico-buffer-frame-consult.el ends here
