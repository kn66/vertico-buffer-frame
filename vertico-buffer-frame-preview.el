;;; vertico-buffer-frame-preview.el --- Candidate previews for vertico-buffer-frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Internal preview rendering helpers for `vertico-buffer-frame'.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'color)

(declare-function vertico--candidate "vertico")
(declare-function consult--grep-position "consult")
(declare-function consult-compile--lookup "consult-compile")
(declare-function consult-imenu--items-safe "consult-imenu")
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")
(declare-function bookmark-get-filename "bookmark")
(declare-function bookmark-get-position "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function package-desc-name "package")
(declare-function package-desc-summary "package")
(declare-function package-desc-version "package")
(declare-function package-version-join "package")
(declare-function xref-item-location "xref")
(declare-function xref-location-group "xref")
(declare-function xref-location-marker "xref")
(declare-function calendar-last-day-of-month "calendar")
(declare-function custom-group-of-mode "cus-edit")
(declare-function custom-load-symbol "cus-edit")
(declare-function Info-extract-menu-item "info")
(declare-function Info-goto-node "info")
(declare-function vertico-buffer-frame--active-minibuffer-buffer "vertico-buffer-frame")
(declare-function vertico-buffer-frame--apply-frame-parameters "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--candidate-frame "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--current-source-window "vertico-buffer-frame")
(declare-function vertico-buffer-frame--ensure-frame-visible "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--face-remapping "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--hide-window-chrome "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--live-minibuffer-buffer-p "vertico-buffer-frame")
(declare-function vertico-buffer-frame--minibuffer-buffer "vertico-buffer-frame")
(declare-function vertico-buffer-frame--position-pair "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--preview-enabled-p "vertico-buffer-frame")
(declare-function vertico-buffer-frame--preview-frame-parameters "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--preview-window "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--refresh-frames "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--top-minibuffer-buffer "vertico-buffer-frame")

(defvar bookmark-alist)
(defvar calendar-month-name-array)
(defvar Info-complete-menu-buffer)
(defvar Info-current-file)
(defvar Info-current-node)
(defvar imenu-space-replacement)
(defvar package-alist)
(defvar package-archive-contents)
(defvar vertico-buffer-frame-mode)
(defvar vertico-buffer-frame-preview)
(defvar vertico-buffer-frame-preview-binary-detect-bytes)
(defvar vertico-buffer-frame-preview-category-functions)
(defvar vertico-buffer-frame-preview-command-functions)
(defvar vertico-buffer-frame-preview-delay)
(defvar vertico-buffer-frame-preview-directory-max-entries)
(defvar vertico-buffer-frame-preview-function)
(defvar vertico-buffer-frame-preview-io-timeout)
(defvar vertico-buffer-frame-preview-max-size)
(defvar vertico-buffer-frame--completion-command)
(defvar vertico-buffer-frame--exiting)
(defvar vertico-buffer-frame--preview-buffer)
(defvar vertico-buffer-frame--preview-enabled)
(defvar vertico-buffer-frame--preview-frame)
(defvar vertico-buffer-frame--preview-timer)

(defvar vertico-buffer-frame--preview-help-buffer
  " *vertico-buffer-frame-help*"
  "Private Help buffer used for documentation previews.")

(defvar vertico-buffer-frame--preview-location-buffer nil
  "Indirect buffer used for marker-based location previews.")

(defvar-local vertico-buffer-frame--preview-recenter nil
  "Non-nil when preview window should be centered around point.")

(defmacro vertico-buffer-frame--with-io-timeout (&rest body)
  "Run BODY under `vertico-buffer-frame-preview-io-timeout'.
Return nil when the timeout fires.  When the option is nil, run BODY without a
timeout."
  (declare (indent 0) (debug t))
  `(if vertico-buffer-frame-preview-io-timeout
       (with-timeout (vertico-buffer-frame-preview-io-timeout nil)
         ,@body)
     (progn ,@body)))

(defun vertico-buffer-frame--local-file-p (file)
  "Return non-nil when FILE can be previewed without remote I/O."
  (and file
       (not (file-remote-p file))
       (not (file-remote-p default-directory))))

(defun vertico-buffer-frame--binary-file-p (file)
  "Return non-nil if FILE appears binary based on a leading-byte scan."
  (when-let* ((bytes vertico-buffer-frame-preview-binary-detect-bytes)
              ((natnump bytes))
              ((> bytes 0)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary))
        (insert-file-contents-literally file nil 0 bytes))
      (goto-char (point-min))
      (search-forward "\0" nil t))))

(defun vertico-buffer-frame--directory-entries (directory)
  "Return a bounded list of preview entries for DIRECTORY."
  (let* ((limit vertico-buffer-frame-preview-directory-max-entries)
         (entries (directory-files directory
                                   nil
                                   directory-files-no-dot-files-regexp
                                   nil
                                   (and limit (1+ limit)))))
    (if (and limit (> (length entries) limit))
        (append (butlast entries) '("..."))
      entries)))

(defun vertico-buffer-frame--directory-content (directory)
  "Return preview content for DIRECTORY."
  (mapconcat #'identity
             (vertico-buffer-frame--directory-entries directory)
             "\n"))

(defun vertico-buffer-frame--file-content (file)
  "Return preview content for regular FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 vertico-buffer-frame-preview-max-size)
    (buffer-string)))

(defun vertico-buffer-frame--current-candidate ()
  "Return the current Vertico candidate string, or nil."
  (when (and (boundp 'vertico--index)
             (boundp 'vertico--total)
             (> vertico--total 0)
             (>= vertico--index 0))
    (ignore-errors
      (vertico--candidate))))

(defun vertico-buffer-frame--completion-category ()
  "Return the current completion category, or nil."
  (vertico-buffer-frame--completion-metadata-category))

(defun vertico-buffer-frame--target (candidate)
  "Return preview category and target for CANDIDATE."
  (let ((category (vertico-buffer-frame--completion-category)))
    (if (eq category 'multi-category)
        (if-let* ((multi (get-text-property 0 'multi-category candidate)))
            (cons (car multi) (cdr multi))
          (cons category candidate))
      (cons category candidate))))

(defun vertico-buffer-frame--candidate-string (candidate)
  "Return CANDIDATE as a plain string."
  (substring-no-properties (format "%s" candidate)))

(defun vertico-buffer-frame--completion-input ()
  "Return the current minibuffer input for completion metadata."
  (if (minibufferp)
      (minibuffer-contents)
    ""))

(defun vertico-buffer-frame--completion-metadata
    (&optional input table predicate extra-properties)
  "Return Emacs completion metadata for the current completion state.
INPUT, TABLE, PREDICATE, and EXTRA-PROPERTIES default to the current
minibuffer completion state."
  (let ((table (or table minibuffer-completion-table))
        (predicate (or predicate minibuffer-completion-predicate))
        (completion-extra-properties
         (or extra-properties completion-extra-properties)))
    (when table
      (ignore-errors
        (completion-metadata (or input
                                 (vertico-buffer-frame--completion-input))
                             table
                             predicate)))))

(defun vertico-buffer-frame--completion-metadata-category
    (&optional input table predicate extra-properties)
  "Return completion category using Emacs completion metadata.
INPUT, TABLE, PREDICATE, and EXTRA-PROPERTIES default to the current
minibuffer completion state."
  (when-let* ((metadata (vertico-buffer-frame--completion-metadata
                         input table predicate extra-properties)))
    (completion-metadata-get metadata 'category)))

(defun vertico-buffer-frame--file-candidate-string (candidate)
  "Return CANDIDATE's file name string, using unquoted text when available."
  (vertico-buffer-frame--candidate-string
   (or (and (stringp candidate)
            (get-text-property 0 'completion--unquoted candidate))
       candidate)))

(defun vertico-buffer-frame--file-completion-directory ()
  "Return the active directory of the current minibuffer file completion."
  (when (and minibuffer-completing-file-name
             (minibufferp))
    (let* ((input (substitute-in-file-name (minibuffer-contents)))
           (directory (file-name-directory input)))
      (and directory (expand-file-name directory)))))

(defun vertico-buffer-frame--file-candidate-name (candidate)
  "Return the absolute file name represented by file CANDIDATE."
  (let ((name (substitute-in-file-name
               (vertico-buffer-frame--file-candidate-string candidate))))
    (expand-file-name
     name
     (or (vertico-buffer-frame--file-completion-directory)
         default-directory))))

(defun vertico-buffer-frame--candidate-symbol (candidate)
  "Return CANDIDATE as an interned symbol."
  (intern-soft (vertico-buffer-frame--candidate-string candidate)))

(defun vertico-buffer-frame--documentation (title body &optional details)
  "Return preview text from TITLE, BODY, and optional DETAILS."
  (when body
    (string-join
     (append (list title)
             (delq nil details)
             (list "" (string-trim (substitute-command-keys body))))
     "\n")))

(defun vertico-buffer-frame--help-preview-buffer (function symbol)
  "Return a private Help buffer generated by calling FUNCTION with SYMBOL."
  (when (and (symbolp symbol)
             (require 'help-fns nil t))
    (let ((buffer (get-buffer-create
                   vertico-buffer-frame--preview-help-buffer))
          (display-buffer-overriding-action
           '(display-buffer-no-window (allow-no-window . t)))
          (help-window-select nil))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq vertico-buffer-frame--preview-recenter nil))
      (cl-letf (((symbol-function 'help-buffer)
                 (lambda () (buffer-name buffer))))
        (ignore-errors
          (save-window-excursion
            (funcall function symbol))
          (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (> (buffer-size) 0))
               buffer))))))

(defun vertico-buffer-frame-preview-default (candidate)
  "Return preview content for CANDIDATE using built-in preview rules."
  (let* ((preview (vertico-buffer-frame--target candidate))
         (category (car preview))
         (target (cdr preview)))
    (or (when-let* ((function (cdr (assoc category
                                          vertico-buffer-frame-preview-category-functions))))
          (funcall function target))
        (when-let* ((function (cdr (assq (or (bound-and-true-p
                                              vertico-buffer-frame--completion-command)
                                             this-command)
                                         vertico-buffer-frame-preview-command-functions))))
          (funcall function target))
        (when minibuffer-completing-file-name
          (vertico-buffer-frame-preview-file candidate))
        (vertico-buffer-frame-preview-metadata candidate))))

(defun vertico-buffer-frame-preview-file (candidate)
  "Return file preview content for CANDIDATE."
  (when-let* ((file (ignore-errors
                      (vertico-buffer-frame--file-candidate-name candidate))))
    (vertico-buffer-frame--file-preview file)))

(defun vertico-buffer-frame-preview-buffer (candidate)
  "Return buffer preview content for CANDIDATE."
  (when-let* ((buffer (if (bufferp candidate)
                          candidate
                        (get-buffer (substring-no-properties candidate)))))
    buffer))

(defun vertico-buffer-frame-preview-string (candidate)
  "Return CANDIDATE as preview content."
  (vertico-buffer-frame--candidate-string candidate))

(defun vertico-buffer-frame--completion-affix (candidate metadata)
  "Return completion affix display text for CANDIDATE using METADATA."
  (let ((candidate (vertico-buffer-frame--candidate-string candidate)))
    (or (when-let* ((function (completion-metadata-get
                               metadata 'affixation-function)))
          (pcase (car-safe (ignore-errors
                             (funcall function (list candidate))))
            (`(,_ ,prefix ,suffix)
             (let ((text (string-trim
                          (concat (and (stringp prefix) prefix)
                                  candidate
                                  (and (stringp suffix) suffix)))))
               (unless (string-empty-p text)
                 text)))))
        (when-let* ((function (completion-metadata-get
                               metadata 'annotation-function))
                    (annotation (ignore-errors
                                  (funcall function candidate)))
                    ((stringp annotation))
                    ((not (string-empty-p annotation))))
          (string-trim (concat candidate annotation))))))

(defun vertico-buffer-frame--completion-group (candidate metadata)
  "Return completion group for CANDIDATE using METADATA."
  (when-let* ((function (completion-metadata-get metadata 'group-function))
              (group (ignore-errors
                       (funcall function
                                (vertico-buffer-frame--candidate-string
                                 candidate)
                                nil)))
              ((stringp group))
              ((not (string-empty-p group))))
    group))

(defun vertico-buffer-frame-preview-metadata (candidate)
  "Return generic preview text for CANDIDATE from completion metadata.
This uses Emacs' built-in `affixation-function', `annotation-function', and
`group-function' metadata when a completion table provides them."
  (when-let* ((metadata (vertico-buffer-frame--completion-metadata)))
    (let* ((candidate-string (vertico-buffer-frame--candidate-string candidate))
           (group (vertico-buffer-frame--completion-group candidate metadata))
           (affix (vertico-buffer-frame--completion-affix
                   candidate metadata)))
      (when (or group affix)
        (string-join
         (delq nil
               (list candidate-string
                     ""
                     (and group (format "Group: %s" group))
                     (and affix (format "Display: %s" affix))))
         "\n")))))

(defun vertico-buffer-frame-preview-command (candidate)
  "Return command documentation preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((commandp symbol)))
    (vertico-buffer-frame--help-preview-buffer #'describe-function symbol)))

(defun vertico-buffer-frame-preview-function (candidate)
  "Return function documentation preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((fboundp symbol)))
    (vertico-buffer-frame--help-preview-buffer #'describe-function symbol)))

(defun vertico-buffer-frame-preview-variable (candidate)
  "Return variable documentation preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((boundp symbol)))
    (vertico-buffer-frame--help-preview-buffer #'describe-variable symbol)))

(defun vertico-buffer-frame-preview-symbol (candidate)
  "Return documentation preview for symbol CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate)))
    (or (vertico-buffer-frame--help-preview-buffer #'describe-symbol symbol)
        (format "%s" symbol))))

(defun vertico-buffer-frame--custom-group-p (symbol)
  "Return non-nil when SYMBOL names or can load a Custom group."
  (and symbol
       (or (get symbol 'custom-group)
           (and (get symbol 'custom-loads)
                (not (get symbol 'custom-autoload))))))

(defun vertico-buffer-frame--custom-group-summary (symbol type label)
  "Return a summary line for Custom group SYMBOL entries of TYPE.
LABEL is the human-readable label used at the start of the summary."
  (when-let* ((items
               (seq-keep
                (lambda (item)
                  (and (consp item)
                       (eq (cadr item) type)
                       (format "%s" (car item))))
                (get symbol 'custom-group))))
    (format "%s: %s%s"
            label
            (string-join (seq-take items 5) ", ")
            (if (> (length items) 5) ", ..." ""))))

(defun vertico-buffer-frame-preview-custom-group (candidate)
  "Return Custom group documentation preview for CANDIDATE."
  (when (require 'cus-edit nil t)
    (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
                ((vertico-buffer-frame--custom-group-p symbol)))
      (ignore-errors
        (custom-load-symbol symbol))
      (vertico-buffer-frame--documentation
       (format "%s" symbol)
       (or (documentation-property symbol 'group-documentation t)
           "Undocumented customization group.")
       (delq nil
             (list
              "Type: Custom group"
              (vertico-buffer-frame--custom-group-summary
               symbol 'custom-group "Groups")
              (vertico-buffer-frame--custom-group-summary
               symbol 'custom-variable "Options")
              (vertico-buffer-frame--custom-group-summary
               symbol 'custom-face "Faces")))))))

(defun vertico-buffer-frame-preview-custom-mode (candidate)
  "Return Custom group preview for mode CANDIDATE."
  (when (require 'cus-edit nil t)
    (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate)))
      (or (when-let* (((fboundp 'custom-group-of-mode))
                      (group (custom-group-of-mode symbol)))
            (vertico-buffer-frame-preview-custom-group (symbol-name group)))
          (vertico-buffer-frame-preview-command candidate)))))

(defun vertico-buffer-frame-preview-color (candidate)
  "Return color preview for CANDIDATE."
  (let* ((color (vertico-buffer-frame--candidate-string candidate))
         (rgb (ignore-errors (color-name-to-rgb color))))
    (when rgb
      (let* ((hex (upcase (apply #'color-rgb-to-hex (append rgb '(2)))))
             (values (color-values color))
             (swatch (propertize "        " 'face `(:background ,color))))
        (format "%s %s\n\nRGB: %s\n16-bit: %S"
                swatch color hex values)))))

(defun vertico-buffer-frame-preview-environment-variable (candidate)
  "Return environment variable preview for CANDIDATE."
  (let* ((name (vertico-buffer-frame--candidate-string candidate))
         (value (getenv name)))
    (format "%s\n\n%s"
            name
            (if value
                (format "Value:\n%s" value)
              "Unset."))))

(defun vertico-buffer-frame-preview-unicode-name (candidate)
  "Return Unicode character preview for CANDIDATE."
  (let* ((name (vertico-buffer-frame--candidate-string candidate))
         (char (char-from-name name t)))
    (when char
      (string-join
       (delq nil
             (list
              (or (char-to-name char) name)
              ""
              (format "Character: %s" (single-key-description char))
              (format "Code point: U+%04X" char)
              (when-let* ((category
                           (get-char-code-property char 'general-category)))
                (format "General category: %s" category))
              (when-let* ((old-name (get-char-code-property char 'old-name)))
                (format "Old name: %s" old-name))))
       "\n"))))

(defun vertico-buffer-frame-preview-calendar-month (candidate)
  "Return calendar month preview for CANDIDATE."
  (when (require 'calendar nil t)
    (let* ((name (vertico-buffer-frame--candidate-string candidate))
           (months (append calendar-month-name-array nil))
           (index (seq-position months name #'string-equal)))
      (when index
        (let* ((month (1+ index))
               (common-days (calendar-last-day-of-month month 2025))
               (leap-days (calendar-last-day-of-month month 2024)))
          (format "%s\n\nMonth number: %d\nDays: %s"
                  name
                  month
                  (if (= common-days leap-days)
                      (number-to-string common-days)
                    (format "%d or %d" common-days leap-days))))))))

(defun vertico-buffer-frame-preview-face (candidate)
  "Return face preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((facep symbol)))
    (vertico-buffer-frame--help-preview-buffer #'describe-face symbol)))

(defun vertico-buffer-frame-preview-theme (candidate)
  "Return theme preview text for CANDIDATE."
  (let* ((name (vertico-buffer-frame--candidate-string candidate))
         (symbol (intern-soft name)))
    (when symbol
      (format "%s\n\n%s"
              symbol
              (if (memq symbol custom-enabled-themes)
                  "This theme is currently enabled."
                "Theme candidate.")))))

(defun vertico-buffer-frame-preview-package (candidate)
  "Return package preview text for CANDIDATE."
  (when (require 'package nil t)
    (let* ((name (vertico-buffer-frame--candidate-symbol candidate))
           (desc (or (cadr (assq name package-alist))
                     (cadr (assq name package-archive-contents)))))
      (when desc
        (format "%s %s\n\n%s"
                (package-desc-name desc)
                (package-version-join (package-desc-version desc))
                (or (package-desc-summary desc) ""))))))

(defun vertico-buffer-frame-preview-bookmark (candidate)
  "Return bookmark preview content for CANDIDATE."
  (when (require 'bookmark nil t)
    (bookmark-maybe-load-default-file)
    (when-let* ((bookmark (assoc (vertico-buffer-frame--candidate-string candidate)
                                 bookmark-alist)))
      (or (when-let* ((file (ignore-errors
                              (bookmark-get-filename bookmark))))
            (let ((position (ignore-errors (bookmark-get-position bookmark))))
              (vertico-buffer-frame--file-preview
               file
               position
               (format "%s\n%s%s\n\n"
                       (car bookmark)
                       (abbreviate-file-name file)
                       (if position (format ":%s" position) "")))))
          (format "%s\n\n%S" (car bookmark) (cdr bookmark))))))

(defun vertico-buffer-frame--position-source-buffer (&optional buffer)
  "Return source BUFFER for an integer preview position."
  (or (and (buffer-live-p buffer) buffer)
      (when (minibufferp)
        (with-minibuffer-selected-window
          (current-buffer)))
      (current-buffer)))

(defun vertico-buffer-frame--position-marker (position &optional buffer)
  "Return a marker for POSITION.
When POSITION is an integer, resolve it in BUFFER or the selected minibuffer
source buffer."
  (cond
   ((markerp position) position)
   ((integerp position)
    (when-let* ((buffer (vertico-buffer-frame--position-source-buffer buffer))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (copy-marker (max (point-min)
                          (min (point-max) position))))))
   ((and (consp position) (bufferp (car position)))
    (set-marker (make-marker) (cdr position) (car position)))))

(defun vertico-buffer-frame--marker-preview (marker &optional title)
  "Return a marker-based preview object for MARKER.
TITLE is retained as metadata for callers that want to inspect it."
  (when (and (markerp marker)
             (buffer-live-p (marker-buffer marker)))
    (list :marker (copy-marker marker) :title title)))

(defun vertico-buffer-frame--marker-preview-p (content)
  "Return non-nil when CONTENT is a marker-based preview object."
  (and (consp content)
       (eq (car content) :marker)
       (markerp (plist-get content :marker))))

(defun vertico-buffer-frame--marker-preview-marker (content)
  "Return CONTENT's marker when CONTENT is a marker preview."
  (and (vertico-buffer-frame--marker-preview-p content)
       (plist-get content :marker)))

(defun vertico-buffer-frame--position-preview
    (position &optional title _matches source-buffer)
  "Return marker preview content for POSITION.
TITLE is retained as preview metadata when non-nil.
When POSITION is an integer, resolve it in SOURCE-BUFFER."
  (vertico-buffer-frame--marker-preview
   (vertico-buffer-frame--position-marker position source-buffer)
   title))

(defun vertico-buffer-frame--file-preview (file &optional position title)
  "Return preview content for local FILE.
When POSITION is an integer or marker and FILE is regular, return a marker
preview object.  TITLE is retained as marker preview metadata."
  (when (vertico-buffer-frame--local-file-p file)
    (vertico-buffer-frame--with-io-timeout
      (cond
       ((file-directory-p file)
        (vertico-buffer-frame--directory-content file))
       ((file-regular-p file)
        (unless (vertico-buffer-frame--binary-file-p file)
          (if (or (integerp position) (markerp position))
              (when-let* ((buffer (find-file-noselect file)))
                (vertico-buffer-frame--position-preview
                 position title nil buffer))
            (vertico-buffer-frame--file-content file))))))))

(defun vertico-buffer-frame-preview-location (candidate)
  "Return location preview content for CANDIDATE."
  (if-let* ((location (and (stringp candidate)
                           (get-text-property 0 'consult-location candidate))))
      (vertico-buffer-frame--position-preview (car location))
    (vertico-buffer-frame--position-preview (or (car-safe candidate) candidate))))

(defun vertico-buffer-frame--consult-imenu-item (candidate)
  "Return Consult imenu item matching CANDIDATE."
  (when (and (stringp candidate)
             (fboundp 'consult-imenu--items-safe))
    (let ((name (vertico-buffer-frame--candidate-string candidate)))
      (with-minibuffer-selected-window
        (catch 'item
          (dolist (item (consult-imenu--items-safe))
            (when (and (consp item)
                       (stringp (car item))
                       (equal name
                              (substring-no-properties (car item))))
              (throw 'item item))))))))

(defun vertico-buffer-frame--imenu-minibuffer-item (candidate)
  "Return imenu item for CANDIDATE from the current completion table."
  (let ((table minibuffer-completion-table))
    (when (and (stringp candidate)
               (listp table))
      (let ((name (vertico-buffer-frame--candidate-string candidate)))
        (catch 'item
          (dolist (item table)
            (when (and (consp item)
                       (stringp (car item))
                       (equal name
                              (substring-no-properties (car item))))
              (throw 'item
                     (or (get-text-property 0 'imenu-choice (car item))
                         item)))))))))

(defun vertico-buffer-frame--imenu-display-name (name prefix)
  "Return possible display names for imenu NAME under PREFIX."
  (let* ((name (format "%s" name))
         (flat (if prefix
                   (concat prefix " " name)
                 name))
         (space (and imenu-space-replacement
                     (aref imenu-space-replacement 0))))
    (delete-dups
     (delq nil
           (list name
                 flat
                 (and space (subst-char-in-string ?\s space name))
                 (and space (subst-char-in-string ?\s space flat)))))))

(defun vertico-buffer-frame--imenu-find-item (name items &optional prefix)
  "Return imenu item in ITEMS whose display name matches NAME.
PREFIX is the flattened submenu prefix."
  (catch 'item
    (dolist (item items)
      (when (consp item)
        (let ((item-name (car item)))
          (if (and (fboundp 'imenu--subalist-p)
                   (imenu--subalist-p item))
              (when-let* ((found
                           (vertico-buffer-frame--imenu-find-item
                            name
                            (cdr item)
                            (if prefix
                                (concat prefix " " (format "%s" item-name))
                              (format "%s" item-name)))))
                (throw 'item found))
            (when (member name
                          (vertico-buffer-frame--imenu-display-name
                           item-name prefix))
              (throw 'item item))))))))

(defun vertico-buffer-frame--imenu-index (buffer)
  "Return imenu index for BUFFER."
  (with-current-buffer buffer
    (imenu--make-index-alist 'noerror)))

(defun vertico-buffer-frame--imenu-item (candidate)
  "Return standard imenu item matching CANDIDATE."
  (when (and (stringp candidate)
             (require 'imenu nil t)
             (fboundp 'imenu--make-index-alist))
    (let ((name (vertico-buffer-frame--candidate-string candidate)))
      (with-minibuffer-selected-window
        (vertico-buffer-frame--imenu-find-item
         name
         (vertico-buffer-frame--imenu-index (current-buffer)))))))

(defun vertico-buffer-frame-preview-consult-candidate (candidate)
  "Return preview content from CANDIDATE's `consult--candidate' property."
  (when-let* ((position (and (stringp candidate)
                             (get-text-property 0 'consult--candidate candidate))))
    (cond
     ((and (markerp position)
           (fboundp 'consult-compile--lookup))
      (vertico-buffer-frame--position-preview
       (or (ignore-errors (consult-compile--lookup position)) position)))
     ((and (consp position) (markerp (car position)))
      (vertico-buffer-frame--position-preview (car position) nil (cdr position)))
     (t
      (vertico-buffer-frame--position-preview position)))))

(defun vertico-buffer-frame--find-file-noselect-local (file)
  "Open local FILE with `find-file-noselect'.
Return nil for remote paths so previews do not block on TRAMP I/O."
  (when (vertico-buffer-frame--local-file-p file)
    (find-file-noselect file)))

(defun vertico-buffer-frame-preview-grep (candidate)
  "Return grep location preview content for CANDIDATE."
  (when (and (stringp candidate)
             (fboundp 'consult--grep-position)
             (not (file-remote-p default-directory)))
    (when-let* ((position
                 (vertico-buffer-frame--with-io-timeout
                   (ignore-errors
                     (consult--grep-position
                      candidate
                      #'vertico-buffer-frame--find-file-noselect-local))))
                (marker (car-safe position)))
      (vertico-buffer-frame--position-preview marker nil (cdr position)))))

(defun vertico-buffer-frame-preview-imenu (candidate)
  "Return imenu preview content for CANDIDATE."
  (let* ((item (cond
                ((or (markerp candidate) (integerp candidate))
                 candidate)
                ((consp candidate)
                 candidate)
                ((stringp candidate)
                 (or (get-text-property 0 'imenu-choice candidate)
                     (vertico-buffer-frame--imenu-minibuffer-item candidate)
                     (vertico-buffer-frame--consult-imenu-item candidate)
                     (vertico-buffer-frame--imenu-item candidate)))))
         (position (cond
                    ((or (markerp item) (integerp item)) item)
                    ((or (markerp (cdr-safe item))
                         (integerp (cdr-safe item)))
                     (cdr item))
                    ((and (consp (cdr-safe item))
                          (or (markerp (cadr item))
                              (integerp (cadr item))))
                     (cadr item)))))
    (vertico-buffer-frame--position-preview position)))

(defun vertico-buffer-frame-preview-org-heading (candidate)
  "Return Org heading preview content for CANDIDATE."
  (when-let* ((marker (and (stringp candidate)
                           (get-text-property 0 'org-marker candidate))))
    (vertico-buffer-frame--position-preview marker)))

(defun vertico-buffer-frame--info-source-buffer ()
  "Return the Info buffer that owns the current menu completion."
  (or (and (boundp 'Info-complete-menu-buffer)
           (buffer-live-p Info-complete-menu-buffer)
           Info-complete-menu-buffer)
      (when-let* ((window (vertico-buffer-frame--current-source-window))
                  ((window-live-p window))
                  (buffer (window-buffer window)))
        (with-current-buffer buffer
          (and (bound-and-true-p Info-current-node)
               buffer)))
      (and (bound-and-true-p Info-current-node)
           (current-buffer))))

(defun vertico-buffer-frame--info-file-name ()
  "Return a concise name for the current Info file."
  (cond
   ((stringp Info-current-file)
    (file-name-nondirectory Info-current-file))
   (Info-current-file
    (format "%s" Info-current-file))
   (t
    "Info")))

(defun vertico-buffer-frame--info-menu-title (source target)
  "Return a preview title for TARGET reached from Info SOURCE."
  (with-current-buffer source
    (format "%s\n%s -> %s\n\n"
            (vertico-buffer-frame--info-file-name)
            (or Info-current-node "Menu")
            target)))

(defun vertico-buffer-frame--info-node-preview (source target title)
  "Return Info TARGET node preview using SOURCE and TITLE."
  (vertico-buffer-frame--with-io-timeout
    (let ((clone (with-current-buffer source
                   (clone-buffer nil nil))))
      (unwind-protect
          (with-current-buffer clone
            (Info-goto-node target)
            (concat title
                    (string-trim-left
                     (buffer-substring-no-properties
                      (point-min) (point-max)))))
        (when (buffer-live-p clone)
          (kill-buffer clone))))))

(defun vertico-buffer-frame-preview-info-menu (candidate)
  "Return Info menu target preview for CANDIDATE."
  (when (require 'info nil t)
    (when-let* ((source (vertico-buffer-frame--info-source-buffer))
                ((buffer-live-p source))
                (target
                 (with-current-buffer source
                   (ignore-errors
                     (Info-extract-menu-item
                      (vertico-buffer-frame--candidate-string candidate))))))
      (or (ignore-errors
            (vertico-buffer-frame--info-node-preview
             source target
             (vertico-buffer-frame--info-menu-title source target)))
          (vertico-buffer-frame--info-menu-title source target)))))

(defun vertico-buffer-frame-preview-info (candidate)
  "Return Info search preview content for CANDIDATE."
  (pcase (and (stringp candidate)
              (get-text-property 0 'consult--info candidate))
    (`(,node ,position ,buffer)
     (when (buffer-live-p buffer)
       (vertico-buffer-frame--position-preview
        (set-marker (make-marker) position buffer)
        (format "%s\n\n" node))))))

(defun vertico-buffer-frame-preview-man (candidate)
  "Return man page preview text for CANDIDATE."
  (let ((page (or (and (stringp candidate)
                       (get-text-property 0 'consult-man candidate))
                  (vertico-buffer-frame--candidate-string candidate))))
    (format "%s\n\n%s" page (vertico-buffer-frame--candidate-string candidate))))

(defun vertico-buffer-frame--xref-location-remote-p (location)
  "Return non-nil when xref LOCATION points to a remote file."
  (when (and location (fboundp 'xref-location-group))
    (let ((group (ignore-errors (xref-location-group location))))
      (and (stringp group) (file-remote-p group)))))

(defun vertico-buffer-frame-preview-xref (candidate)
  "Return xref preview content for CANDIDATE."
  (when (and (fboundp 'xref-item-location)
             (fboundp 'xref-location-marker))
    (let* ((xref (or (and (stringp candidate)
                          (get-text-property 0 'consult-xref candidate))
                     (if (consp candidate) (cdr candidate) candidate)))
           (location (ignore-errors (xref-item-location xref)))
           (marker (and location
                        (not (vertico-buffer-frame--xref-location-remote-p
                              location))
                        (vertico-buffer-frame--with-io-timeout
                          (ignore-errors
                            (xref-location-marker location))))))
      (vertico-buffer-frame--position-preview marker))))

(defun vertico-buffer-frame--insert-preview-content (content)
  "Insert preview CONTENT into the current preview buffer."
  (erase-buffer)
  (cond
   ((stringp content)
    (insert (substring content
                       0
                       (min (length content)
                            vertico-buffer-frame-preview-max-size))))
   ((bufferp content)
    (insert-buffer-substring content
                             (with-current-buffer content (point-min))
                             (with-current-buffer content
                               (min (point-max)
                                    (+ (point-min)
                                       vertico-buffer-frame-preview-max-size))))))
  (goto-char (point-min)))

(defun vertico-buffer-frame--preview-content ()
  "Return preview content for the current candidate."
  (when-let* (((not vertico-buffer-frame--exiting))
              (function vertico-buffer-frame-preview-function)
              (candidate (vertico-buffer-frame--current-candidate)))
    (condition-case err
        (funcall function candidate)
      (error
       (message "vertico-buffer-frame preview: %s" (error-message-string err))
       nil))))

(defun vertico-buffer-frame--preview-content-while-no-input ()
  "Return a cons cell (FINISHED . CONTENT) for the current preview.
FINISHED is nil when Emacs receives input while the preview is being generated."
  (let ((sentinel (make-symbol "vertico-buffer-frame-preview-done"))
        content)
    (if (eq (while-no-input
              (setq content (vertico-buffer-frame--preview-content))
              sentinel)
            sentinel)
        (cons t content)
      (cons nil nil))))

(defun vertico-buffer-frame--cancel-preview-timer (&optional buffer)
  "Cancel the pending preview update timer for BUFFER.
BUFFER defaults to the current or active minibuffer buffer."
  (when-let* ((buffer (or buffer
                          (vertico-buffer-frame--minibuffer-buffer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (timerp vertico-buffer-frame--preview-timer)
        (cancel-timer vertico-buffer-frame--preview-timer))
      (setq-local vertico-buffer-frame--preview-timer nil))))

(defun vertico-buffer-frame--preview-frame-p (frame)
  "Return non-nil when FRAME belongs to the preview child frame."
  (and (frame-live-p frame)
       (frame-parent frame)
       (eq (frame-parameter frame 'share-child-frame)
           'vertico-buffer-frame-preview)))

(defun vertico-buffer-frame--preview-frames ()
  "Return live preview child frames."
  (seq-filter #'vertico-buffer-frame--preview-frame-p (frame-list)))

(defun vertico-buffer-frame--hide-preview (&optional buffer)
  "Hide the preview child frame for BUFFER."
  (when-let* ((buffer (or buffer
                          (vertico-buffer-frame--minibuffer-buffer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (vertico-buffer-frame--cancel-preview-timer buffer)))
  (let (deleted)
    (dolist (frame (vertico-buffer-frame--preview-frames))
      (when (frame-live-p frame)
        (setq deleted t)
        (let ((delete-frame-functions nil))
          (delete-frame frame t))))
    (setq vertico-buffer-frame--preview-frame nil)
    (vertico-buffer-frame--kill-preview-location-buffer)
    (when deleted
      (redisplay t))))

(defun vertico-buffer-frame--kill-preview-location-buffer ()
  "Kill the indirect buffer used for marker location previews."
  (when (buffer-live-p vertico-buffer-frame--preview-location-buffer)
    (kill-buffer vertico-buffer-frame--preview-location-buffer))
  (setq vertico-buffer-frame--preview-location-buffer nil))

(defun vertico-buffer-frame--reset-preview-window (window)
  "Reset WINDOW viewport to the current preview point."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (let ((point (point)))
        (set-window-point window point)
        (set-window-start
         window
         (if vertico-buffer-frame--preview-recenter
             (save-excursion
               (goto-char point)
               (forward-line (- (/ (max 1 (window-body-height window)) 2)))
               (line-beginning-position))
           point)
         t)))
    (set-window-hscroll window 0)))

(defun vertico-buffer-frame--preview-buffer-with-marker
    (content parameters)
  "Return an indirect preview buffer for marker CONTENT styled by PARAMETERS."
  (when-let* ((marker (vertico-buffer-frame--marker-preview-marker content))
              (source (marker-buffer marker))
              ((buffer-live-p source)))
    (vertico-buffer-frame--kill-preview-location-buffer)
    (setq vertico-buffer-frame--preview-location-buffer
          (with-current-buffer source
            (clone-indirect-buffer
             " *vertico-buffer-frame-location-preview*" nil)))
    (with-current-buffer vertico-buffer-frame--preview-location-buffer
      (setq-local cursor-type nil
                  truncate-lines t
                  mode-line-format nil
                  header-line-format nil
                  tab-line-format nil
                  vertico-buffer-frame--preview-recenter t
                  face-remapping-alist
                  (vertico-buffer-frame--face-remapping parameters))
      (widen)
      (goto-char (min (point-max) (max (point-min)
                                       (marker-position marker)))))
    vertico-buffer-frame--preview-location-buffer))

(defun vertico-buffer-frame--preview-buffer-with-content
    (content parameters)
  "Return preview buffer containing CONTENT styled by PARAMETERS."
  (if (vertico-buffer-frame--marker-preview-p content)
      (vertico-buffer-frame--preview-buffer-with-marker content parameters)
    (vertico-buffer-frame--kill-preview-location-buffer)
    (let ((preview-buffer (get-buffer-create
                           vertico-buffer-frame--preview-buffer)))
      (with-current-buffer preview-buffer
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))
          (setq-local cursor-type nil
                      truncate-lines t
                      mode-line-format nil
                      vertico-buffer-frame--preview-recenter nil
                      face-remapping-alist
                      (vertico-buffer-frame--face-remapping parameters))
          (vertico-buffer-frame--insert-preview-content content)))
      preview-buffer)))

(defun vertico-buffer-frame--show-preview-window
    (preview-buffer parameters candidate-frame)
  "Show PREVIEW-BUFFER with PARAMETERS next to CANDIDATE-FRAME."
  (when-let* ((window (vertico-buffer-frame--preview-window
                       preview-buffer parameters)))
    (vertico-buffer-frame--reset-preview-window window)
    (setq vertico-buffer-frame--preview-frame (window-frame window))
    (vertico-buffer-frame--hide-window-chrome window)
    (vertico-buffer-frame--apply-frame-parameters
     vertico-buffer-frame--preview-frame parameters)
    (vertico-buffer-frame--position-pair
     candidate-frame vertico-buffer-frame--preview-frame)
    (vertico-buffer-frame--ensure-frame-visible
     vertico-buffer-frame--preview-frame)))

(defun vertico-buffer-frame--show-nonempty-preview
    (content candidate-frame)
  "Show nonempty preview CONTENT for CANDIDATE-FRAME."
  (let* ((parameters (vertico-buffer-frame--preview-frame-parameters
                      candidate-frame))
         (preview-buffer
          (vertico-buffer-frame--preview-buffer-with-content
           content parameters)))
    (vertico-buffer-frame--show-preview-window
     preview-buffer parameters candidate-frame)))

(defun vertico-buffer-frame--show-preview-content (content)
  "Show preview CONTENT in the preview child frame."
  (let* ((minibuffer-buffer (vertico-buffer-frame--minibuffer-buffer))
         (enabled (and (bound-and-true-p vertico-buffer-frame-mode)
                       (vertico-buffer-frame--preview-enabled-p
                        minibuffer-buffer)))
         (candidate-frame (and enabled
                               (vertico-buffer-frame--candidate-frame))))
    (cond
     ((and enabled (frame-live-p candidate-frame) content)
      (vertico-buffer-frame--show-nonempty-preview content candidate-frame))
     ((and enabled (frame-live-p candidate-frame))
      (vertico-buffer-frame--hide-preview minibuffer-buffer))
     (t
      (vertico-buffer-frame--hide-preview minibuffer-buffer)))))

(defun vertico-buffer-frame--show-preview ()
  "Show preview for the current Vertico candidate.
The preview is generated from the current minibuffer state."
  (if (vertico-buffer-frame--preview-enabled-p (current-buffer))
      (if (vertico-buffer-frame--current-candidate)
          (let ((result (vertico-buffer-frame--preview-content-while-no-input)))
            (when (car result)
              (vertico-buffer-frame--show-preview-content (cdr result))))
        (vertico-buffer-frame--hide-preview (current-buffer)))
    (vertico-buffer-frame--hide-preview (current-buffer))))

(defun vertico-buffer-frame--show-preview-later (buffer)
  "Show preview in minibuffer BUFFER from an idle timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local vertico-buffer-frame--preview-timer nil)
      (when (and (not vertico-buffer-frame--exiting)
                 (vertico-buffer-frame--preview-enabled-p buffer))
        (vertico-buffer-frame--show-preview)))))

(defun vertico-buffer-frame--schedule-preview ()
  "Schedule a preview update for the current Vertico candidate."
  (vertico-buffer-frame--cancel-preview-timer)
  (if (and (vertico-buffer-frame--preview-enabled-p (current-buffer))
           (not vertico-buffer-frame--exiting)
           (vertico-buffer-frame--current-candidate))
      (let ((delay (max 0 (float vertico-buffer-frame-preview-delay)))
            (buffer (current-buffer)))
        (if (zerop delay)
            (vertico-buffer-frame--show-preview)
          (setq-local
           vertico-buffer-frame--preview-timer
           (run-with-idle-timer
            delay nil
            #'vertico-buffer-frame--show-preview-later
            buffer))))
    (vertico-buffer-frame--hide-preview)))

(defun vertico-buffer-frame--refresh-active-preview ()
  "Refresh preview for the active minibuffer, when any."
  (if-let* ((window (active-minibuffer-window))
            ((window-live-p window))
            (buffer (window-buffer window)))
      (with-current-buffer buffer
        (if (vertico-buffer-frame--preview-enabled-p buffer)
            (vertico-buffer-frame--show-preview)
          (vertico-buffer-frame--hide-preview buffer)))
    (vertico-buffer-frame--hide-preview)))

(defun vertico-buffer-frame--refresh-minibuffer-preview (&optional buffer)
  "Refresh preview for minibuffer BUFFER.
BUFFER defaults to the most recent live minibuffer tracked by
`vertico-buffer-frame-mode'."
  (if-let* ((buffer (or buffer
                        (vertico-buffer-frame--active-minibuffer-buffer)
                        (vertico-buffer-frame--top-minibuffer-buffer)))
            ((vertico-buffer-frame--live-minibuffer-buffer-p buffer)))
      (with-current-buffer buffer
        (if (and (not vertico-buffer-frame--exiting)
                 (vertico-buffer-frame--preview-enabled-p buffer))
            (vertico-buffer-frame--show-preview)
          (vertico-buffer-frame--hide-preview buffer)))
    (vertico-buffer-frame--hide-preview)))

(defun vertico-buffer-frame--set-preview-enabled (buffer enabled)
  "Set preview ENABLED state for BUFFER or globally.
When BUFFER is a live minibuffer buffer, only the current completion session is
changed.  Otherwise update the global default."
  (if (vertico-buffer-frame--live-minibuffer-buffer-p buffer)
      (with-current-buffer buffer
        (setq-local vertico-buffer-frame--preview-enabled enabled))
    (setq vertico-buffer-frame-preview enabled)))

(defun vertico-buffer-frame--preview-scope (buffer)
  "Return the display scope name for preview state in BUFFER."
  (if (vertico-buffer-frame--live-minibuffer-buffer-p buffer)
      "session"
    "default"))

(provide 'vertico-buffer-frame-preview)
;;; vertico-buffer-frame-preview.el ends here
