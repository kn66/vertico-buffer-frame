;;; vertico-buffer-frame-preview.el --- Candidate previews for vertico-buffer-frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Internal preview rendering helpers for `vertico-buffer-frame'.

;;; Code:

(require 'subr-x)
(require 'seq)

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
(declare-function help-function-arglist "help-fns")
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
(declare-function vertico-buffer-frame--preview-content-columns "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--preview-enabled-p "vertico-buffer-frame")
(declare-function vertico-buffer-frame--preview-frame-parameters "vertico-buffer-frame-display")
(declare-function vertico-buffer-frame--preview-height-lines "vertico-buffer-frame-display")
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
(defvar vertico-buffer-frame--exiting)
(defvar vertico-buffer-frame--imenu-index-buffer)
(defvar vertico-buffer-frame--imenu-index-cache)
(defvar vertico-buffer-frame--last-preview-candidate)
(defvar vertico-buffer-frame--last-preview-state)
(defvar vertico-buffer-frame--preview-buffer)
(defvar vertico-buffer-frame--preview-category)
(defvar vertico-buffer-frame--preview-command)
(defvar vertico-buffer-frame--preview-completing-file-name)
(defvar vertico-buffer-frame--preview-completion-context-valid)
(defvar vertico-buffer-frame--preview-completion-extra-properties)
(defvar vertico-buffer-frame--preview-completion-input)
(defvar vertico-buffer-frame--preview-completion-metadata)
(defvar vertico-buffer-frame--preview-completion-predicate)
(defvar vertico-buffer-frame--preview-completion-table)
(defvar vertico-buffer-frame--preview-enabled)
(defvar vertico-buffer-frame--preview-fallback-columns)
(defvar vertico-buffer-frame--preview-frame)
(defvar vertico-buffer-frame--preview-frame-name)
(defvar vertico-buffer-frame--preview-location-context)
(defvar vertico-buffer-frame--preview-timer)

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

(defun vertico-buffer-frame--preview-title-lines (title)
  "Return line count occupied by preview TITLE."
  (seq-count (lambda (char) (= char ?\n)) (or title "")))

(defun vertico-buffer-frame--location-content-lines (title)
  "Return visible content lines for a location preview.
TITLE is included to reserve vertical space for the preview heading."
  (if-let* ((height (vertico-buffer-frame--preview-height-lines)))
      (let* ((title-lines (vertico-buffer-frame--preview-title-lines title))
             (content-lines (max 1 (- height title-lines))))
        content-lines)
    (1+ (* vertico-buffer-frame--preview-location-context 2))))

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
  (or (vertico-buffer-frame--completion-metadata-category)
      vertico-buffer-frame--preview-category))

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

(defun vertico-buffer-frame--completion-metadata-for-state
    (&optional input table predicate extra-properties)
  "Return captured completion metadata, or compute it from current state.
When INPUT, TABLE, PREDICATE, or EXTRA-PROPERTIES are non-nil, always compute
metadata for that explicit state."
  (if (or input table predicate extra-properties)
      (vertico-buffer-frame--completion-metadata
       input table predicate extra-properties)
    (if vertico-buffer-frame--preview-completion-context-valid
        vertico-buffer-frame--preview-completion-metadata
      (vertico-buffer-frame--completion-metadata))))

(defun vertico-buffer-frame--completion-metadata-category
    (&optional input table predicate extra-properties)
  "Return completion category using Emacs completion metadata.
INPUT, TABLE, PREDICATE, and EXTRA-PROPERTIES default to the current
minibuffer completion state."
  (when-let* ((metadata (vertico-buffer-frame--completion-metadata-for-state
                         input table predicate extra-properties)))
    (completion-metadata-get metadata 'category)))

(defun vertico-buffer-frame--copy-completion-extra-properties (properties)
  "Return a cache copy of completion extra PROPERTIES."
  (and properties (copy-tree properties)))

(defun vertico-buffer-frame--completion-context-current-p
    (input table predicate extra-properties completing-file-name)
  "Return non-nil when captured completion context matches current state.
INPUT, TABLE, PREDICATE, EXTRA-PROPERTIES, and COMPLETING-FILE-NAME are the
state values to compare with the current preview context."
  (and vertico-buffer-frame--preview-completion-context-valid
       (equal input vertico-buffer-frame--preview-completion-input)
       (eq table vertico-buffer-frame--preview-completion-table)
       (eq predicate vertico-buffer-frame--preview-completion-predicate)
       (equal extra-properties
              vertico-buffer-frame--preview-completion-extra-properties)
       (eq completing-file-name
           vertico-buffer-frame--preview-completing-file-name)))

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

(defun vertico-buffer-frame--candidate-key (candidate)
  "Return a stable comparison key for CANDIDATE."
  (and candidate
       (if minibuffer-completing-file-name
           (list 'file (vertico-buffer-frame--file-candidate-name candidate))
         (vertico-buffer-frame--candidate-string candidate))))

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

(defun vertico-buffer-frame--function-signature (symbol)
  "Return a function signature line for SYMBOL, or nil."
  (when (and (fboundp 'help-function-arglist)
             (fboundp symbol))
    (let ((arglist (ignore-errors (help-function-arglist symbol t))))
      (cond
       ((listp arglist)
        (format "Signature: %S" (cons symbol arglist)))
       ((and (stringp arglist)
             (not (string-empty-p arglist)))
        (format "Signature: %s" arglist))))))

(defun vertico-buffer-frame--command-key-bindings (symbol)
  "Return a concise key binding line for command SYMBOL, or nil."
  (when (commandp symbol)
    (when-let* ((keys (where-is-internal symbol nil nil nil)))
      (let* ((non-menu-keys
              (seq-remove (lambda (key)
                            (and (vectorp key)
                                 (> (length key) 0)
                                 (eq (aref key 0) 'menu-bar)))
                          keys))
             (descriptions
              (mapcar #'key-description
                      (seq-take (or non-menu-keys keys) 3))))
        (when descriptions
          (format "Keys: %s" (string-join descriptions ", ")))))))

(defun vertico-buffer-frame--symbol-documentation (candidate predicate)
  "Return documentation for CANDIDATE when its symbol satisfies PREDICATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((funcall predicate symbol)))
    (vertico-buffer-frame--documentation
     (format "%s" symbol)
     (documentation symbol t)
     (delq nil
           (list (vertico-buffer-frame--function-signature symbol)
                 (vertico-buffer-frame--command-key-bindings symbol))))))

(defun vertico-buffer-frame-preview-default (candidate)
  "Return preview content for CANDIDATE using built-in preview rules."
  (let* ((preview (vertico-buffer-frame--target candidate))
         (category (car preview))
         (target (cdr preview)))
    (or (when-let* ((function (cdr (assoc category
                                          vertico-buffer-frame-preview-category-functions))))
          (funcall function target))
        (when-let* ((function (cdr (assq this-command
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
  (when-let* ((metadata (vertico-buffer-frame--completion-metadata-for-state)))
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
  (vertico-buffer-frame--symbol-documentation candidate #'commandp))

(defun vertico-buffer-frame-preview-function (candidate)
  "Return function documentation preview for CANDIDATE."
  (vertico-buffer-frame--symbol-documentation candidate #'fboundp))

(defun vertico-buffer-frame-preview-variable (candidate)
  "Return variable documentation preview for CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate))
              ((boundp symbol)))
    (let ((doc (documentation-property symbol 'variable-documentation t)))
      (concat
       (format "%s\n\n" symbol)
       (if doc (string-trim (substitute-command-keys doc)) "Undocumented.")
       "\n\n"
       (format "Value:\n%S" (symbol-value symbol))))))

(defun vertico-buffer-frame-preview-symbol (candidate)
  "Return documentation preview for symbol CANDIDATE."
  (when-let* ((symbol (vertico-buffer-frame--candidate-symbol candidate)))
    (or (vertico-buffer-frame-preview-function (symbol-name symbol))
        (vertico-buffer-frame-preview-variable (symbol-name symbol))
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
         (values (ignore-errors (color-values color))))
    (when values
      (let* ((components (mapcar (lambda (value)
                                   (round (/ value 257.0)))
                                 values))
             (hex (apply #'format "#%02X%02X%02X" components))
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
    (let ((doc (face-documentation symbol)))
      (concat
       (format "%s\n\n" symbol)
       (if doc (string-trim doc) "Undocumented face.")
       "\n\n"
       (format "Foreground: %S\nBackground: %S\nWeight: %S\nSlant: %S"
               (face-attribute symbol :foreground nil t)
               (face-attribute symbol :background nil t)
               (face-attribute symbol :weight nil t)
               (face-attribute symbol :slant nil t))))))

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
        (when-let* ((window (minibuffer-selected-window))
                    ((window-live-p window)))
          (window-buffer window)))
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

(defun vertico-buffer-frame--add-face (string beg end face)
  "Add FACE to STRING between BEG and END, clipping to string bounds."
  (let ((beg (max 0 beg))
        (end (min (length string) end)))
    (when (< beg end)
      (add-face-text-property beg end face nil string))))

(defun vertico-buffer-frame--line-slice (point matches columns)
  "Return a bounded preview slice for the current line.
POINT is the target buffer position.  MATCHES is a list of match begin/end
pairs relative to POINT.  COLUMNS limits how much of the line is copied."
  (let* ((line-beg (line-beginning-position))
         (line-end (line-end-position))
         (target-line-p (and (<= line-beg point)
                             (<= point line-end)))
         (target-column (and target-line-p (- point line-beg)))
         (start-offset (if target-column
                           (max 0 (- target-column (/ columns 2)))
                         0))
         (slice-beg (min line-end (+ line-beg start-offset)))
         (slice-end (min line-end (+ slice-beg columns)))
         (slice (buffer-substring-no-properties slice-beg slice-end)))
    (when target-line-p
      (vertico-buffer-frame--add-face
       slice 0 (length slice) 'vertico-buffer-frame-preview-line)
      (dolist (match matches)
        (when (and (consp match)
                   (integerp (car match))
                   (integerp (cdr match)))
          (let ((match-beg (+ point (car match)))
                (match-end (+ point (cdr match))))
            (vertico-buffer-frame--add-face
             slice
             (- match-beg slice-beg)
             (- match-end slice-beg)
             'vertico-buffer-frame-preview-match)))))
    slice))

(defun vertico-buffer-frame--position-lines (point content-lines columns matches)
  "Return bounded preview text around POINT.
CONTENT-LINES is the maximum number of lines to include.  COLUMNS limits each
line slice.  MATCHES is a list of match begin/end pairs relative to POINT."
  (let ((lines nil)
        (remaining content-lines)
        (before-lines (/ (1- content-lines) 2)))
    (goto-char point)
    (forward-line (- before-lines))
    (while (and (> remaining 0)
                (not (eobp)))
      (push (vertico-buffer-frame--line-slice point matches columns)
            lines)
      (setq remaining (1- remaining))
      (forward-line 1))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun vertico-buffer-frame--position-content (point title matches)
  "Return preview content around POINT in the current buffer.
TITLE is inserted above the preview when non-nil.
MATCHES is a list of match begin/end pairs relative to POINT."
  (goto-char point)
  (let* ((title (or title (format "%s:%d\n\n"
                                  (buffer-name)
                                  (line-number-at-pos point))))
         (content-lines (vertico-buffer-frame--location-content-lines title))
         (columns (vertico-buffer-frame--preview-content-columns)))
    (concat
     title
     (vertico-buffer-frame--position-lines
      point content-lines columns matches))))

(defun vertico-buffer-frame--position-preview
    (position &optional title matches source-buffer)
  "Return preview content around POSITION.
TITLE is inserted above the preview when non-nil.
MATCHES is a list of match begin/end pairs relative to POSITION.
When POSITION is an integer, resolve it in SOURCE-BUFFER."
  (let* ((marker (vertico-buffer-frame--position-marker position source-buffer))
         (buffer (and marker (marker-buffer marker)))
         (point (and marker (marker-position marker))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (vertico-buffer-frame--position-content point title matches)))))))

(defun vertico-buffer-frame--file-position-content (file position &optional title)
  "Return preview content for FILE around POSITION, using TITLE."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (vertico-buffer-frame--position-preview position title nil buffer))))

(defun vertico-buffer-frame--file-preview (file &optional position title)
  "Return preview content for local FILE.
When POSITION is an integer or marker and FILE is regular, show content around
that position using TITLE as the preview heading."
  (when (vertico-buffer-frame--local-file-p file)
    (vertico-buffer-frame--with-io-timeout
      (cond
       ((file-directory-p file)
        (vertico-buffer-frame--directory-content file))
       ((file-regular-p file)
        (unless (vertico-buffer-frame--binary-file-p file)
          (if (or (integerp position) (markerp position))
              (vertico-buffer-frame--file-position-content file position title)
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
      (when-let* ((window (minibuffer-selected-window))
                  ((window-live-p window)))
        (with-current-buffer (window-buffer window)
          (catch 'item
            (dolist (item (consult-imenu--items-safe))
              (when (and (consp item)
                         (stringp (car item))
                         (equal name
                                (substring-no-properties (car item))))
                (throw 'item item)))))))))

(defun vertico-buffer-frame--imenu-minibuffer-item (candidate)
  "Return imenu item for CANDIDATE from the current completion table."
  (let ((table (or vertico-buffer-frame--preview-completion-table
                   minibuffer-completion-table)))
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
  "Return cached imenu index for BUFFER in the current minibuffer."
  (if (eq buffer vertico-buffer-frame--imenu-index-buffer)
      vertico-buffer-frame--imenu-index-cache
    (setq-local vertico-buffer-frame--imenu-index-buffer buffer
                vertico-buffer-frame--imenu-index-cache
                (with-current-buffer buffer
                  (imenu--make-index-alist 'noerror)))))

(defun vertico-buffer-frame--imenu-item (candidate)
  "Return standard imenu item matching CANDIDATE."
  (when (and (stringp candidate)
             (require 'imenu nil t)
             (fboundp 'imenu--make-index-alist))
    (let ((name (vertico-buffer-frame--candidate-string candidate)))
      (when-let* ((window (minibuffer-selected-window))
                  ((window-live-p window)))
        (vertico-buffer-frame--imenu-find-item
         name
         (vertico-buffer-frame--imenu-index (window-buffer window)))))))

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
        (let ((this-command (or vertico-buffer-frame--preview-command
                                this-command))
              (minibuffer-completion-table
               (if vertico-buffer-frame--preview-completion-context-valid
                   vertico-buffer-frame--preview-completion-table
                 minibuffer-completion-table))
              (minibuffer-completion-predicate
               (if vertico-buffer-frame--preview-completion-context-valid
                   vertico-buffer-frame--preview-completion-predicate
                 minibuffer-completion-predicate))
              (completion-extra-properties
               (if vertico-buffer-frame--preview-completion-context-valid
                   vertico-buffer-frame--preview-completion-extra-properties
                 completion-extra-properties))
              (minibuffer-completing-file-name
               (if vertico-buffer-frame--preview-completion-context-valid
                   vertico-buffer-frame--preview-completing-file-name
                 minibuffer-completing-file-name)))
          (funcall function candidate))
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

(defun vertico-buffer-frame--capture-preview-context ()
  "Capture completion context for delayed preview rendering."
  (let ((input (vertico-buffer-frame--completion-input))
        (table minibuffer-completion-table)
        (predicate minibuffer-completion-predicate)
        (extra-properties completion-extra-properties)
        (completing-file-name minibuffer-completing-file-name))
    (unless (vertico-buffer-frame--completion-context-current-p
             input table predicate extra-properties completing-file-name)
      (let ((metadata
             (vertico-buffer-frame--completion-metadata
              input table predicate extra-properties)))
        (setq-local
         vertico-buffer-frame--preview-category
         (and metadata (completion-metadata-get metadata 'category))
         vertico-buffer-frame--preview-completion-input
         input
         vertico-buffer-frame--preview-completion-table
         table
         vertico-buffer-frame--preview-completion-predicate
         predicate
         vertico-buffer-frame--preview-completion-extra-properties
         (vertico-buffer-frame--copy-completion-extra-properties
          extra-properties)
         vertico-buffer-frame--preview-completion-metadata
         metadata
         vertico-buffer-frame--preview-completion-context-valid
         t
         vertico-buffer-frame--preview-completing-file-name
         completing-file-name)))))

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
       (or (eq frame vertico-buffer-frame--preview-frame)
           (equal (frame-parameter frame 'name)
                  vertico-buffer-frame--preview-frame-name)
           (eq (frame-parameter frame 'share-child-frame)
               'vertico-buffer-frame-preview)
           (and (frame-parent frame)
                (when-let* ((buffer
                             (get-buffer vertico-buffer-frame--preview-buffer)))
                  (get-buffer-window buffer frame))))))

(defun vertico-buffer-frame--preview-frames ()
  "Return live preview child frames, including stale tracked frames."
  (let (frames)
    (when (frame-live-p vertico-buffer-frame--preview-frame)
      (push vertico-buffer-frame--preview-frame frames))
    (dolist (frame (frame-list))
      (when (vertico-buffer-frame--preview-frame-p frame)
        (push frame frames)))
    (delete-dups frames)))

(defun vertico-buffer-frame--hide-preview (&optional buffer)
  "Hide the preview child frame for BUFFER."
  (when-let* ((buffer (or buffer
                          (vertico-buffer-frame--minibuffer-buffer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (vertico-buffer-frame--cancel-preview-timer buffer)
      (setq-local vertico-buffer-frame--last-preview-candidate nil
                  vertico-buffer-frame--last-preview-state nil)))
  (let (deleted)
    (dolist (frame (vertico-buffer-frame--preview-frames))
      (when (frame-live-p frame)
        (setq deleted t)
        (let ((delete-frame-functions nil))
          (delete-frame frame t))))
    (setq vertico-buffer-frame--preview-frame nil)
    (when deleted
      (redisplay t))))

(defun vertico-buffer-frame--hide-preview-temporarily ()
  "Hide the preview child frame without deleting it.
This keeps the child frame reusable when adjacent candidates do not produce
preview content."
  (when (frame-live-p vertico-buffer-frame--preview-frame)
    (make-frame-invisible vertico-buffer-frame--preview-frame t)))

(defun vertico-buffer-frame--reset-preview-window (window)
  "Reset WINDOW viewport to the beginning of its preview buffer."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (set-window-point window (point-min))
      (set-window-start window (point-min) t))
    (set-window-hscroll window 0)))

(defun vertico-buffer-frame--preview-buffer-with-content
    (content parameters)
  "Return preview buffer containing CONTENT styled by PARAMETERS."
  (let ((preview-buffer (get-buffer-create
                         vertico-buffer-frame--preview-buffer)))
    (with-current-buffer preview-buffer
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (buffer-undo-list t))
        (setq-local cursor-type nil
                    truncate-lines t
                    mode-line-format nil
                    face-remapping-alist
                    (vertico-buffer-frame--face-remapping parameters))
        (vertico-buffer-frame--insert-preview-content content)))
    preview-buffer))

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
      (vertico-buffer-frame--hide-preview-temporarily))
     (t
      (vertico-buffer-frame--hide-preview minibuffer-buffer)))))

(defun vertico-buffer-frame--show-preview (&optional captured-context)
  "Show preview for the current Vertico candidate.
When CAPTURED-CONTEXT is non-nil, reuse the caller's captured completion
state."
  (if (vertico-buffer-frame--preview-enabled-p (current-buffer))
      (progn
        (unless captured-context
          (vertico-buffer-frame--capture-preview-context))
        (when-let* ((candidate (vertico-buffer-frame--current-candidate)))
          (let ((candidate-key (vertico-buffer-frame--candidate-key candidate))
                (result (vertico-buffer-frame--preview-content-while-no-input)))
            (when (car result)
              (setq-local vertico-buffer-frame--last-preview-candidate
                          candidate-key
                          vertico-buffer-frame--last-preview-state
                          (if (cdr result) 'content 'empty))
              (vertico-buffer-frame--show-preview-content (cdr result))))))
    (vertico-buffer-frame--hide-preview (current-buffer))))

(defun vertico-buffer-frame--show-preview-if-current
    (buffer candidate-key &optional captured-context)
  "Show preview in BUFFER when CANDIDATE-KEY is still current.
When CAPTURED-CONTEXT is non-nil, reuse the caller's captured completion
state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local vertico-buffer-frame--preview-timer nil)
      (when-let* (((not vertico-buffer-frame--exiting))
                  ((vertico-buffer-frame--preview-enabled-p buffer))
                  (candidate (vertico-buffer-frame--current-candidate))
                  ((equal candidate-key
                          (vertico-buffer-frame--candidate-key candidate))))
        (vertico-buffer-frame--show-preview captured-context)))))

(defun vertico-buffer-frame--preview-current-p (candidate-key)
  "Return non-nil when CANDIDATE-KEY already has an up-to-date preview."
  (and (equal candidate-key vertico-buffer-frame--last-preview-candidate)
       (pcase vertico-buffer-frame--last-preview-state
         ('content (frame-live-p vertico-buffer-frame--preview-frame))
         ('empty t))))

(defun vertico-buffer-frame--schedule-preview ()
  "Schedule a preview update for the current Vertico candidate."
  (if (and (vertico-buffer-frame--preview-enabled-p (current-buffer))
           (not vertico-buffer-frame--exiting))
      (if-let* ((candidate (vertico-buffer-frame--current-candidate)))
          (let ((candidate-key (vertico-buffer-frame--candidate-key candidate)))
            (unless (vertico-buffer-frame--preview-current-p candidate-key)
              (vertico-buffer-frame--cancel-preview-timer)
              (vertico-buffer-frame--capture-preview-context)
              (let ((delay (max 0 (float vertico-buffer-frame-preview-delay)))
                    (buffer (current-buffer)))
                (if (zerop delay)
                    (vertico-buffer-frame--show-preview t)
                  (setq-local
                   vertico-buffer-frame--preview-timer
                   (run-with-idle-timer
                    delay nil
                    #'vertico-buffer-frame--show-preview-if-current
                    buffer candidate-key t))))))
        (vertico-buffer-frame--hide-preview))
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
        (setq-local vertico-buffer-frame--last-preview-candidate nil
                    vertico-buffer-frame--last-preview-state nil)
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
