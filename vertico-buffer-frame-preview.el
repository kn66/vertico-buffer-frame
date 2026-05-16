;;; vertico-buffer-frame-preview.el --- Preview support for vertico-buffer-frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Category-aware preview support for `vertico-buffer-frame'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vertico)

(defgroup vertico-buffer-frame nil
  "Display `vertico-buffer-mode' windows in child frames."
  :group 'vertico
  :prefix "vertico-buffer-frame-")

(defcustom vertico-buffer-frame-preview t
  "Non-nil means preview the current candidate."
  :type 'boolean)

(defcustom vertico-buffer-frame-preview-delay 0.24
  "Idle delay in seconds before showing the preview frame."
  :type 'number)

(defcustom vertico-buffer-frame-preview-width nil
  "Maximum width of the preview child frame in characters.
When nil, derive the width from the candidate frame."
  :type '(choice (const :tag "Automatic" nil) natnum))

(defcustom vertico-buffer-frame-preview-height nil
  "Maximum height of the preview child frame in characters.
When nil, derive the height from the candidate frame."
  :type '(choice (const :tag "Automatic" nil) natnum))

(defcustom vertico-buffer-frame-preview-max-file-size 1048576
  "Maximum bytes to read from a regular file for previews.
When nil, read the whole file."
  :type '(choice (const :tag "Unlimited" nil) natnum))

(defcustom vertico-buffer-frame-preview-remote-files nil
  "Non-nil means preview remote files."
  :type 'boolean)

(defcustom vertico-buffer-frame-preview-binary-files nil
  "Non-nil means preview binary file contents.
When nil, binary-looking files are shown as metadata only."
  :type 'boolean)

(defcustom vertico-buffer-frame-preview-directory-entry-limit 200
  "Maximum number of directory entries shown in a preview.
When nil, show all entries."
  :type '(choice (const :tag "Unlimited" nil) natnum))

(defcustom vertico-buffer-frame-preview-search-buffer-limit 20
  "Maximum number of live buffers to scan for text-like previews.
This limits fallback searches used by targets such as `dabbrev'.  The
minibuffer origin buffer, when live, is searched first.  When nil, scan all live
buffers."
  :type '(choice (const :tag "Unlimited" nil) natnum))

(defcustom vertico-buffer-frame-preview-report-errors t
  "Non-nil means report preview errors with `message'.
Errors are still handled by hiding the preview.  When `debug-on-error' is
non-nil, preview errors are not caught."
  :type 'boolean)

(defcustom vertico-buffer-frame-preview-categories
  '(file
    project-file
    buffer
    xref-location
    bookmark
    symbol-help
    command
    function
    variable
    face
    symbol
    unicode-name
    environment-variable
    info-menu
    calendar-month
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
    consult-xref
    consult-location
    consult-grep)
  "Completion categories for which candidate previews are shown."
  :type '(repeat symbol))

(defvar vertico-buffer-frame-preview-target-functions nil
  "Hook functions for additional preview targets.
Each function is called with CATEGORY, CANDIDATE and RAW-CANDIDATE.
It should return a preview target accepted by
`vertico-buffer-frame--show-preview', or nil if it cannot handle the
candidate.")

(with-eval-after-load 'consult
  (require 'vertico-buffer-frame-consult nil t))

(defvar vertico-buffer-frame-mode)
(defvar vertico-buffer-frame--candidate-frame)
(defvar vertico--input)
(defvar vertico--metadata)
(defvar Info-complete-menu-buffer)
(defvar calendar-month-name-array)
(defvar custom-enabled-themes)
(defvar input-method-alist)
(defvar package-alist)
(defvar package-archive-contents)

(defvar-local vertico-buffer-frame--preview-frame nil)
(defvar-local vertico-buffer-frame--preview-window nil)
(defvar-local vertico-buffer-frame--preview-buffer nil)
(defvar-local vertico-buffer-frame--preview-timer nil)
(defvar-local vertico-buffer-frame--preview-last-error-message nil)
(defvar-local vertico-buffer-frame--preview-scheduled-state nil)
(defvar-local vertico-buffer-frame--project-root-cache nil)
(defvar-local vertico-buffer-frame--file-preview-cache nil)
(defvar-local vertico-buffer-frame--imenu-cache nil)

(defconst vertico-buffer-frame-preview--golden-ratio
  (/ (+ 1.0 (sqrt 5.0)) 2.0)
  "Golden ratio used for automatic preview layout.")

(declare-function bookmark-get-bookmark "bookmark")
(declare-function bookmark-get-filename "bookmark")
(declare-function bookmark-get-position "bookmark")
(declare-function calendar-current-date "calendar")
(declare-function calendar-extract-year "calendar")
(declare-function calendar-generate-month "calendar")
(declare-function calendar-make-alist "calendar")
(declare-function charset-description "mule-cmds")
(declare-function color-defined-p "faces")
(declare-function color-values "faces")
(declare-function custom-available-themes "custom")
(declare-function custom-theme-p "cus-theme")
(declare-function get-charset-property "charset")
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")
(declare-function locate-library "files")
(declare-function mail-extract-address-components "mail-extr")
(declare-function package-desc-kind "package")
(declare-function package-desc-reqs "package")
(declare-function package-desc-summary "package")
(declare-function package-desc-version "package")
(declare-function package-built-in-p "package")
(declare-function package-version-join "package")
(declare-function project-root "project")
(declare-function vertico-buffer-frame--delete-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--make-child-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--parent-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--pixels-to-chars "vertico-buffer-frame")
(declare-function vertico-buffer-frame--place-preview-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--prepare-window "vertico-buffer-frame")
(declare-function vertico-buffer-frame--resize-frame-to-size
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--show-frame "vertico-buffer-frame")

(defun vertico-buffer-frame--category ()
  "Return the current Vertico completion category."
  (when (and (boundp 'vertico--metadata) vertico--metadata)
    (completion-metadata-get vertico--metadata 'category)))

(defun vertico-buffer-frame--candidate ()
  "Return the current raw Vertico candidate."
  (when (and (boundp 'vertico--input) vertico--input
             (fboundp 'vertico--candidate))
    (ignore-errors (vertico--candidate))))

(defun vertico-buffer-frame--candidate-string (candidate)
  "Return CANDIDATE as a string without text properties."
  (and (stringp candidate)
       (substring-no-properties candidate)))

(defun vertico-buffer-frame--text-property-value (property string)
  "Return the first non-nil PROPERTY value found in STRING."
  (when (and (stringp string)
             (> (length string) 0))
    (let ((position (text-property-not-all 0 (length string)
                                           property nil string)))
      (and position
           (get-text-property position property string)))))

(defun vertico-buffer-frame--text-property-end (property string)
  "Return the end of the first non-nil PROPERTY range in STRING."
  (when (and (stringp string)
             (> (length string) 0))
    (when-let* ((position (text-property-not-all 0 (length string)
                                                 property nil string)))
      (next-single-property-change position property string
                                   (length string)))))

(defun vertico-buffer-frame--candidate-entry (raw-candidate metadata-category)
  "Return (CATEGORY . CANDIDATE) for RAW-CANDIDATE and METADATA-CATEGORY.
Consult multi-source candidates report the generic `multi-category'
completion category.  Their real source category and lookup value are stored in
the `multi-category' text property."
  (when-let* ((candidate
               (vertico-buffer-frame--candidate-string raw-candidate)))
    (let ((multi (vertico-buffer-frame--text-property-value
                  'multi-category raw-candidate)))
      (if (and (consp multi)
               (symbolp (car multi)))
          (cons (car multi)
                (if (stringp (cdr multi))
                    (substring-no-properties (cdr multi))
                  (cdr multi)))
        (cons metadata-category candidate)))))

(defun vertico-buffer-frame--readable-file (file)
  "Return expanded FILE when it is readable."
  (when (stringp file)
    (let ((expanded (expand-file-name (substitute-in-file-name file))))
      (and (or vertico-buffer-frame-preview-remote-files
               (not (file-remote-p expanded)))
           (file-readable-p expanded)
           expanded))))

(defun vertico-buffer-frame--file-target (candidate &optional directory)
  "Return a file preview target for CANDIDATE under DIRECTORY."
  (when (stringp candidate)
    (when-let* ((file (vertico-buffer-frame--readable-file
                       (expand-file-name candidate
                                         (or directory default-directory)))))
      (list 'file file))))

(defun vertico-buffer-frame--project-root ()
  "Return the current project root, if any."
  (when (require 'project nil t)
    (let ((key default-directory))
      (if (and (consp vertico-buffer-frame--project-root-cache)
               (equal key (car vertico-buffer-frame--project-root-cache)))
          (cdr vertico-buffer-frame--project-root-cache)
        (let ((root (when-let* ((project (ignore-errors
                                           (project-current nil))))
                      (project-root project))))
          (setq-local vertico-buffer-frame--project-root-cache
                      (cons key root))
          root)))))

(defun vertico-buffer-frame--project-file-target (candidate)
  "Return a file preview target for project-file CANDIDATE."
  (or (vertico-buffer-frame--file-target candidate)
      (when-let* ((root (vertico-buffer-frame--project-root)))
        (vertico-buffer-frame--file-target candidate root))))

(defun vertico-buffer-frame--buffer-target (candidate)
  "Return a buffer preview target for CANDIDATE."
  (cond
   ((buffer-live-p candidate)
    (list 'buffer (buffer-name candidate)))
   ((and (stringp candidate)
         (get-buffer candidate))
    (list 'buffer candidate))))

(defun vertico-buffer-frame--origin-buffer ()
  "Return the buffer from which the minibuffer was entered, if any."
  (when-let* ((window (minibuffer-selected-window)))
    (and (window-live-p window)
         (window-buffer window))))

(defun vertico-buffer-frame--buffer-position-target (buffer position)
  "Return a buffer-position target for BUFFER at POSITION."
  (when (and (buffer-live-p buffer) position)
    (list 'buffer-position buffer position)))

(defun vertico-buffer-frame--symbol-preview-target (candidate)
  "Return a text preview target for symbol CANDIDATE."
  (when-let* ((symbol (intern-soft candidate)))
    (list 'text
          "Symbol"
          (lambda ()
            (vertico-buffer-frame--insert-symbol-preview symbol)))))

(defun vertico-buffer-frame--color-target (candidate)
  "Return a text preview target for color CANDIDATE."
  (when-let* (((color-defined-p candidate))
              (rgb (color-values candidate)))
    (list 'text
          "Color"
          (lambda ()
            (insert candidate "\n\n")
            (insert (propertize "        "
                                'face
                                `(:background ,candidate
                                              :foreground ,candidate))
                    "\n\n")
            (insert (format "#%04X%04X%04X\n"
                            (nth 0 rgb)
                            (nth 1 rgb)
                            (nth 2 rgb)))))))

(defun vertico-buffer-frame--custom-theme-target (candidate)
  "Return a preview target for custom theme CANDIDATE."
  (when (require 'cus-theme nil t)
    (let ((theme (intern-soft candidate)))
      (when (and theme
                 (memq theme (custom-available-themes)))
        (or (when-let* ((file (locate-library
                               (format "%s-theme.el" candidate))))
              (list 'file file))
            (list 'text
                  "Custom Theme"
                  (lambda ()
                    (insert candidate "\n\n")
                    (insert "Available: yes\n")
                    (insert "Loaded: "
                            (if (custom-theme-p theme) "yes" "no")
                            "\n")
                    (insert "Enabled: "
                            (if (memq theme custom-enabled-themes)
                                "yes"
                              "no")
                            "\n"))))))))

(defun vertico-buffer-frame--input-method-target (candidate)
  "Return a text preview target for input method CANDIDATE."
  (when (require 'mule-cmds nil t)
    (when-let* ((entry (assoc candidate input-method-alist)))
      (let ((language (nth 1 entry))
            (title (nth 3 entry))
            (description (nth 4 entry)))
        (list 'text
              "Input Method"
              (lambda ()
                (insert candidate "\n\n")
                (when language
                  (insert "Language: " language "\n"))
                (when title
                  (insert "Title: " title "\n"))
                (when description
                  (insert "\n" description "\n"))))))))

(defun vertico-buffer-frame--coding-system-target (candidate)
  "Return a text preview target for coding system CANDIDATE."
  (let ((coding-system (intern-soft candidate)))
    (when (and coding-system
               (coding-system-p coding-system))
      (list 'text
            "Coding System"
            (lambda ()
              (insert (symbol-name coding-system) "\n\n")
              (when-let* ((doc (coding-system-doc-string coding-system)))
                (insert doc "\n\n"))
              (when-let* ((base (ignore-errors
                                  (coding-system-base coding-system))))
                (insert "Base: " (symbol-name base) "\n"))
              (when-let* ((mnemonic (ignore-errors
                                      (coding-system-mnemonic coding-system))))
                (insert "Mnemonic: " (char-to-string mnemonic) "\n"))
              (let ((eol (ignore-errors
                           (coding-system-eol-type coding-system))))
                (insert "EOL: " (format "%S" eol) "\n")))))))

(defun vertico-buffer-frame--charset-target (candidate)
  "Return a text preview target for character set CANDIDATE."
  (let ((charset (intern-soft candidate)))
    (when (and charset
               (charsetp charset))
      (list 'text
            "Character Set"
            (lambda ()
              (insert (symbol-name charset) "\n\n")
              (when-let* ((description (charset-description charset)))
                (insert description "\n\n"))
              (dolist (property '(short-name long-name iso-final-char
                                             dimension chars))
                (when-let* ((value (get-charset-property charset property)))
                  (insert (capitalize (symbol-name property))
                          ": "
                          (format "%S" value)
                          "\n"))))))))

(defun vertico-buffer-frame--library-target (candidate)
  "Return a file preview target for Emacs Lisp library CANDIDATE."
  (when-let* ((file (or (locate-library (concat candidate ".el"))
                        (locate-library candidate))))
    (list 'file file)))

(defun vertico-buffer-frame--package-descriptor (package)
  "Return PACKAGE descriptor from installed or archive package metadata."
  (or (cadr (assq package package-alist))
      (cadr (assq package package-archive-contents))))

(defun vertico-buffer-frame--package-requirements-string (requirements)
  "Return a display string for package REQUIREMENTS."
  (mapconcat
   (lambda (requirement)
     (format "%s %s"
             (car requirement)
             (package-version-join (cadr requirement))))
   requirements
   ", "))

(defun vertico-buffer-frame--insert-package-descriptor-preview (descriptor)
  "Insert package preview details from DESCRIPTOR."
  (insert "Version: "
          (package-version-join (package-desc-version descriptor))
          "\n")
  (when-let* ((summary (package-desc-summary descriptor)))
    (insert summary "\n"))
  (insert "Kind: "
          (format "%S" (package-desc-kind descriptor))
          "\n")
  (when-let* ((requirements (package-desc-reqs descriptor)))
    (insert "Requires: "
            (vertico-buffer-frame--package-requirements-string requirements)
            "\n")))

(defun vertico-buffer-frame--insert-package-status-preview (package)
  "Insert installation status preview for PACKAGE."
  (insert "Installed: "
          (if (assq package package-alist) "yes" "no")
          "\n"))

(defun vertico-buffer-frame--insert-package-preview (package descriptor)
  "Insert package preview for PACKAGE using DESCRIPTOR when available."
  (insert (symbol-name package) "\n\n")
  (if descriptor
      (vertico-buffer-frame--insert-package-descriptor-preview descriptor)
    (vertico-buffer-frame--insert-package-status-preview package))
  (insert "Built-in: "
          (if (package-built-in-p package) "yes" "no")
          "\n"))

(defun vertico-buffer-frame--package-target (candidate)
  "Return a text preview target for package CANDIDATE."
  (when (require 'package nil t)
    (when-let* ((package (intern-soft candidate)))
      (let ((descriptor (vertico-buffer-frame--package-descriptor package)))
        (list 'text
              "Package"
              (lambda ()
                (vertico-buffer-frame--insert-package-preview
                 package descriptor)))))))

(defun vertico-buffer-frame--register-target (candidate)
  "Return a text preview target for register CANDIDATE."
  (when (= (length candidate) 1)
    (let* ((register (aref candidate 0))
           (value (get-register register)))
      (when value
        (list 'text
              "Register"
              (lambda ()
                (insert (single-key-description register)
                        "\n\n")
                (pcase value
                  ((pred markerp)
                   (insert "Marker: "
                           (or (buffer-name (marker-buffer value))
                               "no buffer")
                           ":"
                           (number-to-string (marker-position value))
                           "\n"))
                  ((pred window-configuration-p)
                   (insert "Window configuration\n"))
                  ((pred frame-configuration-p)
                   (insert "Frame configuration\n"))
                  (_
                   (insert (format "%S\n" value))))))))))

(defun vertico-buffer-frame--process-target (candidate)
  "Return a text preview target for process CANDIDATE."
  (when-let* ((process (get-process candidate)))
    (list 'text
          "Process"
          (lambda ()
            (insert (process-name process) "\n\n")
            (insert "Status: " (format "%S" (process-status process)) "\n")
            (when-let* ((pid (process-id process)))
              (insert "PID: " (number-to-string pid) "\n"))
            (when-let* ((buffer (process-buffer process)))
              (insert "Buffer: " (buffer-name buffer) "\n"))
            (when-let* ((tty (process-tty-name process)))
              (insert "TTY: " tty "\n"))
            (when-let* ((command (process-command process)))
              (insert "Command: " (mapconcat #'identity command " ") "\n"))
            (when-let* ((contact (ignore-errors
                                   (process-contact process t t))))
              (insert "Contact: " (format "%S" contact) "\n"))))))

(defun vertico-buffer-frame--email-target (candidate)
  "Return a text preview target for email CANDIDATE."
  (list 'text
        "Email"
        (lambda ()
          (insert candidate "\n")
          (when (require 'mail-extr nil t)
            (pcase-let ((`(,name ,address)
                         (mail-extract-address-components candidate)))
              (when name
                (insert "\nName: " name "\n"))
              (when address
                (insert "Address: " address "\n")))))))

(defun vertico-buffer-frame--find-buffer-position (candidate &optional buffers)
  "Return a buffer-position target for the first occurrence of CANDIDATE.
Search BUFFERS, or the minibuffer origin buffer followed by live buffers."
  (let* ((explicit-buffers buffers)
         (buffers (or explicit-buffers
                      (delq nil
                            (delete-dups
                             (cons (vertico-buffer-frame--origin-buffer)
                                   (buffer-list))))))
         (limit (and (not explicit-buffers)
                     (integerp
                      vertico-buffer-frame-preview-search-buffer-limit)
                     (max 0
                          vertico-buffer-frame-preview-search-buffer-limit)))
         (searched 0))
    (catch 'found
      (dolist (buffer buffers)
        (when (and (buffer-live-p buffer)
                   (not (minibufferp buffer))
                   (or (null limit)
                       (and (numberp limit)
                            (< searched limit))))
          (cl-incf searched)
          (with-current-buffer buffer
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (when (search-forward candidate nil t)
                  (throw 'found
                         (vertico-buffer-frame--buffer-position-target
                          buffer
                          (match-beginning 0))))))))))))

(defun vertico-buffer-frame--dabbrev-target (candidate)
  "Return a preview target for dabbrev CANDIDATE."
  (or (vertico-buffer-frame--find-buffer-position candidate)
      (list 'text
            "Dabbrev"
            (lambda ()
              (insert candidate "\n")))))

(defun vertico-buffer-frame--environment-variable-target (candidate)
  "Return a text preview target for environment variable CANDIDATE."
  (when-let* ((value (getenv candidate)))
    (list 'text
          "Environment Variable"
          (lambda ()
            (insert candidate "\n\n" value "\n")))))

(defun vertico-buffer-frame--unicode-name-target (candidate)
  "Return a text preview target for Unicode name CANDIDATE."
  (when-let* ((char (or (char-from-name candidate t)
                        (and (string-match-p "\\`[[:xdigit:]]+\\'" candidate)
                             (ignore-errors
                               (decode-char 'ucs
                                            (string-to-number candidate 16)))))))
    (list 'text
          "Unicode Character"
          (lambda ()
            (insert (format "%c\n\nU+%04X\n%s\n"
                            char
                            char
                            (or (get-char-code-property char 'name)
                                candidate)))))))

(defun vertico-buffer-frame--bookmark-target (candidate)
  "Return a preview target for bookmark CANDIDATE."
  (when (require 'bookmark nil t)
    (when-let* ((bookmark (bookmark-get-bookmark candidate 'noerror)))
      (let* ((file (bookmark-get-filename bookmark))
             (position (bookmark-get-position bookmark))
             (readable-file (and file
                                 (vertico-buffer-frame--readable-file file))))
        (cond
         (readable-file
          (list 'file-position
                readable-file
                position))
         (t
          (list 'text
                "Bookmark"
                (lambda ()
                  (insert candidate "\n\n")
                  (when file
                    (insert "File: " (abbreviate-file-name file) "\n"))
                  (when position
                    (insert "Position: " (number-to-string position) "\n"))))))))))

(defun vertico-buffer-frame--xref-location-target (candidate raw-candidate)
  "Return a preview target for xref CANDIDATE and RAW-CANDIDATE."
  (let* ((group (or (vertico-buffer-frame--text-property-value
                     'xref--group raw-candidate)
                    (and (string-match "\\`\\(.+\\):[0-9]+:" candidate)
                         (match-string 1 candidate))))
         (group-end (vertico-buffer-frame--text-property-end
                     'xref--group raw-candidate))
         (line (cond
                ((and group-end
                      (< group-end (length raw-candidate))
                      (eq (aref raw-candidate group-end) ?:)
                      (string-match "\\`\\([0-9]+\\):"
                                    raw-candidate
                                    (1+ group-end)))
                 (string-to-number (match-string 1 raw-candidate)))
                ((string-match "\\`.+:\\([0-9]+\\):" candidate)
                 (string-to-number (match-string 1 candidate))))))
    (when (and group (> (or line 0) 0))
      (when-let* ((file (or (vertico-buffer-frame--readable-file group)
                            (when-let* ((root (vertico-buffer-frame--project-root)))
                              (vertico-buffer-frame--readable-file
                               (expand-file-name group root))))))
        (list 'file-line file line)))))

(defun vertico-buffer-frame--imenu-entry-position (entry)
  "Return the buffer position described by imenu ENTRY."
  (let ((position (cdr entry)))
    (cond
     ((markerp position)
      position)
     ((integer-or-marker-p position)
      position)
     ((overlayp position)
      (overlay-start position)))))

(defun vertico-buffer-frame--imenu-find-entry (candidate entries)
  "Return the imenu entry for CANDIDATE in ENTRIES."
  (catch 'found
    (dolist (entry entries)
      (cond
       ((and (consp entry)
             (string= candidate (substring-no-properties (car entry)))
             (not (imenu--subalist-p entry)))
        (throw 'found entry))
       ((and (consp entry)
             (imenu--subalist-p entry))
        (when-let* ((match (vertico-buffer-frame--imenu-find-entry
                            candidate
                            (cdr entry))))
          (throw 'found match)))))))

(defun vertico-buffer-frame--imenu-cache-key (buffer)
  "Return the cache key for BUFFER's imenu index."
  (with-current-buffer buffer
    (cons buffer (buffer-chars-modified-tick))))

(defun vertico-buffer-frame--imenu-index (buffer)
  "Return a cached imenu index for BUFFER."
  (let ((key (vertico-buffer-frame--imenu-cache-key buffer)))
    (if (and (consp vertico-buffer-frame--imenu-cache)
             (equal key (car vertico-buffer-frame--imenu-cache)))
        (cdr vertico-buffer-frame--imenu-cache)
      (let ((entries (with-current-buffer buffer
                       (ignore-errors
                         (imenu--make-index-alist)))))
        (setq-local vertico-buffer-frame--imenu-cache
                    (cons key entries))
        entries))))

(defun vertico-buffer-frame--imenu-target (candidate raw-candidate)
  "Return a preview target for imenu CANDIDATE and RAW-CANDIDATE."
  (when (require 'imenu nil t)
    (when-let* ((buffer (vertico-buffer-frame--origin-buffer)))
      (let* ((choice (or (and (stringp raw-candidate)
                              (get-text-property 0 'imenu-choice
                                                 raw-candidate))
                         (vertico-buffer-frame--imenu-find-entry
                          candidate
                          (vertico-buffer-frame--imenu-index buffer))))
             (position (and choice
                            (vertico-buffer-frame--imenu-entry-position
                             choice))))
        (cond
         ((markerp position)
          (vertico-buffer-frame--buffer-position-target
           (or (marker-buffer position) buffer)
           position))
         (position
          (vertico-buffer-frame--buffer-position-target
           buffer
           position)))))))

(defun vertico-buffer-frame--bibtex-target (candidate stringp)
  "Return a preview target for BibTeX CANDIDATE.
When STRINGP is non-nil, look for a @String definition."
  (when-let* ((buffer (vertico-buffer-frame--origin-buffer)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((case-fold-search t)
                (key (regexp-quote candidate)))
            (when (re-search-forward
                   (if stringp
                       (concat "@[[:space:]\n]*string"
                               "[[:space:]\n]*[({][[:space:]\n]*"
                               key
                               "[[:space:]\n]*=")
                     (concat "@[[:alpha:]]+"
                             "[[:space:]\n]*[({][[:space:]\n]*"
                             key
                             "[[:space:]\n]*,"))
                   nil t)
              (vertico-buffer-frame--buffer-position-target
               buffer
               (match-beginning 0)))))))))

(defun vertico-buffer-frame--info-menu-target (candidate)
  "Return a text preview target for Info menu CANDIDATE."
  (when (and (boundp 'Info-complete-menu-buffer)
             (buffer-live-p Info-complete-menu-buffer))
    (list 'text
          "Info Menu"
          (lambda ()
            (insert
             (with-current-buffer Info-complete-menu-buffer
               (save-excursion
                 (goto-char (point-min))
                 (let ((case-fold-search t)
                       (pattern (concat "\n\\* +"
                                        (regexp-quote candidate)
                                        ":")))
                   (if (re-search-forward pattern nil t)
                       (let ((start (line-beginning-position))
                             (end (save-excursion
                                    (forward-line 1)
                                    (while (and (not (eobp))
                                                (not (looking-at-p "\\* +")))
                                      (forward-line 1))
                                    (point))))
                         (buffer-substring-no-properties start end))
                     (concat candidate "\n"))))))))))

(defun vertico-buffer-frame--calendar-month-target (candidate)
  "Return a text preview target for calendar month CANDIDATE."
  (when (require 'calendar nil t)
    (let* ((month-array calendar-month-name-array)
           (month (cdr (assoc-string candidate
                                     (calendar-make-alist month-array 1)
                                     t)))
           (year (calendar-extract-year (calendar-current-date))))
      (when month
        (list 'text
              "Calendar Month"
              (lambda ()
                (calendar-generate-month month year 0)))))))

(defun vertico-buffer-frame--preview-target ()
  "Return the current preview target."
  (when-let* ((raw-candidate (vertico-buffer-frame--candidate))
              (entry (vertico-buffer-frame--candidate-entry
                      raw-candidate
                      (vertico-buffer-frame--category)))
              (category (car entry))
              (candidate (cdr entry)))
    (when (memq category vertico-buffer-frame-preview-categories)
      (or (run-hook-with-args-until-success
           'vertico-buffer-frame-preview-target-functions
           category candidate raw-candidate)
          (pcase category
            ('file
             (vertico-buffer-frame--file-target candidate))
            ('project-file
             (vertico-buffer-frame--project-file-target candidate))
            ('buffer
             (vertico-buffer-frame--buffer-target candidate))
            ('xref-location
             (vertico-buffer-frame--xref-location-target candidate raw-candidate))
            ('bookmark
             (vertico-buffer-frame--bookmark-target candidate))
            ((or 'symbol-help 'command 'function 'variable 'face 'symbol
                 'minor-mode 'customization-group 'custom-variable
                 'apropos-symbol)
             (vertico-buffer-frame--symbol-preview-target candidate))
            ('unicode-name
             (vertico-buffer-frame--unicode-name-target candidate))
            ('environment-variable
             (vertico-buffer-frame--environment-variable-target candidate))
            ('info-menu
             (vertico-buffer-frame--info-menu-target candidate))
            ('calendar-month
             (vertico-buffer-frame--calendar-month-target candidate))
            ('color
             (vertico-buffer-frame--color-target candidate))
            ('custom-theme
             (vertico-buffer-frame--custom-theme-target candidate))
            ('theme
             (vertico-buffer-frame--custom-theme-target candidate))
            ('input-method
             (vertico-buffer-frame--input-method-target candidate))
            ('coding-system
             (vertico-buffer-frame--coding-system-target candidate))
            ('charset
             (vertico-buffer-frame--charset-target candidate))
            ('library
             (vertico-buffer-frame--library-target candidate))
            ('package
             (vertico-buffer-frame--package-target candidate))
            ('register
             (vertico-buffer-frame--register-target candidate))
            ('process
             (vertico-buffer-frame--process-target candidate))
            ((or 'email 'ecomplete)
             (vertico-buffer-frame--email-target candidate))
            ('dabbrev
             (vertico-buffer-frame--dabbrev-target candidate))
            ('imenu
             (vertico-buffer-frame--imenu-target candidate raw-candidate))
            ('bibtex-key
             (vertico-buffer-frame--bibtex-target candidate nil))
            ('bibtex-string
             (vertico-buffer-frame--bibtex-target candidate t))
            ('kill-ring
             (list 'text
                   "Kill Ring"
                   (lambda ()
                     (insert candidate "\n")))))))))

(defun vertico-buffer-frame--preview-parent-frame ()
  "Return the parent frame for the preview child frame."
  (if (frame-live-p vertico-buffer-frame--candidate-frame)
      vertico-buffer-frame--candidate-frame
    (vertico-buffer-frame--parent-frame)))

(defun vertico-buffer-frame--preview-auto-pixel-size (parent)
  "Return automatic preview size in pixels for PARENT."
  (let* ((ratio vertico-buffer-frame-preview--golden-ratio)
         (parent-width (max 1 (frame-pixel-width parent)))
         (parent-height (max 1 (frame-pixel-height parent)))
         (top-offset (max 1 (round (/ parent-height (* ratio ratio)))))
         (width (max 1 (round (/ parent-width ratio))))
         (height (max 1 (- parent-height top-offset))))
    (cons width height)))

(defun vertico-buffer-frame--preview-frame-size (parent)
  "Return preview frame size in characters for PARENT."
  (let* ((auto (vertico-buffer-frame--preview-auto-pixel-size parent))
         (auto-width
          (vertico-buffer-frame--pixels-to-chars
           (car auto)
           (frame-char-width parent)))
         (auto-height
          (vertico-buffer-frame--pixels-to-chars
           (cdr auto)
           (frame-char-height parent)))
         (width-cap (and (integerp vertico-buffer-frame-preview-width)
                         (> vertico-buffer-frame-preview-width 0)
                         vertico-buffer-frame-preview-width))
         (height-cap (and (integerp vertico-buffer-frame-preview-height)
                          (> vertico-buffer-frame-preview-height 0)
                          vertico-buffer-frame-preview-height))
         (width (min auto-width
                     (or width-cap most-positive-fixnum)))
         (height (min auto-height
                      (or height-cap most-positive-fixnum))))
    (cons width height)))

(defun vertico-buffer-frame--refresh-preview-frame ()
  "Resize and reposition the current preview frame, if it is live."
  (when (frame-live-p vertico-buffer-frame--preview-frame)
    (let* ((parent (vertico-buffer-frame--preview-parent-frame))
           (size (vertico-buffer-frame--preview-frame-size parent)))
      (vertico-buffer-frame--resize-frame-to-size
       vertico-buffer-frame--preview-frame size)
      (vertico-buffer-frame--place-preview-frame
       vertico-buffer-frame--preview-frame parent)
      (when (window-live-p vertico-buffer-frame--preview-window)
        (force-window-update
         (window-buffer vertico-buffer-frame--preview-window))))))

(defun vertico-buffer-frame--kill-preview-buffer ()
  "Kill the temporary preview buffer owned by this minibuffer."
  (let ((buffer vertico-buffer-frame--preview-buffer))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (when (eq (cdr-safe vertico-buffer-frame--file-preview-cache) buffer)
      (setq-local vertico-buffer-frame--file-preview-cache nil)))
  (setq-local vertico-buffer-frame--preview-buffer nil))

(defun vertico-buffer-frame--kill-old-preview-buffer (old-buffer new-buffer)
  "Kill OLD-BUFFER when it is live and different from NEW-BUFFER."
  (when (and (buffer-live-p old-buffer)
             (not (eq old-buffer new-buffer)))
    (kill-buffer old-buffer)
    (when (eq (cdr-safe vertico-buffer-frame--file-preview-cache)
              old-buffer)
      (setq-local vertico-buffer-frame--file-preview-cache nil))))

(defun vertico-buffer-frame--text-preview-buffer (name inserter)
  "Return a temporary preview buffer named NAME populated by INSERTER."
  (let ((buffer (generate-new-buffer
                 (format " *vertico-buffer-frame-%s-preview*" name))))
    (setq-local vertico-buffer-frame--preview-buffer buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (setq-local truncate-lines t)
        (funcall inserter))
      (goto-char (point-min))
      (setq buffer-read-only t))
    buffer))

(defun vertico-buffer-frame--insert-directory-preview (directory)
  "Insert a simple preview of DIRECTORY."
  (insert (abbreviate-file-name (file-name-as-directory directory)) "\n\n")
  (let ((count 0)
        (shown 0)
        (limit (and (integerp
                     vertico-buffer-frame-preview-directory-entry-limit)
                    (max 0
                         vertico-buffer-frame-preview-directory-entry-limit))))
    (dolist (entry (directory-files directory nil nil t
                                    (and limit (+ limit 3))))
      (unless (member entry '("." ".."))
        (cl-incf count)
        (when (or (null limit)
                  (and (numberp limit)
                       (< shown limit)))
          (cl-incf shown)
          (insert entry
                  (if (file-directory-p (expand-file-name entry directory))
                      "/"
                    "")
                  "\n"))))
    (when (= count 0)
      (insert "Empty directory\n"))
    (when (and limit
               (> count limit))
      (insert "\nMore entries not shown.\n"))))

(defun vertico-buffer-frame--file-size (file)
  "Return FILE size in bytes, or nil if it cannot be determined."
  (ignore-errors
    (file-attribute-size (file-attributes file 'integer))))

(defun vertico-buffer-frame--file-preview-limit (size)
  "Return the byte limit for a file preview of SIZE bytes."
  (when (integerp vertico-buffer-frame-preview-max-file-size)
    (let ((limit (max 0 vertico-buffer-frame-preview-max-file-size)))
      (if (integerp size)
          (min limit size)
        limit))))

(defun vertico-buffer-frame--binary-file-p (file size)
  "Return non-nil when FILE appears to be binary.
SIZE is the file size in bytes, or nil if it is unknown."
  (let ((limit (min 4096 (or size 4096))))
    (and (> limit 0)
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert-file-contents-literally file nil 0 limit)
           (goto-char (point-min))
           (search-forward (string 0) nil t)))))

(defun vertico-buffer-frame--insert-regular-file-preview (file)
  "Insert a guarded preview of regular FILE."
  (let* ((size (vertico-buffer-frame--file-size file))
         (limit (vertico-buffer-frame--file-preview-limit size)))
    (cond
     ((and limit
           (= limit 0))
      (insert (abbreviate-file-name file) "\n\n")
      (insert "File preview is limited to 0 bytes.\n")
      (when size
        (insert "Size: " (number-to-string size) " bytes\n")))
     ((and (not vertico-buffer-frame-preview-binary-files)
           (vertico-buffer-frame--binary-file-p file size))
      (insert (abbreviate-file-name file) "\n\n")
      (insert "Binary file preview skipped.\n")
      (when size
        (insert "Size: " (number-to-string size) " bytes\n")))
     (t
      (insert-file-contents file nil 0 limit)
      (when (and size
                 limit
                 (< limit size))
        (goto-char (point-max))
        (insert "\n\nPreview truncated at "
                (number-to-string limit)
                " of "
                (number-to-string size)
                " bytes.\n"))))))

(defun vertico-buffer-frame--file-preview-cache-key (file)
  "Return a cache key for FILE preview content."
  (when-let* ((attributes (ignore-errors
                            (file-attributes file 'integer))))
    (list file
          (car attributes)
          (nth 5 attributes)
          (nth 7 attributes)
          vertico-buffer-frame-preview-max-file-size
          vertico-buffer-frame-preview-binary-files
          vertico-buffer-frame-preview-directory-entry-limit)))

(defun vertico-buffer-frame--cached-file-preview-buffer (key)
  "Return the cached file preview buffer for KEY, if it is still valid."
  (when (and key
             (consp vertico-buffer-frame--file-preview-cache)
             (equal key (car vertico-buffer-frame--file-preview-cache)))
    (let ((buffer (cdr vertico-buffer-frame--file-preview-cache)))
      (when (buffer-live-p buffer)
        (setq-local vertico-buffer-frame--preview-buffer buffer)
        buffer))))

(defun vertico-buffer-frame--file-preview-buffer (file)
  "Return a temporary preview buffer for FILE."
  (let ((key (vertico-buffer-frame--file-preview-cache-key file)))
    (or (vertico-buffer-frame--cached-file-preview-buffer key)
        (let ((buffer
               (vertico-buffer-frame--text-preview-buffer
                "file"
                (lambda ()
                  (cond
                   ((file-directory-p file)
                    (vertico-buffer-frame--insert-directory-preview file))
                   ((file-regular-p file)
                    (vertico-buffer-frame--insert-regular-file-preview file))
                   (t
                    (insert (abbreviate-file-name file) "\n")))))))
          (when key
            (setq-local vertico-buffer-frame--file-preview-cache
                        (cons key buffer)))
          buffer))))

(defun vertico-buffer-frame--insert-symbol-preview (symbol)
  "Insert a preview for SYMBOL."
  (insert (symbol-name symbol) "\n\n")
  (let ((content-start (point)))
    (when (commandp symbol)
      (insert "Command")
      (when-let* ((keys (where-is-internal symbol nil t)))
        (insert ": " (key-description keys)))
      (insert "\n\n"))
    (when (fboundp symbol)
      (when-let* ((doc (ignore-errors (documentation symbol t))))
        (insert doc "\n\n")))
    (when (boundp symbol)
      (when-let* ((doc (documentation-property symbol 'variable-documentation t)))
        (insert doc "\n\n")))
    (when (facep symbol)
      (when-let* ((doc (documentation-property symbol 'face-documentation t)))
        (insert doc "\n\n")))
    (when (= (point) content-start)
      (insert "No documentation available.\n"))))

(defun vertico-buffer-frame--preview-window ()
  "Return the preview window, creating a child frame when needed."
  (unless (and (frame-live-p vertico-buffer-frame--preview-frame)
               (window-live-p vertico-buffer-frame--preview-window))
    (vertico-buffer-frame--delete-frame vertico-buffer-frame--preview-frame)
    (vertico-buffer-frame--kill-preview-buffer)
    (setq-local vertico-buffer-frame--preview-frame nil
                vertico-buffer-frame--preview-window nil)
    (let* ((parent (vertico-buffer-frame--preview-parent-frame))
           (size (vertico-buffer-frame--preview-frame-size parent))
           (frame (vertico-buffer-frame--make-child-frame
                   parent
                   (format "Vertico Preview %s" (minibuffer-depth))
                   (car size)
                   (cdr size)))
           (window (frame-root-window frame)))
      (set-window-parameter window 'no-other-window t)
      (set-window-parameter window 'no-delete-other-windows t)
      (vertico-buffer-frame--prepare-window window)
      (setq-local vertico-buffer-frame--preview-frame frame
                  vertico-buffer-frame--preview-window window)
      (vertico-buffer-frame--place-preview-frame frame parent)))
  (vertico-buffer-frame--refresh-preview-frame)
  vertico-buffer-frame--preview-window)

(defun vertico-buffer-frame--set-preview-window-buffer
    (window buffer &optional old-preview-buffer)
  "Display BUFFER in preview WINDOW and dedicate it afterwards.
When OLD-PREVIEW-BUFFER is live, kill it after BUFFER has replaced it in
WINDOW."
  (set-window-dedicated-p window nil)
  (set-window-buffer window buffer)
  (set-window-dedicated-p window t)
  (vertico-buffer-frame--kill-old-preview-buffer old-preview-buffer buffer))

(defun vertico-buffer-frame--center-window-point (window)
  "Set WINDOW point to point and center it vertically when possible."
  (let ((point (point)))
    (set-window-point window point)
    (save-excursion
      (forward-line (- (/ (max 1 (window-body-height window)) 2)))
      (set-window-start window (line-beginning-position)))))

(defun vertico-buffer-frame--set-window-position (window position)
  "Set WINDOW point to POSITION."
  (when position
    (with-current-buffer (window-buffer window)
      (goto-char (point-min))
      (cond
       ((markerp position)
        (goto-char position))
       ((integerp position)
        (goto-char (max (point-min) (min (point-max) position)))))
      (vertico-buffer-frame--center-window-point window))))

(defun vertico-buffer-frame--set-window-line (window line)
  "Set WINDOW point to LINE."
  (when (and (integerp line) (> line 0))
    (with-current-buffer (window-buffer window)
      (goto-char (point-min))
      (forward-line (1- line))
      (vertico-buffer-frame--center-window-point window))))

(defun vertico-buffer-frame--show-preview (target)
  "Show preview TARGET."
  (pcase target
    (`(file ,file)
     (let ((old-preview-buffer vertico-buffer-frame--preview-buffer)
           (window (vertico-buffer-frame--preview-window)))
       (vertico-buffer-frame--set-preview-window-buffer
        window
        (vertico-buffer-frame--file-preview-buffer file)
        old-preview-buffer)
       (vertico-buffer-frame--show-frame
        vertico-buffer-frame--preview-frame)))
    (`(file-line ,file ,line)
     (let ((old-preview-buffer vertico-buffer-frame--preview-buffer)
           (window (vertico-buffer-frame--preview-window)))
       (vertico-buffer-frame--set-preview-window-buffer
        window
        (vertico-buffer-frame--file-preview-buffer file)
        old-preview-buffer)
       (vertico-buffer-frame--set-window-line window line)
       (vertico-buffer-frame--show-frame
        vertico-buffer-frame--preview-frame)))
    (`(file-position ,file ,position)
     (let ((old-preview-buffer vertico-buffer-frame--preview-buffer)
           (window (vertico-buffer-frame--preview-window)))
       (vertico-buffer-frame--set-preview-window-buffer
        window
        (vertico-buffer-frame--file-preview-buffer file)
        old-preview-buffer)
       (vertico-buffer-frame--set-window-position window position)
       (vertico-buffer-frame--show-frame
        vertico-buffer-frame--preview-frame)))
    (`(buffer ,name)
     (let ((old-preview-buffer vertico-buffer-frame--preview-buffer)
           (window (vertico-buffer-frame--preview-window)))
       (vertico-buffer-frame--set-preview-window-buffer
        window
        (get-buffer name)
        old-preview-buffer)
       (setq-local vertico-buffer-frame--preview-buffer nil)
       (vertico-buffer-frame--show-frame
        vertico-buffer-frame--preview-frame)))
    (`(buffer-position ,buffer ,position)
     (let ((old-preview-buffer vertico-buffer-frame--preview-buffer)
           (window (vertico-buffer-frame--preview-window)))
       (vertico-buffer-frame--set-preview-window-buffer
        window
        buffer
        old-preview-buffer)
       (setq-local vertico-buffer-frame--preview-buffer nil)
       (vertico-buffer-frame--set-window-position window position)
       (vertico-buffer-frame--show-frame
        vertico-buffer-frame--preview-frame)))
    (`(text ,name ,inserter)
     (let ((old-preview-buffer vertico-buffer-frame--preview-buffer)
           (window (vertico-buffer-frame--preview-window)))
       (vertico-buffer-frame--set-preview-window-buffer
        window
        (vertico-buffer-frame--text-preview-buffer name inserter)
        old-preview-buffer)
       (vertico-buffer-frame--show-frame
        vertico-buffer-frame--preview-frame)))))

(defun vertico-buffer-frame--report-preview-error (error)
  "Hide the preview and optionally report ERROR."
  (vertico-buffer-frame--hide-preview)
  (when vertico-buffer-frame-preview-report-errors
    (let ((message (error-message-string error)))
      (unless (equal message
                     vertico-buffer-frame--preview-last-error-message)
        (setq-local vertico-buffer-frame--preview-last-error-message message)
        (message "vertico-buffer-frame preview error: %s" message)))))

(defun vertico-buffer-frame--cancel-preview-timer ()
  "Cancel the pending preview timer for the current minibuffer buffer."
  (when (timerp vertico-buffer-frame--preview-timer)
    (cancel-timer vertico-buffer-frame--preview-timer))
  (setq-local vertico-buffer-frame--preview-timer nil
              vertico-buffer-frame--preview-scheduled-state nil))

(defun vertico-buffer-frame--preview-state ()
  "Return state used to coalesce pending preview refresh timers."
  (list vertico--input
        (vertico-buffer-frame--category)
        (vertico-buffer-frame--candidate)))

(defun vertico-buffer-frame--hide-preview ()
  "Hide the preview child frame."
  (vertico-buffer-frame--cancel-preview-timer)
  (vertico-buffer-frame--delete-frame vertico-buffer-frame--preview-frame)
  (vertico-buffer-frame--kill-preview-buffer)
  (setq-local vertico-buffer-frame--preview-frame nil
              vertico-buffer-frame--preview-window nil
              vertico-buffer-frame--preview-scheduled-state nil))

(defun vertico-buffer-frame--show-preview-later (buffer)
  "Show preview for minibuffer BUFFER after the configured delay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local vertico-buffer-frame--preview-timer nil
                  vertico-buffer-frame--preview-scheduled-state nil)
      (condition-case-unless-debug error
          (if (and vertico-buffer-frame-mode
                   vertico-buffer-frame-preview
                   (bound-and-true-p vertico--input))
              (if-let* ((target (vertico-buffer-frame--preview-target)))
                  (progn
                    (vertico-buffer-frame--show-preview target)
                    (setq-local
                     vertico-buffer-frame--preview-last-error-message nil))
                (vertico-buffer-frame--hide-preview))
            (vertico-buffer-frame--hide-preview))
        (error
         (vertico-buffer-frame--report-preview-error error))))))

(defun vertico-buffer-frame--schedule-preview (&optional state)
  "Schedule preview display for the current minibuffer buffer."
  (unless (and state
               (timerp vertico-buffer-frame--preview-timer)
               (equal state vertico-buffer-frame--preview-scheduled-state))
    (vertico-buffer-frame--cancel-preview-timer)
    (setq-local vertico-buffer-frame--preview-scheduled-state state
                vertico-buffer-frame--preview-timer
                (run-with-idle-timer
                 (max 0 (float vertico-buffer-frame-preview-delay))
                 nil
                 #'vertico-buffer-frame--show-preview-later
                 (current-buffer)))))

(defun vertico-buffer-frame--preview-post-command ()
  "Schedule a delayed preview refresh after Vertico candidate refresh."
  (if (and vertico-buffer-frame-mode
           vertico-buffer-frame-preview
           (bound-and-true-p vertico--input))
      (vertico-buffer-frame--schedule-preview
       (vertico-buffer-frame--preview-state))
    (vertico-buffer-frame--hide-preview)))

(provide 'vertico-buffer-frame-preview)
;;; vertico-buffer-frame-preview.el ends here
