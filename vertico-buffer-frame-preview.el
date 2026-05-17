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
(defvar Info-complete-menu-buffer)
(defvar calendar-month-name-array)
(defvar consult-imenu-config)
(defvar custom-enabled-themes)
(defvar imenu-create-index-function)
(defvar input-method-alist)
(defvar package-alist)
(defvar package-archive-contents)

(defvar-local vertico-buffer-frame--preview-frame nil)
(defvar-local vertico-buffer-frame--preview-window nil)
(defvar-local vertico-buffer-frame--preview-layout-state nil)
(defvar-local vertico-buffer-frame--preview-buffer nil)
(defvar-local vertico-buffer-frame--preview-timer nil)
(defvar-local vertico-buffer-frame--preview-last-error-message nil)
(defvar-local vertico-buffer-frame--preview-scheduled-state nil)
(defvar-local vertico-buffer-frame--project-root-cache nil)
(defvar-local vertico-buffer-frame--file-preview-cache nil)
(defvar-local vertico-buffer-frame--imenu-cache nil)
(defvar-local vertico-buffer-frame--temporary-preview-buffer nil)

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
(declare-function locate-library "files")
(declare-function mail-extract-address-components "mail-extr")
(declare-function package-desc-kind "package")
(declare-function package-desc-reqs "package")
(declare-function package-desc-summary "package")
(declare-function package-desc-version "package")
(declare-function package-built-in-p "package")
(declare-function package-version-join "package")
(declare-function project-root "project")
(declare-function vertico-buffer-frame--obtain-child-frame
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--parent-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--release-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--frame-layout-state
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--pixels-to-chars "vertico-buffer-frame")
(declare-function vertico-buffer-frame--place-preview-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--prepare-window "vertico-buffer-frame")
(declare-function vertico-buffer-frame--resize-frame-to-size
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--show-frame "vertico-buffer-frame")
(declare-function vertico--candidate "vertico")

(defun vertico-buffer-frame--completion-active-p ()
  "Return non-nil when the current buffer is an active completion minibuffer."
  (and (minibufferp)
       (boundp 'minibuffer-completion-table)
       minibuffer-completion-table))

(defun vertico-buffer-frame--minibuffer-input ()
  "Return current minibuffer input without text properties, if available."
  (when (minibufferp)
    (minibuffer-contents-no-properties)))

(defun vertico-buffer-frame--category ()
  "Return the current completion category."
  (when (vertico-buffer-frame--completion-active-p)
    (ignore-errors
      (completion-metadata-get
       (completion-metadata
        (or (vertico-buffer-frame--minibuffer-input) "")
        minibuffer-completion-table
        minibuffer-completion-predicate)
       'category))))

(defun vertico-buffer-frame--candidate ()
  "Return the current raw Vertico candidate."
  (when (and (vertico-buffer-frame--completion-active-p)
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

(defun vertico-buffer-frame--remote-file-name-p (file directory)
  "Return non-nil when FILE names a remote path under DIRECTORY."
  (or (file-remote-p file)
      (and (not (file-name-absolute-p file))
           (file-remote-p directory))))

(defun vertico-buffer-frame--readable-file (file &optional directory)
  "Return expanded FILE under DIRECTORY when it is readable."
  (when (stringp file)
    (let ((file (substitute-in-file-name file))
          (directory (or directory default-directory)))
      (unless (and (not vertico-buffer-frame-preview-remote-files)
                   (vertico-buffer-frame--remote-file-name-p file directory))
        (let ((expanded (expand-file-name file directory)))
          (and (or vertico-buffer-frame-preview-remote-files
                   (not (file-remote-p expanded)))
               (file-readable-p expanded)
               expanded))))))

(defun vertico-buffer-frame--file-target (candidate &optional directory)
  "Return a file preview target for CANDIDATE under DIRECTORY."
  (when (stringp candidate)
    (when-let* ((file (vertico-buffer-frame--readable-file
                       candidate directory)))
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

(defun vertico-buffer-frame--xref-readable-file (file)
  "Return readable xref FILE, optionally relative to the project root."
  (or (vertico-buffer-frame--readable-file file)
      (when-let* ((root (vertico-buffer-frame--project-root)))
        (vertico-buffer-frame--readable-file file root))))

(defun vertico-buffer-frame--xref-file-name-candidates (prefix)
  "Return possible file names from an xref candidate PREFIX."
  (let ((prefix (string-trim prefix))
        candidates)
    (unless (string-empty-p prefix)
      (push prefix candidates)
      (let ((start 0))
        (while (string-match "[[:space:]]+" prefix start)
          (let ((suffix (string-trim-left
                         (substring prefix (match-end 0)))))
            (unless (string-empty-p suffix)
              (push suffix candidates)))
          (setq start (match-end 0)))))
    (nreverse (delete-dups candidates))))

(defun vertico-buffer-frame--xref-location-from-candidate (candidate)
  "Return (FILE . LINE) parsed from xref CANDIDATE."
  (when (stringp candidate)
    (let ((candidate (substring-no-properties candidate))
          (start 0)
          location)
      (while (and (not location)
                  (string-match ":\\([0-9]+\\):" candidate start))
        (let ((line (string-to-number (match-string 1 candidate)))
              (prefix (substring candidate 0 (match-beginning 0))))
          (when-let* ((file (cl-some
                             #'vertico-buffer-frame--xref-readable-file
                             (vertico-buffer-frame--xref-file-name-candidates
                              prefix))))
            (setq location (cons file line))))
        (setq start (match-end 0)))
      location)))

(defun vertico-buffer-frame--xref-location-target (candidate _raw-candidate)
  "Return a preview target for xref CANDIDATE."
  (when-let* ((location
               (vertico-buffer-frame--xref-location-from-candidate candidate)))
    (list 'file-line (car location) (cdr location))))

(defun vertico-buffer-frame--imenu-entry-position (entry)
  "Return the buffer position described by imenu ENTRY."
  (let ((position (cdr entry)))
    (cond
     ((markerp position)
      position)
     ((integer-or-marker-p position)
      position)
     ((overlayp position)
      (overlay-start position))
     ((consp position)
      (let ((position (car position)))
        (cond
         ((markerp position)
          position)
         ((integer-or-marker-p position)
          position)
         ((overlayp position)
          (overlay-start position))))))))

(defun vertico-buffer-frame--imenu-subalist-p (entry)
  "Return non-nil when imenu ENTRY is a nested sub-alist."
  (and (consp entry)
       (consp (cdr entry))
       (listp (cadr entry))
       (not (functionp (cadr entry)))))

(defun vertico-buffer-frame--imenu-index-entries ()
  "Return imenu index entries for the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (funcall imenu-create-index-function))))

(defun vertico-buffer-frame--imenu-find-entry (candidate entries)
  "Return the imenu entry for CANDIDATE in ENTRIES."
  (catch 'found
    (dolist (entry entries)
      (cond
       ((and (consp entry)
             (string= candidate (substring-no-properties (car entry)))
             (not (vertico-buffer-frame--imenu-subalist-p entry)))
        (throw 'found entry))
       ((and (consp entry)
             (vertico-buffer-frame--imenu-subalist-p entry))
        (when-let* ((match (vertico-buffer-frame--imenu-find-entry
                            candidate
                            (cdr entry))))
          (throw 'found match)))))))

(defun vertico-buffer-frame--imenu-entry-name (entry)
  "Return the display name for imenu ENTRY."
  (when (and (consp entry)
             (stringp (car entry))
             (not (vertico-buffer-frame--imenu-subalist-p entry)))
    (substring-no-properties (car entry))))

(defun vertico-buffer-frame--imenu-entry-table (entries)
  "Return a hash table mapping candidate names to imenu ENTRIES."
  (let ((table (make-hash-table :test #'equal)))
    (cl-labels
        ((walk (items)
           (dolist (entry items)
             (when (consp entry)
               (if (vertico-buffer-frame--imenu-subalist-p entry)
                   (walk (cdr entry))
                 (when-let* ((name (vertico-buffer-frame--imenu-entry-name
                                    entry)))
                   (unless (gethash name table)
                     (puthash name entry table))))))))
      (walk entries))
    table))

(defun vertico-buffer-frame--consult-imenu-config ()
  "Return Consult imenu configuration for the current buffer."
  (when (boundp 'consult-imenu-config)
    (cdr (cl-find-if (lambda (entry)
                       (derived-mode-p (car entry)))
                     consult-imenu-config))))

(defun vertico-buffer-frame--consult-imenu-items (entries)
  "Return ENTRIES arranged like Consult imenu candidates."
  (if-let* ((toplevel
             (plist-get (vertico-buffer-frame--consult-imenu-config)
                        :toplevel)))
      (let ((tops (cl-remove-if (lambda (entry)
                                  (listp (cdr entry)))
                                entries))
            (rest (cl-remove-if-not (lambda (entry)
                                      (listp (cdr entry)))
                                    entries)))
        (nconc rest
               (and tops
                    (list (cons toplevel tops)))))
    entries))

(defun vertico-buffer-frame--consult-imenu-flatten (prefix entries)
  "Return Consult-style (NAME . ENTRY) pairs for PREFIX and imenu ENTRIES."
  (mapcan
   (lambda (entry)
     (when (consp entry)
       (let ((name (and (stringp (car entry))
                        (substring-no-properties (car entry)))))
         (cond
          ((and name
                (vertico-buffer-frame--imenu-subalist-p entry))
           (vertico-buffer-frame--consult-imenu-flatten
            (if prefix
                (concat prefix "/" name)
              name)
            (cdr entry)))
          (name
           (list (cons (if prefix
                           (concat prefix " " name)
                         name)
                       entry)))))))
   entries))

(defun vertico-buffer-frame--consult-imenu-entry-table (entries)
  "Return a hash table mapping Consult imenu names to imenu ENTRIES."
  (let ((table (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal)))
    (dolist (pair (vertico-buffer-frame--consult-imenu-flatten
                   nil
                   (vertico-buffer-frame--consult-imenu-items entries)))
      (let* ((name (car pair))
             (count (gethash name counts)))
        (if count
            (setq name (format "%s (%s)"
                               name
                               (puthash name (1+ count) counts)))
          (puthash name 0 counts))
        (unless (gethash name table)
          (puthash name (cdr pair) table))))
    table))

(defun vertico-buffer-frame--imenu-cache-key (buffer)
  "Return the cache key for BUFFER's imenu index."
  (with-current-buffer buffer
    (cons buffer (buffer-chars-modified-tick))))

(defun vertico-buffer-frame--imenu-cache-value (buffer)
  "Return cached imenu index details for BUFFER."
  (let ((key (vertico-buffer-frame--imenu-cache-key buffer)))
    (if (and (consp vertico-buffer-frame--imenu-cache)
             (equal key (car vertico-buffer-frame--imenu-cache)))
        (cdr vertico-buffer-frame--imenu-cache)
      (let ((entries (with-current-buffer buffer
                       (ignore-errors
                         (vertico-buffer-frame--imenu-index-entries)))))
        (setq-local vertico-buffer-frame--imenu-cache
                    (cons key
                          (cons entries
                                (vertico-buffer-frame--imenu-entry-table
                                 entries))))
        (cdr vertico-buffer-frame--imenu-cache)))))

(defun vertico-buffer-frame--imenu-index (buffer)
  "Return a cached imenu index for BUFFER."
  (car (vertico-buffer-frame--imenu-cache-value buffer)))

(defun vertico-buffer-frame--imenu-target (candidate raw-candidate)
  "Return a preview target for imenu CANDIDATE and RAW-CANDIDATE."
  (when (require 'imenu nil t)
    (when-let* ((buffer (vertico-buffer-frame--origin-buffer)))
      (let* ((cache (vertico-buffer-frame--imenu-cache-value buffer))
             (choice (or (and (stringp raw-candidate)
              (get-text-property 0 'imenu-choice
                                 raw-candidate))
                         (gethash candidate (cdr cache))
                         (gethash
                          candidate
                          (vertico-buffer-frame--consult-imenu-entry-table
                           (car cache)))
                         (vertico-buffer-frame--imenu-find-entry
                          candidate
                          (car cache))))
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

(defun vertico-buffer-frame--preview-target (&optional raw-candidate category)
  "Return the current preview target.
When RAW-CANDIDATE or CATEGORY are non-nil, use them instead of reading the
current minibuffer state."
  (when-let* ((raw-candidate (or raw-candidate
                                 (vertico-buffer-frame--candidate)))
              (entry (vertico-buffer-frame--candidate-entry
                      raw-candidate
                      (or category
                          (vertico-buffer-frame--category))))
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
           (size (vertico-buffer-frame--preview-frame-size parent))
           (state (vertico-buffer-frame--frame-layout-state
                   vertico-buffer-frame--preview-frame parent size)))
      (unless (equal state vertico-buffer-frame--preview-layout-state)
        (vertico-buffer-frame--resize-frame-to-size
         vertico-buffer-frame--preview-frame size)
        (vertico-buffer-frame--place-preview-frame
         vertico-buffer-frame--preview-frame parent)
        (setq-local vertico-buffer-frame--preview-layout-state
                    (vertico-buffer-frame--frame-layout-state
                     vertico-buffer-frame--preview-frame parent size)))
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
  (let ((buffer (if (and (buffer-live-p
                          vertico-buffer-frame--preview-buffer)
                         (buffer-local-value
                          'vertico-buffer-frame--temporary-preview-buffer
                          vertico-buffer-frame--preview-buffer)
                         (not (eq (cdr-safe
                                   vertico-buffer-frame--file-preview-cache)
                                  vertico-buffer-frame--preview-buffer)))
                    vertico-buffer-frame--preview-buffer
                  (generate-new-buffer
                   (format " *vertico-buffer-frame-%s-preview*" name)))))
    (setq-local vertico-buffer-frame--preview-buffer buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (setq-local truncate-lines t
                    vertico-buffer-frame--temporary-preview-buffer t)
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
    (vertico-buffer-frame--release-frame
     'preview vertico-buffer-frame--preview-frame)
    (vertico-buffer-frame--kill-preview-buffer)
    (setq-local vertico-buffer-frame--preview-frame nil
                vertico-buffer-frame--preview-window nil)
    (let* ((parent (vertico-buffer-frame--preview-parent-frame))
           (size (vertico-buffer-frame--preview-frame-size parent))
           (frame (vertico-buffer-frame--obtain-child-frame
                   'preview
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
      (vertico-buffer-frame--place-preview-frame frame parent)
      (setq-local vertico-buffer-frame--preview-layout-state
                  (vertico-buffer-frame--frame-layout-state
                   frame parent size))))
  (vertico-buffer-frame--refresh-preview-frame)
  vertico-buffer-frame--preview-window)

(defun vertico-buffer-frame--set-preview-window-buffer
    (window buffer &optional old-preview-buffer)
  "Display BUFFER in preview WINDOW and dedicate it afterwards.
When OLD-PREVIEW-BUFFER is live, kill it after BUFFER has replaced it in
WINDOW."
  (if (eq (window-buffer window) buffer)
      (unless (window-dedicated-p window)
        (set-window-dedicated-p window t))
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t))
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
  (list (vertico-buffer-frame--minibuffer-input)
        (point)
        (vertico-buffer-frame--category)
        (vertico-buffer-frame--candidate)))

(defun vertico-buffer-frame--hide-preview ()
  "Hide the preview child frame."
  (vertico-buffer-frame--cancel-preview-timer)
  (vertico-buffer-frame--release-frame
   'preview vertico-buffer-frame--preview-frame)
  (vertico-buffer-frame--kill-preview-buffer)
  (setq-local vertico-buffer-frame--preview-frame nil
              vertico-buffer-frame--preview-window nil
              vertico-buffer-frame--preview-layout-state nil
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
                   (vertico-buffer-frame--completion-active-p))
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
  "Schedule preview display for the current minibuffer buffer.
When STATE matches the pending timer state, keep the existing timer."
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
           (vertico-buffer-frame--completion-active-p))
      (vertico-buffer-frame--schedule-preview
       (vertico-buffer-frame--preview-state))
    (vertico-buffer-frame--hide-preview)))

(provide 'vertico-buffer-frame-preview)
;;; vertico-buffer-frame-preview.el ends here
