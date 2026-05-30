;;; vertico-buffer-frame-preview.el --- Preview support for vertico-buffer-frame -*- lexical-binding: t; -*-

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

;; Category-aware preview support for `vertico-buffer-frame'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vertico)

(eval-when-compile
  (require 'package))

(defgroup vertico-buffer-frame nil
  "Display `vertico-buffer-mode' windows in child frames."
  :group 'vertico
  :prefix "vertico-buffer-frame-")

(defcustom vertico-buffer-frame-preview t
  "Non-nil means preview the current candidate."
  :type 'boolean)

(defcustom vertico-buffer-frame-preview-delay 0.2
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

(defconst vertico-buffer-frame--default-preview-max-file-size 1048576
  "Default maximum bytes to read from a regular file preview.")

(defcustom vertico-buffer-frame-preview-max-file-size
  vertico-buffer-frame--default-preview-max-file-size
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

(defconst vertico-buffer-frame--default-preview-directory-entry-limit 200
  "Default maximum number of directory entries shown in a preview.")

(defcustom vertico-buffer-frame-preview-directory-entry-limit
  vertico-buffer-frame--default-preview-directory-entry-limit
  "Maximum number of directory entries shown in a preview.
When nil, show all entries."
  :type '(choice (const :tag "Unlimited" nil) natnum))

(defconst vertico-buffer-frame--default-preview-search-buffer-limit 20
  "Default maximum number of live buffers to scan for text previews.")

(defcustom vertico-buffer-frame-preview-search-buffer-limit
  vertico-buffer-frame--default-preview-search-buffer-limit
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

(defcustom vertico-buffer-frame-preview-target-functions nil
  "Hook functions for additional preview targets.
Each function is called with CATEGORY, CANDIDATE and RAW-CANDIDATE.
It should return nil if it cannot handle the candidate, or a target list of
one of these forms:

  (file FILE)
  (file-line FILE LINE)
  (file-position FILE POSITION)
  (buffer BUFFER-NAME)
  (buffer-line BUFFER LINE)
  (buffer-position BUFFER POSITION)
  (text NAME INSERTER)

FILE and BUFFER-NAME are strings.  BUFFER is a live buffer.  LINE is a
one-based line number.  POSITION is an integer or marker.  INSERTER is a
function called in a temporary preview buffer."
  :type 'hook)

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
(defvar-local vertico-buffer-frame--preview-last-target-error-message nil)
(defvar-local vertico-buffer-frame--preview-scheduled-state nil)
(defvar-local vertico-buffer-frame--project-root-cache nil)
(defvar-local vertico-buffer-frame--file-preview-cache nil)
(defvar-local vertico-buffer-frame--imenu-cache nil)
(defvar-local vertico-buffer-frame--consult-imenu-entry-table-cache nil)
(defvar-local vertico-buffer-frame--temporary-preview-buffer nil)
(defvar-local vertico-buffer-frame--preview-owner-buffer nil)

;;;; Preview state

(defun vertico-buffer-frame--clear-preview-frame-state ()
  "Clear buffer-local preview child-frame and window state."
  (setq-local vertico-buffer-frame--preview-frame nil
              vertico-buffer-frame--preview-window nil
              vertico-buffer-frame--preview-layout-state nil))

(defun vertico-buffer-frame--clear-preview-target-state ()
  "Clear buffer-local preview diagnostics and target caches."
  (setq-local vertico-buffer-frame--preview-last-error-message nil
              vertico-buffer-frame--preview-last-target-error-message nil
              vertico-buffer-frame--preview-scheduled-state nil
              vertico-buffer-frame--file-preview-cache nil
              vertico-buffer-frame--imenu-cache nil
              vertico-buffer-frame--consult-imenu-entry-table-cache nil))

(defun vertico-buffer-frame--clear-preview-session-state ()
  "Clear buffer-local preview state that is scoped to one minibuffer session."
  (setq-local vertico-buffer-frame--preview-buffer nil
              vertico-buffer-frame--preview-timer nil
              vertico-buffer-frame--project-root-cache nil))

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
(declare-function charset-description "mule")
(declare-function color-defined-p "faces")
(declare-function color-values "faces")
(declare-function custom-available-themes "custom")
(declare-function custom-theme-p "custom")
(declare-function get-charset-property "mule")
(declare-function locate-library "subr")
(declare-function mail-extract-address-components "mail-extr")
(declare-function project-root "project")
(declare-function vertico-buffer-frame--child-frames-supported-p
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--delete-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--display-buffer-in-child-frame
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--frame-parameter
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--parent-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--frame-layout-state
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--hide-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--pixels-to-chars "vertico-buffer-frame")
(declare-function vertico-buffer-frame--place-preview-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--preview-reference-geometry
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--resize-frame-to-size
                  "vertico-buffer-frame")
(declare-function vertico-buffer-frame--show-frame "vertico-buffer-frame")
(declare-function vertico-buffer-frame--window-frame "vertico-buffer-frame")
(declare-function vertico--candidate "vertico")

;;;; Completion state

(defun vertico-buffer-frame--completion-active-p ()
  "Return non-nil when the current buffer is an active completion minibuffer."
  (condition-case-unless-debug nil
      (and (minibufferp)
           (boundp 'minibuffer-completion-table)
           minibuffer-completion-table)
    (error nil)))

(defun vertico-buffer-frame--minibuffer-input ()
  "Return current minibuffer input without text properties, if available."
  (condition-case-unless-debug nil
      (when (minibufferp)
        (minibuffer-contents-no-properties))
    (error nil)))

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
  (condition-case-unless-debug nil
      (when (and (stringp string)
                 (> (length string) 0))
        (let ((position (text-property-not-all 0 (length string)
                                               property nil string)))
          (and position
               (get-text-property position property string))))
    (error nil)))

(defun vertico-buffer-frame--limit-value (value fallback)
  "Return VALUE as a nonnegative limit, preserving nil as unlimited.
Use FALLBACK when VALUE is not an integer or nil."
  (cond
   ((null value)
    nil)
   ((integerp value)
    (max 0 value))
   (t fallback)))

(defun vertico-buffer-frame--proper-list (value)
  "Return VALUE when it is a proper list, otherwise nil."
  (and (proper-list-p value)
       value))

(defun vertico-buffer-frame--proper-list-prefix (value)
  "Return the proper list prefix of malformed list VALUE."
  (let ((prefix nil)
        (tail value)
        (seen nil))
    (while (and (consp tail)
                (not (memq tail seen)))
      (push tail seen)
      (push (car tail) prefix)
      (setq tail (cdr tail)))
    (nreverse prefix)))

(defun vertico-buffer-frame--normalize-hook-variable (hook &optional local)
  "Normalize malformed list value of HOOK before hook operations.
When LOCAL is non-nil, normalize the buffer-local value in the current buffer."
  (let ((value (and (boundp hook)
                    (if local
                        (buffer-local-value hook (current-buffer))
                      (symbol-value hook)))))
    (when (and (consp value)
               (not (proper-list-p value)))
      (if local
          (set (make-local-variable hook)
               (vertico-buffer-frame--proper-list-prefix value))
        (set hook
             (vertico-buffer-frame--proper-list-prefix value))))))

(defun vertico-buffer-frame--add-hook (hook function &optional depth local)
  "Add FUNCTION to HOOK after normalizing malformed hook storage.
DEPTH and LOCAL are passed through to `add-hook'."
  (vertico-buffer-frame--normalize-hook-variable hook local)
  (add-hook hook function depth local))

(defun vertico-buffer-frame--remove-hook (hook function &optional local)
  "Remove FUNCTION from HOOK after normalizing malformed hook storage.
LOCAL is passed through to `remove-hook'."
  (vertico-buffer-frame--normalize-hook-variable hook local)
  (remove-hook hook function local))

(defun vertico-buffer-frame--preview-max-file-size ()
  "Return the effective maximum bytes for file previews."
  (vertico-buffer-frame--limit-value
   vertico-buffer-frame-preview-max-file-size
   vertico-buffer-frame--default-preview-max-file-size))

(defun vertico-buffer-frame--preview-directory-entry-limit ()
  "Return the effective directory preview entry limit."
  (vertico-buffer-frame--limit-value
   vertico-buffer-frame-preview-directory-entry-limit
   vertico-buffer-frame--default-preview-directory-entry-limit))

(defun vertico-buffer-frame--preview-search-buffer-limit ()
  "Return the effective text preview buffer search limit."
  (vertico-buffer-frame--limit-value
   vertico-buffer-frame-preview-search-buffer-limit
   vertico-buffer-frame--default-preview-search-buffer-limit))

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

;;;; Preview target resolution

;;;;; File, buffer, and project targets

(defun vertico-buffer-frame--remote-file-name-p (file directory)
  "Return non-nil when FILE names a remote path under DIRECTORY."
  (condition-case-unless-debug nil
      (or (file-remote-p file)
          (and (not (file-name-absolute-p file))
               (file-remote-p directory)))
    (error nil)))

(defun vertico-buffer-frame--readable-file (file &optional directory)
  "Return expanded FILE under DIRECTORY when it is readable."
  (condition-case-unless-debug nil
      (when (stringp file)
        (let ((file (substitute-in-file-name file))
              (directory (or directory default-directory)))
          (unless (and (not vertico-buffer-frame-preview-remote-files)
                       (vertico-buffer-frame--remote-file-name-p file
                                                                 directory))
            (let ((expanded (expand-file-name file directory)))
              (and (or vertico-buffer-frame-preview-remote-files
                       (not (file-remote-p expanded)))
                   (file-readable-p expanded)
                   expanded)))))
    (error nil)))

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
        (let ((root (condition-case-unless-debug nil
                        (when-let* ((project (project-current nil)))
                          (project-root project))
                      (error nil))))
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
  (when-let* ((window (ignore-errors
                        (minibuffer-selected-window))))
    (and (vertico-buffer-frame--preview-live-window-p window)
         (ignore-errors
           (window-buffer window)))))

(defun vertico-buffer-frame--buffer-position-target (buffer position)
  "Return a buffer-position target for BUFFER at POSITION."
  (when (and (buffer-live-p buffer) position)
    (list 'buffer-position buffer position)))

;;;;; Text and metadata targets

(defun vertico-buffer-frame--symbol-preview-target (candidate)
  "Return a text preview target for symbol CANDIDATE."
  (when-let* (((stringp candidate))
              (symbol (intern-soft candidate)))
    (list 'text
          "Symbol"
          (lambda ()
            (vertico-buffer-frame--insert-symbol-preview symbol)))))

(defun vertico-buffer-frame--color-target (candidate)
  "Return a text preview target for color CANDIDATE."
  (when-let* (((stringp candidate))
              (rgb (vertico-buffer-frame--color-values candidate)))
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

(defun vertico-buffer-frame--color-values (color)
  "Return RGB values for COLOR, or nil when COLOR cannot be read."
  (condition-case-unless-debug nil
      (let ((rgb (and (color-defined-p color)
                      (color-values color))))
        (and (integerp (nth 0 rgb))
             (integerp (nth 1 rgb))
             (integerp (nth 2 rgb))
             rgb))
    (error nil)))

(defun vertico-buffer-frame--custom-theme-target (candidate)
  "Return a preview target for custom theme CANDIDATE."
  (when (and (stringp candidate)
             (require 'cus-theme nil t))
    (let ((theme (intern-soft candidate)))
      (when (and theme
                 (memq theme (vertico-buffer-frame--custom-available-themes)))
        (or (when-let* ((file (ignore-errors
                                (locate-library
                                 (format "%s-theme.el" candidate)))))
              (list 'file file))
            (list 'text
                  "Custom Theme"
                  (lambda ()
                    (insert candidate "\n\n")
                    (insert "Available: yes\n")
                    (insert "Loaded: "
                            (if (ignore-errors
                                  (custom-theme-p theme))
                                "yes"
                              "no")
                            "\n")
                    (insert "Enabled: "
                            (if (vertico-buffer-frame--custom-theme-enabled-p
                                 theme)
                                "yes"
                              "no")
                            "\n"))))))))

(defun vertico-buffer-frame--custom-available-themes ()
  "Return available custom themes as a safe proper list."
  (vertico-buffer-frame--proper-list
   (ignore-errors
     (custom-available-themes))))

(defun vertico-buffer-frame--custom-theme-enabled-p (theme)
  "Return non-nil when custom THEME is enabled."
  (and (boundp 'custom-enabled-themes)
       (memq theme
             (vertico-buffer-frame--proper-list custom-enabled-themes))))

(defun vertico-buffer-frame--input-method-target (candidate)
  "Return a text preview target for input method CANDIDATE."
  (when (and (stringp candidate)
             (boundp 'input-method-alist)
             (proper-list-p input-method-alist))
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
  (when (stringp candidate)
    (let ((coding-system (intern-soft candidate)))
      (when (and coding-system
                 (ignore-errors
                   (coding-system-p coding-system)))
        (list 'text
              "Coding System"
              (lambda ()
                (insert (symbol-name coding-system) "\n\n")
                (when-let* ((doc (ignore-errors
                                   (coding-system-doc-string
                                    coding-system))))
                  (insert (format "%s" doc) "\n\n"))
                (when-let* ((base (ignore-errors
                                    (coding-system-base coding-system))))
                  (insert "Base: " (format "%s" base) "\n"))
                (when-let* ((mnemonic
                             (ignore-errors
                               (coding-system-mnemonic coding-system))))
                  (when (characterp mnemonic)
                    (insert "Mnemonic: " (char-to-string mnemonic) "\n")))
                (let ((eol (ignore-errors
                             (coding-system-eol-type coding-system))))
                  (insert "EOL: " (format "%S" eol) "\n"))))))))

(defun vertico-buffer-frame--charset-target (candidate)
  "Return a text preview target for character set CANDIDATE."
  (when (stringp candidate)
    (let ((charset (intern-soft candidate)))
      (when (and charset
                 (ignore-errors
                   (charsetp charset)))
        (list 'text
              "Character Set"
              (lambda ()
                (insert (symbol-name charset) "\n\n")
                (when-let* ((description (ignore-errors
                                           (charset-description charset))))
                  (insert (format "%s" description) "\n\n"))
                (dolist (property '(short-name long-name iso-final-char
                                               dimension chars))
                  (when-let* ((value (ignore-errors
                                       (get-charset-property charset
                                                             property))))
                    (insert (capitalize (symbol-name property))
                            ": "
                            (format "%S" value)
                            "\n")))))))))

(defun vertico-buffer-frame--library-target (candidate)
  "Return a file preview target for Emacs Lisp library CANDIDATE."
  (when-let* (((stringp candidate))
              (file (ignore-errors
                      (or (locate-library (concat candidate ".el"))
                          (locate-library candidate)))))
    (list 'file file)))

;;;;; Package and register targets

(defun vertico-buffer-frame--package-descriptor (package)
  "Return PACKAGE descriptor from installed or archive package metadata."
  (or (vertico-buffer-frame--package-descriptor-in-list
       package
       (vertico-buffer-frame--package-metadata-list 'package-alist))
      (vertico-buffer-frame--package-descriptor-in-list
       package
       (vertico-buffer-frame--package-metadata-list
        'package-archive-contents))))

(defun vertico-buffer-frame--package-metadata-list (symbol)
  "Return package metadata held by SYMBOL as a safe proper list."
  (when (and (boundp symbol)
             (proper-list-p (symbol-value symbol)))
    (symbol-value symbol)))

(defun vertico-buffer-frame--package-descriptor-p (descriptor)
  "Return non-nil when DESCRIPTOR is a package descriptor."
  (and (fboundp 'package-desc-p)
       (package-desc-p descriptor)))

(defun vertico-buffer-frame--package-descriptor-in-list (package entries)
  "Return PACKAGE descriptor found in package ENTRIES."
  (when-let* ((entry (and (symbolp package)
                          (assq package entries)))
              (tail (cdr entry))
              ((consp tail))
              (descriptor (car tail))
              ((vertico-buffer-frame--package-descriptor-p descriptor)))
    descriptor))

(defun vertico-buffer-frame--package-installed-p (package)
  "Return non-nil when PACKAGE appears installed."
  (and (symbolp package)
       (assq package
             (vertico-buffer-frame--package-metadata-list 'package-alist))))

(defun vertico-buffer-frame--package-built-in-p (package)
  "Return non-nil when PACKAGE is built in."
  (condition-case-unless-debug nil
      (and (symbolp package)
           (fboundp 'package-built-in-p)
           (package-built-in-p package))
    (error nil)))

(defun vertico-buffer-frame--package-version-string (version)
  "Return VERSION formatted for display."
  (condition-case-unless-debug nil
      (cond
       ((proper-list-p version)
        (package-version-join version))
       ((stringp version)
        version)
       (t
        (format "%S" version)))
    (error (format "%S" version))))

(defun vertico-buffer-frame--package-requirement-string (requirement)
  "Return a display string for a single package REQUIREMENT."
  (when (and (consp requirement)
             (symbolp (car requirement))
             (cdr requirement))
    (let ((version (if (consp (cdr requirement))
                       (cadr requirement)
                     (cdr requirement))))
      (format "%s %s"
              (car requirement)
              (vertico-buffer-frame--package-version-string version)))))

(defun vertico-buffer-frame--package-requirements-string (requirements)
  "Return a display string for package REQUIREMENTS."
  (when (proper-list-p requirements)
    (string-join
     (delq nil
           (mapcar #'vertico-buffer-frame--package-requirement-string
                   requirements))
     ", ")))

(defun vertico-buffer-frame--insert-package-descriptor-preview (descriptor)
  "Insert package preview details from DESCRIPTOR."
  (condition-case-unless-debug error
      (progn
        (insert "Version: "
                (vertico-buffer-frame--package-version-string
                 (package-desc-version descriptor))
                "\n")
        (when-let* ((summary (package-desc-summary descriptor)))
          (insert (format "%s" summary) "\n"))
        (insert "Kind: "
                (format "%S" (package-desc-kind descriptor))
                "\n")
        (when-let* ((requirements
                     (vertico-buffer-frame--package-requirements-string
                      (package-desc-reqs descriptor)))
                    ((not (string-empty-p requirements))))
          (insert "Requires: " requirements "\n")))
    (error
     (insert "Package metadata preview failed.\n")
     (insert "Error: " (error-message-string error) "\n"))))

(defun vertico-buffer-frame--insert-package-status-preview (package)
  "Insert installation status preview for PACKAGE."
  (insert "Installed: "
          (if (vertico-buffer-frame--package-installed-p package)
              "yes"
            "no")
          "\n"))

(defun vertico-buffer-frame--insert-package-preview (package descriptor)
  "Insert package preview for PACKAGE using DESCRIPTOR when available."
  (insert (symbol-name package) "\n\n")
  (if descriptor
      (vertico-buffer-frame--insert-package-descriptor-preview descriptor)
    (vertico-buffer-frame--insert-package-status-preview package))
  (insert "Built-in: "
          (if (vertico-buffer-frame--package-built-in-p package)
              "yes"
            "no")
          "\n"))

(defun vertico-buffer-frame--package-target (candidate)
  "Return a text preview target for package CANDIDATE."
  (when (and (stringp candidate)
             (require 'package nil t))
    (when-let* ((package (intern-soft candidate)))
      (let ((descriptor (vertico-buffer-frame--package-descriptor package)))
        (list 'text
              "Package"
              (lambda ()
                (vertico-buffer-frame--insert-package-preview
                 package descriptor)))))))

(defun vertico-buffer-frame--register-target (candidate)
  "Return a text preview target for register CANDIDATE."
  (when (and (stringp candidate)
             (= (length candidate) 1))
    (let* ((register (aref candidate 0))
           (value (ignore-errors
                    (get-register register))))
      (when value
        (list 'text
              "Register"
              (lambda ()
                (insert (vertico-buffer-frame--register-name register)
                        "\n\n")
                (pcase value
                  ((pred markerp)
                   (vertico-buffer-frame--insert-register-marker-preview
                    value))
                  ((pred window-configuration-p)
                   (insert "Window configuration\n"))
                  ((pred frame-configuration-p)
                   (insert "Frame configuration\n"))
                  (_
                   (insert (format "%S\n" value))))))))))

(defun vertico-buffer-frame--register-name (register)
  "Return a display name for REGISTER."
  (or (ignore-errors
        (single-key-description register))
      (ignore-errors
        (char-to-string register))
      (format "%S" register)))

(defun vertico-buffer-frame--insert-register-marker-preview (marker)
  "Insert a preview for register MARKER."
  (insert "Marker: "
          (or (ignore-errors
                (buffer-name (marker-buffer marker)))
              "no buffer")
          ":"
          (format "%s" (ignore-errors
                         (marker-position marker)))
          "\n"))

(defun vertico-buffer-frame--email-target (candidate)
  "Return a text preview target for email CANDIDATE."
  (when (stringp candidate)
    (list 'text
          "Email"
          (lambda ()
            (insert candidate "\n")
            (when (require 'mail-extr nil t)
              (condition-case-unless-debug nil
                  (pcase-let ((`(,name ,address)
                               (mail-extract-address-components candidate)))
                    (when name
                      (insert "\nName: " name "\n"))
                    (when address
                      (insert "Address: " address "\n")))
                (error nil)))))))

;;;;; Search and location targets

(defun vertico-buffer-frame--find-buffer-position (candidate &optional buffers)
  "Return a buffer-position target for the first occurrence of CANDIDATE.
Search BUFFERS, or the minibuffer origin buffer followed by live buffers."
  (when (and (stringp candidate)
             (not (string-empty-p candidate)))
    (let* ((explicit-buffers buffers)
           (buffers (cond
                     ((null explicit-buffers)
                      (delq nil
                            (delete-dups
                             (cons (vertico-buffer-frame--origin-buffer)
                                   (buffer-list)))))
                     ((bufferp explicit-buffers)
                      (list explicit-buffers))
                     ((proper-list-p explicit-buffers)
                      explicit-buffers)
                     (explicit-buffers
                      nil)))
           (limit (and (not explicit-buffers)
                       (vertico-buffer-frame--preview-search-buffer-limit)))
           (searched 0))
      (catch 'found
        (dolist (buffer buffers)
          (when (and (buffer-live-p buffer)
                     (not (minibufferp buffer))
                     (or (null limit)
                         (and (numberp limit)
                              (< searched limit))))
            (cl-incf searched)
            (condition-case-unless-debug nil
                (with-current-buffer buffer
                  (save-excursion
                    (save-restriction
                      (widen)
                      (goto-char (point-min))
                      (when (search-forward candidate nil t)
                        (throw 'found
                               (vertico-buffer-frame--buffer-position-target
                                buffer
                                (match-beginning 0)))))))
              (error nil))))))))

(defun vertico-buffer-frame--dabbrev-target (candidate)
  "Return a preview target for dabbrev CANDIDATE."
  (when (stringp candidate)
    (or (vertico-buffer-frame--find-buffer-position candidate)
        (list 'text
              "Dabbrev"
              (lambda ()
                (insert candidate "\n"))))))

(defun vertico-buffer-frame--unicode-name-target (candidate)
  "Return a text preview target for Unicode name CANDIDATE."
  (when-let* (((stringp candidate))
              (char (or (char-from-name candidate t)
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
  (when (and (stringp candidate)
             (require 'bookmark nil t))
    (when-let* ((bookmark (ignore-errors
                            (bookmark-get-bookmark candidate 'noerror))))
      (let* ((file (ignore-errors
                     (bookmark-get-filename bookmark)))
             (position (ignore-errors
                         (bookmark-get-position bookmark)))
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
                    (insert "File: "
                            (vertico-buffer-frame--display-file-name file)
                            "\n"))
                  (when position
                    (insert "Position: " (format "%s" position) "\n"))))))))))

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
  (when (consp entry)
    (let ((position (cdr entry)))
      (cond
       ((markerp position)
        position)
       ((integer-or-marker-p position)
        position)
       ((overlayp position)
        (ignore-errors
          (overlay-start position)))
       ((consp position)
        (let ((position (car position)))
          (cond
           ((markerp position)
            position)
           ((integer-or-marker-p position)
            position)
           ((overlayp position)
            (ignore-errors
              (overlay-start position))))))))))

(defun vertico-buffer-frame--imenu-subalist-p (entry)
  "Return non-nil when imenu ENTRY is a nested sub-alist."
  (and (consp entry)
       (consp (cdr entry))
       (proper-list-p (cdr entry))
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
    (dolist (entry (vertico-buffer-frame--proper-list entries))
      (cond
       ((and (consp entry)
             (equal candidate
                    (vertico-buffer-frame--imenu-entry-name entry))
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
           (dolist (entry (vertico-buffer-frame--proper-list items))
             (when (consp entry)
               (if (vertico-buffer-frame--imenu-subalist-p entry)
                   (walk (cdr entry))
                 (when-let* ((name (vertico-buffer-frame--imenu-entry-name
                                    entry)))
                   (unless (gethash name table)
                     (puthash name entry table))))))))
      (walk entries))
    table))

(defun vertico-buffer-frame--consult-imenu-config (buffer)
  "Return Consult imenu configuration for BUFFER."
  (condition-case-unless-debug nil
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (boundp 'consult-imenu-config)
                     (proper-list-p consult-imenu-config))
            (cdr (cl-find-if (lambda (entry)
                               (and (consp entry)
                                    (symbolp (car entry))
                                    (ignore-errors
                                      (derived-mode-p (car entry)))))
                             consult-imenu-config)))))
    (error nil)))

(defun vertico-buffer-frame--consult-imenu-items (entries buffer)
  "Return ENTRIES arranged like Consult imenu candidates for BUFFER."
  (if-let* ((toplevel
             (plist-get (vertico-buffer-frame--consult-imenu-config buffer)
                        :toplevel)))
      (let ((entries (cl-remove-if-not
                      #'consp
                      (vertico-buffer-frame--proper-list entries))))
        (let ((tops (cl-remove-if (lambda (entry)
                                    (and (consp entry)
                                         (listp (cdr entry))))
                                  entries))
              (rest (cl-remove-if-not (lambda (entry)
                                        (and (consp entry)
                                             (listp (cdr entry))))
                                      entries)))
          (nconc rest
                 (and tops
                      (list (cons toplevel tops))))))
    (vertico-buffer-frame--proper-list entries)))

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
   (vertico-buffer-frame--proper-list entries)))

(defun vertico-buffer-frame--consult-imenu-entry-table (entries buffer)
  "Return a hash table mapping Consult imenu names to imenu ENTRIES.
Use BUFFER to resolve mode-specific Consult imenu configuration."
  (let ((table (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal)))
    (dolist (pair (vertico-buffer-frame--consult-imenu-flatten
                   nil
                   (vertico-buffer-frame--consult-imenu-items
                    entries buffer)))
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

(defun vertico-buffer-frame--consult-imenu-entry-table-cache-key
    (entries buffer)
  "Return a cache key for Consult imenu ENTRIES in BUFFER."
  (condition-case-unless-debug nil
      (and (buffer-live-p buffer)
           (with-current-buffer buffer
             (list entries
                   buffer
                   major-mode
                   (and (boundp 'consult-imenu-config)
                        consult-imenu-config))))
    (error nil)))

(defun vertico-buffer-frame--consult-imenu-entry-table-cached
    (entries buffer)
  "Return cached Consult-style imenu table for ENTRIES in BUFFER."
  (let ((key (vertico-buffer-frame--consult-imenu-entry-table-cache-key
              entries buffer)))
    (if (and key
             (consp vertico-buffer-frame--consult-imenu-entry-table-cache)
             (equal key (car
                         vertico-buffer-frame--consult-imenu-entry-table-cache)))
        (cdr vertico-buffer-frame--consult-imenu-entry-table-cache)
      (let ((table (vertico-buffer-frame--consult-imenu-entry-table
                    entries buffer)))
        (when key
          (setq-local vertico-buffer-frame--consult-imenu-entry-table-cache
                      (cons key table)))
        table))))

(defun vertico-buffer-frame--imenu-cache-key (buffer)
  "Return the cache key for BUFFER's imenu index."
  (condition-case-unless-debug nil
      (with-current-buffer buffer
        (list buffer
              major-mode
              imenu-create-index-function
              (buffer-chars-modified-tick)))
    (error nil)))

(defun vertico-buffer-frame--imenu-cache-value (buffer)
  "Return cached imenu index details for BUFFER."
  (condition-case-unless-debug nil
      (let ((key (vertico-buffer-frame--imenu-cache-key buffer)))
        (when key
          (if (and (consp vertico-buffer-frame--imenu-cache)
                   (equal key (car vertico-buffer-frame--imenu-cache)))
              (cdr vertico-buffer-frame--imenu-cache)
            (let ((entries (with-current-buffer buffer
                             (ignore-errors
                               (vertico-buffer-frame--imenu-index-entries)))))
              (setq entries (vertico-buffer-frame--proper-list entries))
              (setq-local vertico-buffer-frame--imenu-cache
                          (cons key
                                (cons entries
                                      (vertico-buffer-frame--imenu-entry-table
                                       entries))))
              (cdr vertico-buffer-frame--imenu-cache)))))
    (error nil)))

(defun vertico-buffer-frame--imenu-target (candidate raw-candidate)
  "Return a preview target for imenu CANDIDATE and RAW-CANDIDATE."
  (condition-case-unless-debug nil
      (when (and (stringp candidate)
                 (require 'imenu nil t))
        (when-let* ((buffer (vertico-buffer-frame--origin-buffer)))
          (let* ((cache (vertico-buffer-frame--imenu-cache-value buffer))
                 (entries (car-safe cache))
                 (table (cdr-safe cache))
                 (consult-table
                  (vertico-buffer-frame--consult-imenu-entry-table-cached
                   entries buffer))
                 (choice (or (and (stringp raw-candidate)
                                  (get-text-property 0 'imenu-choice
                                                     raw-candidate))
                             (and (hash-table-p table)
                                  (gethash candidate table))
                             (and (hash-table-p consult-table)
                                  (gethash candidate consult-table))
                             (vertico-buffer-frame--imenu-find-entry
                              candidate entries)))
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
               position))))))
    (error nil)))

(defun vertico-buffer-frame--bibtex-target (candidate stringp)
  "Return a preview target for BibTeX CANDIDATE.
When STRINGP is non-nil, look for a @String definition."
  (when-let* (((stringp candidate))
              (buffer (vertico-buffer-frame--origin-buffer)))
    (condition-case-unless-debug nil
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
                   (match-beginning 0)))))))
      (error nil))))

(defun vertico-buffer-frame--info-menu-target (candidate)
  "Return a text preview target for Info menu CANDIDATE."
  (when-let* (((stringp candidate))
              (buffer (vertico-buffer-frame--info-menu-buffer)))
    (list 'text
          "Info Menu"
          (lambda ()
            (insert
             (or (vertico-buffer-frame--info-menu-entry-text
                  buffer candidate)
                 (concat candidate "\n")))))))

(defun vertico-buffer-frame--info-menu-buffer ()
  "Return the live Info completion menu buffer, if any."
  (let ((buffer (and (boundp 'Info-complete-menu-buffer)
                     Info-complete-menu-buffer)))
    (and (bufferp buffer)
         (buffer-live-p buffer)
         buffer)))

(defun vertico-buffer-frame--info-menu-entry-text (buffer candidate)
  "Return Info menu entry text for CANDIDATE in BUFFER."
  (condition-case-unless-debug nil
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (let ((case-fold-search t)
                (pattern (concat "\n\\* +"
                                 (regexp-quote candidate)
                                 ":")))
            (when (re-search-forward pattern nil t)
              (let ((start (line-beginning-position))
                    (end (save-excursion
                           (forward-line 1)
                           (while (and (not (eobp))
                                       (not (looking-at-p "\\* +")))
                             (forward-line 1))
                           (point))))
                (buffer-substring-no-properties start end))))))
    (error nil)))

(defun vertico-buffer-frame--calendar-month-target (candidate)
  "Return a text preview target for calendar month CANDIDATE."
  (when (and (stringp candidate)
             (require 'calendar nil t))
    (condition-case-unless-debug nil
        (let* ((month-array calendar-month-name-array)
               (month (cdr (assoc-string candidate
                                         (calendar-make-alist month-array
                                                              1)
                                         t)))
               (year (calendar-extract-year (calendar-current-date))))
          (when month
            (list 'text
                  "Calendar Month"
                  (lambda ()
                    (condition-case-unless-debug nil
                        (calendar-generate-month month year 0)
                      (error
                       (insert candidate "\n")))))))
      (error nil))))

(defun vertico-buffer-frame--simple-text-target (candidate name)
  "Return a text preview target for CANDIDATE named NAME."
  (when (stringp candidate)
    (list 'text
          name
          (lambda ()
            (insert candidate "\n")))))

;;;;; Resolver dispatch

(defconst vertico-buffer-frame--preview-target-resolvers
  '(;; File and live-buffer targets.
    (file vertico-buffer-frame--file-target)
    (project-file vertico-buffer-frame--project-file-target)
    (buffer vertico-buffer-frame--buffer-target)
    (xref-location vertico-buffer-frame--xref-location-target :raw-candidate)
    (bookmark vertico-buffer-frame--bookmark-target)

    ;; Symbol-like targets all share the same documentation preview.
    (symbol-help vertico-buffer-frame--symbol-preview-target)
    (command vertico-buffer-frame--symbol-preview-target)
    (function vertico-buffer-frame--symbol-preview-target)
    (variable vertico-buffer-frame--symbol-preview-target)
    (face vertico-buffer-frame--symbol-preview-target)
    (symbol vertico-buffer-frame--symbol-preview-target)
    (minor-mode vertico-buffer-frame--symbol-preview-target)
    (customization-group vertico-buffer-frame--symbol-preview-target)
    (custom-variable vertico-buffer-frame--symbol-preview-target)
    (apropos-symbol vertico-buffer-frame--symbol-preview-target)

    ;; Metadata text previews.
    (unicode-name vertico-buffer-frame--unicode-name-target)
    (info-menu vertico-buffer-frame--info-menu-target)
    (calendar-month vertico-buffer-frame--calendar-month-target)
    (color vertico-buffer-frame--color-target)
    (custom-theme vertico-buffer-frame--custom-theme-target)
    (theme vertico-buffer-frame--custom-theme-target)
    (input-method vertico-buffer-frame--input-method-target)
    (coding-system vertico-buffer-frame--coding-system-target)
    (charset vertico-buffer-frame--charset-target)
    (library vertico-buffer-frame--library-target)
    (package vertico-buffer-frame--package-target)
    (register vertico-buffer-frame--register-target)
    (email vertico-buffer-frame--email-target)
    (ecomplete vertico-buffer-frame--email-target)

    ;; Search-like candidates.
    (dabbrev vertico-buffer-frame--dabbrev-target)
    (imenu vertico-buffer-frame--imenu-target :raw-candidate)
    (bibtex-key vertico-buffer-frame--bibtex-target nil)
    (bibtex-string vertico-buffer-frame--bibtex-target t)
    (kill-ring vertico-buffer-frame--simple-text-target "Kill Ring"))
  "Built-in preview target resolvers keyed by completion category.")

(defun vertico-buffer-frame--preview-target-argument
    (argument raw-candidate)
  "Return ARGUMENT resolved for RAW-CANDIDATE."
  (if (eq argument :raw-candidate)
      raw-candidate
    argument))

(defun vertico-buffer-frame--builtin-preview-target
    (category candidate raw-candidate)
  "Return a built-in preview target for CATEGORY, CANDIDATE and RAW-CANDIDATE."
  (condition-case-unless-debug error
      (when-let* ((resolver (assq category
                                  vertico-buffer-frame--preview-target-resolvers)))
        (apply (cadr resolver)
               candidate
               (mapcar (lambda (argument)
                         (vertico-buffer-frame--preview-target-argument
                          argument raw-candidate))
                       (cddr resolver))))
    (error
     (vertico-buffer-frame--report-preview-target-error error)
     nil)))

(defun vertico-buffer-frame--preview-category-enabled-p (category)
  "Return non-nil when preview CATEGORY is enabled."
  (and (proper-list-p vertico-buffer-frame-preview-categories)
       (memq category vertico-buffer-frame-preview-categories)))

(defun vertico-buffer-frame--preview-target-function-list ()
  "Return preview target functions as a safe proper list."
  (cond
   ((functionp vertico-buffer-frame-preview-target-functions)
    (list vertico-buffer-frame-preview-target-functions))
   ((proper-list-p vertico-buffer-frame-preview-target-functions)
    (cl-remove-if-not #'functionp
                      vertico-buffer-frame-preview-target-functions))
   (t nil)))

(defun vertico-buffer-frame--report-preview-target-error (error)
  "Report preview target resolution ERROR without hiding the current preview."
  (when vertico-buffer-frame-preview-report-errors
    (let ((message (error-message-string error)))
      (unless (equal message
                     vertico-buffer-frame--preview-last-target-error-message)
        (setq-local vertico-buffer-frame--preview-last-target-error-message
                    message)
        (message "vertico-buffer-frame preview target error: %s" message)))))

(defun vertico-buffer-frame--preview-target-from-functions
    (category candidate raw-candidate)
  "Return preview target from user functions.
Pass CATEGORY, CANDIDATE, and RAW-CANDIDATE to each function."
  (catch 'target
    (dolist (function (vertico-buffer-frame--preview-target-function-list))
      (when function
        (condition-case-unless-debug error
            (when-let* ((target
                         (funcall function category candidate raw-candidate)))
              (throw 'target target))
          (error
           (vertico-buffer-frame--report-preview-target-error error)))))
    nil))

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
    (when (vertico-buffer-frame--preview-category-enabled-p category)
      (or (vertico-buffer-frame--preview-target-from-functions
           category candidate raw-candidate)
          (vertico-buffer-frame--builtin-preview-target
           category candidate raw-candidate)))))

;;;; Preview frame layout

(defun vertico-buffer-frame--preview-parent-frame ()
  "Return the parent frame for the preview child frame.
The preview frame is a sibling of the candidate frame.  When the candidate
frame is live, use its parent so preview placement stays in the same coordinate
space."
  (let* ((candidate vertico-buffer-frame--candidate-frame)
         (candidate-parent
          (and (vertico-buffer-frame--preview-live-frame-p candidate)
               (vertico-buffer-frame--frame-parameter
                candidate 'parent-frame))))
    (if (vertico-buffer-frame--preview-live-frame-p candidate-parent)
        candidate-parent
      (vertico-buffer-frame--parent-frame))))

(defun vertico-buffer-frame--preview-size-source-frame (parent)
  "Return the frame used to derive preview size under PARENT.
When the live candidate frame belongs to PARENT, derive the preview size from
that candidate frame so resize-to-fit candidates and previews move together."
  (let ((candidate vertico-buffer-frame--candidate-frame))
    (if (and (vertico-buffer-frame--preview-live-frame-p candidate)
             (eq (vertico-buffer-frame--frame-parameter
                  candidate 'parent-frame)
                 parent))
        candidate
      parent)))

(defun vertico-buffer-frame--preview-live-frame-p (frame)
  "Return non-nil when FRAME is live, ignoring stale frame errors."
  (condition-case-unless-debug nil
      (frame-live-p frame)
    (error nil)))

(defun vertico-buffer-frame--preview-live-window-p (window)
  "Return non-nil when WINDOW is live, ignoring stale window errors."
  (condition-case-unless-debug nil
      (window-live-p window)
    (error nil)))

(defun vertico-buffer-frame--preview-window-current-p (parent)
  "Return non-nil when the current preview window can be reused under PARENT."
  (and (vertico-buffer-frame--preview-live-frame-p
        vertico-buffer-frame--preview-frame)
       (vertico-buffer-frame--preview-live-window-p
        vertico-buffer-frame--preview-window)
       (eq (vertico-buffer-frame--frame-parameter
            vertico-buffer-frame--preview-frame
            'parent-frame)
           parent)))

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

(defun vertico-buffer-frame--preview-frame-layout-state (frame parent size)
  "Return layout state for preview FRAME under PARENT with character SIZE.
Include candidate geometry because preview placement follows the candidate frame
instead of only the parent frame."
  (append (vertico-buffer-frame--frame-layout-state frame parent size)
          (list (vertico-buffer-frame--preview-reference-geometry parent))))

(defun vertico-buffer-frame--refresh-preview-frame ()
  "Resize and reposition the current preview frame, if it is live."
  (condition-case-unless-debug nil
      (when (vertico-buffer-frame--preview-live-frame-p
             vertico-buffer-frame--preview-frame)
        (let* ((parent (vertico-buffer-frame--preview-parent-frame))
               (size-source
                (vertico-buffer-frame--preview-size-source-frame parent))
               (size (vertico-buffer-frame--preview-frame-size size-source))
               (state (vertico-buffer-frame--preview-frame-layout-state
                       vertico-buffer-frame--preview-frame parent size))
               (layout-changed
                (not (equal state vertico-buffer-frame--preview-layout-state)))
               placed)
          (when layout-changed
            (setq placed
                  (and (vertico-buffer-frame--resize-frame-to-size
                        vertico-buffer-frame--preview-frame size)
                       (vertico-buffer-frame--place-preview-frame
                        vertico-buffer-frame--preview-frame parent)))
            (when placed
              (setq-local vertico-buffer-frame--preview-layout-state
                          (vertico-buffer-frame--preview-frame-layout-state
                           vertico-buffer-frame--preview-frame parent size))))
          (when (and placed
                     (vertico-buffer-frame--preview-live-window-p
                      vertico-buffer-frame--preview-window))
            (force-window-update
             (window-buffer vertico-buffer-frame--preview-window)))))
    (error nil)))

;;;; Preview buffer ownership and file rendering

(defun vertico-buffer-frame--kill-temporary-preview-buffer (buffer)
  "Kill temporary preview BUFFER when it is live."
  (when (condition-case-unless-debug nil
            (and (buffer-live-p buffer)
                 (buffer-local-value
                  'vertico-buffer-frame--temporary-preview-buffer
                  buffer))
          (error nil))
    (let ((kill-buffer-query-functions nil))
      (ignore-errors
        (kill-buffer buffer)))))

(defun vertico-buffer-frame--kill-preview-buffer (&optional keep-buffer)
  "Kill temporary preview buffers owned by this minibuffer.
When KEEP-BUFFER is non-nil, preserve that buffer."
  (let ((buffer vertico-buffer-frame--preview-buffer)
        (owner (current-buffer)))
    (when (and (not (eq buffer keep-buffer))
               (vertico-buffer-frame--preview-buffer-owned-by-p buffer owner))
      (vertico-buffer-frame--kill-temporary-preview-buffer buffer))
    (when (eq (cdr-safe vertico-buffer-frame--file-preview-cache) buffer)
      (setq-local vertico-buffer-frame--file-preview-cache nil))
    (vertico-buffer-frame--kill-preview-buffers-owned-by-buffer
     owner keep-buffer))
  (setq-local vertico-buffer-frame--preview-buffer nil))

(defun vertico-buffer-frame--preview-buffer-owned-by-p (buffer owner)
  "Return non-nil when BUFFER is a temporary preview buffer owned by OWNER."
  (condition-case-unless-debug nil
      (and (buffer-live-p buffer)
           (buffer-local-value
            'vertico-buffer-frame--temporary-preview-buffer
            buffer)
           (eq (buffer-local-value
                'vertico-buffer-frame--preview-owner-buffer
                buffer)
               owner))
    (error nil)))

(defun vertico-buffer-frame--kill-preview-buffers-owned-by-buffer
    (owner &optional keep-buffer)
  "Kill temporary preview buffers owned by OWNER.
When KEEP-BUFFER is non-nil, preserve that buffer."
  (dolist (buffer (buffer-list))
    (when (and (not (eq buffer keep-buffer))
               (vertico-buffer-frame--preview-buffer-owned-by-p buffer owner))
      (vertico-buffer-frame--kill-temporary-preview-buffer buffer))))

(defun vertico-buffer-frame--kill-old-preview-buffer (old-buffer new-buffer)
  "Kill OLD-BUFFER when it is live and different from NEW-BUFFER."
  (when (and (buffer-live-p old-buffer)
             (not (eq old-buffer new-buffer)))
    (when (vertico-buffer-frame--preview-buffer-owned-by-p
           old-buffer (current-buffer))
      (vertico-buffer-frame--kill-temporary-preview-buffer old-buffer))
    (when (eq (cdr-safe vertico-buffer-frame--file-preview-cache)
              old-buffer)
      (setq-local vertico-buffer-frame--file-preview-cache nil))))

(defun vertico-buffer-frame--text-preview-buffer (name inserter)
  "Return a temporary preview buffer named NAME populated by INSERTER."
  (let* ((owner (current-buffer))
         (buffer (if (and (vertico-buffer-frame--preview-buffer-owned-by-p
                           vertico-buffer-frame--preview-buffer
                           owner)
                          (not (eq (cdr-safe
                                    vertico-buffer-frame--file-preview-cache)
                                   vertico-buffer-frame--preview-buffer)))
                     vertico-buffer-frame--preview-buffer
                   (generate-new-buffer
                    (format " *vertico-buffer-frame-%s-preview*" name))))
         success)
    (setq-local vertico-buffer-frame--preview-buffer buffer)
    (unwind-protect
        (prog1 buffer
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (fundamental-mode)
              (setq-local truncate-lines t
                          vertico-buffer-frame--temporary-preview-buffer t
                          vertico-buffer-frame--preview-owner-buffer owner)
              (funcall inserter))
            (goto-char (point-min))
            (setq buffer-read-only t)
            (set-buffer-modified-p nil))
          (setq success t))
      (unless success
        (when (eq vertico-buffer-frame--preview-buffer buffer)
          (setq-local vertico-buffer-frame--preview-buffer nil))
        (when (eq (cdr-safe vertico-buffer-frame--file-preview-cache)
                  buffer)
          (setq-local vertico-buffer-frame--file-preview-cache nil))
        (vertico-buffer-frame--kill-temporary-preview-buffer buffer)))))

(defun vertico-buffer-frame--display-file-name (file)
  "Return FILE formatted for preview text."
  (condition-case-unless-debug nil
      (abbreviate-file-name file)
    (error (format "%s" file))))

(defun vertico-buffer-frame--insert-file-preview-error (file operation error)
  "Insert a fallback preview for FILE after OPERATION signaled ERROR."
  (insert (vertico-buffer-frame--display-file-name file) "\n\n")
  (insert operation " failed.\n")
  (insert "Error: " (error-message-string error) "\n"))

(defun vertico-buffer-frame--insert-directory-preview (directory)
  "Insert a simple preview of DIRECTORY."
  (condition-case-unless-debug error
      (prog1 t
        (insert (vertico-buffer-frame--display-file-name
                 (file-name-as-directory directory))
                "\n\n")
        (let ((count 0)
              (shown 0)
              (limit
               (vertico-buffer-frame--preview-directory-entry-limit)))
          (dolist (entry (directory-files directory nil nil t
                                          (and limit (+ limit 3))))
            (unless (member entry '("." ".."))
              (cl-incf count)
              (when (or (null limit)
                        (and (numberp limit)
                             (< shown limit)))
                (cl-incf shown)
                (insert entry
                        (if (ignore-errors
                              (file-directory-p
                               (expand-file-name entry directory)))
                            "/"
                          "")
                        "\n"))))
          (when (= count 0)
            (insert "Empty directory\n"))
          (when (and limit
                     (> count limit))
            (insert "\nMore entries not shown.\n"))))
    (error
     (vertico-buffer-frame--insert-file-preview-error
      directory "Directory preview" error)
     nil)))

(defun vertico-buffer-frame--file-size (file)
  "Return FILE size in bytes, or nil if it cannot be determined."
  (ignore-errors
    (file-attribute-size (file-attributes file 'integer))))

(defun vertico-buffer-frame--file-preview-limit (size)
  "Return the byte limit for a file preview of SIZE bytes."
  (when-let* ((limit (vertico-buffer-frame--preview-max-file-size)))
    (let ((limit (max 0 limit)))
      (if (integerp size)
          (min limit size)
        limit))))

(defconst vertico-buffer-frame--file-line-scan-chunk-size 65536
  "Number of bytes read at a time while locating a preview line.")

(defun vertico-buffer-frame--file-line-byte-position (file line)
  "Return the zero-based byte position where FILE's LINE starts."
  (when (and (integerp line)
             (> line 0))
    (condition-case-unless-debug nil
        (catch 'position
          (when (= line 1)
            (throw 'position 0))
          (let ((remaining-newlines (1- line))
                (offset 0)
                eof)
            (while (not eof)
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (let ((inserted
                       (cadr
                        (insert-file-contents-literally
                         file
                         nil
                         offset
                         (+ offset
                            vertico-buffer-frame--file-line-scan-chunk-size)))))
                  (if (or (not (integerp inserted))
                          (= inserted 0))
                      (setq eof t)
                    (goto-char (point-min))
                    (while (search-forward "\n" nil t)
                      (setq remaining-newlines
                            (1- remaining-newlines))
                      (when (= remaining-newlines 0)
                        (throw 'position
                               (+ offset (1- (point))))))
                    (setq offset (+ offset inserted))
                    (when (< inserted
                             vertico-buffer-frame--file-line-scan-chunk-size)
                      (setq eof t)))))))
          nil)
      (error nil))))

(defun vertico-buffer-frame--file-position-character-position (position)
  "Return the one-based character position described by POSITION."
  (cond
   ((markerp position)
    (marker-position position))
   ((integerp position)
    position)))

(defconst vertico-buffer-frame--file-position-scan-chunk-size 65536
  "Minimum bytes read while mapping file positions to byte offsets.")

(defun vertico-buffer-frame--file-position-prefix-byte-length (position coding)
  "Return file byte length before character POSITION using CODING."
  (length
   (encode-coding-string
    (buffer-substring-no-properties (point-min) position)
    (or coding 'undecided-unix))))

(defun vertico-buffer-frame--file-position-byte-position (file position)
  "Return the zero-based byte position for character POSITION in FILE.
POSITION is an Emacs buffer position.  For multibyte encodings or end-of-line
conversion, character positions do not map to file bytes by subtracting one, so
read a prefix and encode the text before POSITION with the detected file
coding system."
  (when-let* ((position
               (vertico-buffer-frame--file-position-character-position
                position))
              ((> position 0))
              (size (vertico-buffer-frame--file-size file)))
    (condition-case-unless-debug nil
        (with-temp-buffer
          (let ((end (min size
                          (max position
                               vertico-buffer-frame--file-position-scan-chunk-size)))
                byte-position)
            (while (and (not byte-position)
                        end)
              (erase-buffer)
              (insert-file-contents file nil 0 end)
              (when (<= position (point-max))
                (setq byte-position
                      (vertico-buffer-frame--file-position-prefix-byte-length
                       position last-coding-system-used)))
              (setq end
                    (cond
                     (byte-position
                      nil)
                     ((>= end size)
                      nil)
                     (t
                      (min size (* 2 end))))))
            byte-position))
      (error nil))))

(defun vertico-buffer-frame--large-regular-file-preview-data (file)
  "Return (SIZE . LIMIT) when FILE should use a location-focused preview."
  (condition-case-unless-debug nil
      (let ((size (vertico-buffer-frame--file-size file))
            (limit (vertico-buffer-frame--preview-max-file-size)))
        (and (file-regular-p file)
             (integerp size)
             (integerp limit)
             (> limit 0)
             (> size limit)
             (or vertico-buffer-frame-preview-binary-files
                 (not (vertico-buffer-frame--binary-file-p file size)))
             (cons size limit)))
    (error nil)))

(defun vertico-buffer-frame--focused-file-preview-buffer
    (file start size limit)
  "Return a preview buffer for FILE beginning near byte START.
At most LIMIT bytes are read from a file of SIZE bytes."
  (let* ((start (max 0 (min start size)))
         (end (min size (+ start limit))))
    (vertico-buffer-frame--text-preview-buffer
     "file"
     (lambda ()
       (insert-file-contents file nil start end)
       (when (< end size)
         (goto-char (point-max))
         (insert "\n\nPreview starts at byte "
                 (number-to-string start)
                 " and is truncated at "
                 (number-to-string end)
                 " of "
                 (number-to-string size)
                 " bytes.\n"))))))

(defun vertico-buffer-frame--file-line-preview-spec (file line)
  "Return a preview display spec for FILE at LINE."
  (if-let* ((data (vertico-buffer-frame--large-regular-file-preview-data
                   file))
            (start (vertico-buffer-frame--file-line-byte-position
                    file line)))
      (list (vertico-buffer-frame--focused-file-preview-buffer
             file start (car data) (cdr data))
            (cons 'line 1)
            nil)
    (list (vertico-buffer-frame--file-preview-buffer file)
          (cons 'line line)
          nil)))

(defun vertico-buffer-frame--file-position-preview-spec (file position)
  "Return a preview display spec for FILE at POSITION."
  (if-let* ((data (vertico-buffer-frame--large-regular-file-preview-data
                   file))
            (start (vertico-buffer-frame--file-position-byte-position
                    file position)))
      (list (vertico-buffer-frame--focused-file-preview-buffer
             file start (car data) (cdr data))
            (cons 'position 1)
            nil)
    (list (vertico-buffer-frame--file-preview-buffer file)
          (cons 'position position)
          nil)))

(defun vertico-buffer-frame--binary-file-p (file size)
  "Return non-nil when FILE appears to be binary.
SIZE is the file size in bytes, or nil if it is unknown."
  (condition-case-unless-debug nil
      (let ((limit (min 4096 (or size 4096))))
        (and (> limit 0)
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (insert-file-contents-literally file nil 0 limit)
               (goto-char (point-min))
               (search-forward (string 0) nil t))))
    (error nil)))

(defun vertico-buffer-frame--insert-regular-file-preview (file)
  "Insert a guarded preview of regular FILE."
  (condition-case-unless-debug error
      (prog1 t
        (let* ((size (vertico-buffer-frame--file-size file))
               (limit (vertico-buffer-frame--file-preview-limit size)))
          (cond
           ((and limit
                 (= limit 0))
            (insert (vertico-buffer-frame--display-file-name file) "\n\n")
            (insert "File preview is limited to 0 bytes.\n")
            (when size
              (insert "Size: " (number-to-string size) " bytes\n")))
           ((and (not vertico-buffer-frame-preview-binary-files)
                 (vertico-buffer-frame--binary-file-p file size))
            (insert (vertico-buffer-frame--display-file-name file) "\n\n")
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
    (error
     (vertico-buffer-frame--insert-file-preview-error
      file "File preview" error)
     nil)))

(defun vertico-buffer-frame--file-preview-cache-key (file)
  "Return a cache key for FILE preview content."
  (condition-case-unless-debug nil
      (when-let* ((attributes (file-attributes file 'integer)))
        (list file
              (car attributes)
              (nth 5 attributes)
              (nth 7 attributes)
              (vertico-buffer-frame--preview-max-file-size)
              vertico-buffer-frame-preview-binary-files
              (vertico-buffer-frame--preview-directory-entry-limit)))
    (error nil)))

(defun vertico-buffer-frame--cached-file-preview-buffer (key)
  "Return the cached file preview buffer for KEY, if it is still valid."
  (when (and key
             (consp vertico-buffer-frame--file-preview-cache)
             (equal key (car vertico-buffer-frame--file-preview-cache)))
    (let ((buffer (cdr vertico-buffer-frame--file-preview-cache)))
      (when (vertico-buffer-frame--preview-buffer-owned-by-p
             buffer (current-buffer))
        (setq-local vertico-buffer-frame--preview-buffer buffer)
        buffer))))

(defun vertico-buffer-frame--insert-file-preview (file)
  "Insert a preview for FILE and return non-nil when it can be cached."
  (condition-case-unless-debug error
      (cond
       ((file-directory-p file)
        (vertico-buffer-frame--insert-directory-preview file))
       ((file-regular-p file)
        (vertico-buffer-frame--insert-regular-file-preview file))
       (t
        (insert (vertico-buffer-frame--display-file-name file) "\n")
        t))
    (error
     (vertico-buffer-frame--insert-file-preview-error
      file "File preview" error)
     nil)))

(defun vertico-buffer-frame--file-preview-buffer (file)
  "Return a temporary preview buffer for FILE."
  (let ((key (vertico-buffer-frame--file-preview-cache-key file)))
    (or (vertico-buffer-frame--cached-file-preview-buffer key)
        (let* (preview-cacheable
               (buffer
                (vertico-buffer-frame--text-preview-buffer
                 "file"
                 (lambda ()
                   (setq preview-cacheable
                         (vertico-buffer-frame--insert-file-preview file))))))
          (when (and key preview-cacheable)
            (setq-local vertico-buffer-frame--file-preview-cache
                        (cons key buffer)))
          buffer))))

(defun vertico-buffer-frame--insert-symbol-preview (symbol)
  "Insert a preview for SYMBOL."
  (insert (symbol-name symbol) "\n\n")
  (let ((content-start (point)))
    (when (commandp symbol)
      (condition-case-unless-debug nil
          (progn
            (insert "Command")
            (when-let* ((keys (ignore-errors
                                (where-is-internal symbol nil t)))
                        (description (ignore-errors
                                       (key-description keys))))
              (insert ": " description))
            (insert "\n\n"))
        (error nil)))
    (when (fboundp symbol)
      (when-let* ((doc (ignore-errors (documentation symbol t))))
        (insert (format "%s" doc) "\n\n")))
    (when (boundp symbol)
      (when-let* ((doc (ignore-errors
                         (documentation-property
                          symbol 'variable-documentation t))))
        (insert (format "%s" doc) "\n\n")))
    (when (ignore-errors
            (facep symbol))
      (when-let* ((doc (ignore-errors
                         (documentation-property
                          symbol 'face-documentation t))))
        (insert (format "%s" doc) "\n\n")))
    (when (= (point) content-start)
      (insert "No documentation available.\n"))))

;;;; Preview child-frame display

(defun vertico-buffer-frame--preview-placeholder-buffer ()
  "Return the buffer initially displayed by a preview child frame."
  (let ((buffer (get-buffer-create " *vertico-buffer-frame-preview*")))
    (with-current-buffer buffer
      (setq-local cursor-type nil
                  mode-line-format nil
                  header-line-format nil
                  tab-line-format nil))
    buffer))

(defun vertico-buffer-frame--create-preview-window (parent initial-buffer)
  "Create and return a preview child-frame window under PARENT.
INITIAL-BUFFER, when non-nil, is displayed before the preview target is
installed so the target buffer is not killed during stale frame cleanup."
  (let ((size (vertico-buffer-frame--preview-frame-size
               (vertico-buffer-frame--preview-size-source-frame parent)))
        window
        frame
        success)
    (unwind-protect
        (progn
          (setq window
                (vertico-buffer-frame--display-buffer-in-child-frame
                 (or initial-buffer
                     (vertico-buffer-frame--preview-placeholder-buffer))
                 parent
                 (format "Vertico Preview %s" (minibuffer-depth))
                 size
                 'preview))
          (setq frame (or (vertico-buffer-frame--window-frame window)
                          (error "Preview window did not have a live frame")))
          (set-window-parameter window 'no-other-window t)
          (set-window-parameter window 'no-delete-other-windows t)
          (setq-local vertico-buffer-frame--preview-frame frame
                      vertico-buffer-frame--preview-window window)
          (when (vertico-buffer-frame--place-preview-frame frame parent)
            (setq-local vertico-buffer-frame--preview-layout-state
                        (vertico-buffer-frame--preview-frame-layout-state
                         frame parent size)))
          (setq success t)
          window)
      (unless success
        (vertico-buffer-frame--clear-preview-frame-state)
        (vertico-buffer-frame--delete-frame frame)))))

(defun vertico-buffer-frame--preview-window (&optional buffer)
  "Return the preview window, creating a child frame when needed.
When BUFFER is live, use it as the initial child-frame buffer and preserve it
during stale preview cleanup."
  (let ((parent (vertico-buffer-frame--preview-parent-frame))
        (initial-buffer (and (buffer-live-p buffer) buffer)))
    (unless (vertico-buffer-frame--preview-window-current-p parent)
      (vertico-buffer-frame--delete-frame
       vertico-buffer-frame--preview-frame)
      (vertico-buffer-frame--kill-preview-buffer initial-buffer)
      (vertico-buffer-frame--clear-preview-frame-state)
      (vertico-buffer-frame--create-preview-window parent initial-buffer)))
  (vertico-buffer-frame--refresh-preview-frame)
  vertico-buffer-frame--preview-window)

(defun vertico-buffer-frame--set-preview-window-buffer
    (window buffer &optional old-preview-buffer)
  "Display BUFFER in preview WINDOW and dedicate it afterwards.
When OLD-PREVIEW-BUFFER is live, kill it after BUFFER has replaced it in
WINDOW.
Return non-nil when BUFFER was displayed successfully."
  (condition-case-unless-debug nil
      (progn
        (if (eq (window-buffer window) buffer)
            (unless (window-dedicated-p window)
              (set-window-dedicated-p window t))
          (set-window-dedicated-p window nil)
          (set-window-buffer window buffer)
          (set-window-dedicated-p window t))
        (vertico-buffer-frame--kill-old-preview-buffer
         old-preview-buffer buffer)
        t)
    (error nil)))

(defun vertico-buffer-frame--center-window-point (window)
  "Set WINDOW point to point and center it vertically when possible."
  (condition-case-unless-debug nil
      (let ((point (point)))
        (set-window-point window point)
        (save-excursion
          (forward-line (- (/ (max 1 (window-body-height window)) 2)))
          (set-window-start window (line-beginning-position)))
        t)
    (error nil)))

(defun vertico-buffer-frame--set-window-position (window position)
  "Set WINDOW point to POSITION."
  (condition-case-unless-debug nil
      (when position
        (with-current-buffer (window-buffer window)
          (goto-char (point-min))
          (cond
           ((markerp position)
            (goto-char position))
           ((integerp position)
            (goto-char (max (point-min) (min (point-max) position)))))
          (vertico-buffer-frame--center-window-point window)))
    (error nil)))

(defun vertico-buffer-frame--set-window-line (window line)
  "Set WINDOW point to LINE."
  (condition-case-unless-debug nil
      (when (and (integerp line) (> line 0))
        (with-current-buffer (window-buffer window)
          (goto-char (point-min))
          (forward-line (1- line))
          (vertico-buffer-frame--center-window-point window)))
    (error nil)))

(defun vertico-buffer-frame--set-preview-window-location (window location)
  "Move preview WINDOW to LOCATION when LOCATION specifies one."
  (pcase location
    (`(line . ,line)
     (vertico-buffer-frame--set-window-line window line))
    (`(position . ,position)
     (vertico-buffer-frame--set-window-position window position))))

(defun vertico-buffer-frame--kill-replaced-preview-buffer
    (replacement-buffer new-buffer old-preview-buffer)
  "Kill REPLACEMENT-BUFFER when NEW-BUFFER has superseded it.
OLD-PREVIEW-BUFFER may still be the displayed preview, so leave it alone."
  (when (and replacement-buffer
             (not (eq replacement-buffer new-buffer))
             (not (eq replacement-buffer old-preview-buffer)))
    (vertico-buffer-frame--kill-old-preview-buffer
     replacement-buffer new-buffer)))

(defun vertico-buffer-frame--cleanup-failed-preview-display
    (replacement-buffer old-preview-buffer)
  "Restore preview buffer state after a failed live preview display.
REPLACEMENT-BUFFER is the buffer installed while resolving the new target.
OLD-PREVIEW-BUFFER is the buffer that was visible before the display attempt."
  (when (and replacement-buffer
             (not (eq replacement-buffer old-preview-buffer)))
    (vertico-buffer-frame--kill-old-preview-buffer replacement-buffer nil))
  (when (eq vertico-buffer-frame--preview-buffer replacement-buffer)
    (setq-local vertico-buffer-frame--preview-buffer nil)))

(defun vertico-buffer-frame--display-live-preview-buffer
    (buffer location external-buffer old-preview-buffer)
  "Display live preview BUFFER and keep temporary buffer ownership consistent.
LOCATION may be (line . LINE), (position . POSITION), or nil.  When
EXTERNAL-BUFFER is non-nil, BUFFER belongs to the user and must not be recorded
as an owned temporary preview buffer.  OLD-PREVIEW-BUFFER is the preview buffer
that was visible before resolving this target."
  (let ((replacement-buffer vertico-buffer-frame--preview-buffer)
        success)
    (setq-local vertico-buffer-frame--preview-buffer old-preview-buffer)
    (unwind-protect
        (let ((window (vertico-buffer-frame--preview-window buffer)))
          (unless (vertico-buffer-frame--set-preview-window-buffer
                   window buffer old-preview-buffer)
            (error "Preview window was not usable"))
          (setq-local vertico-buffer-frame--preview-buffer
                      (and (not external-buffer)
                           buffer))
          (vertico-buffer-frame--kill-replaced-preview-buffer
           replacement-buffer buffer old-preview-buffer)
          (vertico-buffer-frame--set-preview-window-location window location)
          (vertico-buffer-frame--show-frame
           vertico-buffer-frame--preview-frame)
          (setq success t))
      (unless success
        (vertico-buffer-frame--cleanup-failed-preview-display
         replacement-buffer old-preview-buffer)))))

(defun vertico-buffer-frame--hide-missing-preview-buffer (old-preview-buffer)
  "Hide the preview after target resolution failed to produce a live buffer.
OLD-PREVIEW-BUFFER is the temporary preview buffer that was visible before the
failed target resolution."
  (when (and vertico-buffer-frame--preview-buffer
             (not (eq vertico-buffer-frame--preview-buffer
                      old-preview-buffer)))
    (vertico-buffer-frame--kill-old-preview-buffer
     vertico-buffer-frame--preview-buffer nil))
  (setq-local vertico-buffer-frame--preview-buffer old-preview-buffer)
  (vertico-buffer-frame--kill-old-preview-buffer old-preview-buffer nil)
  (setq-local vertico-buffer-frame--preview-buffer nil)
  (vertico-buffer-frame--hide-preview))

(defun vertico-buffer-frame--display-preview-buffer
    (buffer &optional location external-buffer old-preview-buffer)
  "Display preview BUFFER in the preview child frame.
LOCATION may be (line . LINE), (position . POSITION), or nil.  When
EXTERNAL-BUFFER is non-nil, do not treat the displayed buffer as an owned
temporary preview buffer.  OLD-PREVIEW-BUFFER is the temporary buffer being
replaced."
  (if (buffer-live-p buffer)
      (vertico-buffer-frame--display-live-preview-buffer
       buffer location external-buffer old-preview-buffer)
    (vertico-buffer-frame--hide-missing-preview-buffer old-preview-buffer)))

(defun vertico-buffer-frame--preview-target-display-spec (target)
  "Return a preview display spec for TARGET.
The result is (BUFFER LOCATION EXTERNAL-BUFFER), where LOCATION may be
(line . LINE), (position . POSITION), or nil."
  (pcase target
    (`(file ,file)
     (list (vertico-buffer-frame--file-preview-buffer file) nil nil))
    (`(file-line ,file ,line)
     (vertico-buffer-frame--file-line-preview-spec file line))
    (`(file-position ,file ,position)
     (vertico-buffer-frame--file-position-preview-spec file position))
    (`(buffer ,name)
     (list (get-buffer name) nil t))
    (`(buffer-line ,buffer ,line)
     (list buffer (cons 'line line) t))
    (`(buffer-position ,buffer ,position)
     (list buffer (cons 'position position) t))
    (`(text ,name ,inserter)
     (list (vertico-buffer-frame--text-preview-buffer name inserter) nil nil))))

(defun vertico-buffer-frame--show-preview (target)
  "Show preview TARGET."
  (let ((old-preview-buffer vertico-buffer-frame--preview-buffer))
    (if-let* ((spec (vertico-buffer-frame--preview-target-display-spec
                     target)))
        (pcase-let ((`(,buffer ,location ,external-buffer) spec))
          (vertico-buffer-frame--display-preview-buffer
           buffer
           location
           external-buffer
           old-preview-buffer))
      (vertico-buffer-frame--hide-preview))))

;;;; Preview refresh scheduling

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
  (condition-case-unless-debug nil
      (when (vertico-buffer-frame--preview-timer-p)
        (cancel-timer vertico-buffer-frame--preview-timer))
    (error nil))
  (setq-local vertico-buffer-frame--preview-timer nil
              vertico-buffer-frame--preview-scheduled-state nil))

(defun vertico-buffer-frame--preview-timer-p ()
  "Return non-nil when the current preview timer is a live timer object."
  (condition-case-unless-debug nil
      (timerp vertico-buffer-frame--preview-timer)
    (error nil)))

(defun vertico-buffer-frame--preview-state ()
  "Return cheap state used to coalesce pending preview refresh timers."
  (list (vertico-buffer-frame--minibuffer-input)
        (point)
        (vertico-buffer-frame--candidate)))

(defun vertico-buffer-frame--hide-preview ()
  "Hide the preview child frame."
  (vertico-buffer-frame--cancel-preview-timer)
  (vertico-buffer-frame--hide-frame
   vertico-buffer-frame--preview-frame)
  (setq-local vertico-buffer-frame--preview-scheduled-state nil))

(defun vertico-buffer-frame--refresh-preview ()
  "Refresh the preview for the current minibuffer buffer."
  (if (and vertico-buffer-frame-mode
           vertico-buffer-frame-preview
           (vertico-buffer-frame--completion-active-p)
           (vertico-buffer-frame--child-frames-supported-p
            (vertico-buffer-frame--preview-parent-frame)))
      (if-let* ((target (vertico-buffer-frame--preview-target)))
          (progn
            (vertico-buffer-frame--show-preview target)
            (setq-local vertico-buffer-frame--preview-last-error-message nil))
        (vertico-buffer-frame--hide-preview))
    (vertico-buffer-frame--hide-preview)))

(defun vertico-buffer-frame--show-preview-later (buffer)
  "Show preview for minibuffer BUFFER after the configured delay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local vertico-buffer-frame--preview-timer nil
                  vertico-buffer-frame--preview-scheduled-state nil)
      (condition-case-unless-debug error
          (while-no-input
            (vertico-buffer-frame--refresh-preview))
        (error
         (vertico-buffer-frame--report-preview-error error))))))

(defun vertico-buffer-frame--schedule-preview (&optional state)
  "Schedule preview display for the current minibuffer buffer.
When STATE matches the pending timer state, keep the existing timer."
  (unless (and state
               (vertico-buffer-frame--preview-timer-p)
               (equal state vertico-buffer-frame--preview-scheduled-state))
    (vertico-buffer-frame--cancel-preview-timer)
    (condition-case-unless-debug error
        (setq-local vertico-buffer-frame--preview-scheduled-state state
                    vertico-buffer-frame--preview-timer
                    (run-with-idle-timer
                     (vertico-buffer-frame--preview-delay)
                     nil
                     #'vertico-buffer-frame--show-preview-later
                     (current-buffer)))
      (error
       (setq-local vertico-buffer-frame--preview-scheduled-state nil
                   vertico-buffer-frame--preview-timer nil)
       (vertico-buffer-frame--report-preview-error error)))))

(defun vertico-buffer-frame--preview-delay ()
  "Return `vertico-buffer-frame-preview-delay' as a nonnegative number."
  (if (numberp vertico-buffer-frame-preview-delay)
      (max 0.0 (float vertico-buffer-frame-preview-delay))
    0.0))

(defun vertico-buffer-frame--preview-post-command ()
  "Schedule a delayed preview refresh after Vertico candidate refresh."
  (condition-case-unless-debug error
      (if (and vertico-buffer-frame-mode
               vertico-buffer-frame-preview
               (vertico-buffer-frame--completion-active-p))
          (vertico-buffer-frame--schedule-preview
           (vertico-buffer-frame--preview-state))
        (vertico-buffer-frame--hide-preview))
    (error
     (vertico-buffer-frame--report-preview-error error))))

(provide 'vertico-buffer-frame-preview)
;;; vertico-buffer-frame-preview.el ends here
