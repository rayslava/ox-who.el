;;; ox-who.el --- With-Html-Output Back-End for Org Export Engine  -*- lexical-binding: t; -*-


;; Copyright (C) 2020-2023 Viacheslav Barinov <rayslava@gmail.com>

;; Author: Viacheslav Barinov <rayslava@gmail.com>
;; Keywords: org, who
;; URL: https://github.com/rayslava/ox-who.el
;; Package-Requires: ((emacs "24.4") (org "8.3"))
;; Keywords: Org, who, docs
;; Version: 0.1

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements `with-html-output' back-end for Org exporter, based
;; on `html' back-end and `ox-wk' by Vilibald Wanča <vilibald@wvi.cz>
;;
;; It provides two commands for export, depending on the desired
;; output: `ox-who-export-as-who' (temporary buffer) and
;; `ox-who-export-to-lisp' ("lisp" file).

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ox-html)

;;; User-Configurable Variables

(defgroup org-export-who nil
  "Options specific to WHO export back-end."
  :tag "Org Export WHO"
  :group 'org-export
  :version "24.3"
  :package-version '(Org . "8.3"))

(defcustom ox-who-org-verbatim 'monospace
  "Style used to format = and ~ markups in org file.
I haven't figured out yet how to distinguish these but prefer to use monospace.
This variable can be set to either `monospace' or `verbatim'."
  :group 'org-export-wiki
  :type '(choice
          (const :tag "Use \"Monospace\" markup" monospace)
          (const :tag "Use \"Verbatim\" markup" verbatim)))

(defcustom ox-who-coding-system 'utf-8
  "Coding system for wiki export.
Use utf-8 as the default value."
  :group 'org-export-wiki
  :type 'coding-system)

;;; Define Back-End

(org-export-define-derived-backend 'who 'html
  :menu-entry
  '(?w "Export to WHO"
       ((?W "To temporary buffer" ox-who-export-as-who)
        (?w "To file" ox-who-export-to-lisp)
        (?o "To file and open"
            (lambda (a s v b)
              (if a (ox-who-export-to-lisp t s v)
                (org-open-file (ox-who-export-to-wiki nil s v)))))))
  :translate-alist '((bold . ox-who-bold)
                     (code . ox-who-code)
                     (src-block . ox-who-src-block)
                     (comment . (lambda (&rest args) ""))
                     (comment-block . (lambda (&rest args) ""))
                     (example-block . ox-who-src-block)
                     (fixed-width . ox-who-fixed-width)
                     (footnote-definition . ignore)
                     (footnote-reference . ignore)
                     (headline . ox-who-headline)
                     (horizontal-rule . ox-who-horizontal-rule)
                     (inline-src-block . ox-who-code)
                     (italic . ox-who-italic)
                     (underline . ox-who-underline)
                     (item . ox-who-item)
                     (line-break . ox-who-line-break)
                     (link . ox-who-link)
                     (table . ox-who-table)
                     (table-cell . ox-who-table-cell)
                     (table-row . ox-who-table-row)
                     (paragraph . ox-who-paragraph)
                     (plain-list . ox-who-plain-list)
                     (plain-text . ox-who-plain-text)
                     (quote-block . ox-who-quote-block)
                     (section . ox-who-section)
                     (template . ox-who-template)
                     (verbatim . ox-who-verbatim)
                     ))

;;; Filters

;;; Transcode Functions

;;;; Bold

(defun ox-who-bold (_bold contents _info)
  "Transcode BOLD object.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format " (:b %s)" (string-trim contents)))

;;;; Underline

(defun ox-who-underline (_underline contents _info)
  "Transcode UNDERLINE object.
CONTENTS is the text within underline markup.  INFO is a plist used as
a communication channel."
  (format " (:u %s)" contents))

;;;; Fixed width

(defun ox-who-fixed-width (fixed-width _contents info)
  "Transcode FIXED-WIDTH element.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format " (:tt %s)" contents))

;;;; Code and Verbatim

(defun ox-who-code (code _contents info)
  "Transcode CODE object.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value code))
        (lang (org-element-property :language code)))
    (concat " (:code " (if lang (format ":lang \"%s\" " lang) "") (format "\"%s\")" (string-trim value)))))

(defun ox-who-verbatim (verbatim contents info)
  "Transcode VERBATIM object.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (cond
     ((eq ox-who-org-verbatim 'monospace)(ox-who-fixed-width verbatim contents info))
     (t (format "%%%% %s %%%%" value)))))

;;;; Src-Block

(defun ox-who-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (org-element-property :language src-block))
        (content (org-export-format-code-default src-block info)))
    (concat "(:code " (if lang (format ":lang \"%s\" " lang) "") (format "\"%s\")" (string-trim content)))))

;;;; Headline

(defun ox-who-headline (headline contents info)
  "Transcode HEADLINE element.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                                        headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (format "(:p \"%s\")\n"
                                     (mapconcat 'identity tag-list ", "))))))
           (priority
            (and (plist-get info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
           (heading (concat todo priority title)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((and (org-export-low-level-p headline info)
                 (> (org-export-low-level-p headline info) 6))
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "*" "-" )))
          (concat "  " bullet heading tags "\n\n"
                  (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t (format "(:h%s %s)\n%s%s" level
		  heading
		  (if tags
		      tags
		    "")
		  contents))))))

;;;; Horizontal Rule

(defun ox-who-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode HORIZONTAL-RULE element.
CONTENTS is the horizontal rule contents, none is actually used.
INFO is a plist used as a communication channel."
  "(:hr)")

;;;; Italic

(defun ox-who-italic (_italic contents _info)
  "Transcode ITALIC object.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format " (:i %s)" contents))

;;;; Item

(defun ox-who-item (item contents info)
  "Transcode ITEM element.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (bullet (if (eq ox-who-style 'creole)
                     (if (eq type 'ordered) "#" "*" )
                   (if (eq type 'ordered) "-" "*" )))
         (_counter (org-element-property :counter item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info))))
         (level
          ;; Determine level of current item to determine the
          ;; correct indentation or number of bullets to use.
          (let ((parent item) (level 0))
            (while (memq (org-element-type
                          (setq parent (org-export-get-parent parent)))
                         '(plain-list item))
              (when (eq (org-element-type parent) 'plain-list)
                (cl-incf level)))
            level))
         (prefix (if (eq ox-who-style 'creole) (if (eq type 'ordered)?# ?*) ? )))
    (concat
     (if (eq ox-who-style 'doku) (make-string (* 2 level) prefix )
       (make-string (1- level) prefix))
     bullet " "
     (cl-case checkbox
       (cl-on "[X] ")
       (cl-trans "[-] ")
       (cl-off "[ ] "))
     (and tag (format "**%s:** "(org-export-data tag info)))
     (and contents (org-trim contents)))))

;;;; Line Break

(defun ox-who-line-break (_line-break _contents _info)
  "Transcode LINE-BREAK object.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  \\\\ ")

;;;; Link

(defun ox-who-link (link contents info)
  "Transcode a LINK object from Org to HTML.
CONTENTS is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See `org-export-data'."
  (let ((--link-org-files-as-html-maybe
         (function
          (lambda (raw-path info)
            ;; Treat links to `file.org' as links to `file.html', if
            ;; needed.  See `org-html-link-org-files-as-html'.
            (cond
             ((and org-html-link-org-files-as-html
                   (string= ".org"
                            (downcase (file-name-extension raw-path "."))))
              (concat (file-name-sans-extension raw-path) "."
                      (plist-get info :html-extension)))
             (t raw-path)))))
        (type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
           (let ((destination (org-export-resolve-id-link link info)))
             (if (stringp destination)  ; External file.
                 (let ((path (funcall --link-org-files-as-html-maybe
                                      destination info)))
                   (if (not contents) (format "<%s>" path)
                     (format "[[%s|%s]]" path contents)))
               (concat
                (and contents (concat contents " "))
                (format "#%s"
                        (format (org-export-translate "See section %s" :html info)
                                (mapconcat 'number-to-string
                                           (org-export-get-headline-number
                                            destination info)
                                           ".")))))))
          ((org-export-inline-image-p link org-html-inline-image-rules)
           (let ((path (let ((raw-path (org-element-property :path link)))
                         (if (not (file-name-absolute-p raw-path)) raw-path
                           (expand-file-name raw-path)))))
             (format "{{%s|%s}}" path
                     (let ((caption (org-export-get-caption
                                     (org-export-get-parent-element link))))
                       (if caption (org-export-data caption info) path))
                     )))
          ((string= type "coderef")
           (let ((ref (org-element-property :path link)))
             (format (org-export-get-coderef-format ref contents)
                     (org-export-resolve-coderef ref info))))
          ((equal type "radio")
           (let ((destination (org-export-resolve-radio-link link info)))
             (org-export-data (org-element-contents destination) info)))
          ((equal type "fuzzy")
           (let ((destination (org-export-resolve-fuzzy-link link info)))
             (if (org-string-nw-p contents) contents
               (when destination
                 (let ((number (org-export-get-ordinal destination info)))
                   (when number
                     (if (atom number) (number-to-string number)
                       (mapconcat 'number-to-string number "."))))))))
          (t (let* ((raw-path (org-element-property :path link))
                    (path (cond
                           ((member type '("http" "https" "ftp"))
                            (concat type ":" raw-path))
                           ((equal type "file")
                            ;; Treat links to ".org" files as ".html",
                            ;; if needed.
                            (setq raw-path
                                  (funcall --link-org-files-as-html-maybe
                                           raw-path info))
                            ;; If file path is absolute, prepend it
                            ;; with protocol component - "file://".
                            (if (not (file-name-absolute-p raw-path)) raw-path
                              (concat "file://" (expand-file-name raw-path))))
                           (t raw-path))))
               (if (not contents) (format "%s" path)
                 (format " (:a :href \"%s\" %s)" path contents)))))))

;;;; Paragraph

(defun ox-who-paragraph (paragraph contents _info)
  "Transcode PARAGRAPH element.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (format "(:p %s)" (string-trim contents)))

;;;; Plain List

(defun ox-who-plain-list (_plain-list contents _info)
  "Transcode PLAIN-LIST element.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)

;;;; Plain Text

(defun ox-who-plain-text (text info)
  "Transcode a TEXT string.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `ox-who-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Protect `, *, _ and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  (format "\"%s\"" text))

;;;; Quote Block

(defun ox-who-quote-block (_quote-block contents _info)
  "Transcode QUOTE-BLOCK element.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
    (format "(:blockquote \n%s)"
	  contents))

;;;; Section

(defun ox-who-section (_section contents _info)
  "Transcode SECTION element.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

;;;; Template

(defun ox-who-template (contents _info)
  "Return complete document string after conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)

;;;; Table

(defun ox-who-table (_table contents _info)
  "Transcode TABLE element.
CONTENTS is the table contents.  INFO is a plist used
as a communication channel."
  contents)

(defun ox-who-table-row  (table-row contents info)
  "Transcode TABLE-ROW element.
CONTENTS is the row contents.  INFO is a plist used
as a communication channel."
  (cond
   ((eq ox-who-style 'creole)
    (concat
     (if (org-string-nw-p contents) (format "|%s" contents) "")))
   (t (concat
       (if (org-string-nw-p contents) (format "%s" contents)
         "")
       (when (org-export-table-row-ends-header-p table-row info)
         "^")))))

(defun ox-who-table-cell  (table-cell contents info)
  "Transcode TABLE-CELL element.
CONTENTS is the table-cell contents.  INFO is a plist used
as a communication channel.  Treat Header cells differently.
FIXME : support also row header cells, now headers are in columns only"
  (let ((table-row (org-export-get-parent table-cell)))
    (cond
     ((org-export-table-row-starts-header-p table-row info)
      (if (eq ox-who-style 'doku)(concat "^ " contents)
        (format "=%s|" contents)))
     ((org-export-table-cell-starts-colgroup-p table-cell info)
      (if (eq ox-who-style 'doku) (concat "|" contents "|")
        (format "%s|" contents)))
     (t (concat contents "|")))))

;;; Interactive function

;;;###autoload

(defun ox-who-export-as-who
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Wiki buffer.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Export is done in a buffer named \"*Org Wiki Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'who "*Org WHO Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t)))
    (indent-region (point-min) (point-max) nil))

;;;###autoload
(defun ox-who-convert-region-to-who ()
  "Assume current region has ‘org-mode’ syntax and convert it to Wiki syntax.
This can be used in any buffer.  For example, you can write an
itemized list in ‘org-mode’ syntax in a Wiki sytntax buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'who))

;;;###autoload
(defun ox-who-export-to-lisp
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Wiki sytntax text file.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".lisp" subtreep))
        (org-export-coding-system ox-who-coding-system))
    (org-export-to-file 'who outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file)
	(with-temp-buffer
	  (interactive)
	  (insert-file-contents-literally file)
          (indent-region (point-min) (point-max) nil)
	  (write-region (point-min) (point-max) file))))))

(provide 'ox-who)

;;; ox-who.el ends here
