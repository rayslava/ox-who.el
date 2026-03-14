;;; ox-who-test.el --- org-mode who export tests

;;; Commentary:

;; Run standalone with this,
;;   emacs -batch -L . -l ox-who-test.el -f ert-run-tests-batch

;;; Code:

(setq load-prefer-newer t)

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'ox-who)

(defconst ox-who-test-root
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Root directory for ox-who tests.")

(defun ox-who-load-file-str (filepath)
  "Return FILEPATH file content as string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filepath ox-who-test-root))
    (org-trim (buffer-string))))

(defun ox-who-test--fixture-basenames ()
  "Return sorted fixture basenames from the test directory."
  (sort
   (mapcar #'file-name-sans-extension
           (directory-files (expand-file-name "test" ox-who-test-root) nil "\\.org\\'"))
   #'string<))

(defun ox-who-test--assert-fixture-pair (basename)
  "Export test fixture BASENAME.org and compare with BASENAME.lisp."
  (let* ((org-path (expand-file-name (format "test/%s.org" basename) ox-who-test-root))
         (lisp-path (expand-file-name (format "test/%s.lisp" basename) ox-who-test-root)))
    (should (file-exists-p org-path))
    (should (file-exists-p lisp-path))
    (with-current-buffer (find-file-noselect org-path)
      (let ((code (org-export-as 'who)))
        (should (string-equal
                 (ox-who-load-file-str (format "test/%s.lisp" basename))
                 (org-trim code)))))))

(defun ox-who-test--parse-first (text type)
  "Parse TEXT as Org and return the first element or object of TYPE."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (org-element-map (org-element-parse-buffer) type #'identity nil t)))

(defun ox-who-test--info (&rest pairs)
  "Return an export info plist from PAIRS with sane WHO defaults."
  (append pairs
          '(:html-extension "html"
            :with-todo-keywords t
            :with-tags t
            :with-priority t
            :with-smart-quotes nil
            :with-special-strings nil
            :preserve-breaks nil)))

(defun ox-who-testcover--stats ()
  "Return test coverage stats for the current `edebug-form-data'."
  (let ((total 0)
        (uncovered 0)
        rows)
    (dolist (x edebug-form-data)
      (let* ((sym (car x))
             (data (get sym 'edebug))
             (coverage (get sym 'edebug-coverage))
             (points (nth 2 data))
             (len (if points (length points) 0))
             (miss 0))
        (when (and data coverage points)
          (dotimes (i len)
            (let ((entry (aref coverage i)))
              (cl-incf total)
              (when (and (not (eq entry 'edebug-ok-coverage))
                         (not (memq (car-safe entry)
                                    '(testcover-1value maybe noreturn))))
                (cl-incf uncovered)
                (cl-incf miss))))
          (when (> miss 0)
            (push (cons sym miss) rows)))))
    (list :total total
          :uncovered uncovered
          :covered (- total uncovered)
          :rows (sort rows (lambda (a b) (> (cdr a) (cdr b)))))))

(defun ox-who-testcover--report (stats)
  "Return a printable coverage report string for STATS."
  (let* ((total (plist-get stats :total))
         (covered (plist-get stats :covered))
         (uncovered (plist-get stats :uncovered))
         (percent (if (zerop total)
                      100.0
                    (* 100.0 (/ (float covered) total))))
         (rows (plist-get stats :rows)))
    (concat
     (format "Coverage summary: %.1f%% (%d/%d)\n" percent covered total)
     (format "Uncovered forms: %d\n" uncovered)
     (if rows
         (concat
          "Top uncovered definitions:\n"
          (mapconcat
           (lambda (row)
             (format "  %s %d" (car row) (cdr row)))
           rows
           "\n")
          "\n")
       ""))))

(defun ox-who-batch-testcover-report ()
  "Run the ERT suite under `testcover' and print a coverage report."
  (interactive)
  (require 'testcover)
  (let ((default-directory ox-who-test-root)
        (target (expand-file-name "ox-who.el" ox-who-test-root)))
    (unload-feature 'ox-who t)
    (setq edebug-form-data nil)
    (testcover-start target)
    (require 'ox-who)
    (let ((stats (ert-run-tests-batch nil)))
      (princ "\n")
      (princ (ox-who-testcover--report (ox-who-testcover--stats)))
      (when (and noninteractive
                 (> (ert-stats-completed-unexpected stats) 0))
        (kill-emacs 1)))))

(ert-deftest ox-who-export-fixtures ()
  "Export every paired .org/.lisp fixture in the test directory."
  (dolist (basename (ox-who-test--fixture-basenames))
    (ox-who-test--assert-fixture-pair basename)))

(ert-deftest ox-who-export-lists-and-tables ()
  "Export lists and tables as WHO forms."
  (with-temp-buffer
    (org-mode)
    (insert "- first\n- [X] second\n\n| h |\n|---|\n| c |\n")
    (let ((code (org-trim (org-export-as 'who))))
      (should (string-match-p
               (regexp-quote "(:ul")
               code))
      (should (string-match-p
               (regexp-quote "(:li \"first\")")
               code))
      (should (string-match-p
               (regexp-quote "(:li \"[X] \" \"second\")")
               code))
      (should (string-match-p
               (regexp-quote "(:table")
               code))
      (should (string-match-p
               (regexp-quote "(:tr (:th \"h\"))")
               code))
      (should (string-match-p
               (regexp-quote "(:tr (:td \"c\"))")
               code)))))

(ert-deftest ox-who-export-extended-org-syntax ()
  "Export more Org syntax without leaking raw HTML."
  (with-temp-buffer
    (org-mode)
    (insert "* S\n+strike+ H_{2}O x^2\n\n#+begin_quote\nq\n#+end_quote\n\n#+begin_verse\nroses\nare red\n#+end_verse\n\n#+begin_center\nmid\n#+end_center\n")
    (let ((code (org-trim (org-export-as 'who))))
      (should-not (string-match-p (regexp-quote "<div") code))
      (should-not (string-match-p (regexp-quote "<del>") code))
      (should-not (string-match-p (regexp-quote "<sub>") code))
      (should-not (string-match-p (regexp-quote "<sup>") code))
      (should (string-match-p (regexp-quote "(:h1 \"S\")") code))
      (should (string-match-p (regexp-quote "(:s \"strike\")") code))
      (should (string-match-p (regexp-quote "\"H\"(:sub \"2\")\"O x\"(:sup \"2\")") code))
      (should (string-match-p (regexp-quote "(:blockquote") code))
      (should (string-match-p (regexp-quote "(:pre :class \"verse\" \"roses\nare red\")") code))
      (should (string-match-p (regexp-quote "(:div :style \"text-align:center;\"") code))
      (should (string-match-p (regexp-quote "(:p \"mid\n\")") code)))))

(ert-deftest ox-who-transcoder-primitives ()
  "Exercise direct translator branches that do not need full export."
  (let* ((info (ox-who-test--info :with-special-strings t :preserve-breaks t))
         (code '(verbatim (:value "code")))
         (src-block '(src-block (:language "emacs-lisp" :value "(+ 1 2)\n")))
         (fixed-width '(fixed-width (:value "sample text\n")))
         (entity (ox-who-test--parse-first "\\alpha" 'entity))
         (latex '(latex-fragment (:value "\\(x^2\\)")))
         (table-cell (ox-who-test--parse-first "| h |\n|---|\n| c |\n" 'table-cell))
         (table-row (ox-who-test--parse-first "| h |\n|---|\n| c |\n" 'table-row)))
    (should (equal (ox-who--unwrap-paragraph " (:p \"x\") ") "\"x\""))
    (should (equal (ox-who--trim-quoted-text "\"x\n\n\"") "\"x\""))
    (should (equal (ox-who-bold nil "\"x\"" nil) " (:b \"x\")"))
    (should (equal (ox-who-italic nil "\"x\"" nil) " (:i \"x\")"))
    (should (equal (ox-who-underline nil "\"x\"" nil) " (:u \"x\")"))
    (should (equal (ox-who-strike-through nil "\"x\"" nil) " (:s \"x\")"))
    (should (equal (ox-who-subscript nil "\"2\"" nil) "(:sub \"2\")"))
    (should (equal (ox-who-superscript nil "\"2\"" nil) "(:sup \"2\")"))
    (should (equal (ox-who-line-break nil nil nil) "  \\\\ "))
    (should (equal (ox-who-horizontal-rule nil nil nil) "(:hr)"))
    (should (equal (ox-who-center-block nil "\"x\"" nil)
                   "(:div :style \"text-align:center;\" \"x\")"))
    (should (equal (ox-who-special-block
                    (ox-who-test--parse-first "#+begin_note\nhi\n#+end_note\n" 'special-block)
                    "\"x\"" nil)
                   "(:div :class \"note\" \"x\")"))
    (should (equal (ox-who-template "\"x\"" nil) "\"x\""))
    (should (equal (ox-who-inner-template "\"x\"" nil) "\"x\""))
    (should (equal (ox-who-section nil "\"x\"" nil) "\"x\""))
    (should (equal (ox-who-entity entity nil nil) "\"α\""))
    (should (equal (ox-who-latex-fragment latex nil nil) "\"\\(x^2\\)\""))
    (should (equal (ox-who-fixed-width fixed-width nil nil) " (:tt \"sample text\")"))
    (should (equal (ox-who-code code nil nil) " (:code \"code\")"))
    (should (equal (ox-who-src-block src-block nil (ox-who-test--info))
                   "(:code :lang \"emacs-lisp\" \"(+ 1 2)\")"))
    (let ((ox-who-org-verbatim 'verbatim))
      (should (equal (ox-who-verbatim code nil nil) "%% code %%")))
    (let ((ox-who-org-verbatim 'monospace))
      (should (equal (ox-who-verbatim code nil nil) " (:tt \"code\")")))
    (should (equal (ox-who-verse-block nil "\"roses\n\"" nil)
                   "(:pre :class \"verse\" \"roses\")"))
    (should (equal (ox-who-table nil "(:tr x)" nil)
                   "(:table\n(:tr x))"))
    (should (equal (ox-who-table-row nil "\"cells\"" nil)
                   "(:tr \"cells\")"))
    (should-not (ox-who-table-row nil "" nil))
    (should (equal (ox-who-table-cell table-cell "\"h\"" (ox-who-test--info))
                   "(:th \"h\")"))
    (should (equal (ox-who-plain-text "#\n![x]*_\\\\ --" info)
                   "\"#  \n\\\\![x]\\*\\_\\\\\\\\ --\""))))

(ert-deftest ox-who-link-branches ()
  "Cover the major `ox-who-link' branches directly."
  (let ((info (ox-who-test--info))
        (id-link (ox-who-test--parse-first "[[id:doc]]" 'link))
        (img-link (ox-who-test--parse-first "[[file:img.png]]" 'link))
        (coderef-link (ox-who-test--parse-first "[[(r1)]]" 'link))
        (fuzzy-link (ox-who-test--parse-first "[[*Headline]]" 'link))
        (http-link (ox-who-test--parse-first "[[http://example.com]]" 'link))
        (https-link (ox-who-test--parse-first "[[https://example.com][d]]" 'link))
        (org-file-link (ox-who-test--parse-first "[[file:notes.org]]" 'link))
        (mail-link (ox-who-test--parse-first "[[mailto:a@example.com][m]]" 'link)))
    (cl-letf (((symbol-function 'org-export-resolve-id-link)
               (lambda (&rest _) "doc.org")))
      (should (equal (ox-who-link id-link nil info) "<doc.html>"))
      (should (equal (ox-who-link id-link "\"desc\"" info)
                     "[[doc.html|\"desc\"]]")))
    (cl-letf (((symbol-function 'org-export-resolve-id-link)
               (lambda (&rest _) 'headline-dest))
              ((symbol-function 'org-export-get-headline-number)
               (lambda (&rest _) '(2 3)))
              ((symbol-function 'org-export-translate)
               (lambda (_backend _lang _info) "See section %s")))
      (should (equal (ox-who-link id-link "\"See\"" info)
                     "\"See\" #See section 2.3")))
    (cl-letf (((symbol-function 'org-export-inline-image-p)
               (lambda (&rest _) t))
              ((symbol-function 'org-export-get-parent-element)
               (lambda (&rest _) 'parent))
              ((symbol-function 'org-export-get-caption)
               (lambda (&rest _) 'caption))
              ((symbol-function 'org-export-data)
               (lambda (value _info)
                 (if (eq value 'caption) "cap" value))))
      (should (equal (ox-who-link img-link nil info)
                     "{{img.png|cap}}")))
    (cl-letf (((symbol-function 'org-export-inline-image-p)
               (lambda (&rest _) nil))
              ((symbol-function 'org-export-get-coderef-format)
               (lambda (_ref _contents) "<%s>"))
              ((symbol-function 'org-export-resolve-coderef)
               (lambda (&rest _) 17)))
      (should (equal (ox-who-link coderef-link "\"d\"" info)
                     "<17>")))
    (cl-letf (((symbol-function 'org-export-inline-image-p)
               (lambda (&rest _) nil))
              ((symbol-function 'org-export-resolve-radio-link)
               (lambda (&rest _) 'radio-dest))
              ((symbol-function 'org-element-contents)
               (lambda (_obj) '("radio text")))
              ((symbol-function 'org-export-data)
               (lambda (value _info) (if (listp value) "radio text" value))))
      (let ((radio-link (copy-tree fuzzy-link)))
        (setf (org-element-property :type radio-link) "radio")
        (should (equal (ox-who-link radio-link nil info) "radio text"))))
    (cl-letf (((symbol-function 'org-export-inline-image-p)
               (lambda (&rest _) nil))
              ((symbol-function 'org-export-resolve-fuzzy-link)
               (lambda (&rest _) 'fuzzy))
              ((symbol-function 'org-export-get-ordinal)
               (lambda (&rest _) '(4 2))))
      (should (equal (ox-who-link fuzzy-link nil info) "4.2"))
      (should (equal (ox-who-link fuzzy-link "\"desc\"" info) "\"desc\"")))
    (cl-letf (((symbol-function 'org-export-inline-image-p)
               (lambda (&rest _) nil)))
      (should (equal (ox-who-link http-link nil info)
                     "http://example.com"))
      (should (equal (ox-who-link https-link "\"d\"" info)
                     " (:a :href \"https://example.com\" \"d\")"))
      (should (equal (ox-who-link org-file-link nil info)
                     "notes.html"))
      (let ((abs-link (copy-tree img-link)))
        (setf (org-element-property :path abs-link)
              (expand-file-name "README.org" ox-who-test-root))
        (should (string-prefix-p "file://" (ox-who-link abs-link nil info))))
      (should (equal (ox-who-link mail-link "\"m\"" info)
                     " (:a :href \"a@example.com\" \"m\")")))))

(ert-deftest ox-who-export-headlines-and-lists ()
  "Export headlines and list variants from Org text."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO [#A] Title :tag1:tag2:\nBody\n\n- [ ] bullet\n- term :: body\n")
    (let ((code (org-trim (org-export-as 'who))))
      (should (string-match-p (regexp-quote "(:h1 \"TODO\" \"Title\")") code))
      (should (string-match-p (regexp-quote "(:p \"tag1, tag2\")") code))
      (should (string-match-p (regexp-quote "(:p \"Body\n\")") code))
      (should (string-match-p (regexp-quote "(:li \"[ ] \" \"bullet\")") code))))
  (with-temp-buffer
    (org-mode)
    (insert "1. one\n2. two\n")
    (let ((code (org-trim (org-export-as 'who))))
      (should (string-match-p (regexp-quote "(:ol") code))
      (should (string-match-p (regexp-quote "(:li \"one\")") code))
      (should (string-match-p (regexp-quote "(:li \"two\")") code))))
  (cl-letf (((symbol-function 'org-export-get-relative-level) (lambda (&rest _) 7))
            ((symbol-function 'org-export-low-level-p) (lambda (&rest _) 7))
            ((symbol-function 'org-export-numbered-headline-p) (lambda (&rest _) nil)))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO [#A] Title :tag1:tag2:\nBody\n")
      (let ((code (org-trim (org-export-as 'who))))
        (should (string-match-p (regexp-quote "* ") code))
        (should-not (string-match-p (regexp-quote "*\"") code))))))

(ert-deftest ox-who-export-helper-branches ()
  "Cover the interactive export helper wrappers."
  (let ((buffer-called nil)
        (file-called nil)
        (replace-called nil)
        (opened-file nil))
    (cl-letf (((symbol-function 'org-export-to-buffer)
               (lambda (backend name async subtreep visible-only body-only ext-plist post-fn)
                 (setq buffer-called (list backend name async subtreep visible-only body-only ext-plist))
                 (with-current-buffer (get-buffer-create name)
                   (insert "x")
                   (funcall post-fn))
                 (get-buffer name)))
              ((symbol-function 'org-export-to-file)
               (lambda (backend outfile async subtreep visible-only body-only ext-plist post-fn)
                 (setq file-called (list backend outfile async subtreep visible-only body-only ext-plist))
                 (with-temp-file outfile (insert "x"))
                 (funcall post-fn outfile)
                 outfile))
              ((symbol-function 'org-export-output-file-name)
               (lambda (_ext _subtreep) (expand-file-name "out.lisp" temporary-file-directory)))
              ((symbol-function 'org-export-replace-region-by)
               (lambda (backend) (setq replace-called backend)))
              ((symbol-function 'org-open-file)
               (lambda (path) (setq opened-file path)))
              ((symbol-function 'indent-region)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (org-mode)
        (insert "* hi")
        (ox-who-export-as-who t t t t '(:x 1))
        (should (equal buffer-called '(who "*Org WHO Export*" t t t t (:x 1))))
        (should (get-buffer "*Org WHO Export*"))
        (let ((file (ox-who-export-to-lisp nil t nil t '(:y 2))))
          (should (equal file-called
                         (list 'who file nil t nil t '(:y 2))))
          (should (file-exists-p file))
          (delete-file file))
        (set-mark (point-min))
        (goto-char (point-max))
        (activate-mark)
        (ox-who-convert-region-to-who)
        (should (eq replace-called 'who))))
    (let* ((menu (org-export-backend-menu (org-export-get-backend 'who)))
           (open-entry (nth 2 (caddr menu)))
           (fn (nth 2 open-entry)))
      (cl-letf (((symbol-function 'ox-who-export-to-lisp)
                 (lambda (&rest _) "/tmp/out.lisp"))
                ((symbol-function 'org-open-file)
                 (lambda (path) (setq opened-file path))))
        (funcall fn nil nil nil nil)
        (should (equal opened-file "/tmp/out.lisp"))))))
