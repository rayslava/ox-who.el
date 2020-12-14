;;; ox-who-test.el --- org-mode who export tests

;;; Commentary:

;; Run standalone with this,
;;   emacs -batch -L . -l ox-who-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'org)
(require 'ox-who)

(defun ox-who-load-file-str (filepath)
  "Return FILEPATH file content as string."
  (with-temp-buffer
    (insert-file-contents filepath)
    (org-trim (buffer-string))))

(ert-deftest ox-who-export ()
  "Test the validate function."
  (with-current-buffer (find-file-noselect "test/test.org")
    (let ((code (org-export-as 'who)))
      (should
       (string-equal
        (ox-who-load-file-str "test.lisp")
        (org-trim code))))))
