;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest walkman--integration-test ()
  (let ((entry "* Request with callbacks
  POST https://httpbin.org/post
  - Accept: application/json
  #+begin_src json
    {
      \"some\": \"body\"
    }
  #+end_src
  1. First callback
     #+begin_src emacs-lisp
       (lambda (status headers body)
         (should (equal 201 status))
     #+end_src
"))
    (with-temp-buffer
      (insert entry)
      (walkman-at-point))))

;; (ert "walkman--integration-test")

(provide 'walkman-integration)
;;; walkman-integration.el ends here
