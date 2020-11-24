(require 'package)

(defconst make-packages
  '(org transient json-mode))

(defun make-init ()
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(defun make-install-packages ()
  (make-init)
  (package-refresh-contents)
  (package-install 'package-lint)
  (dolist (pkg make-packages)
    (package-install pkg)))

(defun make-ert ()
  (make-init)
  (load-file "/walkman/walkman.el")
  (load-file "/walkman/test/walkman-test.el")
  (ert-run-tests-batch-and-exit))

(defun make-integration ()
  (make-init)
  (load-file "/walkman/walkman.el")
  (load-file "/walkman/test/walkman-integration.el")
  (ert-run-tests-batch-and-exit))

(defun make-compile ()
  (make-init)
  (setq byte-compile-error-on-warn nil)
  (batch-byte-compile))

(defun make-lint ()
  (make-init)
  (require 'package-lint)
  (setq package-lint-batch-fail-on-warnings t)
  (package-lint-batch-and-exit))

(provide 'make)
;;; make.el ends here
