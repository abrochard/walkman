;;; walkman.el --- Write HTTP requests in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020, Adrien Brochard

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 1.0
;; Author: Adrien Brochard
;; Keywords: walkman http curl org comm
;; URL: https://github.com/abrochard/walkman
;; License: GNU General Public License >= 3
;; Package-Requires: ((transient "0.1.0") (org "8.3.5") (json-mode "1.6.0") (emacs "26.3"))

;;; Commentary:

;; Write HTTP requests in Org mode and replay them at will using cURL

;;; Setup:

;; To use this package, add following code to your init file.

;;   (with-eval-after-load 'org
;;     (require 'walkman)
;;     (walkman-setup))

;; Or M-x walkman-setup to add the default bindings to org-mode

;;; Usage:

;; C-c C-RETURN   to execute the entry at point
;; C-c C-'        for the menu

;;; Code:

(require 'org)
(require 'org-element)
(require 'transient)
(require 'cl-lib)
(require 'json)
(require 'json-mode)

(defvar walkman-keep-headers nil)

(defvar walkman--tmp-keep-headers nil)
(defvar walkman--tmp-callbacks nil)
(defvar walkman--tmp-skip-callbacks nil)

(defconst walkman--verb-regexp "\\(POST\\|GET\\|PUT\\|DELETE\\)")

(cl-defstruct (walkman-request (:constructor walkman-request--create)
                               (:copier nil))
  url method headers form-headers body callbacks)

(cl-defstruct (walkman-response (:constructor walkman-response--create)
                                (:copier nil))
  code status headers body)

(defun walkman--exec (args)
  "Exec the request.

ARGS is the curl args."
  (let ((buffer "*walkman*"))
    (when (get-buffer buffer)
      (kill-buffer buffer))
    (make-process :name "walkman-proc" :buffer buffer :command (push "curl" args) :sentinel #'walkman--curl-sentinel)))

(defun walkman--curl-sentinel (proc _msg)
  "Process entire output of the curl command."
  (when (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc))
             (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (pop-to-buffer (current-buffer))
      (setq response (walkman--parse-response walkman--tmp-keep-headers))
      (view-mode)
      (let* ((code (walkman-response-code response))
             (headers (walkman-response-headers response))
             (callbacks walkman--tmp-callbacks)
             (body (walkman-response-body response)))
        (message "Response status code: %s" code)
        (unless walkman--tmp-skip-callbacks
          (dolist (fct callbacks)
            (funcall (car (read-from-string fct)) code headers body)))))))

(defun walkman--parse-response (&optional keep-headers)
  "Parse response buffer.

KEEP-HEADERS is a bool to tell wether or not to keep headers"
  (save-excursion
    (goto-char (point-min))
    ;; clean up the buffer
    (save-excursion
      (while (search-forward-regexp "\\\\|\\\\[1m\\|\\\\[0m" (point-max) t)
        (replace-match "")))
    (let ((headers-end (save-excursion (re-search-forward "^$"))))
      (re-search-forward "^HTTP/[0-9]\\.?[0-9]? \\([0-9]\\{3\\}\\) \\([A-Z ]+\\)?")
      (let ((code (string-to-number (match-string 1)))
            (status (match-string 2))
            (headers (walkman--parse-headers headers-end)))
        (goto-char headers-end)
        (forward-char 1)
        (unless (or walkman-keep-headers keep-headers)
          (delete-region (point-min) (point)))
        (dolist (header headers)
          (if (and (string= "content-type" (downcase (car header)))
                   (string-prefix-p "application/json" (cdr header) t))
              (save-excursion
                (json-pretty-print (point) (point-max))
                (json-mode))))
        (walkman-response--create
         :code code :status status :headers headers
         :body (buffer-substring-no-properties (point) (point-max)))))))

(defun walkman--parse-headers (headers-end)
  "Parse headers into list.

HEADERS-END is the end of headers point."
  (let ((headers '()))
    (while (re-search-forward "^\\(.*\\): \\(.*\\)$" headers-end t)
      (push (cons (match-string 1) (match-string 2)) headers))
    (nreverse headers)))

(defun walkman--prefix-list (prefix list quoted)
  "Prefix every element of the LIST with another PREFIX element.

QUOTED indicated if the elements of the list need to be quoted."
  (apply #'append (mapcar (lambda (x) (list prefix (if quoted (format "'%s'" x) x))) list)))

(defun walkman--to-args (req &optional insecure quoted)
  "Parse into curl args.

REQ is the walkman-request struct.
INSECURE is optional flag to make insecure request.
QUOTED is the optional flag to quote or not headers."
  (let ((body (walkman-request-body req)))
    (append (list "--silent" "-i" "-X"
                  (walkman-request-method req)
                  (walkman-request-url req))
            (walkman--prefix-list "-H" (walkman-request-headers req) quoted)
            (walkman--prefix-list "-F" (walkman-request-form-headers req) quoted)
            (if insecure '("-k") '())
            (if body
                (walkman--prefix-list "-d" (list (walkman-request-body req)) quoted)
              '()))))

(defun walkman--eval-and-replace (local-variables)
  "Evaluate Lisp expression and replace with the result.

LOCAL-VARIABLES is the alist of local variables from original buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "`\\(.*?\\)`" nil t)
      (let* ((variable (car (read-from-string (match-string 1))))
             (local-value (assoc-default variable local-variables)))
        (replace-match (format "%s" (or local-value (eval variable))))))))

(defun walkman--org-child (element n)
  "Recursively get N times the first child of ELEMENT."
  (if (equal 0 n)
      element
    (walkman--org-child (car (org-element-contents element)) (1- n))))

(defun walkman--org-text (element)
  "Extract the plain text string of ELEMENT."
  (buffer-substring-no-properties (org-element-property :begin element)
                                  (1- (org-element-property :end element))))

(defun walkman--extract-url (elements)
  "Extract the URL out of an org section parsed into org ELEMENTS."
  (car (org-element-map (walkman--org-child elements 2) 'link
         (lambda (link) (org-element-property :raw-link link)))))

(defun walkman--extract-method (elements)
  "Extract the HTTP method out of an org section parsed into org ELEMENTS."
  (let ((content (walkman--org-text (walkman--org-child elements 2))))
    (when (string-match walkman--verb-regexp content)
      (match-string-no-properties 1 content))))

(defun walkman--extract-headers (elements)
  "Extract the HTTP headers out of an org section parsed into org ELEMENTS."
  (car (org-element-map elements 'plain-list
         (lambda (plain-list)
           (when (and (eq 'section (org-element-type (org-element-property :parent plain-list)))
                      (eq 'unordered (org-element-property :type plain-list)))
             (org-element-map plain-list 'item
               (lambda (item)
                 (walkman--org-text (walkman--org-child item 1)))))))))

(defun walkman--extract-form-headers (elements)
  "Extract the form headers out of an org section parsed into org ELEMENTS."
  (car (org-element-map elements 'drawer
         (lambda (drawer)
           (when (equal "FORM" (org-element-property :drawer-name drawer))
             (org-element-map drawer 'item
               (lambda (item)
                 (let ((link (car (org-element-map item 'link 'identity)))
                       (item (car (org-element-contents item))))
                   (replace-regexp-in-string
                    ": *" "="
                    (if link
                        (concat (buffer-substring-no-properties (org-element-property :begin item)
                                                                (org-element-property :begin link))
                                "@" (org-element-property :path link))
                      (walkman--org-text item)))))))))))

(defun walkman--extract-body (elements)
  "Extract the body out of an org section parsed into org ELEMENTS."
  (car (org-element-map elements 'src-block
         (lambda (src-block)
           (when (eq 'section (org-element-type (org-element-property :parent src-block)))
             (org-remove-indentation (org-element-property :value src-block)))))))


(defun walkman--extract-callbacks (elements)
  "Extract the callbacks out of an org section parsed into org ELEMENTS."
  (org-element-map elements 'src-block
    (lambda (src-block)
      (when (eq 'ordered (org-element-property :type (org-element-property :parent (org-element-property :parent src-block))))
        (org-remove-indentation (org-element-property :value src-block))))))

(defun walkman--current ()
  "Extract current org request."
  (save-excursion
    (org-back-to-heading)
    (let ((el (org-element-at-point)))
      (buffer-substring (org-element-property :contents-begin el)
                        (org-element-property :contents-end el)))))

(defun walkman--parse-request ()
  "Parse current org request."
  (let ((raw (walkman--current))
        (local-variables file-local-variables-alist))
    (with-temp-buffer
      (insert raw)
      (walkman--eval-and-replace local-variables)
      (let ((elements (org-element-parse-buffer)))
        (walkman-request--create :url (walkman--extract-url elements)
                                 :method (walkman--extract-method elements)
                                 :headers (walkman--extract-headers elements)
                                 :form-headers (walkman--extract-form-headers elements)
                                 :body (walkman--extract-body elements)
                                 :callbacks (walkman--extract-callbacks elements))))))

(defun walkman--parse-curl (cmd)
  "Parse a curl command and arguments into an a walkman request structure.

CMD is the curl command string."
  (let ((url "")
        (headers '())
        (body "")
        (method "GET"))
    (with-temp-buffer
      (insert cmd)
      ;; get url
      (goto-char (point-min))
      (re-search-forward "'?\"?\\(https?://[^ '\"]+\\)'?\"?")
      (setq url (match-string 1))
      (replace-match "")

      ;; get headers backwards to preserve the insertion order
      (goto-char (point-max))
      (while (re-search-backward
              "\\(--header\\|-H\\) ['\"]\\([^'\"]+\\)['\"]"
              (point-min) t)
        (push (match-string 2) headers)
        (replace-match ""))

      ;; get method
      (goto-char (point-min))
      (re-search-forward "\\(-X\\|--request\\) \\([^ ]+\\)" (point-max) t)
      (unless (equal "" (match-string 2))
        (setq method (match-string 2))
        (replace-match ""))

      ;; get body
      (goto-char (point-min))
      (re-search-forward "\\(-d\\|--data-raw\\|--data-binary\\) '\\([^\000]*\\)??'" (point-max) t)
      (unless (equal "" (match-string 2))
        (setq body (match-string 2))
        (replace-match "")))

    (walkman-request--create :url url :method method :headers headers :body body)))

(defun walkman--assemble-org (request)
  "Build a walkman org entry from curl parsed request.

REQUEST is a walkman-request struct, result of the walkman parse curl function."
  (let ((output "")
        (body (walkman-request-body request)))
    (setq output (format "* Import Curl\n  %s %s\n" (walkman-request-method request) (walkman-request-url request)))
    (setq output (concat output (mapconcat (lambda (x) (format "  - %s" x)) (walkman-request-headers request) "\n")))
    (unless (equal "" body)
      (setq output (format "%s\n#+begin_src json\n%s\n#+end_src" output body)))
    output))

(defun walkman-curl-to-org (cmd)
  "Hacky function to import a curl command to org walkman entry.

CMD is the curl command string."
  (interactive "MCurl: ")
  (insert (walkman--assemble-org (walkman--parse-curl cmd))))

(defun walkman--assemble-curl (insecure)
  "Assemble curl command.

INSECURE is the flag to add a -k to the command."
  (format "curl %s"
          (mapconcat #'identity (walkman--to-args (walkman--parse-request) insecure t) " ")))

(defun walkman-copy-as-curl (&optional args)
  "Copy current org request as curl request.

ARGS is the arg list from transient."
  (interactive
   (list (transient-args 'walkman-transient)))
  (kill-new (walkman--assemble-curl (member "-k" args)))
  (message "Copied to kill ring"))

(defun walkman-at-point (&optional args)
  "Execute request at point.

ARGS is the arg list from transient."
  (interactive
   (list (transient-args 'walkman-transient)))
  (let* ((req (walkman--parse-request))
         (callbacks (walkman-request-callbacks req)))
    (setq walkman--tmp-callbacks callbacks)
    (setq walkman--tmp-skip-callbacks (member "--skip" args))
    (setq walkman--tmp-keep-headers (member "--verbose" args))
    (walkman--exec (walkman--to-args req))))

(defun walkman-execute-buffer ()
  "Execute all requests in a buffer."
  (interactive)
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (hl)
      (goto-char (org-element-property :begin hl))
      (walkman-at-point))))

(transient-define-prefix walkman-transient ()
  "Walkman Menu"
  ["Arguments"
   ("-k" "Insecure" "-k")
   ("-s" "Skip callbacks" "--skip")
   ("-v" "Verbose response with headers" "--verbose")]
  ["Actions"
   ("x" "Execute" walkman-at-point)
   ("c" "Copy as curl" walkman-copy-as-curl)
   ("i" "Import curl command" walkman-curl-to-org)])

;;;###autoload
(defun walkman-setup ()
  "Add the walkman bindings to org mode map."
  (interactive)
  (define-key org-mode-map (kbd "C-c C-'") #'walkman-transient)
  (define-key org-mode-map (kbd "C-c <C-return>") #'walkman-at-point)
  (message "Activated walkman"))

(provide 'walkman)
;;; walkman.el ends here
