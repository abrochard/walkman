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
;; Package-Requires: ((transient "0.1.0") (org "8.3.5") (emacs "25.3"))

;;; Commentary:

;; Write HTTP requests in Org mode and replay them at will using cURL

;;; Setup:

;; M-x walkman-mode to add the default bindings to org-mode

;;; Usage:

;; C-c C-RETURN   to execute the entry at point
;; C-c C-'        for the menu

;;; Code:

(require 'org)
(require 'org-element)
(require 'transient)

(defvar walkman-keep-headers nil)

(defconst walkman--verb-regexp "\\(POST\\|GET\\|PUT\\|DELETE\\)")

(defun walkman--exec (args &optional keep-headers)
  "Exec the request.

ARGS is the curl args.
KEEP-HEADERS is a bool to tell wether or not to keep headers."
  (let ((buffer "*walkman*"))
    (when (get-buffer buffer)
      (kill-buffer buffer))
    (apply #'call-process "curl" nil buffer nil args)
    (pop-to-buffer buffer)
    (walkman--parse-response keep-headers)))

(defun walkman--parse-response (&optional keep-headers)
  "Parse response buffer.

KEEP-HEADERS is a bool to tell wether or not to keep headers"
  (save-excursion
    (goto-char (point-min))
    ;; clean up the buffer
    (save-excursion
      (while (search-forward "" (point-max) t)
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
        (list (cons :code code) (cons :status status)
              (cons :headers headers)
              (cons :body (buffer-substring-no-properties (point) (point-max))))))))

(defun walkman--parse-headers (headers-end)
  "Parse headers into list.

HEADERS-END is the end of headers point."
  (let ((headers '()))
    (while (re-search-forward "^\\(.*\\): \\(.*\\)$" headers-end t)
      (push (cons (match-string 1) (match-string 2)) headers))
    (reverse headers)))

(defun walkman--to-args (args &optional insecure)
  "Parse into curl args.

ARGS is the arg list.
INSECURE is optional flag to make insecure request."
  (let ((verb (cdr (assoc :verb args)))
        (host (cdr (assoc :host args)))
        (headers (cdr (assoc :headers args)))
        (body (cdr (assoc :body args))))
    (append (list "--silent" "-i" "-X" verb host) headers
            (if insecure (list "-k") '())
            (if body (list "-d" body) '()))))

(defun walkman--eval-and-replace ()
  "Evaluate Lisp expression and replace with the result."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "`\\(.*\\)`" nil t)
      (replace-match
       (format "%s" (eval (car (read-from-string (match-string 1)))))))))

(defun walkman--extract-verb ()
  "Extract HTTP verb."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward walkman--verb-regexp)
    (match-string 1)))

(defun walkman--extract-host ()
  "Extract HTTP host."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^ *%s \\(.*\\)$" walkman--verb-regexp))
    (match-string 2)))

(defun walkman--extract-headers (&optional with-quote)
  "Extract HTTP headers.

WITH-QUOTE indicates that header values need to be quoted."
  (save-excursion
    (goto-char (point-min))
    (let ((headers '()))
      (while (re-search-forward "^ *- \\(.+:.+\\)$" (point-max) t)
        (if with-quote
            (push (format "'%s'" (match-string 1)) headers)
          (push (match-string 1) headers))
        (push "-H" headers))
      headers)))

(defun walkman--extract-body ()
  "Extract body."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat
                        ;; (1) indentation                 (2) lang
                        "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
                        ;; (3) switches
                        "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
                        ;; (4) header arguments
                        "\\([^\n]*\\)\n"
                        ;; (5) body
                        "\\([^\000]*?\n\\)??[ \t]*#\\+end_src") nil t)
    (match-string 5)))

(defun walkman--extract-callbacks ()
  "Extract callbacks."
  (save-excursion
    (goto-char (point-min))
    (let ((callbacks '()))
      (while (re-search-forward
              "[0-9]+\..*\n[ \t]*#\\+begin_src emacs-lisp\n\\([^\000]*?\n\\)??[ \t]*#\\+end_src"
              (point-max) t)
        (push (match-string-no-properties 1) callbacks))
      (reverse callbacks))))

(defun walkman--current ()
  "Extract current org request."
  (save-excursion
    (org-back-to-heading)
    (let ((el (org-element-at-point)))
      (buffer-substring (org-element-property :contents-begin el)
                        (org-element-property :contents-end el)))))

(defun walkman--parse-request (&optional with-quote)
  "Parse current org request.

WITH-QUOTE indicates that header values need to be quoted."
  (let ((raw (walkman--current)))
    (with-temp-buffer
      (insert raw)
      (walkman--eval-and-replace)
      (let ((verb (walkman--extract-verb))
            (host (walkman--extract-host))
            (headers (walkman--extract-headers with-quote))
            (body (walkman--extract-body))
            (callbacks (walkman--extract-callbacks)))
        (list (cons :verb verb) (cons :host host)
              (cons :headers headers) (cons :body body)
              (cons :callbacks callbacks))))))

(defun walkman--parse-curl (cmd)
  "Parse a curl command and arguments into an a walkman request structure.

CMD is the curl command string."
  (let ((host "")
        (headers '())
        (body "")
        (verb "GET"))
    (with-temp-buffer
      (insert cmd)
      ;; get host
      (goto-char (point-min))
      (re-search-forward "'?\"?\\(https?://[^ '\"]+\\)'?\"?")
      (setq host (match-string 1))
      (replace-match "")

      ;; get headers backwards to preserve the insertion order
      (goto-char (point-max))
      (while (re-search-backward
              "\\(--header\\|-H\\) ['\"]\\([^'\"]+\\)['\"]"
              (point-min) t)
        (push (match-string 2) headers)
        (replace-match ""))

      ;; get verb
      (goto-char (point-min))
      (re-search-forward "\\(-X\\|--request\\) \\([^ ]+\\)" (point-max) t)
      (unless (equal "" (match-string 2))
        (setq verb (match-string 2))
        (replace-match ""))

      ;; get body
      (goto-char (point-min))
      (re-search-forward "\\(-d\\|--data-raw\\|--data-binary\\) '\\([^\000]*\\)??'" (point-max) t)
      (unless (equal "" (match-string 2))
        (setq body (match-string 2))
        (replace-match "")))

    (list (cons :host host) (cons :headers headers)
          (cons :verb verb) (cons :body body))))

(defun walkman--assemble-org (data)
  "Build a walkman org entry from curl parsed data.

DATA is the result of the walkman parse curl function."
  (let ((output ""))
    (setq output (format "* Import Curl\n  %s %s\n" (assoc-default :verb data) (assoc-default :host data)))
    (setq output (concat output (mapconcat (lambda (x) (format "  - %s" x)) (assoc-default :headers data) "\n")))
    (unless (equal "" (assoc-default :body data))
      (setq output (format "%s\n#+begin_src json\n%s\n#+end_src" output (assoc-default :body data))))
    output))

(defun walkman-curl-to-org (cmd)
  "Hacky function to import a curl command to org walkman entry.

CMD is the curl command string."
  (interactive "MCurl: ")
  (insert (walkman--assemble-org (walkman--parse-curl cmd))))

(defun walkman-copy-as-curl (&optional args)
  "Copy current org request as curl request.

ARGS is the arg list from transient."
  (interactive
   (list (transient-args 'walkman-transient)))
  (kill-new
   (format "curl %s"
           (mapconcat (lambda (x)
                        (if (string-match "\n" x)
                            (format "'%s'" x)
                          x))
                      (walkman--to-args (walkman--parse-request t) (member "-k" args)) " ")))
  (message "Copied to kill ring"))

(defun walkman-at-point (&optional args)
  "Execute request at point.

ARGS is the arg list from transient."
  (interactive
   (list (transient-args 'walkman-transient)))
  (let* ((req (walkman--parse-request))
         (callbacks (assoc :callbacks req))
         (res (walkman--exec (walkman--to-args req (member "-k" args))
                             (member "--verbose" args)))
         (code (cdr (assoc :code res)))
         (headers (cdr (assoc :headers res)))
         (body (cdr (assoc :body res))))
    (message "Response status code: %s" code)
    (unless (member "--skip" args)
      (dolist (fct (cdr callbacks))
        (funcall (car (read-from-string fct)) code headers body)))))

(defun walkman-execute-buffer ()
  "Execute all requests in a buffer."
  (interactive)
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (hl)
      (goto-char (org-element-property :begin hl))
      (walkman-at-point))))

(define-transient-command walkman-transient ()
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
(defun walkman-mode ()
  "Add the walkman bindings to org mode map."
  (interactive)
  (define-key org-mode-map (kbd "C-c C-'") #'walkman-transient)
  (define-key org-mode-map (kbd "C-c <C-return>") #'walkman-at-point)
  (message "Activated walkman"))

(provide 'walkman)
;;; walkman.el ends here
