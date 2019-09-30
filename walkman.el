;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-element)
(require 'transient)

(defvar walkman-keep-headers nil)

(defconst walkman--verb-regexp "\\(POST\\|GET\\|PUT\\|DELETE\\)")

(defun walkman--exec (args)
  "Exec the request.

ARGS is the curl args."
  (let ((buffer "*walkman*"))
    (when (get-buffer buffer)
      (kill-buffer buffer))
    (apply #'call-process "curl" nil buffer nil args)
    (pop-to-buffer buffer)
    (walkman--parse-response)))

(defun walkman--parse-response ()
  "Parse response buffer."
  (let ((code nil)
        (status nil)
        (headers nil))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9]\\.[0-9] \\([0-9]\\{3\\}\\) \\([A-Z ]+\\)")
      (setq code (string-to-number (match-string 1)))
      (setq status (match-string 2))
      (setq headers (walkman--parse-headers)))
    (re-search-backward "\n")
    (forward-char 2)
    (unless walkman-keep-headers
      (delete-region (point-min) (point)))
    (list (cons :code code) (cons :status status)
          (cons :headers headers)
          (cons :body (buffer-substring-no-properties (point) (point-max))))))

(defun walkman--parse-headers ()
  "Parse headers into list."
  (let ((headers '()))
    (while (re-search-forward "^\\(.*\\): \\(.*\\)$" (point-max) t)
      (push (cons (match-string 1) (match-string 2)) headers))
    headers))

(defun walkman--to-args (args)
  "Parse into curl args.

ARGS is the arg list."
  (let ((verb (cdr (assoc :verb args)))
        (host (cdr (assoc :host args)))
        (headers (cdr (assoc :headers args)))
        (body (cdr (assoc :body args))))
    (append (list "--silent" "-i" "-X" verb host) headers (if body (list "-d" body) '()))))

(defun walkman--eval-and-replace ()
  "Evaluate Lisp expression and replace with the result."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "`\\(.*\\)`" nil t)
      (replace-match (eval (car (read-from-string (match-string 1))))))))

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

(defun walkman--extract-headers ()
  "Extract HTTP headers."
  (save-excursion
    (goto-char (point-min))
    (let ((headers '()))
      (while (re-search-forward "^ *- \\(.+:.+\\)$" (point-max) t)
        (push (match-string 1) headers)
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

(defun walkman--parse-request ()
  "Parse current org request."
  (let ((raw (walkman--current)))
    (with-temp-buffer
      (insert raw)
      (walkman--eval-and-replace)
      (let ((verb (walkman--extract-verb))
            (host (walkman--extract-host))
            (headers (walkman--extract-headers))
            (body (walkman--extract-body))
            (callbacks (walkman--extract-callbacks)))
        (list (cons :verb verb) (cons :host host)
              (cons :headers headers) (cons :body body)
              (cons :callbacks callbacks))))))

(defun walkman-curl-to-org (cmd)
  "Hacky function to import a curl command to org walkman entry.

CMD is the curl command string."
  (interactive "MCurl: ")
  (insert (with-temp-buffer
            (insert cmd)
            (setq output "* Import Curl\n")
            (goto-char (point-min))
            (re-search-forward "-X \\([^ ]+\\)")
            (if (match-string 1)
                (setq output (format "%s  %s " output (upcase (match-string 1))))
              (setq output (format "%s GET " output)))
            (goto-char (point-min))
            (re-search-forward "\\(https?://[^ ]+\\)")
            (setq output (concat output (match-string 1) "\n"))
            (goto-char (point-min))
            (while (re-search-forward "-H '\\([^']+\\)" (point-max) t)
              (setq output (format "%s  - %s\n" output (match-string 1))))
            (goto-char (point-min))
            (re-search-forward "-d '\\([^\000]*\\)??'")
            (if (match-string 1)
                (setq output (format "%s  #+begin_src json\n%s\n  #+end_src" output (match-string 1))))
            output)))

(defun walkman-copy-as-curl ()
  "Copy current org request as curl request."
  (interactive)
  (kill-new
   (format "curl %s"
           (mapconcat (lambda (x)
                        (if (string-match "\n" x)
                            (format "'%s'" x)
                          x))
                      (walkman--to-args (walkman--parse-request)) " ")))
  (message "Copied to kill ring"))

(defun walkman-at-point (&optional no-callbacks)
  "Execute request at point.

NO-CALLBACKS disables callbacks."
  (interactive)
  (let* ((req (walkman--parse-request))
         (callbacks (assoc :callbacks req))
         (res (walkman--exec (walkman--to-args req)))
         (code (cdr (assoc :code res)))
         (headers (cdr (assoc :headers res)))
         (body (cdr (assoc :body res))))
    (unless no-callbacks
      (dolist (fct (cdr (assoc :callbacks req)))
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
   ("-s" "Skip callbacks" "--skip")]
  ["Actions"
   ("x" "Execute" walkman-at-point)
   ("c" "Copy as curl" walkman-copy-as-curl)])

(defvar walkman-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-'") 'walkman-transient)
    (define-key map (kbd "C-c <C-return>") 'walkman-at-point)
    map))

(define-minor-mode walkman-mode
  "Walkman mode"
  :lighter " Walkman"
  :group 'walkman
  :init-value t)

(provide 'walkman)
;;; walkman.el ends here
