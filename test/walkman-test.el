;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest walkman--test-parse-curl ()
  (let ((tests '(
                 ;; chrome curl export
                 ((:input . "curl 'http://httpbin.org/get' -H 'pragma: no-cache' -H 'cache-control: no-cache' -H 'dnt: 1' -H 'upgrade-insecure-requests: 1' --compressed")
                  (:verb . "GET") (:host . "http://httpbin.org/get") (:body . "")
                  (:headers . ("pragma: no-cache" "cache-control: no-cache" "dnt: 1" "upgrade-insecure-requests: 1")))


                 ;; postman export
                 ((:input . "curl -X POST \
  http://httpbin.org/post \
  -H 'Postman-Token: 2ce7ad4d-11c9-442a-a380-59a62af2bc0c' \
  -H 'cache-control: no-cache' \
  -H 'key: value' \
  -d 'some body'")
                  (:verb . "POST") (:host . "http://httpbin.org/post") (:body . "some body")
                  (:headers . ("Postman-Token: 2ce7ad4d-11c9-442a-a380-59a62af2bc0c" "cache-control: no-cache" "key: value")))

                 ;; verbose postman export
                 ((:input . "curl --location --request POST 'https://test.com/chat' \
--header \"Authorization: XXXXX\" \
--header 'Content-Type: application/json' \
--data-raw '{
	\"id\":\"unique-id\",
	\"recipient\":\"+1XXXXXXXXXX\",
	\"content\":\"hello world\"
}'")
                  (:verb . "POST") (:host . "https://test.com/chat") (:body . "{
	\"id\":\"unique-id\",
	\"recipient\":\"+1XXXXXXXXXX\",
	\"content\":\"hello world\"
}")
                  (:headers . ("Authorization: XXXXX" "Content-Type: application/json")))
                 )))
    (dolist (test tests)
      (let ((data (walkman--parse-curl (assoc-default :input test))))
        (should (equal (assoc-default :verb test) (assoc-default :verb data)))
        (should (equal (assoc-default :host test) (assoc-default :host data)))
        (should (equal (assoc-default :body test) (assoc-default :body data)))
        (should (equal (assoc-default :headers test) (assoc-default :headers data)))))))

(ert-deftest walkman--test-assemble-org ()
  (let ((input '((:host . "http://httpbin.org/get") (:verb . "GET") (:body . "")
                 (:headers . ("pragma: no-cache" "cache-control: no-cache" "dnt: 1" "upgrade-insecure-requests: 1"))))
        (output "* Import Curl
  GET http://httpbin.org/get
  - pragma: no-cache
  - cache-control: no-cache
  - dnt: 1
  - upgrade-insecure-requests: 1"))
    (should (equal output (walkman--assemble-org input)))))


(ert-deftest walkman--test-to-args ()
  (let ((input '((:verb . "PUT") (:host . "localhost") (:headers . ("-H 'Content-Type: application/json'")) (:body . "some body")))
        (output '("--silent" "-i" "-X" "PUT" "localhost" "-H 'Content-Type: application/json'" "-d" "some body")))
    (should (equal output (walkman--to-args input)))))

(ert-deftest walkman--test-parse-response ()
  (let ((response (with-temp-buffer
                    (insert-file-contents "sample-response")
                    (walkman--parse-response))))
    (should (equal response
                   '((:code . 200) (:status . "OK")
                     (:headers . (("date" . "Thu, 30 Jan 2020 21:34:59 GMT")
                                  ("content-type" . "application/json")
                                  ("content-length" . "602")
                                  ("server" . "gunicorn/19.9.0")
                                  ("access-control-allow-origin" . "*")
                                  ("access-control-allow-credentials" . "true")))
                     (:body . "{
  \"args\": {},
  \"data\": \"\",
  \"files\": {},
  \"headers\": {
    \"Accept\": \"*/*\",
    \"Content-Length\": \"87\",
    \"Host\": \"httpbin.org\",
    \"User-Agent\": \"curl/7.54.0\",
  },
  \"json\": null,
  \"origin\": \"3.93.254.153\",
  \"url\": \"https://httpbin.org/post\"
}
"))))))

(ert-deftest walkman--test-parse-request ()
  (let ((request (with-temp-buffer
                   (insert-file-contents "sample-request")
                   (walkman--parse-request))))
    (should (equal request
                   '((:verb . "POST") (:host . "https://httpbin.org/post")
                     (:headers . ("-F" "file=@/home/user/sample.jpg" "-F" "type=document"
                                  "-H" "Content-Type: application/json" "-H" "Accept: application/json"))
                     (:body . "    {
      \"data\": {
        \"title\": \"Hello\",
        \"hello\": \"world\"
      }
    }
") (:callbacks . ()))))))

(ert-deftest walkman--test-at-point ()
  (flet ((walkman--exec (args &optional keep-headers)
                        '((:code . 200) (:status . "OK") (:headers . ()) (:body . ""))))
    (with-temp-buffer
      (insert-file-contents "sample-request")
      (should (not (walkman-at-point))))))


(ert-deftest walkman--test-eval-and-replace ()
  (let ((tests '(
                 ((:input . "`(+ 1 2)`")
                  (:output . "3"))
                 ((:input . "`(funcall (lambda () (- 500 100)))`")
                  (:output . "400"))
                 ((:input . "`local-var`")
                  (:output . "XXX"))
                 )))
    (dolist (test tests)
      (with-temp-buffer
        (insert (assoc-default :input test))
        (walkman--eval-and-replace '((local-var . "XXX")))
        (should (equal (assoc-default :output test) (buffer-string)))))))

(ert-deftest walkman--test-format-headers ()
  (let ((tests '(
                 ((:k . "Auth") (:v . "Bearer") (:q . nil) (:f . nil)
                  (:output . "Auth: Bearer"))
                 ((:k . "Auth") (:v . "Bearer") (:q . t) (:f . nil)
                  (:output . "'Auth: Bearer'"))
                 ((:k . "type") (:v . "document") (:q . nil) (:f . t)
                  (:output . "type=document"))
                 ((:k . "type") (:v . "document") (:q . t) (:f . t)
                  (:output . "'type=document'"))
                 ((:k . "file") (:v . "[[/home/user/sample_document.jpg]]") (:q . t) (:f . t)
                  (:output . "'file=@/home/user/sample_document.jpg'"))
                 )))
    (dolist (test tests)
      (should (equal (assoc-default :output test)
                     (walkman--format-headers
                      (assoc-default :k test)
                      (assoc-default :v test)
                      (assoc-default :q test)
                      (assoc-default :f test)))))))


;; (ert "walkman--test-.*")

(provide 'walkman-test)
;;; walkman-test.el ends here
