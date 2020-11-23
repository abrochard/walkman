;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest walkman--test-parse-response ()
  (let ((response (with-temp-buffer
                    (insert-file-contents "test/sample-response")
                    (walkman--parse-response))))
    (should (equal (walkman-response-code response) 200))
    (should (equal (walkman-response-status response) "OK"))
    (should (equal (walkman-response-headers response) '(("date" . "Thu, 30 Jan 2020 21:34:59 GMT")
                                                         ("content-type" . "application/json")
                                                         ("content-length" . "602")
                                                         ("server" . "gunicorn/19.9.0")
                                                         ("access-control-allow-origin" . "*")
                                                         ("access-control-allow-credentials" . "true"))))
    (should (equal (walkman-response-body response)
                   "{
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
"))))

(ert-deftest walkman--test-prefix-list ()
  (let ((l '("header1: value1" "header2: value2")))
    (should (equal '("-H" "header1: value1" "-H" "header2: value2") (walkman--prefix-list "-H" l nil)))
    (should (equal '("-H" "'header1: value1'" "-H" "'header2: value2'") (walkman--prefix-list "-H" l t)))
    (should (equal '("-F" "header1: value1" "-F" "header2: value2") (walkman--prefix-list "-F" l nil)))))

(ert-deftest walkman--test-to-args ()
  (let ((input (walkman-request--create :method "PUT" :url "localhost" :headers '("Content-Type: application/json") :body "some body"))
        (output '("--silent" "-i" "-X" "PUT" "localhost" "-H" "'Content-Type: application/json'" "-d" "'some body'")))
    (should (equal output (walkman--to-args input nil t)))))

(ert-deftest walkman--test-eval-and-replace ()
  (let ((test (lambda (input local-variables expected)
                (should (equal expected (with-temp-buffer
                                          (insert input)
                                          (walkman--eval-and-replace local-variables)
                                          (buffer-string)))))))
    (funcall test "abc" '() "abc")
    (funcall test "`emacs-version`" '() emacs-version)
    (funcall test "`some-local-var`" '((some-local-var . "value")) "value")
    (funcall test "`(+ 1 2)`" '() "3")
    (funcall test "`(funcall (lambda () (- 500 100)))`" '() "400")
    (funcall test "`(version)`" '() (version))
    (funcall test "(version)" '() "(version)")
    ;; should handle multiple replacements in the same string
    (funcall test "`emacs-version`-`emacs-version`" '() (format "%s-%s" emacs-version emacs-version))))

(ert-deftest walkman--test-parse-request ()
  (let ((request (with-temp-buffer
                   (insert-file-contents "test/sample-request")
                   (walkman--parse-request))))
    (should (equal (walkman-request-method request) "POST"))
    (should (equal (walkman-request-url request) "https://httpbin.org/post"))
    (should (equal (walkman-request-headers request)
                   '("Accept: application/json" "Content-Type: application/json")))
    (should (equal (walkman-request-form-headers request) '("type=document" "file=@/home/user/sample.jpg")))
    (should (equal (walkman-request-body request) "{
  \"data\": {
    \"title\": \"Hello\",
    \"hello\": \"world\"
  }
}
"))
    (should (not (walkman-request-callbacks request)))))

(ert-deftest walkman--test-parse-curl ()
  (let ((tests '(
                 ;; chrome curl export
                 ((:input . "curl 'http://httpbin.org/get' -H 'pragma: no-cache' -H 'cache-control: no-cache' -H 'dnt: 1' -H 'upgrade-insecure-requests: 1' --compressed")
                  (:method . "GET") (:url . "http://httpbin.org/get") (:body . "")
                  (:headers . ("pragma: no-cache" "cache-control: no-cache" "dnt: 1" "upgrade-insecure-requests: 1")))


                 ;; postman export
                 ((:input . "curl -X POST \
  http://httpbin.org/post \
  -H 'Postman-Token: 2ce7ad4d-11c9-442a-a380-59a62af2bc0c' \
  -H 'cache-control: no-cache' \
  -H 'key: value' \
  -d 'some body'")
                  (:method . "POST") (:url . "http://httpbin.org/post") (:body . "some body")
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
                  (:method . "POST") (:url . "https://test.com/chat")
                  (:headers . ("Authorization: XXXXX" "Content-Type: application/json")) (:body . "{
	\"id\":\"unique-id\",
	\"recipient\":\"+1XXXXXXXXXX\",
	\"content\":\"hello world\"
}"))
                 )))
    (dolist (test tests)
      (let ((data (walkman--parse-curl (assoc-default :input test))))
        (should (equal (assoc-default :method test) (walkman-request-method data)))
        (should (equal (assoc-default :url test) (walkman-request-url data)))
        (should (equal (assoc-default :body test) (walkman-request-body data)))
        (should (equal (assoc-default :headers test) (walkman-request-headers data)))))))

(ert-deftest walkman--test-assemble-org ()
  (let ((input (walkman-request--create
                :url "http://httpbin.org/get" :method "GET" :body ""
                :headers '("pragma: no-cache" "cache-control: no-cache" "dnt: 1" "upgrade-insecure-requests: 1")))
        (output "* Import Curl
  GET http://httpbin.org/get
  - pragma: no-cache
  - cache-control: no-cache
  - dnt: 1
  - upgrade-insecure-requests: 1"))
    (should (equal output (walkman--assemble-org input)))))

(ert-deftest walkman--test-assemble-curl ()
  (flet ((walkman--parse-request ()
                                    (walkman-request--create :url "http://localhost/post"
                                                             :method "POST" :body "some body")))
    (should (equal "curl --silent -i -X POST http://localhost/post -d 'some body'"
                   (walkman--assemble-curl nil)))
    ))

(ert-deftest walkman--test-at-point ()
  (flet ((walkman--exec (args &optional keep-headers)
                        (walkman-response--create :code 200 :status "OK" :headers '() :body "")))
    (with-temp-buffer
      (insert-file-contents "test/sample-request")
      (should (not (walkman-at-point))))))

;; (ert "walkman--test-.*")

(provide 'walkman-test)
;;; walkman-test.el ends here
