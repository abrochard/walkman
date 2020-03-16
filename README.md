[![MELPA](https://melpa.org/packages/walkman-badge.svg)](https://melpa.org/#/walkman)

# Walkman

Write HTTP requests in Org mode and replay them at will using cURL

![walkman demo gif](walkman.gif)

## Features
  * write HTTP requests as org mode "walkman entries"
  * execute walkman entries via curl
  * export walkman entries to a curl command
  * import curl command to walkman entries (beta)
  * support lisp variable or functions in a walkman entry
  * execute a series of lisp callbacks, passing the status code, headers, and response body

## Installation
Load up the `walkman.el` file.

## Usage
By default, after calling  `M-x walkman-mode`, these bindings will be added to org-mode:
```
C-c C-RETURN   to execute the entry at point
C-c C-'        for the walkman menu
C-c C-' c      to copy the entry at point as a curl command
C-c C-' i      to import a curl command and insert as walkman entry
```

## How to write a walkman entry
See the [sample.org](sample.org) file for example of walkman entries.

The general structure is
```org
* Request Title
  GET/POST/PUT/... URL
  - Header1: value
  - Header2: value
  :FORM:
  - type: document
  - file: [[/home/user/document.jpg]]
  :END:
  #+begin_src
    {
      "body": "in any mode/format"
    }
  #+end_src
  1. First Callback
     #+begin_src emacs-lisp
       (lambda (status headers body)
         (message "status %s, headers %s, body %s" status headers body))
     #+end_src
  2. Second Callback
     #+begin_src emacs-lisp
       (lambda (status headers body)
         (message "Second callback"))
     #+end_src
```
Note that only the HTTP action and URL are required, everything else is up to you.

### Simple GET request
```org
* Simple GET request
  GET https://httpbin.org/get
```

### Simple POST request with JSON body
```org
* Simple POST request
  POST https://httpbin.org/post
  - Accept: application/json
  - Content-Type: application/json
  #+begin_src json
    {
      "data": {
        "title": "Hello",
        "hello": "world"
      }
    }
  #+end_src
```

### Multipart upload
You can upload multipart document using the `:FORM:` Org drawer syntax:
```org
* Multi part
  POST https://httpbin.org/post
  - Accept: application/json
  :FORM:
  - type: document
  - file: [[/home/username/sample_document.jpg]]
  :END:
```
will result in
```shell
curl --silent -i -X POST https://httpbin.org/post -F 'file=@/home/username/sample_document.jpg' -F 'type=document' -H 'Accept: application/json'
```
Things to know:
1. Headers inside the org drawer `:FORM:` will be set with the curl `-F` flag for form
2. org links to file will get their path prefixed with a `@`
3. `:FORM:` headers must be **AFTER** regular headers for the parser to work properly

This is an example that **will not** work:
```org
* Wrong multi part
  POST https://httpbin.org/post
  :FORM:
  - file: [[/home/username/sample_document.jpg]]
  :END:
  - Accept: application/json
```
because the `Accept` header will not be parsed correctly.


### Request with lisp variable
Define `my-http-status` with
```emacs-lisp
(setq my-http-status "400")
```
and run
```org
* Embedded lisp variable
  GET https://httpbin.org/status/`my-http-status`
```

### Request with callbacks
```org
* Request with callbacks
  POST https://httpbin.org/post
  #+begin_src json
    {
      "some": "body"
    }
  #+end_src
  1. First callback
     #+begin_src emacs-lisp
       (lambda (status headers body)
         (message "status %s, headers %s, body %s" status headers body))
     #+end_src
  2. Second callback
     #+begin_src emacs-lisp
       (lambda (status headers body)
         (pp (assoc 'url (json-read-from-string body))))
     #+end_src
```

## Customization

### Always keep the headers

If you don't want to bother with the `-v` flag to keep the headers in the response buffer, you can do
```
(setq walkman-keep-headers t)
```

### Custom key bindings

By default, running `walkman-mode` will run
```
(define-key org-mode-map (kbd "C-c C-'") #'walkman-transient)
(define-key org-mode-map (kbd "C-c <C-return>") #'walkman-at-point)
```

If you want to setup your own binding, don't run `walkman-mode` and instead bind
  * `walkman-at-point` for quick execution under the cursor
  * `walkman-transient` for the transient-based interactive menu

## TODO
  * insert response in org doc?
  * execute all requests sequentially
  * unit tests
  * option to run async
  * multiple backends

## Similar projects
  * [restclient](https://github.com/pashky/restclient.el)
  * [verb](https://github.com/federicotdn/verb)
