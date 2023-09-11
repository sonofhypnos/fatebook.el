;;; test-fatebook.el --- Description -*- lexical-binding: t; -*-

;;; FIXME I haven't figured out how to write tests for asynchronous functions.
;;; Evaluating the following expressions and looking at the messages buffer should help
;;; with debugging though.


(let ((fatebook-api-key-function (lambda () "")) ;;empty api-key should inform user how to set it.
      (fatebook-debug t))
 (fatebook--api-call "test" "2050-01-01" 0.5))
(let ((fatebook-api-key-function nil)
      (fatebook-debug t))
 (fatebook--api-call "test" "2050-01-100" 0.5)) ;;impossible date leads to 500 error
(let ((fatebook-api-key-function nil)
      (fatebook-debug t))
 (fatebook--api-call "test" "2050-01-01" 0.5)) ;;should create regular question
