#lang racket/base

(require racket/string
         racket/system)

(require "logger.rkt")

(provide make-thumbnail)

(define (make-thumbnail source destination [width 200])
  (let ((stdout-ob    (open-output-bytes))
        (stderr-ob    (open-output-bytes)))
    (parameterize ((current-output-port stdout-ob)
                   (current-error-port  stderr-ob))
      (unless (system* (find-executable-path "convert")
                       (path->string source)
                       "-filter" "Catrom"
                       "-resize" (string-append (number->string width) "x")
                       "-unsharp" "0x.6+0.5+0"
                       (path->string destination))
        (info "Make thumbnail failed for ~s~n" source)
        (debug "STDOUT:")
        (debug "~s~n" (string-split
                       (string-replace (bytes->string/locale stdout-ob) "\t" "    ")
                       "\n"))
        (debug "STDERR:")
        (debug "~s~n" (string-split
                       (string-replace (bytes->string/locale stderr-ob) "\t" "    ")
                       "\n"))
        (raise-user-error 'images "resizing image ~s failed.~n" source)))))
