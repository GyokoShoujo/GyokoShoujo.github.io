#lang racket/base

(require "logger.rkt")

(provide render-template)

(define template-cache (make-hash))


(define (render-template src-path dest-path variable-hash)
  (debug "Rendering ~s to ~s~n" src-path dest-path)
  (unless (hash-has-key? template-cache src-path)
    (hash-set! template-cache src-path (dynamic-require src-path 'render)))
  (call-with-output-file* dest-path
                          (λ (port)
                            (let ((render (hash-ref template-cache src-path)))
                              (render variable-hash port)))
                          #:mode 'text #:exists 'error))
