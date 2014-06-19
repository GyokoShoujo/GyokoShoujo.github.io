#lang racket/base

(require "logger.rkt")

(provide render-template)

(define (render-template src-path dest-path variable-hash)
  (debug "Rendering ~s to ~s~n" src-path dest-path)    
  (call-with-output-file* dest-path
                          (λ (port) 
                            (define render 
                              (dynamic-require src-path 'render))
                            (render variable-hash port)) 
                          #:mode 'text #:exists 'error))
