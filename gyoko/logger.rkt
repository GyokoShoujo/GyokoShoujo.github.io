#lang racket

(provide log-level
         MESSAGE
         INFO
         DEBUG
         logger
         log-lines
         message
         info
         debug)

#|
 log-level determines how noisy to be
     Verbosity 0: only calls to message are displayed
     Verbosity 1: message and info calls are displayed
     Verbosity 2: message, info, and debug calls are displayed
 |#
(define log-level (make-parameter 2)) ;;todo: should be 0
(define MESSAGE 0)
(define INFO 1)
(define DEBUG 2)
(define (logger level . params)
  (when (<= level (log-level))
    (apply printf params)))
(define (log-lines level lines)
  (when (not (zero? (length lines)))
      (begin
        (logger level "~s~n" (car lines))
        (log-lines level (cdr lines)))))
      
(define (message msg . params) (apply logger MESSAGE msg params))
(define (info    msg . params) (apply logger INFO msg params))
(define (debug   msg . params) (apply logger DEBUG msg params))


