#lang racket

(require racket)

(provide git)

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


(define (clean-output bytes)
  (string-split 
   (string-replace (bytes->string/locale bytes) "\t" "    ") 
   "\n"))

;; local-git is our current repository. It is the source used to pull
;; from.
(define local-git (find-system-path 'run-file))
;; working-git is the git repo that will be cloned into and that the
;; site will actually be generated in.
(define build-git (build-path (find-system-path 'temp-dir) 
                              (string-append "gyo" (~a (current-seconds)))))

;; Parameter that determines where (git) works.
(define working-git (make-parameter local-git))

(define (git command arg-list)
  (let ((full-args (if (> (log-level) 1) 
                       (remove "--quiet" arg-list)
                       arg-list))
        (stdout-ob (open-output-bytes))
        (stderr-ob (open-output-bytes))
        (stdin-ib  (open-input-bytes #"")))
    
    (let* ((exit-code (parameterize ((current-output-port stdout-ob)
                                     (current-error-port  stderr-ob)
                                     (current-input-port  stdin-ib))
                        (apply system*/exit-code 
                               (find-executable-path "git") 
                               "-C" (working-git) command 
                               arg-list)))
           (stdout-lines (clean-output (get-output-bytes stdout-ob)))
           (stderr-lines (clean-output (get-output-bytes stderr-ob))))
      (if (zero? exit-code)
          (begin
            (debug "Output of git ~s:~n" command)
            (log-lines DEBUG stdout-lines)
            (list stdout-lines stderr-lines))
          (begin
            (info "Git command failed:~n~s ~s" command (string-join arg-list))
            (info "stdout:~n")
            (log-lines INFO stdout-lines)
            (info "stderr:~n")
            (log-lines INFO stdout-lines)
            (raise-user-error 'git "git command failed"))))))


(define (git-status)
  (parameterize ([working-git (build-path "/Users/travis/src/uln")])
    (git "status" '())))
