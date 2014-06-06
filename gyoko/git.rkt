#lang racket

(require "logger.rkt")

(provide git)

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

