#lang racket/base

(require racket/system
         racket/format
         racket/string
         racket/file)

(require "logger.rkt")

(provide local-git
         build-git
         find-git-base
         checkout-site
         git-status)

(define (clean-output bytes)
  (string-split
   (string-replace (bytes->string/locale bytes) "\t" "    ")
   "\n"))


(define (find-git-base path)
  (if (directory-exists? (build-path path ".git"))
      path
      (if (string=? (path->string path) "/")
          (raise-user-error 'git 
                            "unable to find local git directory")
          (find-git-base (simplify-path (build-path path ".."))))))

;; local-git is the git repository we are contained within.
;; It is the source used to pull from.
(define local-git (make-parameter ""))

;; working-git is the git repo that will be cloned into and that the
;; site will actually be generated in.
(define build-git (make-parameter ""))

;; Parameter that determines where (git) works.
(define working-git (make-parameter (local-git)))

(define (git command . arg-list)
  (let* ((full-args (append (list #"-C" (working-git) command)
                            (if (> (log-level) 1)
                                (remove #"--quiet" arg-list)
                                arg-list)))
         (stdout-ob (open-output-bytes))
         (stderr-ob (open-output-bytes))
         (stdin-ib  (open-input-bytes #""))
         (exit-code (parameterize ((current-output-port stdout-ob)
                                   (current-error-port  stderr-ob)
                                   (current-input-port  stdin-ib))
                      (apply system*/exit-code
                             (find-executable-path "git") full-args)))
         (stdout-lines (clean-output (get-output-bytes stdout-ob)))
         (stderr-lines (clean-output (get-output-bytes stderr-ob))))
      (if (zero? exit-code)
          (begin
            (debug "Output of git ~s~n" (map ~a full-args))
            (log-lines DEBUG stdout-lines)
            (list stdout-lines stderr-lines))
          (begin
            (info "Git command failed: git ~s ~s~n" command (map ~a full-args))
            (info "stdout:~n")
            (log-lines INFO stdout-lines)
            (info "stderr:~n")
            (log-lines INFO stderr-lines)
            (raise-user-error 'git "git command failed")))))


(define (git-status)
  ; You should parameterize working-git before calling this.
  (git #"status"))

(define (local-up-to-date? branch)
  (git #"remote" #"update")
  (string=? (caar (git #"rev-parse" branch))
            (caar (git #"rev-parse" (string-append "origin/" branch)))))


;; Prepare the temporary git repository for site generation
(define (checkout-site branch)
  (define (clear-checkout-tree)
    (for ([f (directory-list (build-git))]
          #:when (memf (λ (arg) (not (string=? (path->string f) arg)))
                       (list ".git")))
      (debug "Removing ~s~n" f)
      (delete-directory/files (build-path (build-git) f))))
  (debug "testing that local repository is up to date~n")
  (parameterize ((working-git (local-git)))
    (when (not (local-up-to-date? branch))
      (raise-user-error 'git "Local repository doesn't match remote. Fix that first.")))
  (debug "cloning ~s to ~s~n" branch (build-git))
  (parameterize ((working-git (build-git)))
    (make-directory (working-git))
    (git #"clone" #"--quiet" #"--branch" branch (local-git) #".")
    (clear-checkout-tree)))
