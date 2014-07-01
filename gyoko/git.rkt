#lang racket/base

(require racket/date
         racket/file
         racket/format
         racket/string
         racket/system)

(require "logger.rkt"
         "paths.rkt")

(provide build-git
         checkout-site
         commit-site
         did-site-change?
         find-git-base
         git-status
         local-git
         push-site
         working-git)

(define (clean-output bytes)
  (string-split
   (string-replace (bytes->string/locale bytes) "\t" "    ")
   "\n"))


(define (find-git-base test-path)
  (cond [(directory-exists? (path test-path / ".git"))
         test-path]
        [(member test-path (filesystem-root-list))
         (raise-user-error 'git "unable to find local git directory")]
        [else
         (find-git-base (path test-path / ".."))]))

;; local-git is the git repository we are contained within.
;; It is the source used to pull from.
(define local-git (make-parameter ""))

;; working-git is the git repo that will be cloned into and that the
;; site will actually be generated in.
(define build-git (make-parameter ""))

;; Parameter that determines where (git) works.
(define working-git (make-parameter (local-git)))

(define (git command . arg-list)
  (let* ((full-args    (append
                        (list #"-C" (working-git) command)
                        (if (> (log-level) INFO) (remove #"--quiet" arg-list) arg-list)))
         (stdout-ob    (open-output-bytes))
         (stderr-ob    (open-output-bytes))
         (stdin-ib     (open-input-bytes #""))
         (git-exe      (find-executable-path "git"))
         (exit-code    (parameterize ((current-output-port stdout-ob)
                                      (current-error-port  stderr-ob)
                                      (current-input-port  stdin-ib))
                         (apply system*/exit-code git-exe full-args)))
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
      (delete-directory/files (path (build-git) / f))))
  (debug "testing that local repository is up to date~n")
  (parameterize ((working-git (local-git)))
    (when (not (local-up-to-date? branch))
      (raise-user-error 'git "Local repository doesn't match remote. Fix that first.")))
  (debug "cloning ~s to ~s~n" branch (build-git))
  (parameterize ((working-git (build-git)))
    (make-directory (working-git))
    (git #"clone" #"--quiet" #"--branch" branch (local-git) #".")
    (clear-checkout-tree)))

;; Commit the build git repository
(define (commit-site)
  (debug "committing site in ~s~n" (build-git))
  (parameterize ((working-git (build-git)))
    (let ((commit-msg (string-append "Site generated "
                                     (date->string (current-date) #t))))
      (git #"add" #"--all")
      (git #"commit" #"--quiet" (string-join (list "--message" commit-msg) "=")))))

;; Push the build git repo to its remote
(define (push-site remote branch)
  (debug "Pushing site in ~v to ~v ~v (through local repo)~n"
         (build-git) remote branch)
  (parameterize ((working-git (build-git)))
    (git #"push" #"--quiet" #"origin" branch))
  (parameterize ((working-git (local-git)))
    (git #"push" #"--quiet" remote (string-join (list branch branch) ":"))))

;; Tests if there were any changes to the generated site
(define (did-site-change?)
  (parameterize ((working-git (build-git)))
    (not (null? (car (git #"status" #"--porcelain"))))))
