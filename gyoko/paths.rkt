#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide path
         path/)

(module+ test
  (require rackunit))

(define-syntax (path stx)
  (define-splicing-syntax-class pp
    (pattern (~seq part (~optional (~literal /)))))
  (syntax-parse stx
    [(_ p:pp ...) #'(do-create-path p.part ...)]))

(define-syntax (path/ stx)
  (define-splicing-syntax-class pp
    (pattern (~seq part (~optional (~literal /)))))
  (syntax-parse stx
    [(_ p:pp ...) #'(do-create-abs-path p.part ...)]))

(define (do-create-path . path-parts)
  (simplify-path 
   (let ((path (apply build-path path-parts)))
     (if (absolute-path? path)
         path
         (build-path (current-directory) path)))))

(define (do-create-abs-path . path-parts)
  (simplify-path
   (if (null? path-parts) 
       (car (filesystem-root-list))
       (let ((path (apply build-path path-parts)))
         (if (absolute-path? path)
             path
             (build-path (car (filesystem-root-list)) path))))))

(module+ test
  (check-equal? (path "/foo") (build-path "/foo"))
  (check-equal? (path "foo" "bar") (build-path (current-directory) "foo/bar"))
  (check-equal? (path "foo" / "bar") (build-path (current-directory) "foo/bar"))
  (check-equal? (path "foo" / ".." / "bar") (build-path (current-directory) "bar"))
  (parameterize ((current-directory (build-path "/tmp")))
    (check-equal? (path "foo" / ".." / "bar") (build-path "/tmp/bar")))
  (check-equal? (path/ "foo" / "bar") (build-path "/foo/bar"))
  (check-equal? (path/ "foo" / "bar" / "baz") (build-path "/foo/bar/baz"))
  (check-equal? (path/ "/foo" / "bar" / "baz") (build-path "/foo/bar/baz"))
  (check-equal? (path/ "/foo/bar" / "baz") (build-path "/foo/bar/baz"))
  (check-equal? (path/ "/foo" / ".." / "bar" / "baz") (build-path "/bar/baz"))
  (check-equal? (path/) (build-path "/"))
  (check-exn #rx"arity mismatch" (λ () (path)))
)
