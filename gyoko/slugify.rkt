#lang racket/base

(require racket/list)

(module+ test
  (require rackunit))

(provide slugify)

;; Converts a string that might have spaces and special characters into
;; a string that only has ascii characters.

(define (slugify str)
  (letrec ((a (char->integer #\a))
           (z (char->integer #\z))
           (A (char->integer #\A))
           (Z (char->integer #\Z))
           (zero (char->integer #\0))
           (nine (char->integer #\9))
           (slugify-next
            (λ (s acc last)
              (if (null? s)
                (list->string (reverse (flatten acc)))
                (begin
                  (let* ((c (car s))
                         (v (char->integer c))
                         (sc (cond
                              [(or (and (>= v a) (<= v z))
                                   (and (>= v zero) (<= v nine))
                                   (eq? v #\-))         c] 
                             [(and (>= v A) (<= v Z))   (char-downcase c)]
                             [(or (char-whitespace? c)
                                  (char-blank? c))      #\-]
                             [else                      #\.])))
                    (if (or (eq? sc #\.)           ;; Should we ignore this character? 
                            (and (eq? sc last)     ;; non-ascii, space or duplicate '-'
                                 (eq? last #\-)))  
                        (slugify-next (cdr s) acc last)
                        (slugify-next (cdr s) (cons sc acc) sc))))))))
    (slugify-next (string->list str) null #f)))


(module+ test
  (check-equal? (slugify "who am i?") "who-am-i")
  (check-equal? (slugify "3 - I'm a what?") "3-im-a-what")
  (check-equal? (slugify "Why am I here?") "why-am-i-here")
  (check-equal? (slugify "Why am I here? ΆΩ That's why.") "why-am-i-here-thats-why"))

