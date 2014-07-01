#lang racket/base

(require racket/list)

(module+ test
  (require rackunit))

(provide slugify)

(define (slugify-next s acc last)
  (if (null? s)
      (list->string (reverse (flatten acc)))
      (let* ((c  (car s))
             (sc (cond
                  [(or (char<=? #\a c #\z)
                       (char<=? #\0 c #\9)
                       (char=? c #\-))        c]
                  [(char<=? #\A c #\Z)        (char-downcase c)]
                  [(or (char-whitespace? c)
                       (char-blank? c))       #\-]
                  [else                       #\.]))) ;; denotes "non-printable"
        ;; If non-printable or a duplicate dash, don't add it to the list
        (if (or (char=? sc #\.) (char=? sc last #\-))
            (slugify-next (cdr s) acc last)
            (slugify-next (cdr s) (cons sc acc) sc)))))


(define (slugify str)
  ;; Converts a string that might have spaces and special characters into
  ;; a string that only has ascii characters.
  (slugify-next (string->list str) null #\.))


(module+ test
  (check-equal? (slugify "who am i?") "who-am-i")
  (check-equal? (slugify "3 - I'm a what?") "3-im-a-what")
  (check-equal? (slugify "Why am I here?") "why-am-i-here")
  (check-equal? (slugify "Why am I here? ΆΩ That's why.") "why-am-i-here-thats-why"))
