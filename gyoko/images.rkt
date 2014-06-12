#lang racket/base

(require racket/draw 
         racket/class
         file/convertible)

(provide make-thumbnail)

(define (make-thumbnail source destination [width 200])
  (let* ((src-bmp  (make-object bitmap% source 'png #f #t))
         (scale    (/ width (send src-bmp get-width)))
         (dest-bmp (make-bitmap width (round (* scale (send src-bmp get-height)))))
         (dc       (new bitmap-dc% [bitmap dest-bmp])))
    (send dc set-scale scale scale)
    (send dc draw-bitmap src-bmp 0 0)
    (define out-bytes (convert dest-bmp 'png-bytes))
    (with-output-to-file destination (lambda () (display out-bytes))
                         #:mode 'binary #:exists 'truncate/replace)))
