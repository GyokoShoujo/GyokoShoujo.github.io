#lang at-exp racket/base

(require scribble/html
         racket/class
         racket/string
         racket/file)

(require "slugify.rkt"
         "logger.rkt"
         "images.rkt")

(provide generate-site
         site-content-path
         site-output-path)

;; Where should we look for the site content?
(define site-content-path (make-parameter null))
;; Where should we generate the site at?
(define site-output-path (make-parameter null))

(module+ test
  (require rackunit))

#|

> (require xml)
> (write-xexpr '(html (body (h1 "Hallo"))))
<html><body><h1>Hallo</h1></body></html>

#lang at-exp racket/base
> (require scribble/html)
> (define (page name)
  (output-xml
   @html{@head{@title{@name}}
         @body{@h1{@name}}}))
> @page{foo}
<html><head><title>hello</title></head><body><h1>hello</h1></body></html>
|#

(define image-directory-name (make-parameter "page-images"))
(define thumbnail-directory-name (make-parameter "page-thumbnails"))

(define chapter%
  (class object%
    (super-new)
    ;; directory: path?, number: natural?, title: string?
    ;; find-pages?: boolean?
    (init-field directory number title)
    
    (when (equal? (site-output-path) null)
        (error 'site "site-output-path needs to be parameterized before creating a chapter"))

    (define/public (add-page page)
      (set-field! pages this (cons page pages)))
    
    (define/public (sorted-pages)
      (let ((sorted-pages (sort pages
                                (λ (p1 p2) (< (get-field number p1)
                                              (get-field number p2))))))
        (set-field! pages this sorted-pages)
        sorted-pages))

    (define/public (generate)
      (for-each make-directory* 
                (list html-directory image-directory thumbnail-directory))
      (copy-directory/files cover-image (build-path image-directory cover-name))
      (make-thumbnail cover-image (build-path thumbnail-directory cover-name))

      (define (page-gen-rec page-list default-title)
        (unless (null? page-list)
          (let ((new-default-title (send (car page-list) generate default-title)))
            (page-gen-rec (cdr page-list) (if (null? new-default-title)
                                              default-title new-default-title)))))
      (page-gen-rec pages title))
    
    (field [cover-image (build-path directory "Cover.png")]
           [pages null]
           [slug (slugify title)]
           [cover-name "cover.png"]
           [markdown-file (build-path directory "index.md")]
           [image-uri (string-join (list "/static" slug (image-directory-name)) "/")]
           [cover-image-uri (string-join (list image-uri cover-name) "/")]
           [thumbnail-uri (string-join (list "/static" slug (thumbnail-directory-name)) "/")]
           [cover-thumbnail-uri (string-join (list thumbnail-uri cover-name) "/")]
           [html-uri (string-append "/" slug)]
           [html-directory (build-path (site-output-path) slug)]
           [html-file-dest (build-path html-directory "index.html")]
           [image-directory (build-path (site-output-path) "static" 
                                        slug (image-directory-name))]
           [thumbnail-directory (build-path (site-output-path) "static" 
                                            slug (thumbnail-directory-name))]
           [cover-image-dest (build-path image-directory cover-name)]
           [cover-thumbnail-dest (build-path thumbnail-directory cover-name)]
           )
    
    ))

(module+ test
  ;; Test chapter% fields
  (check-true (equal? (site-output-path) null))
  (check-exn exn:fail? 
             (lambda () (make-object chapter% (string->path "/thunk") 1 "I'm a failure?")))
  (site-output-path "/var/no")
  (define test-chapter 
    (make-object chapter% (string->path "/tmp") 4 "I'm a chapter!"))
  (check-equal? (get-field slug test-chapter) "im-a-chapter")
  (check-equal? (get-field cover-name test-chapter) "im-a-chapter-cover.png")
  (check-equal? (get-field markdown-file test-chapter) 
                (string->path "/tmp/index.md"))
  (check-equal? (get-field image-uri test-chapter) 
                "/static/im-a-chapter/page-images")
  (check-equal? (get-field cover-image-uri test-chapter) 
                "/static/im-a-chapter/page-images/im-a-chapter-cover.png")
  (check-equal? (get-field thumbnail-uri test-chapter) 
                "/static/im-a-chapter/page-thumbnails")
  (check-equal? (get-field cover-thumbnail-uri test-chapter) 
                "/static/im-a-chapter/page-thumbnails/im-a-chapter-cover.png")
  (check-equal? (get-field html-uri test-chapter) "/im-a-chapter")
  (check-equal? (get-field html-directory test-chapter) 
                (string->path "/var/no/im-a-chapter"))
  (check-equal? (get-field html-file-dest test-chapter) 
                (string->path "/var/no/im-a-chapter/index.html"))
  (check-equal? (get-field image-directory test-chapter) 
                (string->path "/var/no/static/im-a-chapter/page-images"))
  (check-equal? (get-field thumbnail-directory test-chapter) 
                (string->path "/var/no/static/im-a-chapter/page-thumbnails"))
  (check-equal? (get-field cover-image-dest test-chapter) 
                (string->path "/var/no/static/im-a-chapter/page-images/im-a-chapter-cover.png"))
  (check-equal? (get-field cover-thumbnail-dest test-chapter) 
                (string->path "/var/no/static/im-a-chapter/page-thumbnails/im-a-chapter-cover.png"))
  )

(define page%
  (class object%
    (super-new)
    ;; chapter: chapter%?, image: path?, number: natural?, title: string?,
    ;; image was source in old Page class
    (init-field chapter image number title)
    
    (define (stem-name suffix) (string-append stem "." suffix))

    (define/public (generate default-title)
      (copy-directory/files image image-dest)
      (make-thumbnail image thumbnail-dest)
      title
      )
    
    (field [stem (string-append "page-" (number->string number))]
           [html-uri (string-join (list (get-field html-uri chapter) 
                                        (stem-name "html")) "/")]
           [markdown-file (path-replace-suffix image ".md")]
           [thumbnail-uri (string-join (list (get-field thumbnail-uri chapter) 
                                             (stem-name "png")) "/")]
           [image-uri (string-join (list (get-field image-uri chapter) 
                                         (stem-name "png")) "/")]
           [html-file-dest (build-path (get-field html-directory chapter)
                                       (stem-name "html"))]
           [thumbnail-dest (build-path (get-field thumbnail-directory chapter)
                                       (stem-name "png"))]
           [image-dest (build-path (get-field image-directory chapter)
                                   (stem-name "png"))])
    
    (send chapter add-page this)))

(module+ test
  ;; Test page% fields
  (site-output-path "/var/no")
  (define the-chapter 
    (make-object chapter% (string->path "/tmp") 6 "A short chapter"))
  (define p
    (make-object page% the-chapter (build-path "/tmp/6 - A short chapter/1.png") 1
                 "One's Title"))
  (check-equal? (get-field chapter p) the-chapter)
  (check-equal? (get-field image p) 
                (string->path "/tmp/6 - A short chapter/1.png"))
  (check-equal? (get-field number p) 1)
  (check-equal? (get-field title p) "One's Title")
  (check-equal? (get-field stem p) "page-1")
  (check-equal? (get-field html-uri p) "/a-short-chapter/page-1.html")
  (check-equal? (get-field markdown-file p) 
                (string->path "/tmp/6 - A short chapter/1.md"))
  (check-equal? (get-field thumbnail-uri p) 
                "/static/a-short-chapter/page-thumbnails/page-1.png")
  (check-equal? (get-field image-uri p)
                "/static/a-short-chapter/page-images/page-1.png")
  (check-equal? (get-field html-file-dest p) 
                (string->path "/var/no/a-short-chapter/page-1.html"))
  (check-equal? (get-field thumbnail-dest p) 
                (string->path "/var/no/static/a-short-chapter/page-thumbnails/page-1.png"))
  (check-equal? (get-field image-dest p) 
                (string->path "/var/no/static/a-short-chapter/page-images/page-1.png"))
  )

(module+ test
  ;; Make sure that page insertion into chapters works as expected
  (site-output-path "/var/no")
  (define a-chapter 
    (make-object chapter% (string->path "/tmp") 5 "For the page!"))
  (define page-1
    (make-object page% a-chapter (build-path "/tmp/5 - For the page!/1.png") 1 ""))
  (define page-2
    (make-object page% a-chapter (build-path "/tmp/5 - For the page!/2.png") 2 ""))
  (let ((pages (send a-chapter sorted-pages)))
    (check-equal? (get-field stem (car pages)) "page-1")
    (check-equal? (get-field stem (cadr pages)) "page-2"))
  (define page-3
    (make-object page% a-chapter (build-path "/tmp/5 - For the page!/3.png") 3
                 "Threes's Title"))
  (check-equal? (get-field stem (caddr (send a-chapter sorted-pages))) "page-3"))

(define (generate-site)
  (debug "generating site in ~v using source ~v" 
         (site-output-path) (site-content-path))
  (test-path-invariants)
  (let ((chapters (build-chapters)))
    (copy-static-files)
    (generate-chapters chapters)
    (generate-markdown-pages)
    (generate-index chapters))
  )

(define (build-chapters)
  (let ((chapter-path (build-path (site-content-path) "content")))
    (sort (for/list ([test-dir (in-list (directory-list chapter-path))]
                     #:when (regexp-match? #rx"([0-9]+) - (.+)" 
                                           (path->string test-dir)))
            (let* ((chapter-info (regexp-match #rx"([0-9]+) - (.+)" 
                                               (path->string test-dir)))
                   (chapter-dir  (build-path chapter-path test-dir))
                   (chapter-num  (string->number (cadr chapter-info)))
                   (title        (caddr chapter-info))
                   (chapter      (new chapter% 
                                      (directory chapter-dir)
                                      (number    chapter-num)
                                      (title     title))))
              (build-pages-for-chapter chapter)
              chapter))
          (λ (c1 c2) 
            (< (get-field number c1) (get-field number c2))))))


(define (build-pages-for-chapter chapter)
  (for/list ([image (in-list (directory-list (get-field directory chapter)
                                             #:build? #t))]
             #:when (regexp-match #px".*/(\\d+)( - ([^/]+?))?\\.png$"
                                  (path->string image)))
    (let* ((file-info   (regexp-match #px".*/(\\d+)( - ([^/]+?))?\\.png$" 
                                      (path->string image)))
           (page-number (string->number (cadr file-info)))
           (title       (if (cadddr file-info) (cadddr file-info) null)))
      (new page% 
           (chapter chapter) (image image) 
           (number page-number) (title title)))))


(define (generate-chapters chapters)
  (for-each (lambda (chapter)  (send chapter generate))
            chapters))


(define (generate-index chapters)
  "todo: create root index.html from index.md and the list of chapters"
  )

(define (generate-markdown-pages)
  "todo: create html pages from markdown pages other than index.md"
  )

(define (copy-static-files)
  (copy-directory/files (build-path (site-content-path) "static")
                        (build-path (site-output-path) "static")
                        #:keep-modify-seconds? #t)
  )

(define (test-path-invariants)
  (when (equal? (site-output-path) null)
    (error "site-output-path must be set prior to generating the site."))
  (when (equal? (site-content-path) null)
    (error "site-content-path must be set prior to generating the site."))
  (unless (directory-exists? (site-content-path))
    (error "Content path doesn't exist: " (site-content-path)))
  )

