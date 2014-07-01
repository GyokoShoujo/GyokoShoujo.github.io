#lang racket/base

(require racket/class
         racket/list
         racket/string
         racket/file)

(require "slugify.rkt"
         "logger.rkt"
         "images.rkt"
         "paths.rkt"
         "render.rkt")

(provide generate-site
         site-content-path
         site-output-path)

;; Where should we look for the site content?
(define site-content-path (make-parameter null))
;; Where should we generate the site at?
(define site-output-path (make-parameter null))

(module+ test
  (require rackunit))

(define image-directory-name (make-parameter "page-images"))
(define thumbnail-directory-name (make-parameter "page-thumbnails"))

(define (template-path name)
  (let ((template_name
         (string-append (if (symbol? name) (symbol->string name) name) ".ms")))
    (path (site-content-path) / "templates" / template_name)))


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

    (define/public (generate page-count prev-chap-uri next-chap-uri)
      ;; Generates the chapter and pages html files. Returns a pair
      ;; containing the current page count and uri of the last page in
      ;; this chapter.
      (info "Generating chapter: ~s~n" title)
      (when (null? pages)
        (raise-user-error 'site (string-append "Empty page list in chapter:" title)))

      (for-each make-directory*
                (list html-directory image-directory thumbnail-directory))
      (copy-directory/files cover-image (path image-directory / cover-name))
      (make-thumbnail cover-image (path thumbnail-directory / cover-name))

      ;; Make sure pages are sorted before we start.
      (let ((current-pages (sorted-pages)))
        (render-template (template-path 'chapter) html-file-dest
                         (hash "chapter_title" title
                               "page1_uri" (get-field html-uri (car current-pages))
                               "cover_image_uri" cover-image-uri
                               "content_markdown" ""))

        (define (page-gen-rec page-count page-list default-title prev-uri)
          (if (null? page-list)
              (cons page-count prev-uri)
              (let* ((next-pages        (cdr page-list))
                     (cur-page          (car page-list))
                     (next-uri          (if (null? next-pages) next-chap-uri
                                            (get-field html-uri (car next-pages))))
                     (new-default-title (send cur-page generate
                                              page-count default-title prev-uri next-uri)))
                (page-gen-rec (+ page-count 1) next-pages new-default-title
                              (get-field html-uri cur-page)))))
        (page-gen-rec page-count current-pages title html-uri)))


    (field [cover-image          (path directory / "Cover.png")]
           [pages                null]
           [slug                 (slugify title)]
           [cover-name           "cover.png"]
           [markdown-file        (path directory / "index.md")]
           [image-uri            (string-join (list "/static" slug
                                                    (image-directory-name)) "/")]
           [cover-image-uri      (string-join (list image-uri cover-name) "/")]
           [thumbnail-uri        (string-join (list "/static" slug
                                                    (thumbnail-directory-name)) "/")]
           [cover-thumbnail-uri  (string-join (list thumbnail-uri cover-name) "/")]
           [html-uri             (string-append "/" slug)]
           [html-directory       (path (site-output-path) / slug)]
           [html-file-dest       (path html-directory / "index.html")]
           [image-directory      (path (site-output-path) / "static" /
                                       slug / (image-directory-name))]
           [thumbnail-directory  (path (site-output-path) / "static" /
                                       slug / (thumbnail-directory-name))]
           [cover-image-dest     (path image-directory / cover-name)]
           [cover-thumbnail-dest (path thumbnail-directory / cover-name)]
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

    (define/public (generate page-number default-title prev-uri next-uri)
      (info "  page: ~s~n" number)
      (copy-directory/files image image-dest)
      (make-thumbnail image thumbnail-dest)
      (let ((current-title (if (null? title) default-title title)))
        ;; todo: load markdown and include as content_markdown
        (render-template (template-path 'page) html-file-dest
                         (hash "has_previous?" (not (null? prev-uri))
                               "has_next?" (not (null? next-uri))
                               "previous_uri" prev-uri
                               "next_uri" next-uri
                               "image_uri" image-uri
                               "page_title" current-title
                               "page_number" page-number
                               "content_markdown" ""))
        current-title))

    (field [stem           (string-append "page-" (number->string number))]
           [html-uri       (string-join (list (get-field html-uri chapter)
                                              (stem-name "html")) "/")]
           [markdown-file  (path-replace-suffix image ".md")]
           [thumbnail-uri  (string-join (list (get-field thumbnail-uri chapter)
                                              (stem-name "png")) "/")]
           [image-uri      (string-join (list (get-field image-uri chapter)
                                              (stem-name "png")) "/")]
           [html-file-dest (path (get-field html-directory chapter) /
                                 (stem-name "html"))]
           [thumbnail-dest (path (get-field thumbnail-directory chapter) /
                                 (stem-name "png"))]
           [image-dest     (path (get-field image-directory chapter) /
                                 (stem-name "png"))])

    ;; Add ourselves to the chapter when created.
    (send chapter add-page this)))

(module+ test
  ;; Test page% fields
  (site-output-path "/var/no")
  (define the-chapter
    (make-object chapter% (string->path "/tmp") 6 "A short chapter"))
  (define p
    (make-object page% the-chapter (path/ "tmp" / "6 - A short chapter" / "1.png")
                 1 "One's Title"))
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
    (make-object page% a-chapter (path/ "tmp" / "5 - For the page! " / "1.png") 1 ""))
  (define page-2
    (make-object page% a-chapter (path/ "tmp" / "5 - For the page! " / "2.png") 2 ""))
  (let ((pages (send a-chapter sorted-pages)))
    (check-equal? (get-field stem (car pages)) "page-1")
    (check-equal? (get-field stem (cadr pages)) "page-2"))
  (define page-3
    (make-object page% a-chapter (path/ "tmp" / "5 - For the page!" / "3.png") 3
                 "Threes's Title"))
  (check-equal? (get-field stem (caddr (send a-chapter sorted-pages))) "page-3"))

(define (generate-site)
  (info "Generating site in ~v using source ~v~n" (site-output-path) (site-content-path))
  (test-path-invariants)
  (let ((chapters (build-chapters)))
    (debug "Found ~s chapters~n" (length chapters))
    (copy-static-files)
    (generate-html-for-chapters chapters)
    (generate-markdown-pages)
    (generate-table-of-contents chapters))
  )

(define (build-chapters)
  (info "Building list of chapters and pages to generate~n")
  (let ((chapter-path (path (site-content-path) / "content")))
    (sort (for/list ([test-dir (in-list (directory-list chapter-path))]
                     #:when (regexp-match? #rx"([0-9]+) - (.+)"
                                           (path->string test-dir)))
            (let* ((chapter-info (regexp-match #rx"([0-9]+) - (.+)"
                                               (path->string test-dir)))
                   (chapter-dir  (path chapter-path test-dir))
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
  (info "  Building pages for ~s~n" (get-field title chapter))
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


(define (generate-html-for-chapters chapters)
  (info "Generating html and static content for chapters~n")
  (define (gen-chapters-rec chapter-list prev-uri page-count)
    (unless (null? chapter-list)
      (let* ((next-chapters    (cdr chapter-list))
             (next-uri         (if (null? next-chapters) null
                                   (get-field html-uri (car next-chapters))))
             (chapter-pair     (send (car chapter-list) generate
                                     page-count prev-uri next-uri))
             (end-page-count   (car chapter-pair))
             (last-chapter-uri (cdr chapter-pair)))
        (gen-chapters-rec next-chapters last-chapter-uri end-page-count))))
  (gen-chapters-rec chapters "/" 1))


(define (generate-table-of-contents chapters)
  (info "Generating table of contents page~n")

  (define (generate-chapter-hash chapter)
    (apply hash (flatten (map
                          (λ (name) (list (string-replace (symbol->string name) "-" "_")
                                          (dynamic-get-field name chapter)))
                          '(html-uri cover-thumbnail-uri number title)))))
  (let ((chapter-list (map generate-chapter-hash chapters)))
    (render-template (template-path 'index)
                     (path (site-output-path) / "index.html")
                     (hash "chapters" chapter-list
                           "chapter1_uri" (get-field html-uri (car chapters))
                           "content_markdown" ""))))

(define (generate-markdown-pages)
  (info "Generating html pages from markdown files")
  (let ((source-dir (path (site-content-path) / "content")))
    (for ([content-file (in-list (directory-list source-dir))]
          #:when (regexp-match #rx".*.md$" (path->string content-file)))
      (debug "Rendering static file: ~s~n" content-file)
      (let ((content (render-markdown (path source-dir / content-file))))
        (render-template (template-path 'content)
                         (path (site-output-path) /
                               (path-replace-suffix content-file ".html"))
                         (hash "content_markdown" content))))))


(define (copy-static-files)
  (info "Copying static files")
  (copy-file (path (site-content-path) / "CNAME")
             (path (site-output-path) / "CNAME"))
  (copy-directory/files (path (site-content-path) / "static")
                        (path (site-output-path) / "static")
                        #:keep-modify-seconds? #t)
  )


(define (test-path-invariants)
  (when (equal? (site-output-path) null)
    (error "site-output-path must be set prior to generating the site."))
  (when (equal? (site-content-path) null)
    (error "site-content-path must be set prior to generating the site."))
  (unless (directory-exists? (site-content-path))
    (error "Content path doesn't exist: " (site-content-path))))
