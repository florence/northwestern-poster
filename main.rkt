#lang racket
(provide area title assemble)
(require pict racket/draw)

(define page-ratio 4/3)
;; calibrated for a poster 36 inches wide.
(define page-width
  (pict-width
   (text "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         "Calibri"
         54)))
(define page-width+margin
  (pict-width
   (text "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         "Calibri"
         54)))
    
(define page-height (* page-width (/ page-ratio)))
(define page-height+margin (* page-width+margin (/ page-ratio)))
(define column-width (/ page-width 3))

(define title-color (make-color #x41 #x43 #x85))
(define subtext-color (make-color #x6D #x6E #x71))

(define (title title authors deps inst affils)
  (define supertitle 
   (text (~a deps ", " inst) (list subtext-color) 24))
  (define t (text title (list title-color) 54))
  (define a
    (page-wrap-hbl-append
     (for/list ([a (in-list authors)])
       (apply
        hbl-append 
        (text (first a) (list subtext-color) 36)
        (map (lambda (a) (text (~a a) (list 'superscript subtext-color) 36))
             (append (second a) (list " ")))))))
  (define aff
    (page-wrap-hbl-append
     (for/list  ([a (in-list affils)] [i (in-naturals 1)])
       (hbl-append
        (text (~a i) (list 'superscript subtext-color) 24)
        (text a (list subtext-color) 24)))))
  (define line-width page-width)
  (vl-append
   supertitle
   (colorize (hline line-width 10) subtext-color)
   t
   (ghost (text " " null 24))
   a
   (hline line-width 5)
   aff))

(define (area title p)
  (define t 
    (text (~a title) (list title-color) 36))
  (vl-append
   10
   t
   (hline (max column-width (pict-width t) (pict-width p)) 5)
   p))


(define (assemble title . bodies)
  (define base (filled-rectangle page-width+margin page-height+margin #:draw-border? #f #:color "white"))
  (pin-over
   base
   (/ (- page-width+margin page-width) 2)
   (/ (- page-height+margin page-height) 2)
   (vl-append
    title
    (page-wrap-hbl-append bodies #:append ht-append #:padding 20))))

(define (page-wrap-hbl-append picts #:append [appnd hbl-append] #:padding [padding 0])
  (for/fold ([p (list (blank 0))]
             #:result (inset (apply vl-append (reverse p)) (- padding) 0 0 0))
            ([new (in-list picts)])
    (cond
      [(>= (+ (pict-width (first p)) (pict-width new)) page-width)
       (cons (appnd padding (blank 0) new) p)]
      [else
       (cons
        (appnd padding (first p) new)
        (rest p))])))
          