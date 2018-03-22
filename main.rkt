#lang racket
(provide area title assemble title-color subtext-color print body-text-style body-text-size
         page-width page-height page-width+margin page-height+margin body-padding)
(require pict racket/draw)

(define font "Calibri")
(define body-text-style font)
(define body-text-size 28)
(define subtext-font-size 24)
(define section-title-font-size 36)
(define title-font-size 54)
(define body-padding 20)

(define page-ratio 4/3)
;; calibrated for a poster 36 inches wide.
(define page-width
  (pict-width
   (text "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         font
         title-font-size)))
(define page-width+margin
  (pict-width
   (text "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         font
         title-font-size)))
    
(define page-height (* page-width (/ page-ratio)))
(define page-height+margin (* page-width+margin (/ page-ratio)))
(define column-width (/ page-width 3))

(define title-color (make-color #x41 #x43 #x85))
(define subtext-color (make-color #x6D #x6E #x71))

(define (title title authors deps inst affils)
  (define supertitle 
   (text (~a deps ", " inst) (list subtext-color) subtext-font-size))
  (define t (text title (list title-color) title-font-size))
  (define a
    (page-wrap-hbl-append
     (for/list ([a (in-list authors)])
       (apply
        hbl-append 
        (text (first a) (list subtext-color) section-title-font-size)
        (map (lambda (a) (text (~a a) (list* 'superscript subtext-color font) section-title-font-size))
             (append (second a) (list " ")))))))
  (define aff
    (page-wrap-hbl-append
     (for/list  ([a (in-list affils)] [i (in-naturals 1)])
       (hbl-append
        (text (~a i) (list* 'superscript subtext-color font) subtext-font-size)
        (text (~a a " ") (list* subtext-color font) subtext-font-size)))))
  (define line-width page-width)
  (vl-append
   supertitle
   (colorize (hline line-width 10) subtext-color)
   t
   (ghost (text " " font subtext-font-size))
   a
   (hline line-width 5)
   aff
   (ghost (text " " font subtext-font-size))
   (ghost (text " " font subtext-font-size))))

(define (area title p)
  (define t 
    (text (~a title) (list* title-color font) section-title-font-size))
  (vl-append
   10
   t
   (hline (max column-width (pict-width t) (pict-width p)) 5)
   p))


(define (assemble title #:frame-margin? [fm? #f] . bodies)
  (define base (filled-rectangle page-width+margin page-height+margin #:draw-border? #f #:color "white"))
  (define x-pin (/ (- page-width+margin page-width) 2))
  (define y-pin (/ (- page-height+margin page-height) 2))
  (pin-over
   (cc-superimpose base (if fm? (rectangle page-width page-height) (blank)))
   x-pin
   y-pin
   (vl-append
    title
    (page-wrap-hbl-append bodies #:append ht-append #:padding body-padding))))

(define (page-wrap-hbl-append picts #:append [appnd hbl-append] #:padding [padding 0])
  (for/fold ([p (list (blank 0))]
             #:result (inset (apply vl-append (reverse p)) (- padding) 0 0 0))
            ([new (in-list picts)])
    (cond
      [(> (+ (pict-width (first p)) (pict-width new)) page-width)
       (cons (appnd padding (blank 0) new) p)]
      [else
       (cons
        (appnd padding (first p) new)
        (rest p))])))

(define (print poster)
  (define ps-setup (make-object ps-setup%))
  (send ps-setup copy-from (current-ps-setup))
  (send ps-setup set-file "poster.pdf")
  (send ps-setup set-orientation 'portrait)
  (send ps-setup set-scaling 1 1)
  
  (define pdf-dc (parameterize ([current-ps-setup ps-setup])
                   (new pdf-dc%
                        [interactive #f]
                        [use-paper-bbox #f]
                        [width page-width+margin]
                        [height page-height+margin])))
  (send pdf-dc set-scale 1 1)
  (send pdf-dc start-doc "")
  (send pdf-dc start-page)
  (draw-pict poster pdf-dc 0 0)
  (send pdf-dc end-page)
  (send pdf-dc end-doc))