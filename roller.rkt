#lang racket/base

(require racket/gui/base racket/match racket/class pict)

(define (d6-pip scale exists)
  (inset (if exists (disk scale) (blank scale)) (/ scale 2) (/ scale 2) 0 0))

(define (d6-pips scale p1 p2 p3 p4 p5 p6 p7 p8 p9)
  (vl-append (ht-append (d6-pip scale p1) (d6-pip scale p2) (d6-pip scale p3))
             (ht-append (d6-pip scale p4) (d6-pip scale p5) (d6-pip scale p6))
             (ht-append (d6-pip scale p7) (d6-pip scale p8) (d6-pip scale p9))))

(define (d6 scale n)
  (lt-superimpose (filled-rounded-rectangle (* 5 scale) (* 5 scale) scale #:color "white")
                  (match n
                    [0 (d6-pips scale #f #f #f
                                      #f #f #f
                                      #f #f #f)]
                    [1 (d6-pips scale #f #f #f
                                      #f #t #f
                                      #f #f #f)]
                    [2 (d6-pips scale #t #f #f
                                      #f #f #f
                                      #f #f #t)]
                    [3 (d6-pips scale #t #f #f
                                      #f #t #f
                                      #f #f #t)]
                    [4 (d6-pips scale #t #f #t
                                      #f #f #f
                                      #t #f #t)]
                    [5 (d6-pips scale #t #f #t
                                      #f #t #f
                                      #t #f #t)]
                    [6 (d6-pips scale #t #f #t
                                      #t #f #t
                                      #t #f #t)])))

(define first-roll 0)
(define second-roll 0)

(define frame (new frame%
                   [label "Example"]))

(define dice (new horizontal-pane% [parent frame]))

(define first-die
  (new canvas% [parent dice]
               [min-width 50]
               [min-height 50]
               [style '(transparent)]
               [paint-callback
                (lambda (canvas dc)
                  (define-values (w h) (send canvas get-client-size))
                  (define scale (/ (- (min w h) 1) 5))
                  (send dc set-smoothing 'smoothed)
                  (draw-pict (d6 scale first-roll) dc 0.5 0.5))]))

(define second-die
  (new canvas% [parent dice]
               [min-width 50]
               [min-height 50]
               [style '(transparent)]
               [paint-callback
                (lambda (canvas dc)
                  (define-values (w h) (send canvas get-client-size))
                  (define scale (/ (- (min w h) 1) 5))
                  (send dc set-smoothing 'smoothed)
                  (draw-pict (d6 scale second-roll) dc 0.5 0.5))]))

(new button% [parent frame]
             [label "Roll"]
             [callback (lambda (button event)
                         (set! first-roll (random 1 7))
                         (set! second-roll (random 1 7))
                         (send first-die refresh)
                         (send second-die refresh))])

(send frame show #t)