#lang racket/gui

(require "figures.rkt")

(define pit-width/cells 20)
(define pit-height/cells 30)

; pit содержит ячейки - границы стакана
; формируется на основе длины/ширины стакана

(define pit (list->set (append (map (lambda (x) `(1 . ,x)) (build-list pit-height/cells (lambda (x) (add1 x))))
                               (map (lambda (x) `(,pit-width/cells . ,x)) (build-list pit-height/cells (lambda (x) (add1 x))))
                               (map (lambda (x) `(,x . ,pit-height/cells)) (build-list (- pit-width/cells 2) (lambda (x) (+ x 2)))))))
(define figure (random-figure-at 10 1))
(define pile (set))

;==========

(define (pile-check line)
  (= (length (filter (lambda (x) (= (cdr x) line)) (set->list pile))) (- pit-width/cells 2)))
(define (pile-pack line)
  (set! pile (set-subtract pile (list->set (filter (lambda (x) (= (cdr x) line)) (set->list pile)))))
  (set! pile (list->set (set-map pile (lambda (x) (if (< (cdr x) line)
                                                      (cons (car x) (add1 (cdr x)))
                                                      x))))))
(define (pile-check-pack (i (- pit-height/cells 1)))
  (when (> i 0)
    (if (pile-check i)
        (begin
          (pile-pack i)
          (pile-check-pack i))
        (pile-check-pack (sub1 i)))))

(define (move-down)
  (let ([new-figure (figure->set (move-rel figure 0 1))])
    (if (and (set-empty? (set-intersect new-figure pit)) (set-empty? (set-intersect new-figure pile)))
        (begin
          (set! figure (move-rel figure 0 1))
          #t)
        (begin
          (set! pile (set-union pile (figure->set figure)))
          (set! figure (random-figure-at 10 1))
          (pile-check-pack)
          #f))))
(define (move-left)
  (let ([new-figure (figure->set (move-rel figure -1 0))])
    (if (and (set-empty? (set-intersect new-figure pit)) (set-empty? (set-intersect new-figure pile)))
        (begin
          (set! figure (move-rel figure -1 0))
          #t)
        #f)))
(define (move-right)
  (let ([new-figure (figure->set (move-rel figure 1 0))])
    (if (and (set-empty? (set-intersect new-figure pit)) (set-empty? (set-intersect new-figure pile)))
        (begin
          (set! figure (move-rel figure 1 0))
          #t)
        #f)))
(define (move-rotate)
  (let ([new-figure (figure->set (rotate figure))])
    (if (and (set-empty? (set-intersect new-figure pit)) (set-empty? (set-intersect new-figure pile)))
        (begin
          (set! figure (rotate figure))
          #t)
        #f)))

(define move-flag #f)

(define cell-width/px 16)
(define cell-height/px 16)
(define (px->cell/width x)
  (add1 (quotient x cell-width/px)))
(define (px->cell/height y)
  (add1 (quotient y cell-height/px)))
(define (cell->px/width x)
  (* (sub1 x) cell-width/px))
(define (cell->px/height y)
  (* (sub1 y) cell-height/px))
(define (draw-cell dc cell)
  (send dc draw-rectangle (cell->px/width (car cell)) (cell->px/height (cdr cell)) cell-width/px cell-height/px))

(define tetris-canvas% (class canvas%
                         (define/override (on-char event)
                           (case (send event get-key-code)
                             ['left (set! move-flag 'left)]
                             ['right (set! move-flag 'right)]
                             ['up (set! move-flag 'rotate)]
                             ['down (set! move-flag 'drop)]))
                         (super-new)))
(define frame (new frame%
                   [label "Tetris"]
                   [min-width (* pit-width/cells cell-width/px)]
                   [min-height (* pit-height/cells cell-height/px)]
                   [stretchable-width #f]
                   [stretchable-height #f]))
(define canvas (new tetris-canvas%
                    [parent frame]
                    [paint-callback (lambda (canvas dc)
                                      (send dc set-pen "black" 1 'transparent)
                                      (send dc set-brush "gray" 'solid)
                                      (set-for-each pit (curry draw-cell dc))
                                      (send dc set-brush "blue" 'solid)
                                      (set-for-each (figure->set figure) (curry draw-cell dc))
                                      (send dc set-brush "darkblue" 'solid)
                                      (set-for-each pile (curry draw-cell dc))
                                      )]
                    ))
(define timer1 (new timer%
                    [notify-callback (lambda ()                                      
                                       (move-down)
                                       (send canvas refresh-now)
                                       (when (not (send frame is-shown?))
                                         (send timer1 stop)))]))
(define timer2 (new timer%
                    [notify-callback (lambda ()                                      
                                       (case move-flag
                                         ['left (move-left) (set! move-flag #f) (send canvas refresh-now)]
                                         ['right (move-right) (set! move-flag #f) (send canvas refresh-now)]
                                         ['rotate (move-rotate) (set! move-flag #f) (send canvas refresh-now)]
                                         ['drop (move-down) (set! move-flag #f) (send canvas refresh-now)]
                                         )
                                       (when (not (send frame is-shown?))
                                         (send timer2 stop)))]))
(send frame show #t)
(send canvas focus)
(send timer1 start 500)
(send timer2 start 100)