#lang racket
(require "objects.rkt")
(require 2htdp/image)
(require lang/posn)

(provide (all-defined-out))

;=====================================================
;; bricks utilities
;===================================================== 0 - 700
(define-values (xs xd xnum ys yd ynum) (values 50 60 11 100 40 5))
(define power-index 0)
;;create a hash-table (posn  brick-state)
(define (create-bricks level)
  (define brick-hash (make-hash))
  (define (helper1 ys yd xs xd l)
    (cond[(null? l) (void)]
         [else (helper2 ys xs xd (car l))
               (helper1 (+ ys yd) yd  xs xd (cdr l))]))
  
  (define (helper2 y xs xd l)
    (cond [(null? l) (void)]
          [(car l) (hash-set! brick-hash (make-posn xs y) (car l))
                   (helper2 y (+ xs xd) xd (cdr l))]
          [else  (helper2 y (+ xs xd) xd (cdr l))]))
  
  (define pnum 0)
  (define (set-powerup key value)
    (cond[(and (< pnum 7) (= value 1)) (let* ([ rn (random 10)])
                                         (cond [(= rn 4) (hash-set! brick-hash key "e") (set! pnum (+ pnum 1))]
                                               [(= rn 2) (hash-set! brick-hash key "c") (set! pnum (+ pnum 1))]
                                               [(= rn 3) (hash-set! brick-hash key "li") (set! pnum (+ pnum 1))]
                                               [(= rn 1) (hash-set! brick-hash key "ld") (set! pnum (+ pnum 1))]
                                               [else (void)]))]
         [else (void)]))
  (cond [(= level 1) (helper1 100 40 50 60 (list (list 1 #f 1 #f 1 #f 1 #f 1 #f 1)
                                                 (list #f 1 #f 1 #f 1 #f 1 #f 1 #f)
                                                 (list 1 #f 1 #f 1 #f 1 #f 1 #f 1)
                                                 (list #f 1 #f 1 #f 1 #f 1 #f 1 #f)
                                                 (list 1 #f 1 #f 1 #f 1 #f 1 #f 1)))]
        
        [(= level 2) (helper1 100 40 50 60 (list (list 1 2 1 2 1 2 1 2 1 2 1)
                                                 (list 2 1 2 1 2 1 2 1 2 1 2)
                                                 (list 1 2 1 2 1 2 1 2 1 2 1)
                                                 (list 2 1 2 1 2 1 2 1 2 1 2)
                                                 (list 1 2 1 2 1 2 1 2 1 2 1)))]

        [(= level 3) (helper1 100 40 50 60 (list (list #f #f 1  1 #f #f #f  1  1 #F #f)
                                                 (list #f 1  1  1  1 #f  1  1  1  1 #f)
                                                 (list 3  1  1  1  1  3  1  1  1  1  3)
                                                 (list #f 3  1  1  3 #f  3  1  1  3 #f)
                                                 (list #f #f 3  3 #f #f #f  3  3 #F #f)))]

        [(= level 4) (helper1 100 40 50 60 (list (list 3 3 3 3 3 3 3 3 3 3 3)
                                                 (list 3 1 2 1 2 1 2 1 2 1 3)
                                                 (list 3 2 1 2 1 2 1 2 1 2 3)
                                                 (list 3 1 2 1 2 1 2 1 2 1 3)
                                                 (list 3 3 3 3 3 3 3 3 3 3 3)))]
        
        [(= level 5) (helper1 100 40 50 60 (list (list 3 3 3 3 3 #f 3 3 3 3 3)
                                                 (list 2 1 2 1 3 #f 3 1 2 1 2)
                                                 (list 1 2 1 2 3 #f 3 2 1 2 1)
                                                 (list 2 1 2 1 3 #f 3 1 2 1 2)
                                                 (list 3 3 3 3 3 #f 3 3 3 3 3)))]

        [(= level 6) (helper1 150 40 50 60 (list (list #f #f 3  3 #f #f #f  3  3 #f #f)
                                                 (list #f 3  1  1  3 #f  3  1  1  3 #f)
                                                 (list 3  1  1  1  1  3  1  1  1  1  3)
                                                 (list #f 3  1  1  3 #f  3  1  1  3 #f)
                                                 (list #f #f 3  3 #f #f #f  3  3 #F #f)))])
  (hash-for-each brick-hash set-powerup)
  brick-hash)
;===================
(define (get-bricks bricks x1 x2 y1 y2) ;;returns list of pairs
  (define (helper l acc)
    (match l
      ['() acc]
      [(cons (cons (posn x y) val) rest) (if (and (< x x2) (> x x1) (< y y2) (> y y1))
                                             (helper (cdr l) (cons (cons x y) acc))
                                             (helper (cdr l) acc))]))
  (helper (hash->list bricks) '()))
;===================
(define (brick-ref bricks x y)
  (hash-ref bricks (make-posn x y) #f))
;=================
(define (brick-set! bricks x y val)
  (hash-set! bricks (make-posn x y) val))
;===================
(define (level-down-brick bricks x0 y0 powers chance)
  (let ([value (brick-ref bricks x0 y0)])
      (cond[(equal? value #f) '()]
           [(equal? value 1) (hash-remove! bricks (make-posn x0 y0))]
           [(number? value) (brick-set! bricks x0 y0 (- value 1))]
           [else (hash-remove! bricks (make-posn x0 y0))
                 (hash-set! powers power-index (powerup value (if (equal? chance "lower") 3 -3) x0 y0))
                 (set! power-index (+ 1 power-index))])))
;===================
(define (make-posn-list bricks)
  (define (helper l acc)
    (cond [(null? l) acc]
          [else (match (car l)
                  [(cons pos val)
                     (helper (cdr l) (cons (cons (val->brick val) (car acc)) (cons pos (cdr acc))))])]))
  (helper (hash->list bricks) '(())))

(define (val->brick val)
  (cond[(equal? val 1) brick1]
       [(equal? val 2) brick2]
       [(equal? val 3) brick3]
       [else brick1]))
;=============================
(define (make-power-list powers)
  (define (helper l acc)
    (cond[(null? l) acc]
         [else (match (car l)
                 [(cons key (powerup name v x y))
                      (helper (cdr l) (cons (cons (name->image name) (car acc)) (cons (make-posn x y) (cdr acc))))])]))
  (helper (hash->list powers ) '(()) ))

(define (name->image name)
  (cond[(equal? name "e") enlarge]
       [(equal? name "c") compress]
       [(equal? name "li") livei]
       [(equal? name "ld") lived]))
;===================
(define (place-bricks bricks powers bg)
  (let ([brick+posn (make-posn-list bricks)]
        [pwlist (make-power-list powers)])
    (place-images (append (car pwlist) (car brick+posn))
                  (append (cdr pwlist) (cdr brick+posn))
                  bg)))
