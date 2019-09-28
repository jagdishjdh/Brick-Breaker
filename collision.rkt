#lang racket
(require "objects.rkt")
(require "brick-utils.rkt")
(require rsound)
(provide (all-defined-out))

(define wall (resample 3 (rs-read "sounds/hockey stocks.wav")))
;;=========================================
;; definations
;;=========================================
(define (mymodulo a n) ;; will work for real numbers
  (+ (modulo (floor a) n) (- a (floor a))))

(define (bring-inside ws)
  (match ws
    [(WS p1 p2 (Ball bx by bvx bvy r) bricks powers chance score pause lives level mode)
     (let*([next-x (- (+ bx bvx) r)]
           [next-y (- (+ by bvy) r)]
           [BW (- width (* 2 r))]
           [BH (- height (* 2 r))]
           [bnx (if (= next-x (mymodulo next-x BW)) next-x (- BW (mymodulo next-x BW)))]
           [bny (if (= next-y (mymodulo next-y BH)) next-y (- BH (mymodulo next-y BH)))]
           [bnvx (if (= bnx next-x) bvx (- bvx))]
           [bnvy (if (= bny next-y) bvy (- bvy))])
       (set-WS-ball! ws (Ball (+ bnx r) (+ bny r) bnvx bnvy r))
       (play wall)
       ws)]))

(define (next-outside? ws)
  (match ws
    [(WS p1 p2 (Ball bx by bvx bvy r) bricks powers chance score pause lives level mode)
        (let ([bnx (+ bx bvx)]
              [bny (+ by bvy)])
          (or (> bnx (- width r)) (< bnx r) (< bny r) (> bny (- height r))))]))

(define (inside? x1 x2 y1 y2 x y)
  (and (<= x x2) (>= x x1) (>= y y1) (<= y y2)))

(define (nearest l x y)
  (define (helper l pair d)
    (match l
      ['() pair]
      [(cons (cons x1 y1) rest) (let([d1 (sqrt (+ (sqr (- x1 x)) (sqr (- y1 y))))])
                                  (if (< d1 d)
                                      (helper (cdr l) (car l) d1)
                                      (helper (cdr l) pair d)))]))
  (helper (cdr l) (car l) (sqrt (+ (sqr (- (caar l) x)) (sqr (- (cdar l) y))))))

(define (collide-with-brick? ws)
  (match ws
    [(WS p1 p2 (Ball bx by bvx bvy r) bricks powers chance score pause lives level mode)
       (let*([bnx (+ bx bvx)]
             [bny (+ by bvy)]
             [x1 (- bnx bw r)]
             [x2 (+ bnx bw r)]
             [y1 (- bny bh r)]
             [y2 (+ bny bh r)]
             [ls (get-bricks bricks x1 x2 y1 y2)])
         (cond[(= (length ls) 1) (vec (caar ls) (cdar ls))]
              [(null? ls) #f]
              [else (let*([x (nearest ls bnx bny)])
                      (vec (car x) (cdr x)))]))]))

(define (collide-with-paddle? ws)
  (match ws
    [(WS (Paddle px py ph pw pv) p2 (Ball bx by bvx bvy r) bricks powers chance score pause lives level mode)
     (let ([loc-y1 (- py 5 r)] ;; for lower one
           [m (/ bvy bvx)]
           [bnx (+ bx bvx)]
           [bny (+ by bvy)])
       (cond[(and (> bny loc-y1) (<= (abs (- (+ bx (/ (- loc-y1 by) m)) px)) 40)) "lower"]
            [p2 (let ([loc-y2 (+ (Paddle-y p2) 5 r)])
                     (if (and (< bny loc-y2) (<= (abs (- (+ bx (/ (- loc-y2 by) m)) (Paddle-x p2))) 40))
                         "upper"  #f))]
            [else #f])
       )]))

(define (get-collide-with-paddle ws paddle pw ph)
  (match ws
    [(WS (Paddle px py ph pw pv) p2 (Ball bx by bvx bvy r) bricks powers chance score pause lives level mode)
     ;====================================================
       (define (reflect-from y-line x0)
         (let*([m (/ bvy bvx)]
               [v (sqrt (+ (sqr bvx) (sqr bvy)))]
               [dis-x0 (- (+ bx (/ (- y-line by) m)) x0)] ;distance from x0 at the line of collision "with sign"
               [next-bvx (cond[(>= (* (sgn dis-x0) (sgn bvx)) 0)
                                        (min (+ bvx (* (- v (abs bvx)) (/ dis-x0 pw))) (sqrt (- (sqr v) 1)))]
                              [(>= dis-x0 0) (- bvx (* bvx (/ dis-x0 pw)))] 
                              [else (+ bvx (* bvx (/ dis-x0 pw)))])]
               [next-bvy (sqrt (- (+ (sqr bvx) (sqr bvy)) (sqr next-bvx)))])
           (set-WS-ball! ws (Ball (+ bx bvx) (+ by (- (* 2 (- y-line by)) bvy)) next-bvx (if (equal? paddle "upper") next-bvy (- next-bvy)) r))))
       ;====================================================
  
        (cond[(equal? paddle "lower") (reflect-from (- py 5 r) px)
                                      (set-WS-chance! ws "lower")]
             [(equal? paddle "upper") (reflect-from (+ (Paddle-y p2) 5 r) (Paddle-x p2))
                                      (set-WS-chance! ws "upper")]
             [else '()])
        (play click-1)
        ws]))

(define (get-collide-with-brick  ws  brick_centre  bw bh)
  (match (cons ws brick_centre)
    [(cons (WS p1 p2 (Ball bx by bvx bvy r) bricks powers chance score pause lives level mode) (vec x0 y0))
     
     (define (helper2 locy)
       (set-WS-ball! ws (Ball (+ bx bvx) (- by (- (* 2 (- locy by)) bvy)) bvx (- bvy) r)))
     (define (helper1 locx)
       (set-WS-ball! ws (Ball (- bx (- (* 2 (- locx bx)) bvx)) (+ by bvy) (- bvx) bvy r)))
     
     (let ([locx1 (- x0 bw r)]
           [locx2 (+ x0 bw r)]
           [locy1 (- y0 bh r)]
           [locy2 (+ y0 bh r)]
           [m (/ bvy bvx)])
       (cond[(and (>= bx locx1) (<= bx locx2) (<= by locy1)) (helper2 locy1)]
            [(and (>= bx locx1) (<= bx locx2) (>= by locy2)) (helper2 locy2)]
            [(and (>= by locy1) (<= by locy2) (<= bx locx1)) (helper1 locx1)]
            [(and (>= by locy1) (<= by locy2) (>= bx locx2)) (helper1 locx2)]
            [(and (<= bx locx1) (<= by locy1)) (if (<= (* (- locx1 bx) m) (- locy1 by))
                                                 (helper2 locy1) (helper1 locx1))] ;1
            [(and (>= bx locx2) (<= by locy1)) (if (<= (* (- locx2 bx) m) (- locy1 by))
                                                 (helper2 locy1)  (helper1 locx2))] ;2
            [(and (>= bx locx2) (>= by locy2)) (if (>= (* (- locx2 bx) m) (- locy2 by))
                                                 (helper2 locy2)  (helper1 locx2))] ;3
            [(and (<= bx locx1) (>= by locy2)) (if (>= (* (- locx1 bx) m) (- locy2 by))
                                                 (helper2 locy2)  (helper1 locx1))]
            [else (println "inside ball")]))
     ;(brick-set! bricks x0 y0 #f);
     (level-down-brick bricks x0 y0 powers chance)
     (play clap-1)
     (if (equal? chance "lower")
         (set-2d-down! score (+ 10 (2d-down score)))
         (set-2d-up! score (+ 10 (2d-up score))))]))