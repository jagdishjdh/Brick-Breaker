#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require rsound)
(require "objects.rkt")
(require "brick-utils.rkt")
(require "collision.rkt")
;=====================================================
;; definations
;=====================================================
(define powers       (make-hash))
(define-values       (bvelx bvely bpos lives) (values 5 -6 (vec 200 368) 3))
;(define vs-computer  (interface developing  (list (cons #\b #t))))
(define iface-single (interface on-single-image (list (cons 1 (λ () (create-single-ws 1))) (cons 2 (λ () (create-single-ws 2)))
                                                      (cons 3 (λ () (create-single-ws 3))) (cons 4 (λ () (create-single-ws 4)))
                                                      (cons 5 (λ () (create-single-ws 5))) (cons #\b #t)) ))
(define iface-double (interface on-double-image (list (cons 3 (λ () (create-double-ws 'vs-computer)))
                                                      (cons 4 (λ () (create-double-ws 'double))) (cons #\b #t))))
(define iface-play   (interface on-play-image (list (cons 3 iface-single) (cons 4 iface-double) (cons #\b #t))))
(define iface-start  (interface start-image (list (cons 3 iface-play) (cons 4 "exit"))))
(define stack  (list iface-start))
(define bgsong (rs-read "sounds/bgsong.wav"))
(define on-click (rs-read "sounds/all-buttons.wav"))
(define back (rs-read "sounds/back-button.wav"))
;=====================================================
;; auxulary functions
;=====================================================
(define (create-single-ws level)
  (play bgsong)
  (WS (Paddle 350 480 10 80 10) #f (Ball 350 468 0 0 7) (create-bricks level); (create-bricks xs xd xnum ys yd ynum)
      powers "lower" (2d 0 0) #f (2d 1 3) level 'single))

(define (create-double-ws type)
  (play bgsong)
  (WS (Paddle 350 480 10 80 10) (Paddle 350 20 10 80 10) (Ball 350 468 0 0 7)  (create-bricks 6)
      powers "lower" (2d 0 0) #f (2d 3 3) 1 type))

(define (play-computer p2 ball chance)
  (match (cons p2 ball)
    [(cons (Paddle px py ph pw pv) (Ball bx by bvx bvy r))
     
             (define (hof px1 pw1)
               (let ([diff (- bx px1)])
                 (cond[(> diff (/ pw1 3))  (set-Paddle-x! p2 (min (+ px pv) (- width (/ pw 2))))]
                      [(< diff (/ pw1 -3)) (set-Paddle-x! p2 (max (- px pv) (/ pw 2)))]
                      [else (void)])))
             
             (cond[(> (abs bvx) (abs bvy)) (if (> bvx 0) (hof (- (+ px 10) (/ pw 2)) (/ pw 2))
                                                         (hof (+ (- px 10) (/ pw 2)) (/ pw 2)))]
                  [(and (= bvx 0) (= bvy 0) (equal? chance "upper")) (set-Ball-vx! ball bvelx)
                                                                     (set-Ball-vy! ball bvely)]
                  [else (hof px pw)])]))
  
(define (pause-game ws)
  (set-WS-pause! ws #t))

(define (play-game ws)
  (set-WS-pause! ws #f))
;=====================================================
;; Event Handlers
;=====================================================
(define (render ws)
  (match ws
    [(WS (Paddle px py ph pw pvx) p2 (Ball bx by bvx bvy r) bricks powers chance (2d sc_u sc_d) pause (2d lu ld) level mode)
     
            (define paddle1 (rectangle pw ph 'solid 'blue))
            (define ball (circle r 'solid 'red))
            
            (let ([orig (place-images (list paddle1 ball
                                            (text (~a sc_d) 20 'maroon)
                                            (text (~a ld) 20 'maroon)
                                            (text (~a level) 20 'maroon))
                                      (list (make-posn px py)
                                            (make-posn bx by)
                                            (make-posn 750 (if p2 475 45))
                                            (make-posn 750 (if p2 415 105))
                                            (make-posn 750 255))
                                      (place-bricks bricks powers (if p2 background2 background1)))])
              (if p2 (match p2
                       [(Paddle p2x p2y p2h p2w p2vx)
                             (define paddle2 (rectangle p2w p2h 'solid 'blue))
                             (place-images (list paddle2
                                            (text (~a sc_u) 20 'maroon)
                                            (text (~a lu) 20 'maroon))
                                      (list (make-posn p2x p2y)
                                            (make-posn 750 45)
                                            (make-posn 750 105))
                                      (if pause (place-image (text "Pause" 50 'blue)
                                                             350 250 orig)
                                          orig))])
                  (if pause (place-image (text "Pause" 50 'blue)
                                         350 250 orig)
                      orig)))]
    [(interface img l) img]))
;=============================
(define (mouse-handler ws x y event)
  (match ws
    [(WS (Paddle px py ph pw pvx) p2 b bricks powers chance score pause lives level mode)
         (cond [pause ws]
               [(or (mouse=? event "move") (mouse=? event "drag"))
                                       (cond[(> x (- width (/ pw 2))) (set-WS-pdl1! ws (Paddle (- width (/ pw 2)) py ph pw pvx))]
                                            [(< x (/ pw 2)) (set-WS-pdl1! ws (Paddle (/ pw 2) py ph pw pvx))]
                                            [else (set-WS-pdl1! ws (Paddle x py ph pw pvx))])
                                       (if (and (equal? (Ball-vy b) 0) (equal? chance "lower"))
                                           (set-Ball-x! b px)
                                           (void))]
               [(mouse=? event "button-down")
                                       (cond [(inside? 705 795 285 315 x y) (begin (play back)
                                                                                   (set! ws (score-board ws))) (void)])
                                       (if (equal? (Ball-vy b) 0) (begin
                                                                   (set-Ball-vx! b bvelx)
                                                                   (set-Ball-vy! b bvely)) (void))]
               [(mouse=? event "button-up") '()]
               [(mouse=? event "enter") '()]
               [(mouse=? event "leave") '()])
         ;(print ws)
         ws]
    [(interface img l) (cond [(mouse=? event "button-down")
                                 (cond[(inside? 375 525 105 135 x y) (cond [(and (assoc 1 l) (procedure? (cdr (assoc 1 l))))
                                                                            (play on-click) ((cdr (assoc 1 l)))]
                                                                           [else ws])]
                                      [(inside? 375 525 155 185 x y) (cond [(and (assoc 2 l) (procedure? (cdr (assoc 2 l))))
                                                                            (play on-click) ((cdr (assoc 2 l))) ]
                                                                           [(assoc 2 l) (begin (set! stack (cons (cdr (assoc 2 l)) stack))
                                                                                               (play on-click)
                                                                                               (car stack))]
                                                                           [else ws])]
                                      [(inside? 375 525 205 235 x y) (cond [(and (assoc 3 l) (procedure? (cdr (assoc 3 l))))
                                                                            (play on-click) ((cdr (assoc 3 l))) ]
                                                                           [(assoc 3 l) (begin (set! stack (cons (cdr (assoc 3 l)) stack))
                                                                                               (play on-click)
                                                                                               (car stack))]
                                                                           [else ws])]
                                      [(inside? 375 525 255 285 x y) (cond [(and (assoc 4 l) (procedure? (cdr (assoc 4 l))))
                                                                            (play on-click) ((cdr (assoc 4 l))) ]
                                                                           [(assoc 4 l) (begin (set! stack (cons (cdr (assoc 4 l)) stack))
                                                                                               (play on-click)
                                                                                               (car stack))]
                                                                           [else ws])]
                                      [(inside? 375 525 305 335 x y) (cond [(and (assoc 5 l) (procedure? (cdr (assoc 5 l))))
                                                                            (play on-click) ((cdr (assoc 5 l))) ]
                                                                           [(assoc 5 l) (begin (set! stack (cons (cdr (assoc 5 l)) stack))
                                                                                               (play on-click)
                                                                                               (car stack))]
                                                                           [else ws])]
                                      [(inside? 562 638 438 463 x y) (cond [(assoc #\b l) (begin (set! stack (cdr stack))
                                                                                                 (play back)
                                                                                                 (car stack))]
                                                                           [else ws])]
                                      [else ws])]
                             [else ws])]))
;==============================
(define (key-handler ws key)
  (match ws
    [(WS p1 p2 b bricks powers chance score pause lives level mode)
     (match p1
       [(Paddle px py ph pw pv)
          (cond [pause (if (key=? key " ") (play-game ws) ws)]
                [(key=? key "left") (set-Paddle-x! p1 (max (- px pv) (/ pw 2)))]
                [(key=? key "right") (set-Paddle-x! p1 (min (+ px pv) (- width (/ pw 2))))]
                [(key=? key "a") (if (equal? mode 'double) (match p2
                                          [(Paddle px py ph pw pv) (set-Paddle-x! p2 (max (- px pv) (/ pw 2)))
                                                                   (if (and (equal? (Ball-vy b) 0) (equal? chance "upper"))
                                                                       (set-Ball-x! b (Paddle-x p2))
                                                                       (void))]) (void))]
                [(key=? key "d") (if (equal? mode 'double) (match p2
                                          [(Paddle px py ph pw pv) (set-Paddle-x! p2 (min (+ px pv) (- width (/ pw 2))))
                                                                   (if (and (equal? (Ball-vy b) 0) (equal? chance "upper"))
                                                                       (set-Ball-x! b (Paddle-x p2))
                                                                       (void))]) (void))]
                [(key=? key "s") (if (and (equal? (Ball-vy b) 0) (equal? chance "upper"))
                                     (begin (set-Ball-vx! b bvelx)
                                            (set-Ball-vy! b (- bvely)))
                                     (void))]
                [(key=? key " ") (pause-game ws)]
                [else '()])
          ws])]
    [else ws]))
;==============================
(define (tick-handler ws)
  (match ws
    [(WS (Paddle px py ph pw pv) p2 (Ball bx by bvx bvy r) bricks powers chance score pause lives level mode);(WS (vec p1x p1y) p2 bricks powers chance score pause l)
          ;===================================================
          (define (process-powerup pow_ups)
            (define (helper l) ;; l is list of (n powerup)...
              (cond[(null? l) '()]
                   [else (match (car l)
                           [(cons key (powerup name vel-y x0 y0))
                            (cond [(and (> vel-y 0) (inside? (- px 50) (+ px 50) (- py 10) (+ py 5) x0 y0))
                                                 (apply-powerup name "lower" ws)
                                                 (hash-remove! pow_ups key)]
                                  [(and (> vel-y 0) (> y0 height)) (hash-remove! pow_ups key)]
                                  [(and (< vel-y 0) (< y0 0)) (hash-remove! pow_ups key)]
                                  [(< vel-y 0) (match p2
                                                 [(Paddle px py ph pw pv) (if (inside? (- px 50) (+ px 50) (- py 10) (+ py 5) x0 y0)
                                                                              (begin (apply-powerup name "upper" ws)
                                                                                     (hash-remove! pow_ups key))
                                                                              (hash-set! pow_ups key (powerup name vel-y x0 (+ y0 vel-y))))])]
                                  [else (hash-set! pow_ups key (powerup name vel-y x0 (+ y0 vel-y)))])])
                         (helper (cdr l))]))
            (helper (hash->list pow_ups)))
          ;=====================================================
           (cond[(and (not pause) (not (= bvx 0)) (not (= bvy 0)))
                   (let ([brick_centre (collide-with-brick? ws)]
                         [paddle (collide-with-paddle? ws)])
                     (cond [brick_centre (get-collide-with-brick ws brick_centre bw bh)]
                           [paddle (get-collide-with-paddle ws paddle 40 5)]
                           [(out? ws) (set-WS-pdl1! ws (Paddle 350 480 10 80 20))
                                      (if p2 (match p2
                                               [(Paddle p2x p2y p2h p2w p2v)
                                                (let ([looser (out? ws)])
                                                  (set-WS-pdl2! ws (Paddle 350 20 10 80 20))
                                                  (if (equal? looser "lower") (begin (set-2d-down! lives (- (2d-down lives) 1))
                                                                                     (set-WS-ball! ws (Ball 350 (+ p2y r 5) 0 0 r))
                                                                                     (set-WS-chance! ws "upper"))
                                                                              (begin (set-2d-up! lives (- (2d-up lives) 1))
                                                                                     (set-WS-ball! ws (Ball 350 (- py r 5) 0 0 r))
                                                                                     (set-WS-chance! ws "lower"))))])
                                            (begin (set-2d-down! lives (- (2d-down lives) 1))
                                                   (set-WS-ball! ws (Ball px (- py r 5) 0 0 r))))
                                        (match lives
                                          [(2d lu ld) (if (or (= lu 0) (= ld 0))
                                                          (set! ws (score-board ws))
                                                          (void))])]
                           [(next-outside? ws) (bring-inside ws)]
                           [(hash-empty? bricks) (set! ws (score-board ws))]
                           [else (set-WS-ball! ws (Ball (+ bx bvx) (+ by bvy) bvx bvy r))]))])
           (cond[(not pause) (process-powerup powers)
                             (if (and (equal? mode 'vs-computer) (WS? ws))
                                 (play-computer p2 (WS-ball ws) chance)
                                 (void))])
           ws]
    [else ws]))

(define (apply-powerup name paddle ws)
  (match ws
    [(WS p1 p2 b bricks powers chance score pause lives level mode)
     (cond[(equal? name "c") (if (equal? paddle "lower")
                                 (set-Paddle-W! p1 60)
                                 (set-Paddle-W! p2 60))]
          [(equal? name "e") (if (equal? paddle "lower")
                                 (set-Paddle-W! p1 100)
                                 (set-Paddle-W! p2 100))]
          [(equal? name "li") (if (equal? paddle "lower")
                                  (set-2d-down! lives (+ (2d-down lives) 1))
                                  (set-2d-up! lives (+ (2d-up lives) 1)))]
          [(equal? name "ld") (if (equal? paddle "lower")
                                  (set-2d-down! lives (- (2d-down lives) 1))
                                  (set-2d-up! lives (- (2d-up lives) 1)))])]))

(define (out? ws)
  (cond [(WS-pdl2 ws) (cond [(> (Ball-y (WS-ball ws)) (- height 20)) "lower"]
                            [(< (Ball-y (WS-ball ws)) 20) "upper"]
                            [else #f])]
        [else (if (> (Ball-y (WS-ball ws)) (- height 20)) "lower" #f)]))

(define (done? ws)
  (match ws
    [(WS p1 p2 b bricks powers chance score pause lives level mode) #f]
    [(interface img l) #f]
    ["exit" (stop) #t]
    [else (print "error")]))

(define (score-board ws)
  (stop)
  (match ws
    [(WS p1 p2 b bricks powers chance (2d su sd) pause lives level mode)
        (let*([img1 (cond [p2 (place-images (list (text (~a "Player2 " su) 35 'red)
                                                  (text (~a "Player1 " sd) 35 'red))
                                            (list (make-posn 400 180)
                                                  (make-posn 400 230))
                                             (empty-scene (+ 100 width) height 'yellow))]
                          [else (place-image (text (~a "Your Score " sd) 35 'red)
                                            400 250
                                            (empty-scene (+ 100 width) height 'yellow))])]
              [img (place-images (list (rectangle 75 25 'outline 'red)
                                       (text "Back" 18 'brown))
                                 (list (make-posn 600 450)
                                       (make-posn 600 450))
                                 img1)]
              [iface-new (interface img (list (cons #\b #t)))])
          (set! stack (cons iface-new (cdr stack)))
          iface-new)]))

;=====================================================
;; Big-Bang
;=====================================================
(define (main)
  (big-bang iface-start
    (to-draw render)
    (on-tick tick-handler)
    (on-mouse mouse-handler)
    (on-key key-handler)
    (stop-when done? )
    (name "Brick Breaker")
    ))
