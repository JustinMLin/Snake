;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex9-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "snake-lib.rkt")

; a game is
; (make-game snake food obstacles nat)
; a nat is just a natural number
;(define-struct game (snake food obstacles ticks))

; a direction is either
; - 'up
; - 'down
; - 'left
; - 'right
; If this type looks new to you, its just a symbol.
; That is ‘up is a symbol and “up” is a string.
; Symbols are like strings without spaces. 


; a snake is
; (make-snake direction body)
;(define-struct snake (heading segments))

; a body is either
; - (cons posn empty)
; - (cons posn body)
; That is, a body is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; a food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-food : game posn -> game
; Given a game and posn, returns a new game where food has been added
;   at that posn. 
(define (add-food g p)
 (make-game (game-snake g) (cons p (game-food g)) (game-obstacles g) (game-ticks g)))

(check-expect
 (add-food (make-game (make-snake 'up (list (make-posn 1 2)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      5)
           (make-posn 6 7))
 (make-game (make-snake 'up (list (make-posn 1 2)))
            (list (make-posn 6 7) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            5))

; change-direction : game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 
(define (change-direction g d)
  (make-game (make-snake d (snake-segments (game-snake g))) (game-food g) (game-obstacles g) (game-ticks g)))

(check-expect
 (change-direction
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'left)
 (make-game (make-snake 'left (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5))

; game-score : game -> nat
; Given a game, returns a score (as a number)
(define (game-score g)
  (- (* 100 (length (snake-segments (game-snake g)))) (game-ticks g)))

; no tests are provided for game-score because it is open-ended

; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise. 
(define (game-over? g)
  (local[(define (out-of-bounds g segments)
           (cond[(empty? segments)                     false]
                [(or (<= (posn-x (first segments)) 0)
                     (>= (posn-x (first segments)) 51))  true]
                [(or (<= (posn-y (first segments)) 0)
                     (>= (posn-y (first segments)) 51)) true]
                [else                                  (out-of-bounds g (rest segments))]))
         
         (define (exists? posn lop)
           (cond[(empty? lop) false]
                [(equal? posn (first lop)) true]
                [else (exists? posn (rest lop))]))
         
         (define (same-posn g segments)
           (cond[(empty? segments) false]
                [(exists? (first segments) (rest segments)) true]
                [else (same-posn g (rest segments))]))
         
         (define (hit-obstacle g segments)
           (cond[(empty? segments) false]
                [(exists? (first segments) (game-obstacles g)) true]
                [else (hit-obstacle g (rest segments))]))
         ]

    (or (out-of-bounds g (snake-segments (game-snake g)))
        (same-posn g (snake-segments (game-snake g)))
        (hit-obstacle g (snake-segments (game-snake g))))))
    
                              
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn -1 1))) empty empty 5))
 true)

; advance-game : game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not. 
(define (advance-game g)
  (local[(define (remove p food) ;removes the food at posn p
           (filter (λ(f)
                     (not (equal? p f)))
                   food))]
    (make-game (make-snake (snake-heading (game-snake g))
                           (cond[(equal? (snake-heading (game-snake g)) 'up) (if (member? (make-posn (posn-x (first (snake-segments (game-snake g)))) (+ 1 (posn-y (first (snake-segments (game-snake g))))))
                                                                                          (game-food g))
                                                                                 (cons (make-posn (posn-x (first (snake-segments (game-snake g)))) (+ 1 (posn-y (first (snake-segments (game-snake g)))))) (snake-segments (game-snake g)))
                                                                                 (cons (make-posn (posn-x (first (snake-segments (game-snake g)))) (+ 1 (posn-y (first (snake-segments (game-snake g)))))) (reverse (rest (reverse (snake-segments (game-snake g)))))))]
                                [(equal? (snake-heading (game-snake g)) 'down) (if (member? (make-posn (posn-x (first (snake-segments (game-snake g)))) (- (posn-y (first (snake-segments (game-snake g)))) 1))
                                                                                          (game-food g))
                                                                                 (cons (make-posn (posn-x (first (snake-segments (game-snake g)))) (- (posn-y (first (snake-segments (game-snake g)))) 1)) (snake-segments (game-snake g)))
                                                                                 (cons (make-posn (posn-x (first (snake-segments (game-snake g)))) (- (posn-y (first (snake-segments (game-snake g)))) 1)) (reverse (rest (reverse (snake-segments (game-snake g)))))))]
                                [(equal? (snake-heading (game-snake g)) 'left) (if (member? (make-posn (- (posn-x (first (snake-segments (game-snake g)))) 1) (posn-y (first (snake-segments (game-snake g)))))
                                                                                          (game-food g))
                                                                                 (cons (make-posn (- (posn-x (first (snake-segments (game-snake g)))) 1) (posn-y (first (snake-segments (game-snake g))))) (snake-segments (game-snake g)))
                                                                                 (cons (make-posn (- (posn-x (first (snake-segments (game-snake g)))) 1) (posn-y (first (snake-segments (game-snake g))))) (reverse (rest (reverse (snake-segments (game-snake g)))))))]
                                [(equal? (snake-heading (game-snake g)) 'right) (if (member? (make-posn (+ 1 (posn-x (first (snake-segments (game-snake g))))) (posn-y (first (snake-segments (game-snake g)))))
                                                                                          (game-food g))
                                                                                 (cons (make-posn (+ 1 (posn-x (first (snake-segments (game-snake g))))) (posn-y (first (snake-segments (game-snake g))))) (snake-segments (game-snake g)))
                                                                                 (cons (make-posn (+ 1 (posn-x (first (snake-segments (game-snake g))))) (posn-y (first (snake-segments (game-snake g))))) (reverse (rest (reverse (snake-segments (game-snake g)))))))]))
               (cond[(equal? (snake-heading (game-snake g)) 'up) (if (member? (make-posn (posn-x (first (snake-segments (game-snake g)))) (+ 1 (posn-y (first (snake-segments (game-snake g))))))
                                                                              (game-food g))
                                                                     (remove (make-posn (posn-x (first (snake-segments (game-snake g)))) (+ 1 (posn-y (first (snake-segments (game-snake g)))))) (game-food g))
                                                                     (game-food g))]
                    [(equal? (snake-heading (game-snake g)) 'down) (if (member? (make-posn (posn-x (first (snake-segments (game-snake g)))) (- (posn-y (first (snake-segments (game-snake g)))) 1))
                                                                              (game-food g))
                                                                     (remove (make-posn (posn-x (first (snake-segments (game-snake g)))) (- (posn-y (first (snake-segments (game-snake g)))) 1)) (game-food g))
                                                                     (game-food g))]
                    [(equal? (snake-heading (game-snake g)) 'left) (if (member? (make-posn (- (posn-x (first (snake-segments (game-snake g)))) 1) (posn-y (first (snake-segments (game-snake g)))))
                                                                              (game-food g))
                                                                     (remove (make-posn (- (posn-x (first (snake-segments (game-snake g)))) 1) (posn-y (first (snake-segments (game-snake g))))) (game-food g))
                                                                     (game-food g))]
                    [(equal? (snake-heading (game-snake g)) 'right) (if (member? (make-posn (+ 1 (posn-x (first (snake-segments (game-snake g))))) (posn-y (first (snake-segments (game-snake g)))))
                                                                              (game-food g))
                                                                     (remove (make-posn (+ 1 (posn-x (first (snake-segments (game-snake g))))) (posn-y (first (snake-segments (game-snake g))))) (game-food g))
                                                                     (game-food g))])
               (game-obstacles g)
               (+ 1 (game-ticks g)))))
             
                                                                                                                                                
(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             empty
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)))
            empty
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             (list (make-posn 2 1) (make-posn 8 9))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)
                                    (make-posn 3 3)))
            (list (make-posn 8 9))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))

; a starting game to experiment with
(define game-start
  (make-game (make-snake 'up (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game advance-game add-food change-direction game-score game-over?))

;to start a game
;(play game-start)