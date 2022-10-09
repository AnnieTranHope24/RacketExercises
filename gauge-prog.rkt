;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gauge-prog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

; Constants for drawing a mood bar on background scene




(define SCENE-LENGTH 100)                                
(define BACKGROUND (empty-scene SCENE-LENGTH 50))
(define Y-CAR 25)
(define moodbar (rectangle 100 50 "solid" "red"))

; CHECK-EXPECT: test cases to verify that tock is working properly
(check-expect (tock 300) 299.9)
(check-expect (tock 3) 2.9)
(check-expect (tock 9) 8.9)
(check-expect (tock 20) 19.9)
(check-expect (tock 0) 0)
(define (tock ws)
  (cond [(>= ws 0.1 ) (- ws 0.1)]
        [else ws]))


(define (render ws)
  (place-image moodbar (- ws 50) Y-CAR BACKGROUND))

(check-expect (move-to-edge 150 "down") 120)
(check-expect (move-to-edge 60 "up") 80)
(check-expect (move-to-edge 15 "up") 20)
(define (move-to-edge ws a-key)
  (cond [(key=? "down" a-key) (/ (* ws 4) 5)]
        [(key=? "up" a-key) (/ (* ws 4) 3)]
        [else ws]))
(check-expect (cat-off-screen? 327) false)
(check-expect (cat-off-screen? 0) true)
(define (cat-off-screen? ws) 
  (cond [(<= ws 0 ) true]
        [else false]))

 ; Initial World State is zero pixels from the left border of the scene
(define (main ws)
  (big-bang ws 
            [on-tick tock]
            (to-draw render)
            (stop-when cat-off-screen?)
            (on-key move-to-edge)))
(main 100)