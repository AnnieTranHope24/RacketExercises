;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname RacketLab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;exercise 139
; A List-of-numbers is one of: 
; – '()
(check-expect (pos? (cons 5 '())) #true)
(check-expect (pos? (cons 12 (cons -1 '()))) #false)
(check-expect (pos? '()) #true)
(define (pos? l)
  (cond
    [(empty? l) #true] ;base case
    [(cons? l)
;     (cond[(empty? (rest l)) (positive? (first l))]
;          [else
          (and (positive? (first l))
               (pos? (rest l)))]))

(define (sum aloa)
  (cond
    [(empty? aloa) 0] ; base case
    [(cons? aloa)
     (+ (first aloa) (sum (rest aloa)))]))
;List-of-numbers --> Number
(check-expect (checked-sum (cons 5 (cons 10 '()))) 15)
(check-error (checked-sum (cons 12 (cons -1 '()))) "Cannot compute the sum for this!")
(check-expect (checked-sum '()) 0)
(define (checked-sum l)
  (cond
    [(pos? l) (sum l)]
    [else (error "Cannot compute the sum for this!")]))
;exercise 140
;all-true
;List-of-Boolean-values--> Boolean
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(check-expect (all-true '()) #true)
(define (all-true booleanVal)
  (cond
    [(empty? booleanVal) #true] ;base case
    [(cons? booleanVal) (and (first booleanVal)
          (all-true (rest booleanVal)))]))
;exercise 140
;one-true
;List-of-Boolean-values--> Boolean
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true '()) #false)
(define(one-true booleanVal)
  (cond
    [(empty? booleanVal) #false] ;base case
    [(cons? booleanVal)
     (or (first booleanVal)
         (one-true(rest booleanVal)))]))
;exercise 141
; List-of-string -> String
; concatenates all strings in l into one long string
 
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect
  (cat (cons "ab" (cons "cd" (cons "ef" '()))))
  "abcdef")
 
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))
;exercise 145
(check-expect (sorted>? '()) #true)
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(check-expect (sorted>? (cons 3 (cons 2 '()))) #true)
(check-expect (sorted>? (cons 0(cons 3 (cons 2 '())))) #false)
(define (sorted>? l)
  (cond
    [(empty? l) #true]
    [(empty? (rest l)) #true]
    [(cons? l)
     (and ( > (first l) (first (rest l)))
      (sorted>? (rest l)))]))
;exercise 150
;add-to-pi
; N -> Number
; computes (+ n pi) without using +
 
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond
    [(= n 0) pi]
    [else (add1 (add-to-pi (- n 1)))]))

;add
(check-within (add 3 4.5) (+ 3 4.5) 0.001)
(check-within (add 3 -4.5) (+ 3 -4.5) 0.001)
(check-within (add 0 4.5) (+ 0 4.5) 0.001)
(define (add n x)
  (cond
    [(zero? n) x]
    [else (add1 (add (sub1 n) x))]))
;exercise 151
(check-within (multiply 3 4.5) (* 3 4.5) 0.001)
(check-within (multiply 0 4.5) (* 0 4.5) 0.001)
(define (multiply n x)
  (cond
  [(zero? n) 0]
  [else (+ x (multiply(sub1 n) x))]))
;exercise 163
; List-of-numbers -> List-of-numbers
(check-expect (convert 32) 0)
(check-expect (convert 59) 15)
(define (convert n)
  (/(*(- n 32) 5)9))
(check-expect (convertFC '()) '())
(check-expect (convertFC (list 32 59 68)) (list 0 15 20))
(define (convertFC l)
  (cond
    [(empty? l)'()]
    [(cons? l) (cons (convert (first l)) (convertFC (rest l)))]))
;exercise 164
; List-of-numbers -> List-of-numbers
(check-expect (convert-euro* 2 (list 32 10 12)) (list 64 20 24))
(check-expect (convert-euro* 2 '()) '())
(define (convert-euro* rate l)
  (cond
    [(empty? l)'()]
    [(cons? l) (cons (* rate (first l)) (convert-euro* rate(rest l)))]))
;exercise 165
(check-expect (subs "hi" "hello" "hello") "hi")
(check-expect (subs "hi" "hello" "good") "good")
(define (subs new old word)
  (cond
   [(string=? word old) new]
   [else word]))
(check-expect (substitute "annie" "ngoc" (list "anna" "ngoc" "holland" "ngoc" )) (list "anna" "annie" "holland" "annie"))
(check-expect (substitute  "annie" "ngoc" '()) '())
(define (substitute new old l)
  (cond
    [(empty? l)'()]
    [(cons? l)
     (cons (subs new old (first l)) (substitute new old (rest l)))]))
;exercise 189
; Number List-of-numbers -> Boolean
(check-expect (search-sorted 2 (list 1 3 7)) #false)
(check-expect (search-sorted 2 (list 1 2 7)) #true)
(check-expect (search-sorted 2 '()) #false)
(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (and(>= n (first(rest alon)))
              (search-sorted n (rest alon))))]))
