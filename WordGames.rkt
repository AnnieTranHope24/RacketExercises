;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname WordGames) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
; FIGURE 82 code for Finding alternative words

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
 
; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))

; List-of-words -> List-of-strings
; turns all Words in low into Strings
(check-expect (words->strings (list(list "c" "a" "t") (list "d" "o" "g"))) (list "cat""dog"))
(check-expect (words->strings '()) '())
(define (words->strings low)
  (cond
    [(empty? low) '()] ; base case
    [(cons? low) (cons (word->string (first low))(words->strings (rest low))) ]))   
  
 
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
; look at section 12.1 to load the dictionary AS-LIST
(check-expect (in-dictionary (list "Aani" "aam" "aaam")) (list "Aani" "aam"))
(check-expect (in-dictionary '()) '())
(define AS-LIST (read-lines "words.txt"))
(define (in-dictionary los)
  (cond
    [(empty? los) '()] ; base case
    [(cons? los) (cond
       [(member? (first los) AS-LIST) (cons (first los)(in-dictionary (rest los)))]
       [else (in-dictionary (rest los))])]))

; EXERCISE 209

; String -> Word as a list-of-letters
; converts s to the chosen word representation
(check-expect (string->word "") '())
(check-expect (string->word "cat") (list "c" "a" "t"))
(define (string->word s)
    (cond
    [(empty? s) '()] ; base case
    [else(explode s)]))
 
; Word as a list-of-letters -> String
; converts w to a string
(check-expect (word->string (list "c" "a" "t")) "cat")
(check-expect (word->string '()) "")
(define (word->string w)
  (cond
    [(empty? w) ""] ; base case
    [(cons? w) (implode w)]))


; SECTION 12.4 code

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; Word as a list-of-letters -> List-of-words
; finds all rearrangements of word
(check-expect (arrangements '()) (list '()) )
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else
     (cond
       [(empty? (rest w)) (list w)]
       [else
       (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))])]))

; add insert-everywhere/in-all-words to your wish list
; add insert-everywhere/in-one-word to your wish list (DrDJ's recommendation)
; need a 3rd function that takes a letter and a list-of-words and adds
; the letter to the front of each word

;add-letter-to-front
(check-expect (add-letter-to-front "c" (list (list "o") (list "o" "w")))
              (list (list "c" "o") (list "c" "o" "w")))
(check-expect (add-letter-to-front "e" '()) '())
(define (add-letter-to-front letter lolol)
  (cond [(empty? lolol) '()]
        {else (cons (cons letter (first lolol)) (add-letter-to-front letter (rest lolol)))}))
;insert-everywhere/in-one-word
(check-expect (insert-everywhere/in-one-word "c" (list "a""t"))(list(list"c""a""t")(list"a""c""t")(list"a""t""c") ))
(check-expect(insert-everywhere/in-one-word "c" '()) '())
(define (insert-everywhere/in-one-word c word)             
                (cond [(empty? word) '()]
                      [else (cons (cons c word)(add-letter-to-front (first word)
                                                                    (cond
                                                                      [(empty? (rest word)) (list (list c))]
                                                                      [else (insert-everywhere/in-one-word c (rest word))])))]))
                                                                    
;insert-everywhere/in-all-words to your wish list
(check-expect (insert-everywhere/in-all-words "c" (list(list "a""t") (list"a""r")))(list(list"c""a""t")(list"a""c""t")(list"a""t""c")(list"c""a""r")(list"a""c""r")(list"a""r""c") ))
(check-expect(insert-everywhere/in-all-words "c" '()) '())
(define (insert-everywhere/in-all-words c words)
  (cond [(empty? words) '()]
        [else (append (insert-everywhere/in-one-word c (first words)) (insert-everywhere/in-all-words c (rest words)))]))
              