;; Spades, clubs, Hearts, Diamonds
;; Coding cards
; Deck By hand ordered specified by the CW.
(define clubs (list (cons 1 #\C) (cons 2 #\C) (cons 3 #\C) (cons 4 #\C) (cons 5 #\C) (cons 6 #\C) (cons 7 #\C) (cons #\J #\C) (cons #\Q #\C) (cons #\K #\C)) )
(define hearts (list (cons 1 #\H) (cons 2 #\H) (cons 3 #\H) (cons 4 #\H) (cons 5 #\H) (cons 6 #\H) (cons 7 #\H) (cons #\J #\H) (cons #\Q #\H) (cons #\K #\H)) )
(define spades (list (cons 1 #\S) (cons 2 #\S) (cons 3 #\S) (cons 4 #\S) (cons 5 #\S) (cons 6 #\S) (cons 7 #\S) (cons #\J #\S) (cons #\Q #\S) (cons #\K #\S)) )
(define diamonds (list (cons 1 #\D) (cons 2 #\D) (cons 3 #\D) (cons 4 #\D) (cons 5 #\D) (cons 6 #\D) (cons 7 #\D) (cons #\J #\D) (cons #\Q #\D) (cons #\K #\D)) )

(require racket/list)  ;; shuffle


; F1 [DONE] F2 [DONE] Except Optional | Working on F3
;---------------------------------------------------------------------------------------------------------------------
; F1
; ii.
; numeral, and simbol
(define (numeral card)
  ; Use the card build in method to get the first value
  (car card)
  )

(define (suite card)
  ; Use the card build in method to get the first value
  (cdr card)
  )

; i.
; Write a predicate function called card?, so that given a value, yields #t when it corresponds to the following
; agreement, and #f otherwise.
(define (contains list x)
  ; If x is the first element return #t, else call itself
  ; passing the list excluding the first element
  ; #f if the element not found and list is empty

	(cond [(null? list) #f]
		[(equal? (car list) x) #t]
		[else (contains (cdr list) x)]))

(define (card? card)
  ; Valid Faces and Valid Symbols
  (define numerals (list 1 2 3 4 5 6 7 #\J #\Q #\K))
  (define symbols (list #\H #\S #\C #\D)) 
  
  (cond [(eqv? (pair? card) #f) #f] ;if card is not a pair return false
        [(eqv? (contains numerals (numeral card)) #f) #f] ; if numeral is not valid return false
        [(eqv? (contains symbols (suite card)) #f) #f] ; if symbol is not valid return false
        [else #t] ; if card passed all traps, return true
        )
)

; iii.
; Write a function face? returning either #t if a given card is a face card (Jack, Queen or King) or #f if it is
; not, assuming a valid card has been passed

(define (face? card)
   (define faceCards (list #\J #\Q #\K))
    ; if the face is not in faceCards return #f, otherwise #t
  (cond [(eqv? (contains faceCards (numeral card)) #f) #f]
        [else #t]
        )
 )

; IV
; Write a function value yielding the value for a card according to the rules explained above.

(define (value card)
   ; if numeral (face) in faceCards, return 0.5, otherwise the numeral of the card
   (define faceCards (list #\J #\Q #\K))
  (cond [(eqv? (contains faceCards (numeral card)) #t) 0.5]
        [else (numeral card)]
        )
 )

; V
; Write a function called card->string. Given a valid card, it returns a human readable string for it.
(define (card->string card)
  ;assuming the card is valid
  (define faceCards (list #\K #\Q #\J))
  ; if card face is K, Q or J return the concatenation of the numeral and suite
  ; if card is not K, Q or J convert the number to string,
  ; concatenate it to the suite and return it

  (cond [(eqv? (contains faceCards (numeral card))#t)
         (string-append (string (numeral card)) (string (suite card)))]
        [else (string-append (number->string (numeral card)) (string (suite card)))]
  )
  )


;F2
; i.
; Using the function card? above, declare a function deck? Which, provided a list of values, returns #t if it is
; a valid deck or #f otherwise
 (define numerals (list 1 2 3 4 5 6 7 #\J #\Q #\K))
 (define symbols (list #\H #\S #\C #\D)) 
; The deck. ; Both work as well but im using mine just because it prints the deck in order from 1-7 j-k from each suite.
;Creation of the deck by John
;(define n '(1 2 3 4 5 6 7 #\J #\Q #\K))
;(define s '(#\H #\S #\C #\D))
;(define (deck)
;  (apply append
;      (map (lambda (x)
;              (map (lambda (y) (cons x y))
;                  s))
;            n))
;  )
;(define deck (append hearts spades clubs diamonds))
;Creation of the deck by Luis

(define (deck? lst)
  ; Recursive way of iterating through the list
  ; If the first card is not valid return #f
  ; call itself passing the lst excluding the first element
  ; if empty is null, return #t as none of the elements gave #f

 (cond [(eqv? (null? lst) #t) #t]
         [(eqv? (card? (first lst)) #f) #f]
         [else (deck? (cdr lst))]
         )
  )

;(deck? deck) ; To call this deck needs to be declared above it.

; ii.
; Given a list of cards, declare a function
; valueOf which calculates the sum of each of the cards

(define (valueOf lst)
  ; if the list is empty return 0
  ; else return value of the face of the first card + the value that the program will ; get by calling itself passing the same list excluding the first element 

    (if
    (null? lst)
    0
    (+ (value (first lst)) (valueOf (cdr lst)))
  )
  )



; iii.
;Declare a function do-suite, such that given a valid suit (#\C, #\H, #\D,#\S) returns the whole series for it,
;including the numbers from 1 to 7 and the faces #\J, #\Q and #\K.

(define (do-suite validSuit)
  ; clubs, hearts, spades and diamonds are predefined lists that contain their cards
  ; Single check if validSuit match C, H, S or D to return the list of cards.

  (cond [(eqv? (eqv? validSuit #\C) #t) clubs]
        [(eqv? (eqv? validSuit #\H) #t) hearts]
        [(eqv? (eqv? validSuit #\S) #t) spades]
        [(eqv? (eqv? validSuit #\D) #t) diamonds]
        )
  )

; iV.
; Redefine the constant symbol deck, so that it includes the entire list of the 40 valid cards ((7 + 3) * 4).
(define deck (append hearts spades clubs diamonds))

; V.
; Define a function deck->strings so that passed a valid deck, it returns a human representation of it, i.e., a
; list of strings.
; 2h on this 3 lines of code :(
(define (deck->strings lstCards)
  ; Using map, we can target each card in the lstCards.
  ; The card->string will convert each card into a string and print it.

 (map (lambda (i) (card->string i))
       lstCards))

;(deck->string deck )

; VI.
; OPTIONAL: Define a function strings->deck so that given a valid list of formatted strings, it produces a
; valid deck.


; F3.
; From basic statistics, we know that the probability of a success is obtained by dividing the number of favourable
; occurrences by the total number of events. If 4 cards would give you a good outcome the probability of success
; would be 4 / 40 = 0.1 (or 10%).

(define (probability expression n lst)
  ; Base Case: list is empty, so return 0
  ; otherwise, return the sum of cards that evaluate to true for the given
  ; expression
  
  ; is expression correct? Add 1. Is it not? Add 0 
  ; Repeat this for every face of every card

   (if
   
    (null? lst)
    0
    (+ (if (expression (value (first lst)) n) 1 0) (probability expression n (cdr lst)))
  )
  
  )

; Cheat
(define cheat #t)

;; F4.- Game.
;; DO NO CHANGE THE  FUNCTIONS BELOW THIS LINE
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
(define (show-statistics deck hand)
  ; This function displays the deck and the
  ; probabilities of the player obtaining a 
  ; card that is greater, smaller and    equal to what they want.

  (let
    ([toCheck (- 7.5 (valueOf hand))])
    (display
     (format
      "P(>7.5):~a/~a\nP(<7.5):~a/~a\nP(=7.5):~a/~a\nHAND:~a~nVALUE:~a\nDECK:~a...\n"
      (probability > toCheck deck)
      (length deck)
      (probability < toCheck deck)
      (length deck)
      (probability = toCheck deck)
      (length deck)                     
      (deck->strings hand)
      (valueOf hand)
      (if cheat (deck->strings (take deck
                                   (max 0 
                                    (min 4 (length deck) )))) "****")
      )
     )))

;; Human interaction.
(define (play deck hand)
  (begin      
      (show-statistics deck hand)
      ;; Control
      (cond
      [(= (valueOf hand) 7.5) (display "WIN")]
      [(> (valueOf hand) 7.5) (display "LOST")]
      [(empty? deck) (display "NO CARDS LEFT") ]
      [(let
           ([ command (read)])
           (cond
             [(equal? command 'accept)
               (play (rest deck) (cons (first deck) hand))]
             [(equal? command 'pass)
               (play (drop deck 1) hand)]
             [(equal? command 'end) (void)]
             [else (play deck hand)]))])))



(play (shuffle deck) '())