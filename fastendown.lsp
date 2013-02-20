#!/usr/bin/env newlisp
;@+leo-ver=5-thin
;@+node:peckj.20130212140318.1380: * @file fastendown.lsp
;@@first
;@@color
;@@language lisp
;@@tabwidth -2
;@+<< comment header >>
;@+node:peckj.20130212140318.1381: ** << comment header >>
;; 
;; fastendown
;; an AI for Button Up!
;; Jacob Peck
;; 20130213
;; https://github.com/gatesphere/fastendown
;;

;;
;; Button Up! rules available here:
;; http://boiteajeux.net/jeux/btu/aide.php
;; 

;;
;; usage: newlisp fastendown.lsp
;;
;@-<< comment header >>
;@+others
;@+node:peckj.20130212140318.1382: ** board + pieces
;@+node:peckj.20130212140318.1384: *3* stack
; stack class is a list of the format (Stack (Piece1 Piece2 ... Piece9))
(new Class 'Stack)

(define (Stack:Stack (contents))
  (list Stack contents))
  
(define (Stack:print)
  (dolist (piece (self 1))
    (:print piece))
    (println))

(define (Stack:pop)
  (pop (self 1) -1))

(define (Stack:push (piece))
  (push piece (self 1)))

(define (Stack:length)
  (length (self 1)))
  
(define (Stack:clone)
  (setq a (list Stack (self 1)))
  (list Stack (self 1)))

;@+others
;@+node:peckj.20130212140318.1386: *4* predicates
(define (Stack:moveagain?)
  (and (>= (length (self 1)) 2)
       (= ((self 1) 0) ((self 1) 1))))

(define (Stack:validmove?)
  (member '(Piece W) (self 1)))
;@-others
;@+node:peckj.20130212140318.1383: *3* board
; board class is a list of the format (Board (Stack1 Stack2 ... Stack9))
(new Class 'Board)

(define (Board:Board (stacks))
  (list Board stacks))

(define (Board:print)
  (dolist (stack (self 1))
    (print $idx ": ")
    (:print stack)))

(define (Board:clone)
  (let ((l '()))
    (dolist (s (self 1))
      (setq l (append l (list (:clone s)))))
    (list Board l)))

;@+others
;@+node:peckj.20130212140318.1388: *4* predicates + queries
(define (Board:validmove? (stack 0))
  (:validmove? ((self 1) stack)))
  
(define (Board:validmoves)
  (let ((l '()))
    (dolist (stack (self 1))
      (if (:validmove? stack)
        (setq l (push $idx l))))
    l))

(define (Board:gameover?)
  (= 1 (length (self 1))))

; + score -> red wins
; - score -> black wins
(define (Board:score)
  (let ((s (reverse (((self 1) 0) 1))))
    (setq red (apply '+ (map (fn (x) (+ 1 x)) (index (fn (x) (= '(Piece R) x)) s))))
    (setq black (apply '+ (map (fn (x) (+ 1 x)) (index (fn (x) (= '(Piece B) x)) s))))
    (- red black)))
;@+node:peckj.20130212140318.1387: *4* makemove
; move procedure:
; make new board with 1 fewer stack
; distribute pieces on stacks sequentially
(define (Board:makemove (stack 0))
  (letn ((b1 (0 stack (self 1)))
         (b2 ((+ 1 stack) (self 1)))
         (s ((self 1) stack))
         (l (append b1 b2))
         (l2 '())
         (b '()))
    ; make a new board
    (dolist (x l)
      (setq l2 (append l2 (list (:clone x)))))
    (setq b (Board l2))
    
    ; drop a chip on each stack
    (setq idx (- stack 1))
    (dotimes (n (:length s))
      (if (< n (length (b 1)))
        (inc idx))
      (if (>= idx (length (b 1))) (setq idx 0))
      (:push ((b 1) idx) (:pop s)))
    
    ; return a list of the new board + moveagain state
    (list b (:moveagain? ((b 1) idx)))))
;@-others
;@+node:peckj.20130212140318.1385: *3* piece
(new Class 'Piece)

(define (Piece:Piece (type))
  (list Piece type))

(define (Piece:print)
  (print (term (self 1)) " "))
;@+node:peckj.20130213082445.1969: ** players
;@+node:peckj.20130213082445.1970: *3* random player
(new Class 'RandomPlayer)

(define (RandomPlayer:RandomPlayer (color '(Piece R)))
  (list RandomPlayer color))

(define (RandomPlayer:print)
  (println "RandomPlayer, color " (:print (self 1))))

(define (RandomPlayer:makemove (board))
  (let ((validmoves (:validmoves board)))
    (setq move (rand (length validmoves)))
    (println "Player (" (term ((self 1) 1)) ") making move: " (validmoves move))
    (:makemove board (validmoves move))))
;@+node:peckj.20130214101432.1390: *3* minimax player
(new Class 'MinimaxPlayer)

(define (MinimaxPlayer:MinimaxPlayer (color '(Piece R)))
  (list MinimaxPlayer color))

(define (MinimaxPlayer:print)
  (println "MinimaxPlayer, color " (:print (self 1))))

;@+others
;@+node:peckj.20130214101432.1392: *4* findmove
(define (MinimaxPlayer:findmove (board) (move) (maximizing) (depth 0))
  ;(println "in findmove: " move )
  (let ((b (:makemove board move)))
    ;(println b)
    ;(:print (b 0))
    ;(println "game over? " (:gameover? (b 0)))
    (if (:gameover? (b 0))
      ; if game is over, return score
      ;(println (:score (b 0)))
      (:score (b 0))
      ; else, return minimaxed score
      (let ((score 0))
        (dolist (m (:validmoves (b 0)))
          (let ()
            (setq x 
              (MinimaxPlayer:findmove 
                (:clone (b 0)) 
                m 
                (if (b 1) maximizing (not maximizing)) 
                (+ 1 depth)))
            ;(println "winding up... " x " " maximizing " " score " " depth)
            (if (or 
                  (and maximizing (> x score)) 
                  (and (not maximizing) (< x score)))
              (let ()
                (setq score x)
                ;(println "score set: " score)
                ;(println b)
                ))))
         score))))
;@+node:peckj.20130214101432.1393: *4* makemove
(define (MinimaxPlayer:makemove (board))
  (let ((validmoves (:validmoves board)))
    (setq move (validmoves 0)) ; default move to make
    (setq maximizing (= (self 1) '(Piece R)))
    (setq score 0)
    ;(println "move: " move " maximizing: " maximizing " score: " score)
    (dolist (m validmoves)
      (setq s (MinimaxPlayer:findmove board m maximizing))
      ;(println "m: " m " s: " s)
      (if (or (and maximizing (> s score)) (and (not maximizing) (< s score))) 
        (let () (setq move m) (setq score s))))
    (println "Player (" (term ((self 1) 1)) ") making move: " move)
    (:makemove board move)))
;@-others
;@+node:peckj.20130213082445.1977: *3* human player
(new Class 'HumanPlayer)

(define (HumanPlayer:HumanPlayer (color '(Piece R)))
  (list HumanPlayer color))

(define (HumanPlayer:print)
  (println "HumanPlayer, color " (:print (self 1))))

(define (HumanPlayer:makemove (board))
  (let ((validmoves (:validmoves board)))
    (setq move nil)
    (while (= nil move)
      (println "Valid moves are: " validmoves)
      (print "Please enter your move: ")
      (setq move (read-line))
      (if (= move "q") (exit))
      (setq move (int move))
      (if (not (member move validmoves))
        (let ()
          (println "That move is invalid!")
          (setq move nil))))
    (println "Player (" (term ((self 1) 1)) ") making move: " move)
    (:makemove board move)))
;@+node:peckj.20130213082445.1971: ** game
;@+node:peckj.20130213082445.1973: *3* random board
(define (randomboard)
  (let ((b '(
             (Stack ((Piece R)))
             (Stack ((Piece R)))
             (Stack ((Piece R)))
             (Stack ((Piece W)))
             (Stack ((Piece W)))
             (Stack ((Piece W)))
             (Stack ((Piece B)))
             (Stack ((Piece B)))
             (Stack ((Piece B))))))
  (setq *gameboard* (Board (randomize b true)))))

    
;@+node:peckj.20130214101432.1395: *3* custom board
(define (customboard (b))
  (let ((in (eval-string b)))
    (setq stks '())
    (dolist (s in)
      (setq currstk '())
        (dolist (p s)
          (setq currstk (append currstk (list (Piece p)))))
      (setq stks (append stks (list (Stack currstk)))))
    (setq *gameboard* (Board stks))))
;@+node:peckj.20130213082445.1974: *3* swap turns
(define (swapturns)
  (reverse *currentturn*))
;@+node:peckj.20130213082445.1976: *3* init game
(define (init-game)
  (seed (time-of-day))
  (setq myargs (2 (main-args)))
  (if (= 4 (length myargs))
    (let ()
      (setq b-type (myargs 0))
      (setq rp (myargs 1))
      (setq bp (myargs 2))
      (setq fp (myargs 3)))
    (let ()
      (print "Board: (r)andom, or (c)ustom? ")
      (setq b-type (read-line))
      (if (= b-type "c")
        (let ()
          (print "Please enter board layout: ")
          (setq b-type (read-line))))
      (print "Red: (h)uman, (r)andom, or (m)inimax? ")
      (setq rp (read-line))
      (print "Black: (h)uman, (r)andom, or (m)inimax? ")
      (setq bp (read-line))
      (print "Who goes first, (r)ed, or (b)lack? ")
      (setq fp (read-line))))
      
  (if (= b-type "r") 
    (randomboard)
    (customboard b-type))
  
  (if (= rp "h") (setq *player1* (HumanPlayer '(Piece R))))
  (if (= rp "r") (setq *player1* (RandomPlayer '(Piece R))))
  (if (= rp "m") (setq *player1* (MinimaxPlayer '(Piece R))))

  (if (= bp "h") (setq *player2* (HumanPlayer '(Piece B))))
  (if (= bp "r") (setq *player2* (RandomPlayer '(Piece B))))
  (if (= bp "m") (setq *player2* (MinimaxPlayer '(Piece B))))
  (setq *currentturn* (list *player1* *player2*))
  
  (if (= fp "b") (swapturns)))
;@+node:peckj.20130213082445.1972: *3* game loop
(define (game-loop)
  (until (:gameover? *gameboard*)
    (:print *gameboard*)
    (make-a-move))
  (:print *gameboard*)
  (println "Final score: " (:score *gameboard*)))

  
;@+node:peckj.20130213082445.1975: *3* make a move
(define (make-a-move)
  (let ((p (*currentturn* 0)))
    (:print p)
    (setq retval (:makemove p (:clone *gameboard*)))
    (setq *gameboard* (retval 0))
    (if (not (retval 1)) (swapturns))))
;@-others

(init-game)
(game-loop)

(exit)
;@-leo
