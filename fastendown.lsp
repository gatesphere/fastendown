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
  (letn (
        (b1 (0 stack (self 1)))
        (b2 ((+ 1 stack) (self 1)))
        (s ((self 1) stack))
        (b (Board (append b1 b2))))
    (setq idx (- stack 1))
    (dotimes (n (:length s))
      (inc idx)
      (if (>= idx (length (b 1))) (setq idx 0))
      (:push ((b 1) idx) (:pop s)))
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
      (setq move (int (read-line)))
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

    
;@+node:peckj.20130213082445.1974: *3* swap turns
(define (swapturns)
  (reverse *currentturn*))
;@+node:peckj.20130213082445.1976: *3* init game
(define (init-game)
  (seed (time-of-day))
  (setq *gameboard* (randomboard))
  (setq *player1* (HumanPlayer '(Piece R)))
  (setq *player2* (RandomPlayer '(Piece B)))
  (setq *currentturn* (list *player1* *player2*)))
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
    (setq retval (:makemove p *gameboard*))
    (setq *gameboard* (retval 0))
    (if (not (retval 1)) (swapturns))))
;@-others

(init-game)
(game-loop)

(exit)
;@-leo
