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
;; 20130212
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
;@+node:peckj.20130212140318.1383: *3* board
; board class is a list of the format (Board (Stack1 Stack2 ... Stack9))
(new Class 'Board)

(define (Board:Board (stacks))
  (list Board stacks))

(define (Board:print)
  (dolist (stack (self 1))
    (print $idx ": ")
    (:print stack)))

; move procedure:
; make new board with 1 fewer stack
; distribute pieces on stacks sequentially
(define (Board:makemove (stack 0))
  (letn (
        (b1 (0 stack (self 1)))
        (b2 ((+ 1 stack) (self 1)))
        (s ((self 1) stack))
        (b (Board (append b1 b2))))
    (setq idx stack)
    (dotimes (n (:length s))
      (:push ((b 1) idx) (:pop s))
      (inc idx)
      (if (>= idx (length (b 1))) (setq idx 0)))
    b))
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
;@+node:peckj.20130212140318.1385: *3* piece
(new Class 'Piece)

(define (Piece:Piece (type))
  (list Piece type))

(define (Piece:print)
  (print (term (self 1)) " "))
;@-others

;; driver code
(setq test-board 
  (Board '(
    (Stack ((Piece W)))
    (Stack ((Piece R)))
    (Stack ((Piece R)))
    (Stack ((Piece B)))
    (Stack ((Piece R)))
    (Stack ((Piece W)))
    (Stack ((Piece W)))
    (Stack ((Piece B)))
    (Stack ((Piece B))))))
    
(:print test-board)

(println "Testing... moving from stack 3")
(setq test-board (:makemove test-board 3))
(:print test-board)

(println "Testing... moving from stack 3")
(setq test-board (:makemove test-board 3))
(:print test-board)

(println "Testing... moving from stack 0")
(setq test-board (:makemove test-board 0))
(:print test-board)

(println "Testing... moving from stack 0")
(setq test-board (:makemove test-board 0))
(:print test-board)

(println "Testing... moving from stack 0")
(setq test-board (:makemove test-board 0))
(:print test-board)

(println "Testing... moving from stack 0")
(setq test-board (:makemove test-board 0))
(:print test-board)

(println "Testing... moving from stack 0")
(setq test-board (:makemove test-board 0))
(:print test-board)

(println "Testing... moving from stack 0")
(setq test-board (:makemove test-board 0))
(:print test-board)

(exit)
;@-leo
