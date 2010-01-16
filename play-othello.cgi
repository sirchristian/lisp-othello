#!/usr/local/bin/clisp -q
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Christian Heinzmann
;;; CS 380
;;; Assignment 8
;;;
;;; Implements MINIMAX with alpha-beta pruning.  The functions will be used
;;; to play an othello game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "Content-Type: text/plain~%")
(format t "~%")

(load "othello.fas")

;; Global declarations
(defvar *time-move-started*)
(defvar *max-seconds-per-turn*)
(defvar *max-seconds-per-operator*)

(defun MINIMAX-DECISION (game)
  ;; Returns an operator to be applied to the current game that will yield
  ;; the best utility. (i.e. It is my turn and I return what I think is the
  ;; best move)
  (setf ops (current-game-operators-max game))
  (setf *max-seconds-per-operator*
	(/ *max-seconds-per-turn*
	   (count-items ops)))
  (setf value-list '())
  (cond ((not (any-legal-move?  
		(current-game-player game) 
		(current-game-state game)))
	 (return-from MINIMAX-DECISION (current-game-state game)))
	(t 
	  (dolist (op ops)
	    ;(format t "OPERATOR:~A~%" op)
	    (setf value-list
		  (append
		    value-list
		    (cons (make-val-op
			    :value (MAX-VALUE (APPLY-MOVE op game) game nil nil)
			    :op op)
			  nil)))
	    (setf *time-move-started* (get-internal-real-time)))
	  (sort value-list #'> :key #'val-op-value)
	  (val-op-op (car value-list)))))

;; no need for a MINIMAX-VALUE since alpha-beta pruning replaces
;; that with MAX-VALUE and MIN-VALUE functions

(defun MAX-VALUE(state game alpha beta)
  ;; Returns the maximum utility a MAX move can get at the current
  ;; game state
  (cond ((CUTOFF-TEST state)
	 (return-from MAX-VALUE (EVAL-STATE game state))))
  (setf ss (SUCCESSORS state 
		       (all-legal-move-ops
			 (opponent (current-game-player game)) 
			 (all-legal-moves (opponent (current-game-player game)) 
					  state))))
  (dolist (s ss)
    (cond ((null alpha)
	   (setf alpha (MIN-VALUE s game (EVAL-STATE game state) beta)))
	  (t
	    (setf alpha (max alpha (MIN-VALUE s game alpha beta)))))
    (cond ((and (not (null beta)) (< beta alpha))
	   (return-from MAX-VALUE beta))))
  alpha)

(defun MIN-VALUE(state game alpha beta)
  ;; Returns the minimum utility a MIN move can get at the current
  ;; game state
  (cond ((CUTOFF-TEST state)
	 (return-from MIN-VALUE (EVAL-STATE game state))))
  (setf ss (SUCCESSORS state 
		       (all-legal-move-ops
			 (current-game-player game)
			 (all-legal-moves (current-game-player game) state))))
    (dolist (s ss)
    (cond ((equal beta nil)
	   (setf beta (MAX-VALUE s game alpha (EVAL-STATE game state))))
	  (t
	    (setf beta (min beta (MAX-VALUE s game alpha beta)))))
    (cond ((and (not (null alpha)) (> alpha beta))
	   (return-from MIN-VALUE alpha))))
  beta)

(defun CUTOFF-TEST(state)
  ;; This CUTOFF-TEST is based on the time elapsed.  Or if a terminal
  ;; state is reached this will also return t
  (setf time-elapsed (- (get-internal-real-time) *time-move-started*))
  (cond ((> time-elapsed (* *max-seconds-per-operator* 1000000))
	 t)
	((not (any-legal-move? white state))
	 t)
	((not (any-legal-move? black state))
	 t)
	(t
	  nil)))

(defun SUCCESSORS(state operators)
  ;; returns a list of successors given a state and valid operators
  (setf returnval '())
  (dolist (op operators)
    (cond ((listp op)
	   (cond ((equal 
		    (funcall (car op) (cdr op) state)
		    nil))
		 (t
		   (setf next-state
			 (funcall (car op) (cdr op) state))
		   (setf returnval (append returnval (cons next-state nil))))))
	  (t
	    (cond ((equal
		     (funcall op state)
		     nil))
		  (t
		    (setf next-state
			  (funcall op state))
		    (setf returnval (append returnval (cons next-state nil))))))))
  returnval)

(defun EVAL-STATE (game state)
  ; Returns a value based on how good the current state is.
  (setf player (current-game-player game))
  (+
    ; Start with how many pieces I will get
    (count-difference player state)

    ; Utility gets better depending on the corners I have
    ; Note: The * 8 is just so corners play a big role in utility
    ;       I chose 8 because that is the # of elements in a row
    (* 8 (count-corners player state))

    ; Utility gets better depending on the edges I have
    ; Note: the * 3 is so edges get a slightly bigger role then
    ;       regular spaces.  Also the corners are counted in here 
    ;	    again.
    (* 3 (count-edges player state))

    ; Utility gets a little worse if I have to be next to an edge
    ; Note: the * -2 is so the utility gets worse. 
    (* -2 (count-almost-edges player state))

    ; Utility gets even worse if I am next to a corner.  I do not
    ; want my opponent getting a corner.
    (* -7 (count-almost-edges player state))))

(defun APPLY-MOVE (operator game)
  ;; returns a single state of a game when a single operator is applied
  (cond ((listp operator)
	 (funcall (car operator) (cdr operator) 
		  (current-game-state game)))
	(t
	 (funcall operator (current-game-state game)))))
  
(defun MAX-SECONDS (seconds)
  ;; Set the max seconds a turn can take
  (setf *max-seconds-per-turn* seconds))

(defun count-items (l)
  ;; Counts the number of items in a list
  (cond ((null l)
	 0)
	(t
	  (+ (count-items (cdr l)) 1))))

(defun count-corners (player board)
  (setf i 0)
  (setf corners '(11 18 81 88))
  (dolist (c corners)
    (cond ((equal (bref board c) player)
	   (setf i (+ i 1)))))
  i)
  
(defun count-almost-corners (player board)
  (setf i 0)
  (setf a-corners '(22 27 72 77 21 12 17 28 78 87 82 71))
  (dolist (a a-corners)
    (cond ((equal (bref board a) player)
	   (setf i (+ i 1)))))
  i)
  
(defun count-edges (player board)
  (setf i 0)
  (setf edges '(11 12 13 14 15 16 17 18
		21                   28
		31                   38
		41                   48
		51                   58
		61                   68
		71                   78
		81 82 83 84 85 86 87 88))
  (dolist (e edges)
    (cond ((equal (bref board e) player)
	   (setf i (+ i 1)))))
  i)

(defun count-almost-edges (player board)
  (setf i 0)
  (setf a-edges '(22 23 24 25 26 27
		  32             37
		  42             47
		  52             57
		  62             67
		  72 73 74 75 76 77))
  (dolist (a a-edges)
    (cond ((equal (bref board a) player)
	   (setf i (+ i 1)))))
  i)
  
(defstruct current-game
  ;; A structure to represent the game being played
  title		; Title of the game.
  state		; Current state discription of the game.
  player	; what color am I?
  operators-max	; List of valid functions that take a state as 
  		;   an argument and returns a state
  operators-min	; List of valid functions that take a state as 
  		;   an argument and returns a state
  )

(defstruct val-op
  ;; Holds value and their corrosponding operators
  value
  op)

(defun all-legal-moves (player board) 
  (setf move-list '()) 
  (dolist (square all-squares) 
    (if (legal-p square player board)
      (setf move-list (append move-list (cons square nil)))))
  ;I have to admit I borrow the code below that randomly
  ;sorts a list from the internet.  
  ; The list is randomly sorted so when my computer
  ; plays itself the same game is not always played
  (do ((l move-list (cdr l)))
    ((null l) move-list)
    (rotatef (car l) (nth (random (length l)) l))))

;;Define a move operator.  Piece-color-list needs to be in the 
;;form '(43 white)
(defun move-op (piece-color-list board)
  (setq temp-board (copy-board board))
  (let ((piece (car piece-color-list))
	(color (car (cdr piece-color-list))))
    (setf temp (make-move piece color temp-board))
    ;print the board after an operator is applied.  Take out for contest
    ;(print-board temp)
    temp))

(defun all-legal-move-ops (player list-legal-moves)
  (setf returnval '())
  (dolist (move list-legal-moves)
    (setf returnval
	  (append returnval (cons (list 'move-op move player) nil))))
  returnval)

(defun ch44 (color state)
  ; Main othello function
  (MAX-SECONDS 25)
  (setf *time-move-started* (get-internal-real-time))
  (cond ((not (any-legal-move? color state))
	 (return-from ch44 state))
	(t 
	  (make-move 
	    (second 
	      (MINIMAX-DECISION (make-current-game 
				  :title 'OTHELLO
				  :player color
				  :operators-max (all-legal-move-ops
						   color
						   (all-legal-moves color state))
				  :operators-min (all-legal-move-ops
						   (opponent color)
						   (all-legal-moves (opponent color) state))
				  :state state))) color state)
	  (print-board state) 
	  state)))

(defun solo-game ()
  (setf brd (ch44 white (initial-board)))
  (do ((i 0 (+ i 2))) ((> i 61))
    (ch44 black brd)
    (ch44 white brd))
  (print-board (ch44 black brd)))

(format t "~A~%" (system::getenv "QUERY_STRING"))
;(solo-game)
