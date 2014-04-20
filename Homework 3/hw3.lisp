;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly effect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; effect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lisp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "/Users/Alex/Documents/School/2013 Fall/CS 161/Homework 3/a-star.lisp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (and (not (NULL v)) (= v blank)) ; added the NULL check
  )

(defun isWall (v)
  (and (not (NULL v)) (= v wall)) ; added the NULL check
  )

(defun isBox (v)
  (and (not (NULL v)) (= v box)) ; added the NULL check
  )

(defun isKeeper (v)
  (and (not (NULL v)) (= v keeper)) ; added the NULL check
  )

(defun isStar (v)
  (and (not (NULL v)) (= v star)) ; added the NULL check
  )

(defun isBoxStar (v)
  (and (not (NULL v)) (= v boxstar)) ; added the NULL check
  )

(defun isKeeperStar (v)
  (and (not (NULL v)) (= v keeperstar)) ; added the NULL check
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GOAL-TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;no-box-in-list (r)
; This is a helper function for goal-test. Returns 'nil' if there is a box in the row, 
; and t otherwise.
;
; PARAMETERS:
;	r - a row of objects in a given state
;
; RETURNS:
;	'nil' or 't' if there is or isn't a box in the list, respectively
;
(defun no-box-in-list (r)
	(cond
		((NULL r) t)
		((list r) (and (not (isbox (car r))) (no-box-in-list (cdr r))))
		(t nil)
	);end cond
);end

;goal-test (s)
; Checks to see whether or not the state s is a valid final state. Here, a final
; state is define as one with all boxes that are on a grid are on a star, that is,
; isBox will return nil everywhere. (isBoxStar can return 'nil' or 't', however)
; This interpretation would assume that an empty state is a valid solution.
;
; PARAMETERS:
;	s - the state to check (a list of lists of atoms)
;
; RETURNS
;	t 		- s is a valid final state
;	'nil' 	- s is not a valid final state
;
(defun goal-test (s)
	(cond
		((NULL s) t)
		((= (length s) 1) (no-box-in-list (car s)))
		(t (and (no-box-in-list (car s)) (goal-test (cdr s))))
	);end cond
 );end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; next-states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get-square-col (r_list c)
; Retrieves the ID of the 'c'th element of the list r_list, which corresponds to a row of a state.
; This is a helper function for get-square.
;
; PARAMTERS:
;	r_list 	- a single row of a given state
;	c	- the index of the item to be retrieved
;
; RETURNS:
;	The 'c'th element of r_list
;	'nil' if r_list is null or if c isn't a valid index
;
(defun get-square-col (r_list c)
	(cond
		((NULL r_list) NIL)
		((or (< c 0) (>= c (length r_list))) wall)
		(t (car (nthcdr c r_list)))
	);end cond
);end

;get-square (s r c)
; Retrieves the ID of the element at position (r, c) of state s
;
; PARAMETERS:
;	s - the state to be searched
;	r - row
;	c - column
;
; RETURN:
;	The ID of the object located at (r, c) in s. 'nil' if invalid location or null state
;
(defun get-square (s r c)
	(cond
		((NULL s) NIL)
		((or (< r 0) (>= r (length s))) wall)
		(t (get-square-col (car (nthcdr r s)) c))
	);end cond
);end


;SUB-LIST (L START LEN)
; Sub-list function taken from my Homework1 assignment. This function takes a list L
; as input and outputs a sub-list of L starting from index START and having length LEN.
;
(defun SUB-LIST (L START LEN)
	(cond
		((NULL L) NIL)
		((= LEN 0) NIL)
		((> START (length L)) NIL)
		((not (= START 0)) (SUB-LIST (rest L) (- START 1) LEN))
		((= START 0) (cons (first L) (SUB-LIST (rest L) 0 (- LEN 1))))	
		(t NIL)
	);end cond
);end


;set-object-row (row c v)
; Sets the atom at index c in list row to value v. Length of list is preserved, old value discarded
;
; PARAMETERS:
;	row - a row of a state (list of atoms)
;	c   - index of element to be updated 
;	v   - new value
;
; RETURNS:
;	The row updated with the new value v placed at c. 
;	nil if invalid inputs.
;	
(defun set-object-row (row c v)
	(cond
		((NULL row) nil)
		((or (< c 0) (>= c (length row))) NIL)
		((or (< v 0) (> v 6)) NIL)
		(t (append (SUB-LIST row 0 c) (list v) (nthcdr (+ 1 c) row)))
	);end cond
);end

;set-object (s r c v)
; Sets the object at (r, c) in s to value v
;
; PARAMTERS:
;	s - the state to update
;	r - row value to update
;	c - col value to update
;	v - new value to update
;
; RETURNS:
;	Updated state with new value in (r, c)
(defun set-object (s r c v)
	(cond
		((NULL s) nil)
		((or (< r 0) (>= r (length s))) NIL)
		(t (append (SUB-LIST s 0 r) (list (set-object-row (car (nthcdr r s)) c v)) (nthcdr (+ 1 r) s))); end t
	);end cond
);end

;move (s r c d m)
; Executes a move.
;
; PARAMETERS:
;	s - state
;	r - row of keeper
;	c - column of keeper
;	d - direction to move
;	m - move condition variable
;
; RETURNS:
;	The new state with the move of condition m executed in the direction d
;
(defun move (s r c d m)
	(let* ((new_r (cond
				((= d 1) (- r 1)) 	; UP
				((= d 2) r)			; RIGHT
				((= d 3) (+ r 1))	; DOWN
				((= d 4) r)			; LEFT
			));end cond
			(new_c (cond
				((= d 1) c) 		; UP
				((= d 2) (+ c 1))	; RIGHT
				((= d 3) c)			; DOWN
				((= d 4) (- c 1))	; LEFT
			));end cond
			(new_r2 (cond
				((= d 1) (- r 2)) 	; UP
				((= d 2) r)			; RIGHT
				((= d 3) (+ r 2))	; DOWN
				((= d 4) r)			; LEFT
			));end cond
			(new_c2 (cond
				((= d 1) c) 		; UP
				((= d 2) (+ c 2))	; RIGHT
				((= d 3) c)			; DOWN
				((= d 4) (- c 2))	; LEFT
			));end cond
			(new_s s)
			(old (get-square s r c))
			(new_s (if (isKeeper old)					; sets keeper's old position
				(set-object new_s r c blank)
				(set-object new_s r c star)));endif
			(new_s (if (and (>= m 3) (<= 5))			; sets keeper's new position
				(set-object new_s new_r new_c keeperstar)
				(set-object new_s new_r new_c keeper))));endif/end variable declaration
		(cond 											; if there was a box in front of keeper
			((or (= m 1) (= m 4)) (set-object new_s new_r2 new_c2 box))
			((or (= m 2) (= m 5)) (set-object new_s new_r2 new_c2 boxstar))
			(t new_s)
		);end cond
	);end let*
);end

;try-move (s r c d)
;  attempts to move in the given direction, returns the state if successful
;
; PARAMETERS:
;	s, r, c - same as previously defined functions
;	d 		- direction attempting to move
;
; RETURNS:
;	The state if move is successful, null otherwise
;
; KEY:
;	1 - UP
;	2 - RIGHT
; 	3 - DOWN
;	4 - LEFT
;
(defun try-move (s r c d)
	(let* ((new_r (cond
				((= d 1) (- r 1)) 	; UP
				((= d 2) r)			; RIGHT
				((= d 3) (+ r 1))	; DOWN
				((= d 4) r)			; LEFT
			));end cond
			(new_c (cond
				((= d 1) c) 		; UP
				((= d 2) (+ c 1))	; RIGHT
				((= d 3) c)			; DOWN
				((= d 4) (- c 1))	; LEFT
			));end cond
			(new_r2 (cond
				((= d 1) (- r 2)) 	; UP
				((= d 2) r)			; RIGHT
				((= d 3) (+ r 2))	; DOWN
				((= d 4) r)			; LEFT
			));end cond
			(new_c2 (cond
				((= d 1) c) 		; UP
				((= d 2) (+ c 2))	; RIGHT
				((= d 3) c)			; DOWN
				((= d 4) (- c 2))	; LEFT
			));end cond
			(front (get-square s new_r new_c))		; object ID of space in front
			(front2 (get-square s new_r2 new_c2))	; object ID of 2 spaces in front
		);end variable declaration
		(cond
			((NULL s) nil)
			((or (< r 0) (>= r (length s))) NIL)
			((or (< c 0) (>= c (length (first s)))) NIL)
			((isBlank front) (move s r c d 0))
			((isStar front) (move s r c d 3))
			((and (isBox front) (isBlank front2)) (move s r c d 1)) 		; if theres a box in front of keeper
			((and (isBox front) (isStar front2)) (move s r c d 2))
			((and (isBoxStar front) (isBlank front2)) (move s r c d 4))		; if theres a box on a star infront of keeper
			((and (isBoxStar front) (isStar front2)) (move s r c d 5))
			(t nil)
		);end cond
	);end let*
);end



; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
	(let* ((pos (getKeeperPosition s 0))
			(c (car pos))
			(r (cadr pos)) ;c and r are now the coordinate of the keeper in s.
			(up 	1)
			(right 	2)
			(down 	3)
			(left 	4)
	 		(result (list (try-move s r c up) (try-move s r c right) (try-move s r c down) (try-move s r c left))));end declaration
    (cleanUpList result)
	);end let
);end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
 )

; TEST
(h0 NIL) ; 0
(h0 '(0 0 0 0 0)) ; 0

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; Yes this function is admissable, the solution will return 0 and it cannot overestimate.
;
(defun h1 (s)
	(cond
		((NULL s) 0)
		((= (length s) 1) (count 2 (car s)))
		(t (+ (count 2 (car s)) (h1 (cdr s))))
	);end cond
);end

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;My solution works by summing the number of moves between the keeper and any given box,
;then multiplies the result by the number of boxes. This will hopefully direct the keeper
;towards the nearest box then towards a solution.

(defun myH-helper-col (row r c kr kc len)
	(cond
		((null row) 0)
		((or (< c 0) (>= c len)) 0)
		((isbox (car row)) (+ (+ (abs (- r kr)) (abs (- c kc))) (myH-helper-col (cdr row) r (+ 1 c) kr kc len))) ; ++++++++
		(t (myH-helper-col (cdr row) r (+ 1 c) kr kc len));end t
	);end cond
)

; finds the
(defun myH-helper (s r kr kc len)
	(cond 
		((null s) 0)
		((or (< r 0) (>= r len)) 0)
		(t (+ (myH-helper-col (car s) r 0 kr kc (length (car s))) (myH-helper (cdr s) (+ 1 r) kr kc len)))
	);end cond
)

(defun h704050064 (s)
	(let* ((pos (getKeeperPosition s 0))
			(c (car pos))
			(r (cadr pos)) ;c and r are now the coordinate of the keeper in s.
		);end declarations
		(* (myH-helper s 0 r c (length s)) (h1 s))
	);end let
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun