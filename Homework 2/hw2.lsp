;;; Alex Sanciangco
;;; 704050064
;;; HW 2

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Problem 1 ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun DFS (TREE)
	(cond
		((NULL TREE) NIL)
		((atom TREE) (list TREE))
		(t (append (DFS (first TREE)) (DFS (rest TREE))))
	)
)

;;; Problem 1 Test Functions
;(DFS '((w x)(y z)))
;(DFS '((a (b)) c (d)))


;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Problem 2 ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; THIS IS THE FUNCTION THAT NEEDS WORK
;;; The fuction is basically working fine except that it doesn't search to the correct depth. 
;;; Usually incorrect by 1 layer. 
;;; Maybe the issue is if an element is at level 0, it will just be an atom...

(defun DFID_DFS (TREE C_DEPTH)
	(cond
		((NULL TREE) NIL)
		((< C_DEPTH 0) NIL)
		((and (= C_DEPTH 0) (atom TREE)) (list TREE))
		((and (>= C_DEPTH 0) (atom (first TREE)))
				(cons (first TREE) (DFID_DFS (rest TREE) C_DEPTH)))
		((and (> C_DEPTH 0) (listp (first TREE)))
				(append (DFID_DFS (first TREE) (- C_DEPTH 1))
				(DFID_DFS (rest TREE) C_DEPTH)))
		(t (DFID_DFS (rest TREE) C_DEPTH))
	)
)

;;; DFID_iterator
;;; This function iteratively calls a sub function DFID_DFS, effectively causing the "iterative
;;;		deepening" aspect of DFID. Through recursive calls of this function, M_DEPTH doesn't change.
;;;		This function is used because an additional parameter was needed that DFID alone doesn't have.
;;;
;;; Paramters:
;;;		TREE 	- List or atom passed in; formatted in the form of a tree (not necessarily binary).
;;;		C_DEPTH	- The depth to serach of the current iteration.
;;;		M_DEPTH - Integer representing the maximum depth to search within the tree.
;;;
;;;	Returns:
;;;		This function returns a list of atoms representing the order in which the nodes of TREE 
;;;			were visited. 
(defun DFID_iterator (TREE C_DEPTH M_DEPTH)
	(cond 
		((>= C_DEPTH M_DEPTH) NIL)
		(t (append(DFID_DFS TREE C_DEPTH)
				(DFID_iterator TREE (+ 1 C_DEPTH) M_DEPTH)))
	)
)

;;; DFID
;;; This is the top-level function. It does basic checking of the input values to ensure
;;;		They are valid. It calls a sub-function called DFID_iterator to initiate the depth-
;;;		first iterative deepening search of the tree.
;;;
;;; Paramters:
;;;		TREE 	- List or atom passed in; formatted in the form of a tree (not necessarily binary)
;;;		M_DEPTH	- Integer representing the maximum depth to search within the tree
;;;
;;;	Returns:
;;;		This function returns a list of atoms representing the order in which the nodes of TREE 
;;;			were visited. 
(defun DFID (TREE M_DEPTH)
	(cond
		((and (atom TREE) (>= M_DEPTH 0)) (list TREE))
		((NULL TREE) NIL)
		((and (atom M_DEPTH) (< M_DEPTH 0)) NIL)
		(t (DFID_iterator TREE 0 M_DEPTH))
	)
)



;(DFID '((a (b)) c (d)) 0) ; Should output NIL
;(DFID '((a (b)) c (d)) 1) ; Should output (C)
;(DFID '((a (b)) c (d)) 2) ; Should output (C A C D)
;(DFID '((a (b)) c (d)) 3) ; Should output (C A C D A B C D)

;(DFID '(a (b (c (d (e))))) 0) ; NIL
;(DFID '(a (b (c (d (e))))) 1) ; (a)
;(DFID '(a (b (c (d (e))))) 2) ; (a a b)
;(DFID '(a (b (c (d (e))))) 3) ; (a a b a b c)
;(DFID '(a (b (c (d (e))))) 4) ; (a a b a b c a b c d)
;(DFID '(a (b (c (d (e))))) 5) ; (a a b a b c a b c d a b c d e)

;(DFID 'a 0) ; 

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(cond
		((equal s '(3 3 NIL)) T)
		(T NIL)
	) 
)

;;; final-state test functions
;(final-state '(3 3 NIL))	; returns T
;(final-state '(0 3 NIL))	; returns NIL
;(final-state '(3 0 NIL))	; returns NIL
;(final-state '(3 3 T))		; returns NIL

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	(let*
		((current_m (first s))
		(current_c (second s))
		(remaining_m (- current_m m))
		(remaining_c (- current_c c))
		(other_m (+ (- 3 current_m) m))
		(other_c (+ (- 3 current_c) c))
		(new_side (not (third s))))

		(cond
			((> m current_m) NIL)	; if moving too many missionaries
			((> c current_c) NIL)	; if moving too many cannibals
			((> other_m 3) NIL)		; redundant check
			((> other_c 3) NIL)		; redundant check
			((and (not (= remaining_m 0)) (> remaining_c remaining_m)) NIL)
			((and (not (= other_m 0)) (> other_c other_m)) NIL)
			(T (list (list other_m other_c new_side)))
		)
	)
)

;;; next-state test functions
;(next-state '(3 3 t) 1 0) ; should return NIL, too many cannibals on east side
;(next-state '(3 3 t) 0 1) ; ((0 1 NILL))
;(next-state '(3 3 t) 1 1) ; ((1 1 NIL))
;(next-state '(3 3 t) 2 2) ; ((2 2 NIL))
;(next-state '(3 3 t) 4 0) ; NIL
;(next-state '(3 3 t) 0 4) ; NIL

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

;;; NOT DONE
(defun succ-fn (s)
	(let (
			(a (next-state s 0 1))
			(b (next-state s 0 2))
			(c (next-state s 1 0))
			(d (next-state s 1 1))
			(e (next-state s 2 0))
		)
		(append a (append b (append c (append d e))))

	)
)

;;; succ-fn test functions
;(succ-fn '(3 3 t)) ; -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
;(succ-fn '(1 1 t)) ; -> ((3 2 NIL) (3 3 NIL))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
	(cond
		((NULL s) NIL)
		((NULL states) NIL)
		(t (OR (equal s (first states)) (on-path s (rest states))))
	)
)

;;; on-path test functions 
;(on-path '(1 1 T) '((0 0 T)(1 0 T)(0 1 NIL)(1 1 NIL)(1 1 T))) ; T
;(on-path '(1 1 T) '((0 0 T)(1 0 T)(0 1 NIL)(1 1 NIL))) ; N
;(on-path '(1 1 T) ()) 
;(on-path '(1 1 T) '((1 1 T)))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). path is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
	(cond
		((NULL states) NIL)
		((final-state (first states)) (append path (list (first states))))
		((and (not (on-path (first states) path)) (not (NULL (mult-dfs (succ-fn (first states))
				(append path (list (first states))))))) (mult-dfs (succ-fn (first states))
				(append path (list (first states)))))
		(t (mult-dfs (rest states) path))
	)
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
	(cond
		((and (final-state s) (not (NULL path))) (append path (list s)))
		((final-state s) (list s))
		(t (mult-dfs (succ-fn s) (append path (list s))))
	)
)

;(mc-dfs '(3 3 T) NIL)






