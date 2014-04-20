;;; CS 161 HW1
;;; Alex Sanciangco

;;; OVERALL COMMENT ABOUT SOLUTIONS
;;;		These solutions all work off of a similar structure:
;;;		First there are sanity checks to make sure that the parameters are 
;;;		what we expect them to be. Then the base cases are handled to cover the
;;;		simple cases and to return data appropriately for recursive steps.
;;;		Lastly, a recursive or functional call is made to construct the final
;;;		output to be returned. This is often achieved through the commands
;;;		"cons", "append", and other functions defined here.


;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Problem 1 ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;; SUB-LIST
;;; This function creates a sublist from a given index and length of
;;;		another list. 
;;;
;;; Parameters:
;;; L:		Main list that the sublist will be generated from
;;; START:	Index of L that will become first element of the sub-list
;;; LEN:	The desired length of the sublist.
;;;
;;; Returns:
;;;	A sub-list of L specified by the arguments passed in
;;;
;;; START and LEN are assumed to be non-negative atomic integers. 
;;;	LEN is allowed to be longer than (length L), it will simply make
;;;		a list of as elements as it can. 


(defun SUB-LIST (L START LEN)
	(cond
		((NULL L) NIL)	;;; return NIL if empty list
		((= LEN 0) NIL)	;;; return NIL if asking for nothing
		((> START (length L)) NIL)	;;; return NIL if START is outside list

		;;; Work to make the first element the start of the list
		;;; If  START isn't zero: remove the first element,
		;;; 	decrement START, then call the function again with
		;;;		the new parameters.
		((not (= START 0)) (SUB-LIST (rest L) (- START 1) LEN))

		;;; Recursively make the list to return one element at a time
		((= START 0) (cons (first L) (SUB-LIST (rest L) 0 (- LEN 1))))	
		(t NIL)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Problem 2 ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;; SPLIT-LIST
;;; This function splits a list in half, guaranteeing the second list is 
;;;		equal to the length of the first list (if length of original list)
;;;		is even) or the length of the first list +1 (otherwise).
;;;
;;; Parameters:
;;; L:	The list to be split
;;;
;;; Returns:
;;;	A List of 2 lists, containing the atoms of L split evenly between the two
;;;		new sublists. If the total number of atoms is odd, the second sublist
;;;		will have more atoms.
;;;
;;; L is assumed to be a list of atoms, although this function will work
;;;		with lists of lists as well, although only the number of elements
;;; 	will be split "evenly," not atoms.

(defun SPLIT-LIST (L)
	(cond
		((NULL L) NIL)	;;; return NIL if null/empty list
         ((= (length L) 1) L)	;;;	Return if only one element
		((evenp (length L))				;;; if the lenght of L is even...
			(let* ((a (/ (length L) 2))	;;; Calculate the new sub-length
				   (L1 (SUB-LIST L 0 a))	;;; Create sub-lists (first half)
				   (L2 (SUB-LIST L a a)))	;;; This is where we split the list (second half)
				(list L1 L2)	;;; make a list of the two sub-lists
			)
		)
		((oddp (length L))
			(let* ((a (- (/ (length L) 2) 1/2))	;;; Calculate the new sub-lengths
					(b (+ a 1))					;;; Calculate the new sub-lengths
					(L1 (SUB-LIST L 0 a))		;;; Split the list
					(L2 (SUB-LIST L a b)))		;;; Split the list (second half)
				(list L1 L2)	;;; make a list of the two sub-lists
			)
		)
		(t L) ;;; This line should never be executed, it's here just in case
	)
)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Problem 3 ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;; LIST2BTREE
;;; This function takes a list of atoms and turns it into a list formatted
;;;		in the style of a binary tree (((L R)(L R))((L R)(L R)))
;;;
;;; Parameters:
;;; LEAVES:	List of atoms
;;;
;;; Returns:
;;; A list formatted in the style of binary trees; that is, any list contains two
;;;		elements only that can be either two sub-trees or two atoms representing
;;;		the children of that node.
;;;	If only one element was in the initial list, that element will be returned
;;;		as an atom.

(defun LIST2BTREE (LEAVES)
	(cond 
		((NULL LEAVES) NIL)					;;; return NIL if null/empty list
		((= (length LEAVES) 1) (first LEAVES))	;;; if only 1 item, return it as an atom
        
        ;;; If there are only 2 elements, it is assumed to be a correctly formatted
        ;;;		binary tree and is therefore returned. 
        ((= (length LEAVES) 2) LEAVES)
		
        ;;; Otherwise, split the list in half (in the binary style) and turn each
        ;;;		new sublist into binary trees, recursively (this is the recursive
        ;;;		step).
		(t
			(let* ((tempList (SPLIT-LIST LEAVES))	;;; Split the list
        			(L1 (LIST2BTREE (first tempList)))		;;;	convert half
					(L2 (LIST2BTREE (second tempList))))	;;;	convert half
				(list L1 L2)
        	)
        )
	) 
)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Problem 4 ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;; BTREE2LIST
;;; This function takes a list formatted in the binary tree style (see above
;;;		function) and returns a list of atoms, in the same order presented in
;;;		the binary tree list. This solution implements a psuedo-DFS approach.
;;;
;;; Parameters:
;;; TREE: Binary tree (list formatted properly). If there is only one element 
;;;		in TREE, it will be passed as an atom (see above function)
;;;
;;; Returns:
;;; A List of atoms that were in the binary tree, preserving order. If 

(defun BTREE2LIST (TREE)
	(cond
		((NULL TREE) NIL)	;;; Sanity check

		;;; This is the base case. If the first element is an atom, we are
		;;;		finshed and can return that item as a list.
		((atom TREE) (list TREE))

		;;; This is the recursive step. The first part recursively traverses 
		;;;		down the left part of the tree to get the deepest, left most
		;;;		atom and make it the head of the list to be returned. The rest
		;;;		of the function repeats the pattern with the rest of the list
		;;;		and combines it with the list generated in the first part to
		;;;		make a single list of atoms. 
		(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (rest TREE))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; TEST FUNCTIONS ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(SUB-LIST '(a b c d) 0 3)
(SUB-LIST '(a b c d) 3 1)
(SUB-LIST '(a b c d) 2 0)

(SPLIT-LIST '(a))
(SPLIT-LIST '(a b))
(SPLIT-LIST '(a b c))
(SPLIT-LIST '(a b c d))
(SPLIT-LIST '(a b c d e))
(SPLIT-LIST '(a b c d e f))

(LIST2BTREE '(1))
(LIST2BTREE '(1 2))
(LIST2BTREE '(1 2 3))
(LIST2BTREE '(1 2 3 4))
(LIST2BTREE '(1 2 3 4 5))
(LIST2BTREE '(1 2 3 4 5 6))
(LIST2BTREE '(1 2 3 4 5 6 7))
(LIST2BTREE '(1 2 3 4 5 6 7 8))

(BTREE2LIST 1)
(BTREE2LIST '(1 2))
(BTREE2LIST '(1 (2 3)))
(BTREE2LIST '((1 2) (3 4)))
(BTREE2LIST '((1 2) (3 (4 5))))
(BTREE2LIST '((1 (2 3)) (4 (5 6))))
(BTREE2LIST '((1 (2 3)) ((4 5) (6 7))))
(BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))