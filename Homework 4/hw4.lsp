; Homework 4
; N-Queens algorithm
;
; Alex Sanciangco
; 704050064
;
; Like in the previous hw, a state will be represented as a grid
; for ease of calculation and manipulation. 0's will represent empty 
; cells and 1's will represent cells with a queen. 


; absolute (x)
;
; returns absolute value of x.
; I wasn't sure if we were allowed to use an absolute value function so I
; made my own, just in case.
(defun absolute (x)
	(cond 
		((>= x 0) x)
		(t (- 0  x))
	); end cond
); end

; row_sum (r)
;
; adds all the elements of r and returns the sum
(defun row_sum (r)
	(cond
		((null r) 0)
		((= (length r) 1) (car r))
		(t (+ (car r) (row_sum (cdr r)))); end t
	); end cond
); end

(defun get_queen_col (row c)
	(cond
		((null row) nil)
		((>= c (length row)))
		((= (get_index row c) 1) c)
		(t (get_queen_col row (+ c 1)))
	); end cond
); end

; violates_constraint_row (S)
;
; checks the state to see if any two queens are in the same row. 
; returns t if there are, nil otherwise
(defun violates_constraint_row (S)
	(cond 
		((null S) nil)
		((and (= (length S) 1) (> (row_sum (car S)) 1)) t)
		((> (row_sum (car S)) 1) t)
		(t (violates_constraint_row (cdr S))); end t
	); end cond
); end

; get_index (row i)
;
; gets (returns) the item at index i in list row.
; returns 0 if out of range.
(defun get_index (row i)
	(cond 
		((NULL row) 0)
		((or (< i 0) (>= i (length row))) 0)
		(t (car (nthcdr i row)))
	);end cond
);end

;FROM PREVIOUS HW
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
(defun set_index_row (row c v)
	(cond
		((NULL row) nil)
		((or (< c 0) (>= c (length row))) NIL)
		((or (< v 0) (> v 6)) NIL)
		(t (append (SUB-LIST row 0 c) (list v) (nthcdr (+ 1 c) row)))
	);end cond
);end

;set_index (s r c v)
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
(defun set_index (S r c v)
	(cond
		((NULL s) nil)
		((or (< r 0) (>= r (length s))) NIL)
		(t (append (SUB-LIST s 0 r) (list (set_index_row (car (nthcdr r s)) c v)) (nthcdr (+ 1 r) s))); end t
	);end cond
);end

; col_sum (S c)
;
; returns the sum of the elements in column c of state S
(defun col_sum (S c)
	(cond
		((null S) 0)
		((= (length S) 1) (get_index (car S) c))
		(t (+ (get_index (car S) c) (col_sum (cdr S) c))); end t
	); cond
); end

; violates_constraint_column
;
; returns true if more than one queen is in any one column. nil otherwise
(defun violates_constraint_column (S c)
	(cond
		((null S) nil)
		((>= c (length (car S))) nil)
		((and (>= (length (car S)) (+ c 1)) (> (col_sum S c) 1)) t)
		((> (col_sum S c) 1) t)
		(t (violates_constraint_column S (+ c 1))); end t
	); end cond
); end

; sum_diag (S d r c)
;   
; sums up all the elements in the diagonal starting at location (r,c) and
; traversing either up-right (NE) or .down-right (SE). This should only be called on the 
; left and bottom most column/row (for NE) or the left and top most column/row (SE)
; dr = 1 corresponds to SE direction, 
; dr = -1 corresponds to NE direction
(defun sum_diag (S r c dr)
	(cond
		((null S) 0)
		((or (>= r (length S)) (< r 0)) 0)
		((or (>= c (length (car S))) (< c 0)) 0)
		(t (+ (get_index (get_index S r) c) (sum_diag S (+ r dr) (+ c 1) dr)))
	); end cond
); end

; vcd_NE (S r c)
;
; checks all diagonals in the NE direction of S to see if any contain more than 1 queen.
; (r, c) should start at (0, 0) → (N, 0) then (N, 0) → (N, N), where S is a NxN board.
; Returns true if any diagonal contains more than one queen, nil otherwise
(defun vcd_NE (S r c)
	(cond
		((null S) nil)
		((> (sum_diag S r c -1) 1) t)
		; if r and c are less than their max, AND the diagonal is no more than 1, increase r
		; and continue (this “AND” might be accomplished in the second condition)
		((< r (- (length S) 1)) (vcd_NE S (+ r 1) c))

		; if r is the last row and column is less than max,  AND the diagonal
		; is no more than 1, increase column and continue
		((< c (length S)) (vcd_NE S r (+ c 1)))
		(t nil)
	); end cond
); end

; vcd_SE (S r c)
;
; The implementation of this function is the same as vcd_NE except that it traverses from
; (N, 0) → (0, 0) then (0, 0) → (0, N)
(defun vcd_SE (S r c)
	(cond
		((null S) nil)
		((> (sum_diag S r c 1) 1) t)
		; if r and c are less than their max, AND the diagonal is no more than 1, increase r
		; and continue (this “AND” might be accomplished in the second condition)
		((> r 0) (vcd_SE S (- r 1) c))

		; if r is the last row and column is less than max,  AND the diagonal
		; is no more than 1, increase column and continue
		((< c (length S)) (vcd_SE S r (+ c 1)))
		(t nil)
	); end cond
); end

; violates_constraints_diagonals (S)
;
; returns t if 2 queens are in any given diagonal, nil otherwise
(defun violates_constraint_diagonals (S)
	(or (vcd_NE S 0 0) (vcd_SE S (- (length S) 1) 0)); end or
); end

; violates_constraint(S)
;
; Returns true if the state S violates the fundamental N-Queens contraint,
; that is, no two queens are in the same row, column, or diagonal.
(defun violates_constraint (S)
	(OR 
		(violates_constraint_row S) 
		(violates_constraint_column S 0) 
		(violates_constraint_diagonals S)); end or
); end

;returns the number of queens on a board S
(defun count_queens (S)
	(cond 
		((null S) 0)
		((= (length S) 1) (row_sum (car S)))
		(t (+ (row_sum (car S)) (count_queens (cdr S)))); end t
	); end cond
)

;eliminates invalied boards
(defun clean_possibles (L)
	(cond ((null L) nil)
		(t (let ((cur (car L))
				(res (clean_possibles (cdr L)))
				);end declaration
	    		(if (not (violates_constraint_diagonals cur)) 
					(cons cur res)
					res
				); end if
	    	);end let
		);end t
	);end cond
);end 

(defun	next_states_helper_col (S r c N L)
	(cond
		((null S) nil)
		((or (< c 0) (>= c N)) nil)
		;((= (count_queens S) N) nil)
		((= (col_sum S c) 1) (append (next_states_helper_col S r (+ c 1) N L) L))
		(t (cons (set_index S r c 1) (append (next_states_helper_col S r (+ c 1) N L) L)))
	); end cond
)

(defun next_states_helper (S r c N L)
	(cond
		((null S) nil)
		((or (< r 0) (>= r N)) nil)
		;((= (count_queens S) N) nil)
		((= (row_sum (get_index S r)) 1) (append (next_states_helper S (+ r 1) c N L) L))
		(t (clean_possibles (append (next_states_helper_col S r c N L)
									(next_states_helper S (+ r 1) c N L)
									L)); end append
		) ;end t
	); end cond
); end

; get_next_states (S)
;
; Gets all the possible boards with an additional queen added
(defun get_next_states (S N)
	(next_states_helper S 0 0 N '())
); end

; creates an empty column of length N
(defun empty_column(N)
	(cond 
		((= N 1) (list 0))
		(t (cons 0 (empty_column (- N 1))))
	); end cond
); end 

;creates an empty NxN board
(defun empty_board (N old_N)
	(cond 
		((= N 1) (list (empty_column old_N)))
		(t (cons (empty_column old_N) (empty_board (- N 1) old_N)))
	); end cond
); end

(defun board2sol (S)
	(cond
		((null S) nil)
		((= (length S) 1) (list (get_queen_col (car S) 0)))
		(t (cons (get_queen_col (car S) 0) (board2sol (cdr S))))
	);end cond
)

(defun is_solution (S N)
	(and (not (violates_constraint S)) (= (count_queens S) N))
);end 

; L is possible solutions, utilizes DFS
(defun find_solution (S N L)
	(cond
		((null S) nil)
		((is_solution S N) (board2sol S))
		((violates_constraint S) nil)
		(t (let* ((temp (find_solution (car L) N (get_next_states (car L) N))));end declaration
				(if temp
				temp
				(find_solution (cadr S) N (get_next_states (cadr S) N)));end if
			);end let
		);end t

	); end cond
); end

(defun QUEENS (N)
	(let* (
			(board (empty_board N N))
		); end declarations
		(find_solution board N (get_next_states board N))
	); end let
); end

(QUEENS 4)