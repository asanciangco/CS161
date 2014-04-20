; Test Functions

; absolute
(absolute 1) ; 1
(absolute -1); 1
(absolute 0) ; 0

; row_sum
(row_sum '(1 0 0 0 0 0)) ; 1
(row_sum '(1 0 0 0 0 1)) ; 2
(row_sum '(1 2 3 4)) ; 10
(row_sum '())

; violates_constraint_row
(violates_constraint_row '((0 0 0)(0 0 0)(0 0 0))); nil
(violates_constraint_row '((1 0 0)(0 1 0)(0 0 1))); nil
(violates_constraint_row '((1 0 1)(0 0 0)(0 0 0))); t
(violates_constraint_row '((0 0 0)(0 0 0)(1 0 1))); t
(violates_constraint_row '((1 1 0)(1 0 1)(0 1 1))); t

; get_index
(get_index '(0 0 0 1 0 0) 3); 1

; col_sum
(col_sum '((1 0 0)(0 1 0)(0 0 1)) 0) ; 1
(col_sum '((1 0 0)(1 1 0)(1 0 1)) 0) ; 3

; violates_constraint_column
(violates_constraint_column '((0 0 0)(0 0 0)(0 0 0)) 0); nil
(violates_constraint_column '((1 0 0)(0 1 0)(0 0 1)) 0); nil
(violates_constraint_column '((1 0 1)(0 0 0)(0 0 0)) 0); nil
(violates_constraint_column '((0 0 0)(1 0 0)(1 0 1)) 0); t
(violates_constraint_column '((1 1 0)(1 0 1)(0 1 1)) 0); t

; sum_diag
(sum_diag '((0 0 0)
			(0 0 0)
			(0 0 0)) 0 0 1); 0
(sum_diag '((1 0 0)
			(0 1 0)
			(0 0 1)) 0 0 1); 3
(sum_diag '((1 1 1)
			(0 0 1)
			(0 0 0)) 0 1 1); 2
(sum_diag '((0 0 0)
			(1 0 1)
			(1 1 1)) 2 1 -1); 2
(sum_diag '((1 1 0)
			(1 0 1)
			(0 1 1)) 0 2 1); 0

; vcd_NE
(vcd_NE '((0 0 0)
		  (0 0 0)
		  (0 0 0)) 0 0); nil
(vcd_NE '((1 0 0)
		  (0 1 0)
		  (0 0 1)) 0 0); nil
(vcd_NE '((1 1 1)
		  (0 0 1)
		  (0 0 0)) 0 0); nil
(vcd_NE '((0 0 0)
		  (1 0 1)
	  	  (1 1 1)) 0 0); t
(vcd_NE '((1 1 0)
		  (1 0 1)
		  (0 1 1)) 0 0); t

; vcd_SE
(vcd_SE '((0 0 0)
		  (0 0 0)
		  (0 0 0)) 2 0); nil
(vcd_SE '((1 0 0)
		  (0 1 0)
		  (0 0 1)) 2 0); t
(vcd_SE '((1 1 1)
		  (0 0 1)
		  (0 0 0)) 2 0); t
(vcd_SE '((0 0 0)
		  (1 0 1)
	  	  (1 1 1)) 2 0); t
(vcd_SE '((1 1 0)
		  (1 0 1)
		  (0 1 1)) 2 0); t

; violates_constraint
(time (violates_constraint '((0 0 0)
		  				(0 0 0)
	 	  				(0 0 0)))); nil
(violates_constraint '((1 0 0)
		  				(0 1 0)
		  				(0 0 1))); t
(violates_constraint '((1 1 1)
		  				(0 0 1)
		  				(0 0 0))); t
(violates_constraint '((0 0 0)
		  				(1 0 1)
	  	  				(1 1 1))); t
(violates_constraint '((1 1 0)
		  				(1 0 1)
		  				(0 1 1))); t
(violates_constraint '((1 0 0)
						(0 0 1)
						(0 0 0))) ; nil

; set_index
(set_index '((1 0 0)
			(0 0 1)
			(0 0 0)) 0 0 2)