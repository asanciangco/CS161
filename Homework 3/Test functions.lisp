; TESTS
(no-box-in-list '(2)) ; nil
(no-box-in-list '(0 2)) ; nil
(no-box-in-list '(2 0)) ; nil
(no-box-in-list '(0 2 0)) ; nil
(no-box-in-list '(2 2 2 2 2)) ; nil
(no-box-in-list '(0 0 0 0 0)) ; t
(no-box-in-list '(0)) ; t


; TESTS

(goal-test
	 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1))) ; nil
(goal-test
	 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 1 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1))) ; t


;TESTS

(get-square 
	 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)) 0 0) ; 1
(get-square 
	 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)) 1 1) ; 0
(get-square 
	 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)) 1 2) ; 3
(get-square '() 0 0) ; nil
(get-square '((0)) 1 12) ; nil


; TEST
(set-object-row '(0 0 0 0 0 0 0 0 0 0) 2 1) ; (0 0 1 0 0 0 0 0 0 0)
(set-object-row '(0 0 0 0 0 0 0 0 0 0) 0 1) ; (1 0 0 0 0 0 0 0 0 0)
(set-object-row '(0 0 0 0 0 0 0 0 0 0) 9 1) ; (0 0 0 0 0 0 0 0 0 1) 

(set-object
	 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)) 0 0 blank) ;


(printstate (try-move
	 '((1 1 0 1 1 1)
	   (1 0 2 0 0 1)
	   (1 0 3 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)) 2 2 1))	; try up
(printstate (try-move
	 '((1 1 0 1 1 1)
	   (1 0 2 0 0 1)
	   (1 0 3 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)) 2 2 2))	; try right
(printstate (try-move
	 '((1 1 0 1 1 1)
	   (1 0 2 0 0 1)
	   (1 0 3 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)) 2 2 3)) 	; try down
(printstate (try-move
	 '((1 1 0 1 1 1)
	   (1 0 2 0 0 1)
	   (1 0 3 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)) 2 2 4)) 	; try left




(printstates (next-states
	 '((1 1 0 1 1 1)
	   (1 0 2 0 0 1)
	   (1 0 3 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1))) .2) 



(h1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1))) ; 1
(h1 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1))) ; 2
(h1 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
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
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0))) ; 2
(h1 '())



; TEST
(h0 NIL) ; 0
(h0 '(0 0 0 0 0)) ; 0