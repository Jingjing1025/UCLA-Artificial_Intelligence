; Programming Assignment 6
; Jingjing Nie, 304567417


; VALID takes in four arguments. The first one is a list of numbers representing the
; current valid column positions found so far. The second one is a number representing
; the new value to be checked for validity. The third one is a number to be used for 
; marking the row number of the previous rows, and the last one is a number representing
; the row number of the value to be checked for validity. It then checks whether the new
; value is valid or not with the previous positions, and then returns T or NIL based on
; the result
(defun VALID (lst num prevrow nrow)
	(cond ((null lst) T)
		((or (= (car lst) num) (= (abs (- prevrow nrow)) (abs (- (car lst) num))))
			(and NIL (VALID (cdr lst) num (+ prevrow 1) nrow)))
		(t (and T (VALID (cdr lst) num (+ prevrow 1) nrow)))
	)	
)


; HELPER takes in four arguments. The first one is a list of numbers representing the
; current valid column positions found so far. The second one is a number representing
; the new value to be checked for validity. The third one is a number used to mark the
; current row number that is being checked, and the last one is a number representing
; the number of the queens. It then uses backtracking to find the possible solutions
; for the N-queens problem, and returns a list representing the column numbers of each
; row as the result
(defun HELPER (lst num nrow N)
	(cond 
		((> nrow N) '(T))
		((<= num 0) '(NIL))
		(t 
			(cond ((VALID lst num 1 nrow)
				(let ((result (HELPER (append lst (list num)) N (+ nrow 1) N)))
					(cond ((equal T (car result))
						(append '(T) (append (list num) (cdr result))))
					(t (HELPER lst (- num 2) nrow N))))
				)
			(t 
				(HELPER lst (- num 1) nrow N)))
	))
)


; QUEENS takes in one argument N, which stands for the number of the queens that are going
; to be placed. It then starts finding the solutions for the N-queen problems by calling
; function HELPER, and returns a list representing the column numbers of each row as the 
; result
(defun QUEENS (N)
	(cond ((= N 1) 1)
		((and (< N 4) (> N 1)) NIL)
		(t (cdr (HELPER '() N 1 N))))
)