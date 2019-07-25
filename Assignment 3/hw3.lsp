; Programming Assignment 3
; Jingjing Nie

; GETSUM takes in a single argument that is a list of non-negative integers,
; and returns the sum of the input list.
(defun GETSUM (lst)
	(cond ((null lst) 0)
		(t (+ (car lst) (GETSUM (cdr lst)))))
)

; GETMIN takes in two arguments that are two numbers, and returns the smaller
; of the two.
(defun GETMIN (a b)
	(cond ((< a b) a) (t b))
)

; GETABS takes in a single number, and returns the absolute value of that number.
(defun GETABS (a)
	(cond ((> a 0) a) (t (- 0 a)))
)

; SORTINGAUX takes in two arguments that are a number and list, and returns a list
; with the number inserted in to the correct position (such that the resulting list
; is sorted in decreasing order).
(defun SORTINGAUX (a lst)
	(cond ((null lst) 
		(cons a '()))
		(t (cond ((> a (car lst))
			(cons a lst))
		(t (cons (car lst) (SORTINGAUX a (cdr lst))))))
	)
)

; SORTING takes in a single argument that is a list of non-negative numbers, and
; returns a sorted list that is in decreasing order.
(defun SORTING (lst)
	(cond ((null lst) '()) 
		(t (SORTINGAUX (car lst) (SORTING (cdr lst))))
	)
)

; SIMPARTAUX takes in four arguments, that are the list, the sum of the left subset,
; the sum of the right subset, and the sum of the rest numbers, and returns the minimum
; numerical difference between the two partitions of the incoming list. It keeps assigning
; the largest number to the subset with smaller sum, and stops expanding branches or searching
; the other branches in the search tree when either left subset sum is greater than the sum 
; of rest and right subset combined, or when the minimum difference, 0 or 1 is found.
(defun SIMPARTAUX (lst leftsum rightsum restsum)
	; base case, when all numbers are partitioned in a list, the numerical difference is returned
	(cond ((null lst)
		(GETABS (- leftsum rightsum)))
		; Stop expanding the branch if the sum of rest and right numbers in total is smaller than 
		; the sum of the left subset
		(t (cond ((< leftsum (+ rightsum restsum))
			; Assign the largest number in the current list to the subset with smaller sum
			(cond ((< leftsum rightsum)
				(let ((l (SIMPARTAUX (cdr lst) (+ leftsum (car lst)) rightsum (- restsum (car lst)))))
				; Stop searching other branches when the minimum difference, 0 or 1, is found
				(cond ((<= l 1) l) 
				(t (GETMIN l (SIMPARTAUX (cdr lst) leftsum (+ rightsum (car lst)) (- restsum (car lst))))))))
			(t 
				(let ((r (SIMPARTAUX (cdr lst) leftsum (+ rightsum (car lst)) (- restsum (car lst)))))
				(cond ((<= r 1) r) 
				(t (GETMIN r (SIMPARTAUX (cdr lst) (+ leftsum (car lst)) rightsum (- restsum (car lst)))))))))
			) (t (GETABS (- leftsum (+ rightsum restsum))))
			)
		)
	)
) 

; SIMPART takes in a single argument that is the list of non-negative integers, and returns
; the minimum numerical difference between two partitions of the list. It firstly sorts the
; list into decreasing order, figures out the sum of the list, and then calls the SIMPARTAUX
; auxillary function.
(defun SIMPART (lst)
	(SIMPARTAUX (SORTING lst) 0 0 (GETSUM lst))
)

; PARTITIONHELPER takes in two arguments that are two lists, compares the first number of the
; two lists, and returns the list with smaller first number (representing the numberical minimum 
; difference between the two partitions).
(defun PARTITIONHELPER (lst1 lst2)
	(cond ((< (car lst1) (car lst2)) lst1)
		(t lst2))
)

; PARTITIONAUX takes in six arguments. The first argument is the input list. The second, third,
; and forth arguments are the sums of the left partition, right partition, and the reset numbers.
; The final two arguments are the left and right partitions. It returns a list with three components,
; the numerical minimum difference, the left partition, and the right partition. It keeps assigning
; the largest number to the subset with smaller sum, and stops expanding branches or searching
; the other branches in the search tree when either left subset sum is greater than the sum 
; of rest and right subset combined, or when the minimum difference, 0 or 1 is found
(defun PARTITIONAUX (lst leftsum rightsum restsum leftset rightset)
	; base case, when all numbers are partitioned in a list, the three-element list is returned
	(cond ((null lst)
		(append (list (GETABS (- leftsum rightsum))) (append (list leftset) (list rightset))))
		
		; Stop expanding the branch if the sum of rest and right numbers in total is smaller than 
		; the sum of the left subset
		(t (cond ((< leftsum (+ rightsum restsum))
			; Assign the largest number in the current list to the subset with smaller sum
			(cond ((<= leftsum rightsum)
				(let ((l (PARTITIONAUX (cdr lst) (+ leftsum (car lst)) rightsum (- restsum (car lst)) 
					(append leftset (list (car lst))) rightset)))
				; Stop searching other branches when the minimum difference, 0 or 1, is found
				(cond ((<= (car l) 1) l) 
				(t (PARTITIONHELPER l (PARTITIONAUX (cdr lst) leftsum (+ rightsum (car lst)) (- restsum (car lst)) 
					leftset (append rightset (list (car lst))) ))))))
			(t
				(let ((r (PARTITIONAUX (cdr lst) leftsum (+ rightsum (car lst)) (- restsum (car lst)) 
					leftset (append rightset (list (car lst))) )))
				(cond ((<= (car r) 1) r) 
				(t (PARTITIONHELPER r (PARTITIONAUX (cdr lst) (+ leftsum (car lst)) rightsum (- restsum (car lst)) 
					(append leftset (list (car lst))) rightset)))))))
			) (t (append (list (GETABS (- leftsum rightsum))) (append (list leftset) (list rightset))))
			)
		)
	)
)

; PARTITION takes in a single argument that is the list of non-negative integers, and returns
; the three-element list consisting of the minimum numerical difference between two partitions, 
; and the two partitions that achieve such minimum difference. It firstly sorts the list into
; the decreasing order, figures out the sum of the list, and then calls the SIMPARTAUX auxillary
; function.
(defun PARTITION (lst)
	(PARTITIONAUX (SORTING lst) 0 0 (GETSUM lst) '() '())
)

; TESTAUX takes in two arguments that are the length of the input list, and the maximum size of
; the numbers in the list. It returns a randomly generated test list.
(defun TESTAUX (len size)
	(cond ((<= len 0) nil)
		(t (append (TESTAUX (- len 1) size) (list (random size))))
	)
)

; TEST takes in two arguments that are the length of the input list, and the maximum size of
; the numbers in the list. It passes in the resulting list from TESTAUX to the PARTITION function
; for tests.
(defun TEST (len size)
	(PARTITION (TESTAUX len size))
)
