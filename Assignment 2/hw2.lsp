; Programming Assignment 2
; Jingjing Nie, 304567417

; 1. DFS takes in one a single argument that is the list representation of the tree,
; and return a single, top-level list of the terminal nodes in the order they would
; be visited by a left-to-right depth-first search. It performs a depth-first-search
; of a tree.
(defun DFS (lst)
	; base case, when we have reached the end of the list, we return empty list ()
	(cond ((null lst) '())
		; append the element to the result if it is a leaf
		((atom (car lst))(cons (car lst) (DFS (cdr lst))))
		; if it is an internal node, then go one stage deeper
		(t (append (DFS (car lst)) (DFS (cdr lst))))
	)
)

; 2. HELPER takes in two arguments lst and N. lst is the list representation of the tree,
; and N is the integer representing the maximum depth of the tree. It is one of the auxillary
; functions of DFID. It performs DFS but with the constraints of maximum level that can be
; traversed, denoted by the argument N.
(defun HELPER (lst N)
	; if N is smaller or equal to 0, then only root / empty list () will be returned
	; also, if we have traversed the whole list, then return empty list ()
	(cond ((or (<= N 0) (null lst)) '())
		; append the element to the result if it is a leaf
		(t (cond ((atom (car lst)) (cons (car lst) (HELPER (cdr lst) N)))
			; if it is an internal node, then check on the value of N
			; if N is 1, then do not go deeper
			(t (cond ((= N 1) (HELPER (cdr lst) N))
				; if N is bigger than 1, then go a stage deeper
				(t (append (HELPER (car lst) (- N 1)) (HELPER (cdr lst) N)))))))
	)

)

; DFIDAUX takes in three arguments lst and N. lst is the list representation of the tree,
; N is the integer representing the maximum depth of the tree, and num is a number used to
; mark the current level function HELPER is performing on. It is one of the auxillary
; functions of DFID. It determines the number of levels that HELPER needs to go into.
(defun DFIDAUX (lst N num)
	(cond ((< N num) '())
		(t (append (HELPER lst num) (DFIDAUX lst N (+ num 1))))
	)
)

; DFID takes in two arguments lst and N. lst is the list representation of the tree,
; and N is the integer representing the maximum depth of the tree. It uses two auxillary
; functions DFIDAUX and HELPER, and returns a single top-level list of the terminal
; nodes in the order that they would be visited by a left-to-right depth-first 
; iterative-deepening search.
(defun DFID (lst N)
	(DFIDAUX lst N 0)
)


; 3. BFS takes in a single argument that is the list representation of the tree, and return
; a single, top-level list of the terminal nodes in the order they would be visited by a
; left-to-right breadth-first search. It performs a breadth-first-search of a tree.
(defun BFS (lst)
	; base case, when we have reached the end of the list, we return empty list ()
	(cond ((null lst) '())
		; append the element to the result if it is a leaf
		((atom (car lst)) (cons (car lst) (BFS (cdr lst)))) 
		; if is an internal node, then we need to first rotate the list, move this internal
		; node to the end, and then run BFS again with the new list
		(t (BFS (append (cdr lst) (car lst))))
	)
)
