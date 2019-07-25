; Programming Assignment 5
; Jingjing Nie, 304567417

; MAXIMINHELPER taked in five arguments. The first argument is a list representing the
; tree. The second argument is a number, alpha, representing the largest lower bound, 
; and the third argument is a number, beta, representing the smallest upper bound. The
; forth argument is a number marking the value of the updated node, and the last argument
; is a number to count the number of leaves visited by this algorithm. This function then
; performs the Minimax with alpha beta pruning algorithm by assuming that the root node
; is the maximum one, and returns a list consisting of two items, the maximum node in the 
; tree found by this algorithm, and a number counting the number of leaves visited in this
; algorithm.
(defun MAXIMINHELPER (tree alpha beta V counter)
	(cond ((atom tree) (append (list V) (list counter)))
		(t (let ((result (MINIMAXAUX (car tree) alpha beta counter)))
			(let ((currmin (car result)))
			(let ((currcounter (cadr result)))
			(cond ((< V currmin) 
				(cond ((>= currmin beta) (append (list currmin) (list currcounter)))
					(t (cond ((> currmin alpha) 
						(MAXIMINHELPER (cdr tree) currmin beta currmin currcounter))
						(t (MAXIMINHELPER (cdr tree) alpha beta currmin currcounter))) )))
				(t (cond ((>= currmin beta) (append (list V) (list currcounter)))
					(t (cond ((> currmin alpha) 
						(MAXIMINHELPER (cdr tree) currmin beta V currcounter))
						(t (MAXIMINHELPER (cdr tree) alpha beta V currcounter))) ))) )
			)))
		)
	)
)

; MAXIMINAUX takes in four arguents. The first argument is a list representing the
; tree. The second argument is a number, alpha, representing the largest lower bound, 
; and the third argument is a number, beta, representing the smallest upper bound, and 
; the last argument is a number to count the number of leaves visited by this algorithm.
; It then calls MAXIMINHELPER with an initialized node value, which is set to be -1000,
; and returns a list consisting of two items, the maximum node in the tree found by this 
; algorithm, and a number counting the number of leaves visited in this algorithm.
(defun MAXIMINAUX (tree alpha beta counter)
	(cond ((atom tree) (append (list tree) (list (+ counter 1))))
		(t (MAXIMINHELPER tree alpha beta -10000 counter)))
)

; MAXIMIN takes in three arguents. The first argument is a list representing the
; tree. The second argument is a number, alpha, representing the largest lower bound, 
; and the third argument is a number, beta, representing the smallest upper bound. 
; It then calls MAXIMINAUX with an initialized value of count, which is set to be 0,
; and returns a list consisting of two items, the maximum node in the tree found by this 
; algorithm, and a number counting the number of leaves visited in this algorithm.
(defun MAXIMIN (tree alpha beta)
	(MAXIMINAUX tree alpha beta 0)
)

; MINIMAXHELPER taked in five arguments. The first argument is a list representing the
; tree. The second argument is a number, alpha, representing the largest lower bound, 
; and the third argument is a number, beta, representing the smallest upper bound. The
; forth argument is a number marking the value of the updated node, and the last argument
; is a number to count the number of leaves visited by this algorithm. This function then
; performs the Minimax with alpha beta pruning algorithm by assuming that the root node
; is the minimum one, and returns a list consisting of two items, the minimum node in the 
; tree found by this algorithm, and a number counting the number of leaves visited in this
; algorithm.
(defun MINIMAXHELPER (tree alpha beta V counter)
	(cond ((atom tree)  (append (list V) (list counter))) 	
		(t (let ((result (MAXIMINAUX (car tree) alpha beta counter)))
			(let ((currmax (car result)))
			(let ((currcounter (cadr result)))
			(cond ((> V currmax) 
				(cond ((<= currmax alpha) (append (list currmax) (list currcounter)))
						(t (cond ((< currmax beta) 
							(MINIMAXHELPER (cdr tree) alpha currmax currmax currcounter))
							(t (MINIMAXHELPER (cdr tree) alpha beta currmax currcounter))) )))
				(t (cond ((<= currmax alpha) (append (list V) (list currcounter)))
					(t (cond ((< currmax beta) 
						(MINIMAXHELPER (cdr tree) alpha currmax V currcounter))
						(t (MINIMAXHELPER (cdr tree) alpha beta V currcounter))) ))) )
			)))
		)
	)
)

; MINIMAXAUX takes in four arguents. The first argument is a list representing the
; tree. The second argument is a number, alpha, representing the largest lower bound, 
; and the third argument is a number, beta, representing the smallest upper bound, and 
; the last argument is a number to count the number of leaves visited by this algorithm.
; It then calls MINIMAXHELPER with an initialized node value, which is set to be 1000,
; and returns a list consisting of two items, the minimum node in the tree found by this 
; algorithm, and a number counting the number of leaves visited in this algorithm.
(defun MINIMAXAUX (tree alpha beta counter)
	(cond ((atom tree) (append (list tree) (list (+ counter 1))))
		(t (MINIMAXHELPER tree alpha beta 10000 counter)))
)

; MINIMAX takes in three arguents. The first argument is a list representing the
; tree. The second argument is a number, alpha, representing the largest lower bound, 
; and the third argument is a number, beta, representing the smallest upper bound. 
; It then calls MINIMAXAUX with an initialized value of count, which is set to be 0,
; and returns a list consisting of two items, the minimum node in the tree found by this 
; algorithm, and a number counting the number of leaves visited in this algorithm.
(defun MINIMAX (tree alpha beta)
	(MINIMAXAUX tree alpha beta 0)
)