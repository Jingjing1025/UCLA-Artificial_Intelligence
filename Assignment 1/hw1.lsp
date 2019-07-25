; Homework 1
; Jingjing Nie, UID: 304567417

; 1. function FIB, Compute the N'th Fibonacci number.
; Argument: number N, Return Value: Nth fibonacci number
(defun FIB (N)
  	"Compute the N'th Fibonacci number."
  	; the first two fibonacci numbers are 1
  	(cond ((< N 3)
      	1)
      	; FIB(N) = FIB(N-1) + FIB(N-2)
    	(t (+ (FIB (- N 1)) (FIB (- N 2))))))

#| For large N values, there will be program stack overflow error.
   The error occurred because the recursive functions, which recursively 
   called itself, had allocated too much frames and led to stack overflow. |#


; 2. function SUMS, Compute the number of additions required by calculating the N'th Fibonacci number. 
; Argument: number N, Return Value: number of additions required by finding Nth fibonacci number
(defun SUMS (N)
  	"Compute the number of additions required by calculating the N'th Fibonacci number."

  	; SUMS also follows a trend that is similar to fibonacci numbers
  	; for N=1 and N=2 cases, no addition is required, SUMS = 0
  	(cond ((< N 3)
      	0)

  	; for N>=3 cases, SUMS(N) = SUMS(N-1) + SUMS(N-2) + 1
    (t (+ (SUMS (- N 1)) (SUMS (- N 2)) 1))))

#| Relationship betwee FIB and SUMS:
   SUMS and FIB both follow similar recursive structures since SUMS(N) can be
   calculated as SUMS(N) = SUMS(N-1) + SUMS(N-2) + 1. The values returned by SUMS 
   are smaller than the ones returned by FIB because the initial two values of FIB
   are 1, whereas the initial two values of SUMS are 0. |#


; 3.function FASTFIB, Compute the N'th Fibonacci number using approximately N additions.

; function HELPER, the helper function for FASTFIB
; Argument: a, b, and number N, (a and b are two numbers to record temperary results)
; Return Value: Nth fibonacci number
(defun HELPER (a b N)
	; use N as an indicator to terminate the recursion
	(cond ((<= N 3)
		; when N reaches 3, return the Nth fibonacci number
		(+ a b))

	; keep adding and updating the N-1 and N-2 values
	(t (HELPER b (+ a b) (- N 1)))))

; function FASTFIB, Compute the N'th Fibonacci number using approximately N additions.
; Argument: number N, Return Value: Nth fibonacci number
(defun FASTFIB(N)
	"Compute the Nth Fibonacci number using approximately N additions"
	; for N=1 and N=2, the fibonacci numbers are 1
	(cond ((< N 3)
		1)
	; for N >=3, use the helped function
	(t (HELPER 1 1 N))))
	
; Time: the time taken for first part (tested in lnxsrv07)
; FIB 3: 1.8E-5 sec
; FIB 5: 2.4E-5 sec
; FIB 10: 8.4E-5 sec
; FIB 20: 0.008243 sec
; FIB 30: 0.992661 sec
; FIB 40: 122.13988 sec.
; Approximately, FIB 100 might take execution time of 10^14 sec.
; (approximation is found by estimating the trend of time through plot)
