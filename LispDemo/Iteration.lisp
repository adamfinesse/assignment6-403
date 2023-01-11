;break
;backtrace

(defun square-list (L)
  (if 
  	;condition
  	(null L) 
  	;true branch (base case)
	()
  	;false branch (recursive case)
    (cons (* (car L) (car L)) (square-list(cdr L)))
  )
)

;not tail recursive
(defun factorial (n)
  (if 
  	(= n 0)
    1
    (* n (factorial(- n 1)))
  )
)

;tail recursive
(defun factorialTail (n &optional (result 1))
   (if 
   	(= n 1)
    (break)
    (factorialTail (- n 1) (* n result))
   )
)

(defun addList (inlist)
  (if 
  	(null inlist)
    0
    (+ (car inlist) (addList (cdr inlist)))
  )
)

(defun addListTail (inlist &optional (result 0))
  (if 
  	(null inlist)
    result   
    (addListTail (cdr inlist) (+ (car inlist) result)) 
  )
)

(defun myListLength (inlist)
  (if (null inlist)
    0
    (+ 1 (myListLength (cdr inlist)))
  )
)

(defun listLengthTail (inlist &optional (result 0))
  (if 
  	(null inlist)
    result
    (listLengthTail (cdr inlist) (+ 1 result)) 
  )
)

(defun print-squares-cond (low high)
    (cond 
    	;if branch
    	((> low high) NIL)
    	;else branch
    	(T (print (* low low)) (print-squares-cond (+ 1 low) high ))
	)
)

(defun print-squares-if (low high)
	;if statement with no else
    (if (> low high) (RETURN-FROM print-squares-if (break)))
    ;statements to execute
    (print (* low low))
    (print-squares-if (+ 1 low) high)
)