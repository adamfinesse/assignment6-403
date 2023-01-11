; function for part 1
(defun myList () "Returns simple prebuilt list"
; simple just builds a prebuilt list with the list function nested inside each other with different values.
(list 4 (list 7 22) "art" (list "math" (list 8) 99) 100)
)

; function for part2, starts at 2021 since we build the list backwards. Tail Recursive.
(defun leapYear (&optional (l ()) (year 2021)) "returns leap years 1800-2021."
    ; base case if the year is 1800 stop here and return list.
    (if (= 1800 year)
        ; true branch return list
        l
        ; elseif/false branch check if the year is divisible by 4 AND not divisible by 100 OR if the year is divisible by 100 AND by 400, aka remainder is 0.
        (if (or (and (= 0 (mod year 4)) (/= 0 (mod year 100))) (and (= 0 (mod year 100)) (= 0 (mod year 400))) )

            ; true branch, add year to our list and recall leap year and decrement the year
            (leapYear (cons year l) (- year 1))

            ; false branch, recall leap year with the same list and decrement.
            (leapYear l (- year 1))
        )
    )
)

; function for part 3, combine till List 1 is null, then remove duplicates and return the list 2, unless list2 was null then just L1 dedup.
(defun union- (L1 L2)
    ; if the current position is nil meaning we are at the end of the L1 then return the New List 2 which contains all unique elements
    (if (null L1)
        ; true branch delete dups and return List2 if List1 is null
        (remove-duplicates L2)  
        ; else/false check if List 2 is null and return L1 if it is null
        (if (null L2)
            ; if L2 is null delete dups and return n1
            (remove-duplicates L1)
            ; else tail recursively call union- again with the rest of L1 and the new list 2
            (union- (cdr L1) (cons (car L1) L2))
        )
    )
)

; function for q 4, tail recursive.
(defun avg (aList &optional (result 0) (len 0))
    ; if the length of our list is 0 and the list is null return nil.
    (if (and (null aList) (= 0 len))
        nil
        ; else branch check if the list is null and len is greater than 0 then we calculate the avg.
        (if (and (null aList) (< 0 len))
            (/ result len)
            ; else we are still calculating the avg so add the first element to result, increment len and call avg with the rest of list
            (avg (cdr aList) (+ result (car aList)) (+ len 1))
        )
    )
)

; function for q 5: takes in a datatype returns a anonymous func which takes in args.
(defun isType (dataType)
    ; creates anonymous function which takes in args and compares type with given from isType dataType. Automatically returns T or NIL
    (lambda (args) (typep args dataType))
)

; function for q6 ALGO: go through entire list checking each value, if the value at that spot in the array is <= limit
; multiply it by rate add to new list and continue.  reverse it in end and return once the values array is empty. Tail Recursive
(defun taxCalculator (limit rate values &optional (newL ()))
    ; if the values array is empty return the reversed newL
    (if (null values)
        (reverse newL)
        ; else if the value we are looking at is greater than or equal to limit
        (if (<= limit (car values))
            ; multiply value by rate then add to newL and recall taxCalc
            (taxCalculator limit rate (cdr values) (cons (* rate (car values)) newL))
            ; else just add the value to new list after everything recall taxCalculator
            (taxCalculator limit rate (cdr values) (cons (car values) newL))
        )
    )
)

; function for q7 ALGO: call aFunc on each element one at a time, if aFunc returns true add it to our new list else dont.
; If aFunc returns false then may have a list, so check that and if yes call clean on the sublist, and add the sublist to our new list and
; recall clean on the newly built list with the leftovers. else just call clean with the rest of the list as it was neither a list or true.
; not really sure if it is tail recursive or not since clean is called inside of clean, but its semi tail recursive

(defun clean (aFunc aList &optional (newL ()))
    ; if we are out of values to check return newL but reversed since its built backwards.
    (if(null aList)
        (reverse newL)
        ; elseif call afunc on current value and check if it returns true
        (if (eq T (funcall aFunc (car aList)))
            ; if true add the value to our newList and recall clean
            (clean aFunc (cdr aList) (cons (car aList) newL))
            ; else check if we have a nested list
            (if (listp (car aList))
                ; if yes, call clean on the sub list, and add that resulting list to our newList, and call clean on the rest of the old list and new list.
                (clean aFunc (cdr aList) (cons (clean aFunc (car aList)) newL))
                ; else just call aFunc with the rest of the list
                (clean aFunc (cdr aList) newL)
            )
        )
    )
)

; q8
(defmacro threeWayBranch (x y z)
;if the first element of x is true progn the rest of x use comma to calc car and place it there, and ,@ to calc cdr and place it there
`(if ,(car x) 
    (progn ,@(cdr x))
    ;else if the first element of y is true progn the rest of y use comma to calc car and place it there, and ,@ to calc cdr and place it there
    (if ,(car y)
        (progn ,@(cdr y))
        ;else if the first element of z is true progn rest of z use comma to calc car and place it there, and ,@ to calc cdr and place it there
        (if ,(car z)
            (progn ,@(cdr z))
            ;else return nill
            nil
        )
    )
 )
)
;add all values in a list
(defun addlist (lista &optional (result 0))
    ;if the list we got is null aka at the end return result
    (if (null lista)
        result
        ;else add to result and recall addlist
        (addlist (cdr lista) (+ (car lista) result))
    )
)
;avg all values in a list
(defun avglist (lista &optional (top 0) (denom 0))
    (if (null lista)
        ;if null return avg
        (/ top denom)
        ;else add to top, and increment denon recall func
        (avglist (cdr lista) (+ top (car lista)) (+ denom 1))
    )
)