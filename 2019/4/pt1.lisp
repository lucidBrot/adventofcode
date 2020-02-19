;;; Introductory comments are preceded by ";;;"
;;; Function headers are preceded by ";;"
;;; Inline comments are introduced by ";"
;;;

;;
;; Triple the value of a number
;;

(defun triple (X)
  "Compute three times X."  ; Inline comments can
  (* 3 X))                  ; be placed here.

;;
;; Negate the sign of a number
;;

(defun negate (X)
  "Negate the value of X."  ; This is a documentation string.
  (- X))     

;;
;; Test recursion
;;

(defun factorial (N)
  "Compute the factorial of N."
  (if (= N 1)
      1
    (* N (factorial (- N 1)))))

(defun factorial-opt (n &key (result 1))
    (if (= n 1)
        result
        (factorial-opt (- n 1) :result (* result n))))


;;; Actual advent of code stuff
(defun compute (lower_n upper_n)
  "Compute advent of code day 4 pt 1 given the two input numbers"
  (loop for num from lower_n to upper_n
        do (pprint num)                     ; num is every number in the given range
  )
)

;;
;; Check if number contains two adjacent digits
(defun sum-digits (num base)
  (setq two_repeats nil)
  (loop for n = num then q
        for prev = -1 then r
        for (q r) = (multiple-value-list (truncate n base))
        do
        (setq two_repeats (OR two_repeats (= prev r)))
        sum r until (zerop q))
  (if two_repeats
    (pprint two_repeats)
    (pprint "none"))
  )
