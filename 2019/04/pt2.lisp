;;; Actual advent of code stuff
(defun compute (lower_n upper_n)
  "Compute advent of code day 4 pt 1 given the two input numbers"
  (setq num_nums 0)
  (loop for num from lower_n to upper_n                     ; num is every number in the given range
            do(if (check-digits num 10)
                (setq num_nums (+ num_nums 1))
              )
  )
  num_nums
)

;;
;; Assumes that the entered number does not start with the digit 0
;; Assumes that the entered number lies between the given lower and upper bound from adventofcode
;; Assumes that the inputs fulfill the 6-digit-length constraint
;;
;; Check if number contains two adjacent digits
;; Check if the number is never decreasing
;; Return T if the number fulfills both those criteria

(defun check-digits (num base)
  (setq two_repeats nil)
  (setq decreases nil)       ;; decreases from left to right?
  (setq num_repeats 0)
  (loop for n = num then q
        for prev = (+ base 1) then r ;; Reading from right to left by truncating, so prev is to the right of r. It starts at base+1 because that will never be a digit (so never equal) and always larger than any rightmost digit (thus never falsely triggering the decreases check)
        for (q r) = (multiple-value-list (truncate n base))
        do
        (if (= prev r) 
          (setq num_repeats (+ num_repeats 1)) 
         
           (block chris
           (setq two_repeats (OR two_repeats (= num_repeats 2)))
           (setq num_repeats 1)
           )
        )
        (setq decreases (OR decreases (> r prev)))
        ;(format t "~D  decreases: ~D  repeats: ~D
        ;        " r decreases two_repeats)
        sum r until (zerop q))
  (setq two_repeats (OR two_repeats (= num_repeats 2)));; For the case where the loop stopped
  (setq retval nil)
  (if two_repeats
    (setq retval T)
  )
  (if decreases
    (setq retval nil)
  )
  retval
  )
