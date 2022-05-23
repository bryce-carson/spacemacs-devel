(require 'subr-x)

(setq rules '((3  . "Fizz")
              (5  . "Buzz")
              (7  . "Bazz")
              (13 . "Do-you-like-Jazz? 🐝")))

(defun fizzbuzz (max-integer rules)
  "FizzBuzz: count to `MAX-INTEGER', playing FizzBuzz by replacing
numbers according to `RULES', an alist; `MAX-INTEGER' is included in
the counting process."
  (mapconcat (lambda (integer)
               (let ((word (mapconcat (lambda (rule)
                                        (if (= (% integer (car rule)) 0)
                                            (cdr rule)))
                                      rules
                                      "")))
                 (if (string-empty-p word)
                     (number-to-string integer)
                   word)))
             (number-sequence 1 max-integer)
             "\n"))

(fizzbuzz 15 rules)
