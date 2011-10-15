
(load "feat-struc.lisp")
(load "grammar.lisp")
(load "parser.lisp")
(load "ch5.lisp")

(make-ch5-grammar)

(with-open-file (stream "kimout.dat" :direction :output)
  (let* ((kim '("Kim" "loves" "a" "cat"))
	 (dlc '("a" "dog" "loves" "a" "cat"))
	 (kimp (bu-active-edge-parse kim ch5-gram))
	 (dlcp (bu-active-edge-parse dlc ch5-gram))
	 (const1 (first (constituents-spanning-chart kimp)))
	 (const2 (first (constituents-spanning-chart dlcp))))
    (format stream "The types look like *type* and every feature is followed by a colon")
    (format stream "~%================================================~%Parse [the resulting constituent] for ~A:" kim)
    (show-feat-struc const1 stream)
    (format stream "~%================================================~%Parse [the resulting constituent] for ~A:" dlc)
    (show-feat-struc const2 stream)
    nil))
    
