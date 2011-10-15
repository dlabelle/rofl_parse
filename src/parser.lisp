;;; -*- Mode: Lisp; Syntax: Common-Lisp 
;;; bu-active-edge.lisp
#|
bottom-up, active-edge parser --- note: edge and arc mean the same thing
|#

;;====Depth-First version
(defun bu-active-edge-parse (words grammar)
  (let ((chart (make-new-chart words)) ;; chart with arcs (edges)
	(agenda nil) ;; a stack for constituents
	(pos 0) ;; input position
	(current-const nil)) ;; a constituent holder
    (loop
     (if (null agenda)
	 (dolist (new-c (interpretations-of (nth pos words) pos grammar) (setf pos (1+ pos)))
	   (push new-c agenda)))
     (setf current-const (pop agenda))
     (if (and (null current-const) (>= pos (length words)))
	 (return))
     (add-new-arcs chart current-const grammar)
     (setf agenda (add-constituent chart current-const grammar agenda)))
    chart))

;;===Breath-first version
;; inorder to implement this read all words first and make sure the agenda is a queue

;; extends  arcs with a constituent, if an arc is complete, it will add it's 'mother' to the agenda,
;; it will add it's 'mother' to the agenda, then return the agneda back to the parser"
(defmethod add-constituent (chart const grammar agenda)
  (add-arc-to-chart chart (make-new-arc const))
  (mapcar #'(lambda (arc)
	      (if (complete? arc)
		  (push (completer arc) agenda)
		  (add-arc-to-chart chart arc)))
	  (left-corner-matches-in-chart chart const grammar))
  agenda)

;;=======CONSTITUENT=====================================================
;; a complete constituent, from a complete arc or lexical item
(defstruct constituent
  start   ;; starting position in chart
  end     ;; ending pos. in chart 
  mother  ;; dag 
  rhs     ;; "daughters"
  rule)   ;; fresh rule for unification

(defun interpretations-of (word pos grammar)
  (let ((interpretations (interpret-word grammar word)))
    (mapcar #'(lambda (interp)
		(make-constituent
		 :start pos 
		 :end (1+ pos)
		 :rhs (first interp) ;; the word
		 :mother (second interp) ;; the feature structure
		 ;:rule interp ;; i left this out because it's useless! 
		 ))
	    interpretations)))

(defmethod start ((c constituent))
  (constituent-start c))

(defmethod end ((c constituent))
  (constituent-end c))

(defmethod mother ((c constituent))
  (constituent-mother c))

(defmethod rhs ((c constituent))
  (constituent-rhs c))

(defmethod rule ((c constituent))
  (constituent-rule c))

(defmethod label ((c constituent))
  (if (eq 'rule (type-of (constituent-rule c)))
      (lhs (constituent-rule c))
      (rhs c)))

;;=======ARC=======================================================
;; an incomplete constituent
(defstruct arc
  start
  end
  mother
  found
  not-found
  DAG
  rule)

(defmethod start ((x arc))
  (arc-start x))
(defmethod end ((x arc))
  (arc-end x))
(defmethod mother ((x arc))
  (arc-mother x))
(defmethod found ((x arc))
  (arc-found x))
(defmethod not-found ((x arc))
  (arc-not-found x))
(defmethod rule ((x arc))
  (arc-rule x))

(defmethod make-new-arc ((c constituent))
  (make-arc 
   :start (start c)
   :end (end c)
   :mother (label c)
   :found (rhs c)
   :not-found nil
   :DAG (mother c)
   :rule (rule c)))

(defmethod make-arc-copy ((arc arc))
  (make-arc
   :start (arc-start arc)
   :end (arc-end arc)
   :mother (arc-mother arc)
   :found (arc-found arc)
   :not-found (arc-found arc)
   :dag (copy-dag (arc-dag arc))
   :rule (arc-rule arc)))

(defun make-arc-from-rule (r start end)
  (make-arc 
   :start start
   :end end
   :mother (rule-lhs r)
   :found nil
   :not-found (rule-rhs r)
   :dag (copy-dag (rule-dag r))
   :rule r))

(defmethod first-not-found ((arc arc))
  (first (arc-not-found arc)))

(defmethod active? ((arc arc))
  (if (arc-not-found arc)
      t
      nil))

(defmethod complete? ((arc arc))
  (not (active? arc)))

(defmethod completer ((arc arc))
  (make-constituent
   :start (arc-start arc)
   :end (arc-end arc)
   :mother (copy-dag (get-value-from-feature-path (arc-dag arc) (list (arc-mother arc))))
   :rhs (arc-found arc)
   :rule (arc-rule arc)))

(defmethod constit-extends-arc? ((arc arc) (const constituent) grammar)
  (let ((arcs-dag (copy-dag (arc-dag arc)))
	(dag-to-check (copy-dag (mother const))))
    (if (or (not (= (end arc) (start const)))
	    (eq 'fail
		(unify (get-value-from-feature-path arcs-dag (list (first-not-found arc)))
		       dag-to-check
		       grammar
		       'fail)))
	nil
	t)))

(defmethod extend-arc ((arc arc) (const constituent) grammar)
  (let ((c-dag (copy-dag (mother const)))
	(arc-dag (copy-dag (arc-dag arc))))
    (unify c-dag
	   (get-value-from-feature-path arc-dag (list (first-not-found arc)))
	   grammar
	   'fail)
    (make-arc
     :start (start arc)
     :end (end const)
     :mother (mother arc)
     :found (append (found arc) (list (first-not-found arc)))
     :not-found (rest (not-found arc))
     :rule (rule arc)
     :dag arc-dag)))

(defmethod subsumes? ((a1 arc) (a2 arc) grammar)
  ;; we have to carefully make sure that we don't exclude rules
  (let ((equal-ids? (and 
		     (arc-rule a1) ;; this catches the case where an arc is from a lexical item
		     (arc-rule a2)
		     (equal (id (arc-rule a1)) (id (arc-rule a2)))))) 
    ;;(if equal-ids? (format t "~%~A~A~%" (id (arc-rule a1))(id (arc-rule a2))))
    (and 
     equal-ids?
     (= (start a1) (start a2))
     (= (end a1) (end a2))
     (equal (found a1) (found a2))
     (equal (not-found a1) (not-found a2))
     (subsumes? (arc-dag a1) (arc-dag a2) grammar))))

;;====CHART======================================
(defstruct chart
  array
  size)

(defun make-new-chart (words)
  (make-chart :array (make-array (list (length words)))
	      :size (length words)))

(defmethod constituents-spanning-chart ((chart chart))
  (let (spanners)
    (dolist (arc (get-arcs-at chart 0))
      (if (and (= (chart-size chart) (arc-end arc)) (not (active? arc)))
	  (setf spanners (cons (arc-dag arc) spanners))
	  nil))
    spanners))

(defmethod get-arcs-at ((chart chart) start-pos)
  (aref (chart-array chart) start-pos))

(defmethod add-arc-to-chart ((chart chart) (arc arc))
  (setf (aref (chart-array chart) (arc-start arc)) (cons arc (aref (chart-array chart) (arc-start arc)))))

(defmethod add-new-arcs ((chart chart) const grammar)
  (let ((possible-rules (find-all-left-corner-matches grammar (mother const))) ;; filter duplicated arcs
	(start (start const)))
    (mapcar #'(lambda (rule)
		(let ((new-arc (extend-arc (make-arc-from-rule rule start start) const grammar)))
		      (if (some #'(lambda (old-arc) (subsumes? old-arc new-arc grammar)) (get-arcs-at chart start))
			  nil
			  (add-arc-to-chart chart new-arc))))
	    possible-rules)))

(defmethod left-corner-matches-in-chart ((chart chart) (const constituent) grammar)
  "collects all active edges, makes new active (also potentially passive) arcs"
  (let (matches)
    (dotimes (i (chart-size chart))
      (mapcar #'(lambda (arc)
		  (if (and (active? arc) (constit-extends-arc? arc const grammar))
		      (setf matches (append matches (list (extend-arc arc const grammar))))))
	      (aref (chart-array chart) i)))
    matches))
