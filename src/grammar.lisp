;;; -*- Mode: Lisp; Syntax: Common-Lisp 
#| 
  grammar.lisp supports whats necessary to declare a TFS based grammar
  
  -add the necessary support for lexical rules
  
  [type-hierarchy]
  notes: 
  TODO : support multiple inheritance type hierarchies (MITH) I need to make a real greatest common decendant
|#

(defparameter *single-inheritance-only* t "this restriction limits the problems i have to solve in the scope of the spring 2006 semester")

(defstruct grammar
  lexicon
  lexical-rules
  grammar-rules 
  type-hierarchy ;; a tree or network of types
  (type-path-table (make-hash-table :size 100 :rehash-size 1.5))  ;; hash table of paths 
  most-general-type
  (most-general-atom-type 'atom)
  (output-guide-lines nil) ;; used to interpret results of a parse (subj -> <a feature path to subj value> ; object -> the dog
  (dag-unifier #'unify)
  )

(defmethod get-unifier ((grammar grammar))
  (grammar-dag-unifier grammar))

(defmethod compatible-types? ((gram grammar) t1 t2)
  (if (and (eq t1 t2) t1 t2) ;; nil type requires check with grammar
      t
      (exsists-a-path? gram t1 t2)))

(defmethod type-subsumes? ((gram grammar) t1 t2)
  (member t1 (get-path-for gram t2)))

(defmethod exsists-a-path? ((gram grammar) t1 t2)
  (or (member t2 (get-path-for gram t1))
      (member t1 (get-path-for gram t2))))

(defmethod get-path-for ((gram grammar) type)
  (gethash type (grammar-type-path-table gram)))

(defmethod most-specific-type ((gram grammar) t1 t2)
  (if (and (eq t1 t2) t1 t2)
      t1
      (cond ((member t2 (get-path-for gram t1))
	     t1)
	    ((member t1 (get-path-for gram t2))
	     t2)
	    (t
	     nil))))

(defmethod greatest-common-descendant ((gram grammar) t1 t2)
  "currently this is not fully functioning greatest common descendant operator because it doesn't support MITHs"
  (if *single-inheritance-only*
      (most-specific-type gram t1 t2)))

(defmethod most-general-type-in ((gram grammar))
  (grammar-most-general-type gram))

(defmethod get-basic-atomic-type ((gram grammar))
  (grammar-most-general-atom-type gram))

(defun top-type (type-h)
  (first type-h))
   
(defmethod read-in-path-table ((gram grammar) (stuff list))
  "this could also be built from a tree"
  (mapcar #'(lambda (entry)
	      (setf (gethash (first entry) (grammar-type-path-table gram)) entry))
	  stuff)
  t)
   
(defmethod read-in-grammar-rules ((gram grammar) rules)
  (setf (grammar-grammar-rules gram)
	(mapcar #'(lambda (thing) (read-in-grammar-rule thing gram)) rules)))

;; used in a bu-active-edge parser to match a feature structure to the first part of the rhs of a rule
(defmethod find-all-left-corner-matches ((g grammar) fsd)
  (let ((matching-rules nil))
    (mapcar #'(lambda (rule)
		(if (matches-left-corner? fsd rule g)
		    (setf matching-rules (cons (make-rule-copy rule) matching-rules)))) ;; always make a copy of the matching rule!
	    (grammar-grammar-rules g))
    matching-rules))

(defmethod matches-left-corner? (fsd rule grammar)
  (let ((cp-dag (copy-dag fsd))
	(cp-rule-dag (copy-dag (rule-dag rule))))
    (if (eq 'no-path (get-value-from-feature-path cp-rule-dag (list (left-corner rule)) 'no-path))
	nil
	(if (eq 'fail (unify (get-value-from-feature-path cp-rule-dag (list (left-corner rule)))
			     cp-dag
			     grammar
			     'fail))
	    nil
	    t))))

;;=======================
;; lexicon related methods
(defmethod read-in-lexicon ((gram grammar) lexical-entries)
  (let ((new-lexicon (make-hash-table :size 3000 :rehash-size 2.0 :test #'equal)))
    (mapcar #'(lambda (lexeme)
		(setf (gethash (first lexeme) new-lexicon) (mapcar #'(lambda (thing) 
								       (list (first lexeme) (read-in-feat-struc thing gram) thing))
								   (rest lexeme))))
	    lexical-entries)
    (setf (grammar-lexicon gram) new-lexicon)))

(defmethod interpret-word ((gram grammar) word)
  ;; add lexical rule support 
  (if (grammar-lexicon gram)
      (let* ((interps (gethash word (grammar-lexicon gram))))
	(if (and (listp interps))
	    (mapcar #'(lambda (thing)
			;; read in a fresh copy of lexeme 
			(list (word-of-entry thing) (read-in-feat-struc (list-from-entry thing) gram) (list-from-entry thing)))
		    interps)))))

(defun word-of-entry (stuff)
  (first (member-if #'stringp stuff)))

(defun feat-struc-of-entry (stuff)
  (first (member-if #'(lambda (thing) (eq 'feat-struc-dag (type-of thing))) stuff)))

(defun list-from-entry (stuff)
  (first (member-if #'listp stuff)))

(defstruct rule 
  lhs
  rhs
  id ;; only as unique as the grammar writer wants
  dag)

(defmethod lhs ((x rule))
  (rule-lhs x))

(defmethod rhs ((x rule))
  (rule-rhs x))

(defmethod left-corner ((x rule))
  (first (rhs x)))

(defmethod id ((r rule))
  (rule-id r))

(defmethod make-rule-copy ((x rule))
  (make-rule
   :lhs (lhs x)
   :id (rule-id x)
   :rhs (rhs x)
   :dag (copy-dag (rule-dag x))
   ))


#| 
  Rules are (<context-free-part> <DAG>) in format
|#

(defun read-in-grammar-rule (rule grammar)
  (let ((cf (first rule))
	(feat-struc (second rule)))
    (make-rule :lhs (first cf)
	       :id (second cf)
	       :rhs (cddr cf)
	       :dag (if feat-struc
			(read-in-feat-struc feat-struc grammar))
	       )))