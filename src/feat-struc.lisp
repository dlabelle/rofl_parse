;;; -*- Mode: Lisp; Syntax: Common-Lisp 

#| this unfies typed feature structures, from jurafsky and martin (2000), with 

Misc. notes: 
  -when unifing types, type nil and nil are compatible. sometimes you might not provide a type for atoms or indicies.
   it doesn't matter if you do or don't just be consistent.
|#

;; Environmental Parameters
(defparameter *random-id-symbol-char* #\!) ;; ex: !situation !i 
(defparameter *variable-symbol-char* #\?) ;; ex: ?x ?y ?z
(defparameter *default-symbol* '/)		      

(defun unify (fs1 fs2 grammar &optional (failure-symbol 'fails-to-unify))
  (let* ((real-dag1 (real-dag fs1)) ;; "dereferenced" DAGs (careful they may be the same!!!
	 (real-dag2 (real-dag fs2))
	 (real-t1 (type-of-feat-struc real-dag1))
	 (real-t2 (type-of-feat-struc real-dag2))
	 (real1 (real-contents-of real-dag1))
	 (real2 (real-contents-of real-dag2))
	 (fs1-t (type-of-feat-struc fs1))
	 (fs2-t (type-of-feat-struc fs2))
	 (new-type (greatest-common-descendant ;; I believe this operation is order independent
		    grammar
		    (greatest-common-descendant grammar real-t1 real-t2)
		    (greatest-common-descendant grammar fs1-t fs2-t)))
	 (reals-are-same? (equal (id real-dag1) (id real-dag2)))
	 (fs1-real? (equal (id fs1) (id real-dag1))) ;; fs contains no forward pointers
	 (fs2-real? (equal (id fs2) (id real-dag2))))
    (cond (new-type ;; this may be confusing, but if their is a new type between all DAGs then they all have compatible types
	   (mapcar #'(lambda (fs) (change-type-of fs new-type)) (list fs1 fs2 real-dag1 real-dag2))
	   (cond ((empty? real1)
		  (if reals-are-same? nil (set-pointer-for-to real-dag1 real-dag2)) ;; each if is for 
		  (set-pointer-for-to fs1 real-dag2)
		  (if fs2-real? nil (set-pointer-for-to fs2 real-dag2))
		  real-dag2)
		 ((empty? real2)
		  (if reals-are-same? nil (set-pointer-for-to real-dag2 real-dag1))
		  (if fs1-real? nil (set-pointer-for-to fs1 real-dag1))
		  (set-pointer-for-to fs2 real-dag1)
		  real-dag1)
		 ((equal? real1 real2)
		  (if reals-are-same? nil (set-pointer-for-to real-dag1 real-dag2))
		  (set-pointer-for-to fs1 real-dag2)
		  (if fs2-real? nil (set-pointer-for-to fs2 real-dag2))
		  real-dag2)
		 ((and (complex? real1) (complex? real2))
		   (catch failure-symbol	   
		     (merge-features real-dag2 real-dag1 grammar failure-symbol #'unify)
		     (if reals-are-same? nil (set-pointer-for-to real-dag2 real-dag1))
		     (if fs1-real? nil (set-pointer-for-to fs1 real-dag1))
		     (set-pointer-for-to fs2 real-dag1)
		   real-dag1))
		 (t
		  failure-symbol)))
	   (t
	    failure-symbol))))

(defun unify-with-defaults (fs1 fs2 grammar &optional (failure-symbol 'fails-to-unify))
  (let* ((real-dag1 (real-dag fs1)) ;; "dereferenced" DAGs (careful they may be the same!!!
	 (real-dag2 (real-dag fs2))
	 (real-t1 (type-of-feat-struc real-dag1))
	 (real-t2 (type-of-feat-struc real-dag2))
	 (real1 (real-contents-of real-dag1))
	 (real2 (real-contents-of real-dag2))
	 (fs1-t (type-of-feat-struc fs1))
	 (fs2-t (type-of-feat-struc fs2))
	 (new-type (greatest-common-descendant ;; I believe this operation is order independent
		    grammar
		    (greatest-common-descendant grammar real-t1 real-t2)
		    (greatest-common-descendant grammar fs1-t fs2-t)))
	 (reals-are-same? (equal (id real-dag1) (id real-dag2)))
	 (fs1-real? (equal (id fs1) (id real-dag1))) ;; fs contains no forward pointers
	 (fs2-real? (equal (id fs2) (id real-dag2))))
    (cond (new-type ;; this may be confusing, but if their is a new type between all DAGs then they all have compatible types
	   (mapcar #'(lambda (fs) (change-type-of fs new-type)) (list fs1 fs2 real-dag1 real-dag2))
	   (cond ((empty? real1)
		  (if reals-are-same? nil (set-pointer-for-to real-dag1 real-dag2)) ;; each if is for 
		  (set-pointer-for-to fs1 real-dag2)
		  (if fs2-real? nil (set-pointer-for-to fs2 real-dag2))
		  real-dag2)
		 ((empty? real2)
		  (if reals-are-same? nil (set-pointer-for-to real-dag2 real-dag1))
		  (if fs1-real? nil (set-pointer-for-to fs1 real-dag1))
		  (set-pointer-for-to fs2 real-dag1)
		  real-dag1)
		 ((equal? real1 real2)
		  (if reals-are-same? nil (set-pointer-for-to real-dag1 real-dag2))
		  (set-pointer-for-to fs1 real-dag2)
		  (if fs2-real? nil (set-pointer-for-to fs2 real-dag2))
		  real-dag2)
		 ((and (complex? real1) (complex? real2))
		   (catch failure-symbol	   
		     (merge-features real-dag2 real-dag1 grammar failure-symbol #'unify)
		     (if reals-are-same? nil (set-pointer-for-to real-dag2 real-dag1))
		     (if fs1-real? nil (set-pointer-for-to fs1 real-dag1))
		     (set-pointer-for-to fs2 real-dag1)
		   real-dag1))
		 ((and (is-default? real-dag1) (not (is-default? real-dag2)))
		  (if reals-are-same? nil (set-pointer-for-to real-dag1 real-dag2)) ;; each if is for 
		  (set-pointer-for-to fs1 real-dag2)
		  (if fs2-real? nil (set-pointer-for-to fs2 real-dag2))
		  real-dag2)
		 ((and (is-default? real-dag2) (not (is-default? real-dag1)))
		  (if reals-are-same? nil (set-pointer-for-to real-dag2 real-dag1))
		  (if fs1-real? nil (set-pointer-for-to fs1 real-dag1))
		  (set-pointer-for-to fs2 real-dag1)
		  real-dag1)
		 (t
		  failure-symbol)))
	   (t
	    failure-symbol))))

(defun unify-without-types (fs1 fs2 grammar &optional (failure-symbol 'fails-to-unify))
  ;; this is the simple unifier that ignores types 
  "don't use  this woth feat-struc-dag type of dag: method merge-features requires "
  (let ((real1 (real-contents-of fs1))
	(real2 (real-contents-of fs2)))  
    (cond ((empty? real1) 
	   (set-pointer-for-to fs1 fs2) 
	   fs2)
	  ((empty? real2)
	   (set-pointer-for-to fs2 fs1)
	   fs1) 
	  ((equal? real1 real2) 
	   (set-pointer-for-to fs1 fs2)
	   fs2)
	  ((and (complex? real1) (complex? real2))
	   (catch failure-symbol
	     (merge-features fs2 fs1 grammar failure-symbol #'unify-without-types)   
	     (set-pointer-for-to fs2 fs1)	
	     fs1))
	  (t
	   failure-symbol))))

;;================================================================================
;; the following is the directed acyclic graph collection of methods
;;
(defclass feat-struc-dag ()
  ((content :accessor feat-struc-dag-content :initarg :content :initform nil)
   (pointer :accessor feat-struc-dag-pointer :initarg :pointer :initform nil)
   (type :accessor feat-struc-dag-type :initarg :fs-type :initform nil)
   (id :accessor feat-struc-dag-id :initform (string (gensym "dag")))
   (default :initarg :default-status :initform nil)
   ))

(defmethod subsumes? ((dag1 feat-struc-dag) (dag2 feat-struc-dag) grammar)
  "does dag1 subsume dag2"
  (if (type-subsumes? grammar (type-of-feat-struc dag1) (type-of-feat-struc dag2))
      (let ((real1 (real-contents-of dag1))
	    (real2 (real-contents-of dag2)))
	(cond      
	  ((empty? real1) ;; real1 is [] most general feat-struc
	   t)
	  ((equal? real1 real2)
	   t)
	  ((and (complex? real1) (complex? real2))
	   (every #'(lambda (feat)
		      (if (get-feature (feature-name feat) real2) ;; exsists a feature in real2
			  (subsumes? (feature-value feat) (feature-value (get-feature (feature-name feat) real2)) grammar)
			  nil))
		  real1))
	  (t
	   nil)))
      nil))

(defun debug-printer (fsd recurse? &key (stream t) (check-list 'all) (from nil))
  (let ((id (feat-struc-dag-id fsd)) ;; id
	(content (feat-struc-dag-content fsd)) ;; stuff
	(pointer (feat-struc-dag-pointer fsd)) ;; reference
	(type (feat-struc-dag-type fsd))
;	(default (slot-value fsd 'default)) 
	(real (catch :cyclic-graph (real-contents-of fsd))) ;; complete dereference 
	(real-dag (catch :cyclic-graph (real-dag fsd)))
	)
    (if (or (and (listp real) real) (and (listp real-dag) real-dag))
	(format t "~A~A" real real-dag))
    (if (or (null from) (eq 'all check-list) (member from check-list))
	    (format stream 
		    "~%=========================~%FEATURE : ~A~%DAG : ~A~%CONTENT : ~A~%POINTER : ~A~%TYPE : ~A~%REAL-CONTENT : ~A~%REAL-DAG : ~A~%=========================~%"
		    (if from from 'no-feat)
		    id
		    content
		    (if (eq 'feat-struc-dag (type-of pointer)) (feat-struc-dag-id pointer) pointer)
		    type
		    real
		    (feat-struc-dag-id real-dag)))
    (if (and recurse? (listp real) real)
	(mapcar #'(lambda (f)
		    (debug-printer (feature-value f) t :stream stream :check-list check-list :from (feature-name f)))
		real)
	nil)))

(defun chage-type-of-dags (list-dags)
  (mapcar #'change-type-of list-dags))

(defmethod content ((dag feat-struc-dag))
  (feat-struc-dag-content dag))
(defmethod pointer ((dag feat-struc-dag))
  (slot-value dag 'pointer))

;; my memory intensive dag copier, 
(defmethod copy-dag ((fsd feat-struc-dag))
  ;; we use a hash table to cash visited nodes
  (let ((visited-nodes (make-hash-table :size 100 :test #'equal :rehash-size 2.0)))
    (copy-dag-r1 fsd visited-nodes)))

(defun copy-dag-r1 (dag v-nodes)
  (let* ((new-dag (make-instance 'feat-struc-dag 
				 :fs-type (slot-value dag 'type)
				 :default-status (slot-value dag 'default)))
	 (pointer (pointer dag))
	 (content (content dag))
	 (visited (gethash (id dag) v-nodes)))
    (if visited
	visited
	(progn
	  (setf (gethash (id dag) v-nodes) new-dag)
	  (if pointer 
	      (set-pointer-for-to new-dag (copy-dag-r1 pointer v-nodes)))
	  (if content
	      (if (listp content)
		  (dolist (f content)
		    (add-feature new-dag (feature-name f) (copy-dag-r1 (feature-value f) v-nodes)))
		  (set-dag-content new-dag content)))
	  new-dag))))
	  	  
(defmethod merge-features ((feats-to-merge-fs feat-struc-dag) (target-dag feat-struc-dag) grammar fail-sym unify-function)
  "takes all features from feats-to-merge-fs and adds them to target-fs"  
  (let ((target-fs (real-dag target-dag))) ;; dereference pointers, so we're working with the "real" structure!!!
    (mapcar #'(lambda (feature)
		(let ((other-found (get-feature (feature-name feature) (real-contents-of target-fs))))
		  (cond ((not other-found)
			 (add-feature target-fs
				      (feature-name feature)
				      (make-instance 'feat-struc-dag
						     :fs-type (most-general-type-in grammar)
						     :default-status (slot-value (feature-value feature) 'default)))		
			 (setf other-found (get-feature (feature-name feature) (real-contents-of target-fs)))))			 		
		  (when (eq (apply unify-function (list (feature-value feature) (feature-value other-found) grammar fail-sym)) fail-sym)
		    (throw fail-sym fail-sym))))
	    (real-contents-of feats-to-merge-fs))))

(defmethod add-feature ((fsd feat-struc-dag) feature value)
  (setf (feat-struc-dag-content fsd) (append (feat-struc-dag-content fsd) (list (list feature value)))))

(defmethod real-contents-of ((fsd feat-struc-dag))
  (feat-struc-dag-content (real-dag fsd)))

(defmethod real-dag ((fsd feat-struc-dag) &optional 
		     (thing nil)
		     ;(v-nodes (make-hash-table :size 5 :test #'equal)) ;; we can dump this if we want less garbage
		     )
  "dereferences pointer to the structure with no pointer, if there is a cycle, it returns the "
  (declare (ignore thing))
  (cond 
    ;((gethash (id fsd) v-nodes)
     ;(throw :cyclic-graph (list v-nodes fsd)))
     ;;fsd) ;; this will be our real-dag for now!!!!!!
    ((eq nil (feat-struc-dag-pointer fsd))
     ;(setf (gethash (id fsd) v-nodes) t)
     fsd)
    ((eq (type-of (feat-struc-dag-pointer fsd)) 'feat-struc-dag)
     ;(setf (gethash (id fsd) v-nodes) t)
     (real-dag (feat-struc-dag-pointer fsd)))
    (t
     ;(setf (gethash (id fsd) v-nodes) t)
     fsd)))

(defmethod set-pointer-for-to ((fs1 feat-struc-dag) (fs2 feat-struc-dag))
  (setf (feat-struc-dag-pointer fs1) fs2)
  nil)

(defmethod set-dag-content ((fsd feat-struc-dag) stuff)
  (setf (feat-struc-dag-content fsd) stuff))

(defmethod is-default? ((fsd feat-struc-dag))
  (slot-value fsd 'default))

(defmethod set-default-status ((fsd feat-struc-dag) status)
  "only 't or 'nil will be the convention for status"
  (setf (slot-value fsd 'default) status))

(defmethod id ((fsd feat-struc-dag))
  (slot-value fsd 'id))

(defmethod get-value-from-feature-path ((fsd feat-struc-dag) path &optional (no-path-symbol 'no-such-path))
 "takes a feat-struc-dag and a feature path as a list and returns the value on that path"
 (let ((first-feat (first path))
       (rest-path (rest path))
       (stuff (real-contents-of fsd))
       (feature nil))
   (cond ((null first-feat)
	  fsd)
	 ((listp stuff)
	  (if (setf feature (get-feature first-feat stuff))
	      (get-value-from-feature-path (feature-value feature) rest-path no-path-symbol)
	      no-path-symbol))
	 (t
	  no-path-symbol))))

(defmethod type-of-feat-struc ((fsd feat-struc-dag))
  (feat-struc-dag-type fsd))

(defmethod change-type-of ((fsd feat-struc-dag) new-type)
  (setf (feat-struc-dag-type fsd) new-type))
  
(defmethod change-to-default ((fsd feat-struc-dag))
  (setf (slot-value fsd 'default) t))
    
;;============================================================================
;;== methods on the list of features used in unification works on assoc lists
;;== NOTE: these have nothing to do with feat-struc-dag, but with the "actual" arcs stored in the DAG
(defmethod empty? (fs)
  (null fs))

(defmethod equal? (fs1 fs2)
  (eq fs1 fs2))

(defmethod complex? (fs)
  (and (not (null fs)) (listp fs)))

(defmethod get-feature (feat-id feats)
  (assoc feat-id feats))

(defmethod get-feature-value (feature)
  (second feature))

(defmethod feature-value (f/v)
  (second f/v))

(defmethod feature-name (fv-pair)
  (first fv-pair))

;; auxillary feat-struc-dag methods
(defmethod show-feat-struc ((fsd feat-struc-dag) &optional (stream t) (show-id-tag nil))
  (if show-id-tag
      (format stream "=~A=~%~A~%" (feat-struc-dag-id fsd) (show-feat-struc fsd stream t))
      (format stream "~A" (stringify-a-feat-struc fsd))))

(defmethod stringify-a-feat-struc ((feat-struc feat-struc-dag) &optional (depth 0) (new-string ""))
  (cond ((and (eq (type-of feat-struc) 'feat-struc-dag) (listp (real-contents-of feat-struc))(real-contents-of feat-struc))
	 (setf new-string (concatenate 'string new-string (format nil "~%~A[*~A*" (some-whitespace depth) (feat-struc-dag-type feat-struc))))
	 (mapcar #'(lambda (feature)
		     (setf new-string (concatenate 'string new-string (format nil "~%~A~A : ~A" 
									      (some-whitespace (1+ depth)) 
									      (first feature)
									      (stringify-a-feat-struc (second feature) (1+ depth)))
						   )))
		 (real-contents-of feat-struc))
	 (setf new-string (concatenate 'string new-string (format nil "~%~A]" (some-whitespace depth))))
	 new-string)
	((and (eq (type-of feat-struc) 'feat-struc-dag) (empty? (real-contents-of feat-struc)))
	 (concatenate 'string new-string (format nil "*~A*" (string (type-of-feat-struc feat-struc)))))
	((eq (type-of feat-struc) 'feat-struc-dag)
	 (concatenate 'string new-string (string (real-contents-of feat-struc))))
	(t
	 "[]")))

(defun some-whitespace (number &optional (string "    "))
  "woah whitespace"
  (let ((new-string ""))
    (dotimes (i number)
      (setf new-string (concatenate 'string new-string string)))
    new-string))		 
	  
;;========================================================================== 				  
;; The following read in feature structures as lists of feature value pairs
(let ((symbol-table nil)
      (reset-symbol-table? t))
  (defun read-in-feat-struc (feat-struc grammar &optional (process-extra-features t))
    (if reset-symbol-table? (setf symbol-table nil))
    (read-in-feat-struc-sub feat-struc grammar nil process-extra-features))

  ;; 24 APRIL 2006 version
  (defun read-in-feat-struc-sub (stuff grammar &optional (is-default nil) (process-extra-feats t))
    (let ((new-dag (make-instance 'feat-struc-dag)))
      (if is-default
	  (change-to-default new-dag))
      (cond ((listp stuff)
	     ;; set type
	     (setf (feat-struc-dag-type new-dag) (get-type-from stuff))
	     ;; set default status
	     (if (default? stuff) (setf is-default t))
	     ;; stuff is a complex feature structure, else is an atomic value
	     (if (get-feature-list stuff)
		 (mapcar #'(lambda (feature)
			     (add-feature new-dag 
					  (feature-name feature) 
					  (read-in-feat-struc-sub (feature-value feature) grammar is-default process-extra-feats)))
			 (get-feature-list stuff))
		 (if (rand-id-symbol? (get-atomic-value-from stuff)) ;; unique id tag for sementics and the like
		     (setf (feat-struc-dag-content new-dag) (make-unique-symbol (get-atomic-value-from stuff)))
		     (setf (feat-struc-dag-content new-dag) (get-atomic-value-from stuff))))
	     ;; handle varibles
	     (if (get-var-from stuff)
		 (cond ((not-bound? (get-var-from stuff))
			(add-to-symbol-table (get-var-from stuff) new-dag))
		       ((and (bound? (get-var-from stuff)) process-extra-feats)
			(merge-features new-dag (get-binding-for (get-var-from stuff)) grammar 'fail #'unify-with-defaults)
			(set-pointer-for-to new-dag (get-binding-for (get-var-from stuff))) 
			(change-type-of new-dag (type-of-feat-struc (get-binding-for (get-var-from stuff))))))))
	    ((is-var? stuff)
	     ;(print "var cond, activated!!!")
	     (cond 
	       ((not-bound? stuff)
		(add-to-symbol-table stuff new-dag)
		(setf (feat-struc-dag-type new-dag) (most-general-type-in grammar)))
	       ((bound? stuff)
		(setf (feat-struc-dag-pointer new-dag) (get-binding-for stuff))
		(setf (feat-struc-dag-type new-dag) (feat-struc-dag-type (get-binding-for stuff))))))
	    ((atom stuff) 
	     (setf (feat-struc-dag-content new-dag) stuff)
	     (setf (feat-struc-dag-type new-dag) (get-basic-atomic-type grammar)))) ;;grammar should provide explicitly a type for atomic values
      new-dag))

  (defun get-symbol-table ()
    symbol-table)

  (defun dont-reset-symbol-table () ;;use carefully
    (setf reset-symbol-table? nil))

  (defun reset-feat-struc-reader ()
    (setf symbol-table nil)
    (setf reset-symbol-table? t))

  (defun add-to-symbol-table (new-var new-binding)
    (setf symbol-table (cons (list new-var new-binding) symbol-table)))
  
  (defun get-binding-for (var)
    (cadr (assoc var symbol-table)))
  
  (defun not-bound? (thing)
    (if (assoc thing symbol-table)
	nil
	t))
  
  (defun bound? (thing)
    (if (assoc thing symbol-table)
	t
	nil))
  (defun check-reseter ()
    reset-symbol-table?)
)

(defun is-var? (thing)
  "checks if a symbol has a ? at the beginning"
  (if (atom thing)
      (if (char= (char (string thing) 0) *variable-symbol-char*)
	  t
	  nil)
      nil))

(defun default-symbol? (thing)
  (if (eq thing *default-symbol*)
      t
      nil))

(defun rand-id-symbol? (thing)
  (if (and (atom thing) (char= (char (string thing) 0) *random-id-symbol-char*))
      t
      nil))

(defun make-unique-symbol (thing)
  (if (atom thing)
      (gensym (subseq (string thing) 1)))) ;; strip '! from symbol
      
(defun get-type-from (stuff)
  (first stuff))	     

(defun get-var-from (stuff)
  (first (member-if #'is-var? stuff)))

(defun default? (stuff)
  (if (listp stuff)
      (if (eq *default-symbol* (first (member *default-symbol* stuff)))
	  t
	  nil)
      nil))

(defun get-feature-list (stuff)
  (first (member-if #'listp stuff)))

(defun get-atomic-value-from (stuff)
  (let ((atoms (rest stuff))
	(value nil))
    (mapcar  #'(lambda (some-atom)
		 (if (and (not (or (default-symbol? some-atom) (is-var? some-atom)))
			  (atom some-atom)
			  some-atom) ;; this restriction may not be necessary
		     (setf value some-atom)))
	     atoms)
    value))

				
