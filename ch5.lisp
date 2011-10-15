;;; -*- Mode: Lisp; Syntax: Common-Lisp 

(defparameter ch5-most-general-type 'feat-struc)

(defparameter ch5-grammar-rules ())


(setf ch5-grammar-rules 
'(
  ((MOTHER -head-specifier-rule> SPECIFIER HEAD-DAUGHTER) ;; CF portion
   (rule ((MOTHER (phrase ((SYN 
			    (syn-cat ((HEAD (pos ?head-daughter))
				      (VAL (val-cat ((SPR (expr-list <>))
						     (COMPS (expr-list <>))
						     (MOD (expr-list ?mods))))))))
			   (SEM 
			    (sem-cat ((MODE (atom ?head-mode))
				      (INDEX (index ?i))
				      (RESTR (diff-list ((LIST (pred-list ?list1))
							 (LAST (pred-list ?end)))))))))))

	  (SPECIFIER (expression ?spr ((SEM (sem-cat ((RESTR (diff-list
							      ((LIST (pred-list ?list1))
							       (LAST (pred-list ?list2)))))))))))
	  (HEAD-DAUGHTER (expression ((SYN 
				       (syn-cat ((HEAD (pos ?head-daughter))
						 (VAL (val-cat ((SPR (expr-list ((FIRST (expr-list ?spr)
											(REST (expr-list <>))))))
								(COMPS (expr-list <>))
								(MOD (expr-list ?mods))))))))
				      (SEM
				       (sem-cat ((MODE (atom ?head-mode))
						 (INDEX (index ?i))
						 (RESTR (diff-list ((LIST (pred-list ?list2))
								    (LAST (pred-list ?end))))))))))))))
	 
	  
  ((MOTHER -head-complement-rule-1> HEAD-DAUGHTER COMP) ;; CF portion
   (rule ((MOTHER (phrase ((SYN 
			    (syn-cat ((HEAD (pos ?head-dtr))
				      (VAL (val-cat ((SPR (expr-list ?spr))
						     (COMPS (expr-list <>))
						     (MOD (expr-list ?mods))))))))
			   (SEM 
			    (sem-cat ((MODE (atom ?head-mode))
				      (INDEX (index ?i))
				      (RESTR (diff-list ((LIST (pred-list ?list1))
							 (LAST (pred-list ?end)))))))))))
	  (HEAD-DAUGHTER (word ((SYN (syn-cat ((HEAD (pos ?head-dtr))
					       (VAL (val-cat ((SPR (expr-list ?spr))
							      (MOD (expr-list ?mods))
							      (COMPS (expr-list ((FIRST 
										  (expression ?comp 
											      ((SEM (sem-cat 
												     ((RESTR (diff-list 
													      ((LIST (pred-list ?list2))
													       (LAST (pred-list ?end)))))))))))
										 (REST (expr-list <>)))))))))))
				(SEM (sem-cat ((MODE (atom ?head-mode))
					       (INDEX (index ?i))
					       (RESTR (diff-list ((LIST (pred-list ?list1))
								  (LAST (pred-list ?list2)))))))))))
	  (COMP ?comp))))


  ((MOTHER -head-complement-rule-2> HEAD-DAUGHTER COMP1 COMP2) ;; CF portion
   (rule ((MOTHER (phrase ((SYN (syn-cat ((HEAD (pos ?head-dtr))
					  (VAL (val-cat ((SPR (expr-list ?spr))
							 (COMPS (expr-list <>))
							 (MOD (expr-list ?mods))))))))
			   (SEM (sem-cat ((MODE (atom ?head-mode))
					  (INDEX (index ?i))
					  (RESTR (diff-list ((LIST (pred-list ?list1)) 
							     (LAST (pred-list ?end)))))))))))
	  (HEAD-DAUGHTER (word ((SYN (syn-cat ((HEAD (pos ?head-dtr))
					       (VAL (val-cat ((SPR (expr-list ?spr))
							      (MOD (expr-list ?mods))
							      (COMPS (expr-list ((FIRST (expression ?comp1))
										 (REST (expr-list ((FIRST (expression ?comp2))
												   (REST (expr-list <>))))))))))))))
				(SEM (sem-cat ((MODE (atom ?head-mode))
					       (INDEX (index ?i))
					       (RESTR (diff-list ((LIST (pred-list ?list1))
								  (LAST (pred-list ?list2)))))))))))
	  (COMP1 (expression ?comp1 ((SEM
				      (sem-cat 
				       ((RESTR (diff-list 
						((LIST (pred-list ?list2))
						 (LAST (pred-list ?final)))))))))))
	  (COMP2 (expression ?comp2 ((SEM
				      (sem-cat 
				       ((RESTR (diff-list 
						((LIST (pred-list ?final))
						 (LAST (pred-list ?end))))))))))))))
;;=====================
;; rewrite this rule, 
;  ((MOTHER -head-modifier-rule-> HD-DTR MOD)
;   (rule ((MOTHER (phrase ((SYN (syn-cat ((HEAD (pos ?hd-dtr))
;					  (VAL (val-cat
;						((SPR (expr-list ?spr))
;						 (COMPS (expr-list ?comps <>))
;						 (MOD (expr-list ?hd-mods))))))))
;			   (SEM (sem-cat ((MODE (atom ?hd-mode))
;					  (INDEX (index ?hd-ind))
;					  (RESTR (diff-list 
;						  ((LIST (pred-list ?list1))
;						   (LAST (pred-list ?end)))))))))))
;	  (HD-DTR (expression ?x ((SYN (syn-cat ((HEAD (pos ?hd-dtr))
;						 (VAL (val-cat
;						       ((SPR (expr-list ?spr))
;							(COMPS (expr-list ?spr <>))
;							(MOD (expr-list ?hd-mods))))))))
;				  (SEM (sem-cat ((MODE (atom ?hd-mode))
;						 (INDEX (index ?hd-ind))
;						 (RESTR (diff-list ((LIST (pred-list ?list1))
;								    (LAST (pred-list ?list2)))))))))))
;	  (MOD (expression ((SYN (syn-cat ((VAL (val-cat ((COMPS (expr-list <>))
;							  (MOD (expr-list ((FIRST (expression ?x))
;									   (REST (expr-list <>)))))))))))
;			    (SEM (sem-cat ((RESTR (diff-list ((LIST (pred-list ?list2))
;							      (LAST (pred-list ?end))))))))))))))
  ))


      
     

(defparameter ch5-lexicon nil)
(setf ch5-lexicon  '(
    ("dog" (word ((SYN 
		   (syn-cat 
		    ((HEAD (noun 
			    ((AGR (3sing)))))
		     (VAL (val-cat
			   ((SPR (expr-list 
				  ((FIRST (expression ((SYN (syn-cat
							     ((HEAD (det))
							      (VAL (val-cat
								    ((SPR (list <>))
								     (COMPS (list <>))))))))
						       (SEM (sem-cat 
							     ((INDEX (index ?i !i))))))))
				   (REST (expr-list <>)))))
			    (COMPS (expr-list <>))))))))
		  (SEM 
		   (sem-cat
		    ((MODE ref)
		     (INDEX ?i)
		     (RESTR (diff-list ((LIST (pred-list
					       ((FIRST (predication ((RELN dog)
								     (INST ?i))))
						(REST (pred-list ?end)))))
					(LAST ?end))))))))))
    ("cat" (word ((SYN 
		   (syn-cat 
		    ((HEAD (noun 
			    ((AGR (3sing)))))
		     (VAL (val-cat
			   ((SPR (list 
				  ((FIRST (expression 
					   ((SYN (syn-cat
						  ((HEAD (det))
						   (VAL (val-cat
							 ((SPR (expr-list <>))
							  (COMPS (expr-list <>))))))))
					    (SEM (sem-cat 
						((INDEX (index ?i !i))))))))
				   (REST (expr-list <>)))))
			    (COMPS (expr-list <>))))))))
		  (SEM 
		   (sem-cat
		    ((MODE ref)
		     (INDEX (index ?i))
		     (RESTR (diff-list ((LIST (pred-list
					       ((FIRST (predication ((RELN cat)
								     (INST ?i))))
						(REST (pred-list ?end)))))
					(LAST ?end))))))))))
    
    ("a" (word ((SYN
		 (syn-cat ((HEAD (det
				  ((AGR (3sing))
				   (COUNT +))))
			   (VAL (val-cat ((COMPS (list <>))
					  (SPR (list <>))
					  (MOD (list <>))))))))
		(SEM
		 (sem-cat
		  ((MODE none)
		   (INDEX (index ?i))
		   (RESTR (diff-list 
			   ((LIST (pred-list ((FIRST (predication
						      ((RELN exsist)
						       (BV ?i))))
					      (REST (pred-list ?end)))))
			    (LAST ?end)))))))))
     )
    
    ("and" (word ((SYN (syn-cat ((HEAD (conj)))))
		  (SEM (sem-cat ((INDEX (index ?s !sit))
				 (MODE none)
				 (RESTR (diff-list 
					 ((LIST (pred-list ((FIRST (predication
								    ((RELN and)
								     (SIT ?s))))
							    (REST (pred-list ?end)))))
					  (LAST ?end)))))))))
     )
    
    ("today" (word ((SYN (syn-cat ((HEAD (adv))
				   (VAL (val-cat ((SPR (expr-list <>))
						  (COMPS (expr-list <>))
						  (MOD (expr-list ((FIRST (expression 
									   ((SYN 
									     (syn-cat ((HEAD (verb))
										       (VAL (val-cat ((COMPS (list <>))
												      (SPR (expr-list ((FIRST (expression))
														       (REST (list <>)))))))))))
									    (SEM (sem-cat ((INDEX (index ?sit))))))))
								   (REST (expr-list <>)))))))))))
		  (SEM (sem-cat ((INDEX ?sit)
				   (MODE none)
				   (RESTR (diff-list 
					   ((LIST (pred-list ((FIRST (predication
								      ((RELN today)
								       (ARG ?sit))))
							      (REST (pred-list ?end)))))
					    (LAST ?end)))))))))
     )
    ("loves" (word ((SYN
		     (syn-cat ((HEAD (verb
				      ((AGR (3sing)))))
			       (VAL (val-cat
				     ((MOD (expr-list <>))
				      (SPR (expr-list ((FIRST (expression ((SYN (syn-cat ((HEAD (noun))
											  (VAL (val-cat ((COMPS (expr-list <>))
													 (SPR (expr-list <>))))))))
									   (SEM (sem-cat ((INDEX (index ?i)))))))
							      (REST (expr-list <>))))))
				      (COMPS (expr-list ((FIRST (expression ((SYN (syn-cat ((HEAD (noun))
											    (VAL (val-cat ((COMPS (expr-list <>))
													   (SPR (expr-list <>))))))))
									     (SEM (sem-cat ((INDEX (index ?j))))))))
							 (REST (expr-list <>)))))))))))
		    (SEM
		     (sem-cat ((MODE prop)
			       (INDEX (index ?sit !sit))
			       (RESTR (diff-list 
				       ((LIST (pred-list ((FIRST (predication
								  ((RELN love)
								   (SIT ?sit)
								   (LOVER ?i)
								   (LOVED ?j))))					  
							  (REST (pred-list ?end)))))
					(LAST ?end))))))))))
     ("Kim" (word ((SYN 
		    (syn-cat 
		     ((HEAD (noun 
			     ((AGR (3sing)))))
		      (VAL (val-cat
			    ((SPR (expr-list <>))
			     (COMPS (list <>))
			     (MODS (expr-list <>))))))))
		   (SEM 
		    (sem-cat
		     ((MODE ref)
		      (INDEX (index ?i !i))
		      (RESTR (diff-list ((LIST (pred-list
						((FIRST (predication ((RELN name)
								      (NAME  (atom kim))
								      (NAMED ?i))))
						 (REST (pred-list ?end)))))
					(LAST ?end)))))))))
      )

    )) ;; end ch5 lexicon
;; NP
	    

#|
;; base RESTRICTION feature
(RESTR (diff-list 
	((LIST (pred-list ((FIRST (predication))
			   
			   (REST (pred-list ?end)))))
	 (LAST ?end))))
;; NP sub i
(expression ((SYN (syn-cat ((HEAD (noun))
			    (VAL (val-cat ((COMPS (expr-list <>))
					   (SPR (expr-list <>))))))))
	     (SEM (sem-cat ((INDEX ?i))))))
|#	    
			 
		        

		   

(defparameter ch5-atom-type 'atom)

(defparameter ch5-type-hierarchy ())
(setf ch5-type-hierarchy 
      '(feat-struc
	(rule) ;; so we can compress any rule into a valid feat-struc relative to this grammar
	(atom)
	(index)
	(predication)
	(syn-cat)
	(sem-cat)
	(val-cat)
	(list 
	 (l-sequence) 
	 (pred-list)
	 (expr-list))
	(diff-list)
	(pos
	 (adj)
	 (prep)
	 (adv)
	 (conj)
	 (agr-pos
	  (det)
	  (verb)
	  (nominal 
	   (noun) 
	   (comp))))
	(agr-cat
	 (3sing)
	 (non-3sing
	  (1sing)
	  (non-1sing
	   (2sing)
	   (plural))))
	(expression
	 (word)
	 (phrase))))

(defparameter ch5-type-paths
  '((feat-struc)
    
    (rule feat-struc)
    
    (atom feat-struc)
   
    (list feat-struc)
    (expr-list list feat-struc)   
    (pred-list list feat-struc)
    
    (diff-list feat-struc)
 
    (index feat-struc) 
    (syn-cat feat-struc)
    
    (sem-cat feat-struc)
    
    (val-cat feat-struc)
    
    (predication feat-struc)
  
    (expression feat-struc)
    (word expression feat-struc)
    (phrase expression feat-struc)
    
    (pos feat-struc)
    (adj pos feat-struc)
    (prep pos feat-struc)
    (adv pos feat-struc)
    (conj pos feat-struc)
    (agr-pos pos feat-struc)
    (det agr-pos pos feat-struc)
    (verb agr-pos pos feat-struc)
    (noun agr-pos pos feat-struc)
  
    (agr-cat feat-struc)
    (3sing agr-cat feat-struc)
    (non-3sing agr-cat feat-struc)
    (1sing non-3sing agr-cat feat-struc)
    (non-1sing non-3sing agr-cat feat-struc)
    (2sing non-1sing non-3sing agr-cat feat-struc)
    (plural non-1sing non-3sing agr-cat feat-struc)
    )
  )


(defvar ch5-gram (make-grammar :most-general-atom-type ch5-atom-type))

(defun make-ch5-grammar ()
  (setf (grammar-most-general-type ch5-gram) ch5-most-general-type)
  (read-in-path-table ch5-gram ch5-type-paths)
  (read-in-grammar-rules ch5-gram ch5-grammar-rules)
  (read-in-lexicon ch5-gram ch5-lexicon)
  t)
