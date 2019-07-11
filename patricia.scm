;; this uses no fx* operations, to work over arbitrarily large
;; nonnegative integers. fx ints are limited to 2^60, i believe.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bit goods                                                                  ;;

(define mask
  (lambda (k b)
    (logbit0 b (logor k (1- (ash 1 b))))))

(define match-prefix
  (lambda (key Tree)
    (= (mask key (patricia-b Tree))
       (patricia-p Tree))))

;; masked by b, does k get prefix p?
(define match-prefix*
  (lambda (k p b)
    (= (mask k b) p)))

;; branching bit: first bit where p1 p2 disagree
(define branching-bit
  (lambda (p1 p2)
    (1- (bitwise-length (logxor p1 p2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data type and constructors                                                 ;;

;; p for prefix, b for branching bit, capital letter to indicate sub-structure
(define-record-type patricia
  (fields p b L R))

(define-record-type patricia-leaf
  (fields key item))

(define empty-tree 'empty-tree)

;; called when prefixes disagree
(define join
  (lambda (p1 T1 p2 T2)
    (let* ((b (branching-bit p1 p2))
	   (new-prefix (mask p1 b)))
      (if (logbit? b p1)
	  (make-patricia new-prefix b T2 T1)
	  (make-patricia new-prefix b T1 T2)))))

;; called when prefixes agree
(define make-tree
  (lambda (p b L R)
    (cond ((empty? R) L)
	  ((empty? L) R)
	  (else (make-patricia p b L R)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queries                                                                    ;;

(define empty?
  (lambda (Tree)
    (eq? Tree empty-tree)))

(define patricia-tree?
  (lambda (T)
    (or (patricia? T)
	(patricia-leaf? T)
	(empty? T))))

(define lookup
  (lambda (key Tree)
    (if (patricia? Tree)
	(and (match-prefix key Tree)
	     (if (<= key (patricia-p Tree)) 
		 (lookup key (patricia-L Tree)) 
		 (lookup key (patricia-R Tree))))
	(and (patricia-leaf? Tree)
	     (= key (patricia-leaf-key Tree))
	     (leaf->pair Tree)))))

(define insert
  (lambda (key item T)
    (insert-with (lambda (x y) x) key item T)))

(define insert-with
  (lambda (combine key item T)
    (define (aux T)
      (cond ((patricia? T)
	     (let ((b (patricia-b T))
		   (p (patricia-p T))
		   (L (patricia-L T))
		   (R (patricia-R T)))
	       (if (match-prefix key T)
		   (if (<= key p)
		       (make-patricia p b (aux L) R)
		       (make-patricia p b L (aux R)))
		   (join key (make-patricia-leaf key item) p T))))
	    ((patricia-leaf? T)
	     (let ((j (patricia-leaf-key T)))
	       (if (= key j)
		   (make-patricia-leaf key (combine item (patricia-leaf-item T)))
		   (join j T key (make-patricia-leaf key item)))))
	    (else (make-patricia-leaf key item))))
    (aux T)))

(define merge-with
  (lambda (combine S T)
    (define (aux S T)
      (cond ((empty? S) T)
	    ((empty? T) S)
	    ((patricia-leaf? S)
	     (insert-with combine
			  (patricia-leaf-key S)
			  (patricia-leaf-item S)
			  T))
	    ((patricia-leaf? T)
	     (insert-with (lambda (x y) (combine y x))
			  (patricia-leaf-key T)
			  (patricia-leaf-item T)
			  S))
	    (else
	     (let ((p (patricia-p S))
		   (b (patricia-b S))
		   (Sl (patricia-L S))
		   (Sr (patricia-R S))
		   (q (patricia-p T))
		   (c (patricia-b T))
		   (Tl (patricia-L T))
		   (Tr (patricia-R T)))
	       (cond ((and (= p q) (= b c))
		      (make-patricia p b (aux Sl Tl) (aux Sr Tr)))
		     ((and (< b c) (match-prefix* p q c))
		      (if (logbit? c p)
			  (make-patricia q c Tl (aux S Tr))
			  (make-patricia q c (aux Tl S) Tr)))
		     ((and (< c b) (match-prefix* q p b))
		      (if (logbit? b q)
			  (make-patricia p b Sl (aux Sr T))
			  (make-patricia p b (aux Sl T) Sr)))
		     (else (join p S q T)))))))
    (aux S T)))

(define delete
  (lambda (key T)
    (define (aux T)
      (cond ((patricia? T)
	     (let ((b (patricia-b T))
		   (p (patricia-p T))
		   (L (patricia-L T))
		   (R (patricia-R T)))
	       (if (match-prefix key T)
		   (if (<= key p)
		       (make-tree p b (aux L) R)
		       (make-tree p b L (aux R)))
		   T)))
	    ((patricia-leaf? T)
	     (let ((j (patricia-leaf-key T)))
	       (if (= key j)
		   empty-tree
		   T)))
	    (else T)))
    (aux T)))

(define intersect-with
  (lambda (combine S T)
    (define (aux S T)
      (cond ((empty? S) S)
	    ((empty? T) T)
	    ((patricia-leaf? S)
	     (let* ((S-k (patricia-leaf-key S))
		    (T-k (lookup S-k T)))
	       (if (pair? T-k)
		   (make-patricia-leaf S-k (combine (patricia-leaf-item S)
						    (cdr T-k)))
		   empty-tree)))
	    ((patricia-leaf? T)
	     (let* ((T-k (patricia-leaf-key T))
		    (S-k (lookup T-k S)))
	       (if (pair? S-k)
		   (make-patricia-leaf T-k (combine (cdr S-k)
						    (patricia-leaf-item T)))
		   empty-tree)))
	    (else
	     (let ((p (patricia-p S))
		   (b (patricia-b S))
		   (Sl (patricia-L S))
		   (Sr (patricia-R S))
		   (q (patricia-p T))
		   (c (patricia-b T))
		   (Tl (patricia-L T))
		   (Tr (patricia-R T)))
	       (cond ((and (= p q) (= b c))
		      (make-tree p b (aux Sl Tl) (aux Sr Tr)))
		     ((and (< b c) (match-prefix* p q c))
		      (if (logbit? b q)
			  (aux S Tr) 
			  (aux S Tl)))
		     ((and (< c b) (match-prefix* q p b))
		      (if (logbit? c p)
			  (aux Sr T)
			  (aux Sl T)))
		     (else empty-tree))))))
    (aux S T)))

(define difference
  (lambda (S T)
    (define (aux S T)
      (cond ((empty? S) S)
	    ((empty? T) S)
	    ((patricia-leaf? S)
	     (let* ((S-k (patricia-leaf-key S))
		    (T-k (lookup S-k T)))
	       (if (pair? T-k)
		   empty-tree
		   S)))
	    ((patricia-leaf? T) (delete (patricia-leaf-key T) S))
	    (else
	     (let ((p (patricia-p S))
		   (b (patricia-b S))
		   (Sl (patricia-L S))
		   (Sr (patricia-R S))
		   (q (patricia-p T))
		   (c (patricia-b T))
		   (Tl (patricia-L T))
		   (Tr (patricia-R T)))
	       (cond ((and (= p q) (= b c))
		      (make-tree p b (aux Sl Tl) (aux Sr Tr)))
		     ((and (< b c) (match-prefix* p q c))
		      (if (logbit? b q)
			  (aux S Tr) 
			  (aux S Tl)))
		     ((and (< c b) (match-prefix* q p b))
		      (if (logbit? c p)
			  (aux Sr T)
			  (aux Sl T)))
		     (else S))))))
    (aux S T)))

(define symmetric-difference
  (lambda (S T)
    (merge-with (lambda (x y)
		  (error 'symmetric-difference
			 "I should not be called!~%"
			 S T x y))
		(difference S T)
		(difference T S))))

(define (union-with combine . trees)
  (fold-right (lambda (X Y)
		(merge-with combine X Y))
	      empty-tree
	      trees))

(define (intersection-with combine . trees)
  (if (null? trees)
      empty-tree
      (fold-right (lambda (X Y)
		    (intersect-with combine X Y))
		  (car trees)
		  (cdr trees))))

(define singleton
  (lambda (key item)
    (insert key item empty-tree)))

(define tree->alist
  (lambda (T)
    (tree-ifold-right (lambda (k v T)
			(cons (cons k v)
			      T))
		      '()
		      T)))

(define tree->keys
  (lambda (T)
    (if (patricia? T)
	(append (tree->keys (patricia-L T)) (tree->keys (patricia-R T)))
	(or (and (patricia-leaf? T)
		 (list (patricia-leaf-key T)))
	    '()))))

(define leaf->pair
  (lambda (L)
    (cons (patricia-leaf-key L)
	  (patricia-leaf-item L))))

(define tree-equal?
  (lambda (S T)
    (cond ((and (patricia? S) (patricia? T))
	   (and (= (patricia-p S) (patricia-p T))
		(= (patricia-b S) (patricia-b T))
		(tree-equal? (patricia-L S) (patricia-L T))
		(tree-equal? (patricia-R S) (patricia-R T))))
	  ((and (patricia-leaf? S) (patricia-leaf? T))
	   (and (= (patricia-leaf-key S) (patricia-leaf-key T))
		(equal? (patricia-leaf-item S) (patricia-leaf-item T))))
	  (else (and (empty? S) (empty? T))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maps, folds, and misc.                                                     ;;

(define tree-map
  (lambda (proc tree)
    (define (aux T)
      (cond ((patricia? T) (make-patricia (patricia-p T)
					  (patricia-b T)
					  (aux (patricia-L T))
					  (aux (patricia-R T))))
	    ((patricia-leaf? T) (make-patricia-leaf
				 (patricia-leaf-key T)
				 (proc (patricia-leaf-item T))))
	    (else T)))
    (aux tree)))

(define tree-imap
  (lambda (proc tree)
    (define (aux T)
      (cond ((patricia? T) (make-patricia (patricia-p T)
					  (patricia-b T)
					  (aux (patricia-L T))
					  (aux (patricia-R T))))
	    ((patricia-leaf? T) (make-patricia-leaf
				 (patricia-leaf-key T)
				 (proc (patricia-leaf-key T)
				       (patricia-leaf-item T))))
	    (else T)))
    (aux tree)))

(define tree-fold-right
  (lambda (f x tree)
    (cond ((patricia? tree)
	   (let ((y (tree-fold-right f x (patricia-R tree))))
	     (tree-fold-right f y (patricia-L tree))))
	  ((patricia-leaf? tree)
	   (f (patricia-leaf-item tree) x))
	  (else x))))

(define tree-fold-left
  (lambda (f x tree)
    (cond ((patricia? tree)
	   (let ((y (tree-fold-left f x (patricia-L tree))))
	     (tree-fold-left f y (patricia-R tree))))
	  ((patricia-leaf? tree)
	   (f x (patricia-leaf-item tree)))
	  (else x))))

(define tree-ifold-left
  (lambda (f x tree)
    (cond ((patricia? tree)
	   (let ((y (tree-ifold-left f x (patricia-L tree))))
	     (tree-ifold-left f y (patricia-R tree))))
	  ((patricia-leaf? tree)
	   (f x
	      (patricia-leaf-key tree)
	      (patricia-leaf-item tree)))
	  (else x))))

(define tree-ifold-right
  (lambda (f x tree)
    (cond ((patricia? tree)
	   (let ((y (tree-ifold-right f x (patricia-R tree))))
	     (tree-ifold-right f y (patricia-L tree))))
	  ((patricia-leaf? tree)
	   (f (patricia-leaf-key tree)
	      (patricia-leaf-item tree)
	      x))
	  (else x))))

(define successor
  (lambda (key T)
    (if (patricia? T)
	(if (<= (patricia-p T) key)
	    (successor key (patricia-R T))
	    (or (successor key (patricia-L T))
		(successor key (patricia-R T))))
	(and (< key (patricia-leaf-key T))
	     (leaf->pair T)))))

(define predecessor
  (lambda (key T)
    (if (patricia? T)
	(if (< (patricia-p T) key)
	    (or (predecessor key (patricia-R T))
		(predecessor key (patricia-L T)))
	    (predecessor key (patricia-L T)))
	(and (< (patricia-leaf-key T) key)
	     (leaf->pair T)))))

(define tree-sort
  (lambda (X)
    (fold-right (lambda (x y)
		  (append (make-list (cdr x) (car x))
			  y))
		'()
		(tree->alist
		 (fold-right (lambda (x T)
			       (insert-with + x 1 T))
			     empty-tree
			     X)))))

(define view-tree
  (lambda (T)
    (cond ((patricia? T) (let ((L (view-tree (patricia-L T)))
			       (R (view-tree (patricia-R T))))
			   (list 'branch
				 (number->string (patricia-p T) 2)
				 (patricia-b T)
				 L
				 R)))
	  ((patricia-leaf? T) (cons (patricia-leaf-key T)
				    (patricia-leaf-item T)))
	  (else '()))))



