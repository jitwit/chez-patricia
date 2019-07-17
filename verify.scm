;; ad hoc tests/benchmarks
(import (srfi :1)
	(chez patricia))

;; ratio of lookups/insertions. also for benching
(define (random-tree lim trials ratio . verbose?)
  (time
   (let loop ((n 0) (x (singleton 1 0)))
     (if (= n trials)
	 (if (null? verbose?)
	     'done
	     x)
	 (if (< (random 1.) ratio)
	     (begin
	       (lookup (random lim) x)
	       (loop (1+ n) x))
	     (loop (1+ n)
		   (insert (random lim) n x)))))))

;; check random trees by comparing against srfi 1 and hashmaps
(define tree-1 (random-tree 40000 10000 0 'yes))
(define list-1 (tree->keys tree-1))
(define tree-2 (random-tree 40000 10000 0 'yes))
(define list-2 (tree->keys tree-2))
(define hashmap-1 (let ((table (make-eq-hashtable)))
		    (for-each (lambda (x)
				(hashtable-set! table x x))
			      list-1)
		    table))
(define hashmap-2 (let ((table (make-eq-hashtable)))
		    (for-each (lambda (x)
				(hashtable-set! table x x))
			      list-2)
		    table))

(define (merge-hashmaps x y)
  (let ((table (make-eq-hashtable)))
    (vector-for-each (lambda (z)
		       (hashtable-set! table z z))
		     (hashtable-keys x))
    (vector-for-each (lambda (z)
		       (hashtable-set! table z z))
		     (hashtable-keys y))
    table))

(define (time-patricia-merge)
  (time (tree-size (merge-with + tree-1 tree-2))))
(define (time-list-merge)
  (time (length (lset-union = list-1 list-2))))
(define (time-hashmap-merge)
  (time (vector-length
	 (hashtable-keys
	  (merge-hashmaps hashmap-1
			  hashmap-2)))))

(assert (= (length (lset-union = list-1 list-2))
	   (tree-size (merge-with + tree-1 tree-2))))
