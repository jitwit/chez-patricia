(define mask
  (lambda (k b)
    (logbit0 b (logor k (1- (ash 1 b))))))

(define-syntax match-prefix
  (syntax-rules ()
    ((_ k p b)
     (= (logbit0 b (logor k (1- (ash 1 b))))
	p))
    ((_ k T)
     (let ((b (patricia-b T))
	   (p (patricia-p T)))
       (= (logbit0 b (logor k (1- (ash 1 b))))
	  p)))))

(define branch-bit-set? logbit?)

;; branching bit: first bit where p1 p2 disagree
(define branching-bit
  (lambda (p1 p2)
    (1- (bitwise-length (logxor p1 p2)))))

(define p= =)
(define p<= <=)
(define p< <)
