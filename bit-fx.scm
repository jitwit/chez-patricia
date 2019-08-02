(define mask
  (lambda (k b)
    (fxlogbit0 b (fxlogor k (fx1- (fxsll 1 b))))))

(define-syntax match-prefix
  (syntax-rules ()
    ((_ k p b)
     (fx= (fxlogbit0 b (fxlogor k (fx1- (fxsll 1 b))))
	  p))
    ((_ k T)
     (let ((b (patricia-b T))
	   (p (patricia-p T)))
       (fx= (fxlogbit0 b (fxlogor k (fx1- (fxsll 1 b))))
	    p)))))

(define branch-bit-set? fxlogbit?)

(define branching-bit
  (lambda (p1 p2)
    (fx1- (fxlength (fxlogxor p1 p2)))))

(define p= fx=)
(define p<= fx<=)
(define p< fx<)
