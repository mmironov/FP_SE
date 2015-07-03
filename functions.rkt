(define (digitsSum n)
  (if (= n 0) 0
      (+ (remainder n 10) (digitsSum (quotient n 10)))))

(define (iterSum n)
  (define (helperSum n result)
    (if (= n 0) result
        (helperSum (quotient n 10) (+ (remainder n 10) result))
    ))
  (helperSum n 0)
  )

(define (solution f a b eps)
  (define (solutionInc f a b)
    (let* ( (mid (/ (+ a b) 2))
            (value (f mid)))
      (cond ( (< (abs value) eps) value)
            ( (> value 0) (solution f a mid eps))
            (else (solution f mid b eps)))))
  (if (< (f a) (f b))
      (solutionInc f a b)
      (solutionInc (lambda (x) (* -1 (f x))) a b)
      )
  )

(define (line x) (- x 1))

(define l (cons 1 '(2 3 4)))

(cons 1 '(2 3 4))
(list 1 2 3 4)
(append '(1 2 3) '(4))

(list? '(1 2 3)) ;#t
(list? 26) ;#f
(pair? 2) ;#f
(pair? (cons 1 2)) ;#t
(null? '()) ;#t
(length l)
(member '() '(2 () 5 10))

(= 5 5) ;#t
(equal? '(1 2 3) '(1 2 3)) ;#t

(define (removeDuplicates l)
  (cond ( (< (length l) 2) l )
        ( (equal? (car l) (cadr l)) (removeDuplicates (cdr l)) )
        (else (list (car l) (removeDuplicates (cdr l))))
        )
  )

(define (flatten l)
  (cond ( (null? l) l )
        ( (list? (car l)) (append (flatten (car l)) (flatten (cdr l))) )
        ( else (cons (car l) (flatten (cdr l))) )
        ))

(define (myFilter pred? l)
  (cond ( (null? l) '() )
        ( (pred? (car l)) (cons (car l) (myFilter pred? (cdr l))) )
        ( else (myFilter pred? (cdr l)) )
        )
  )

(define (divBy3? n) (= (remainder n 3) 0))

(define (derive f dx)
  (lambda (x) (/ (- (f x) (f (- x dx))) dx)))

(define (derive2 f dx)
  (define (fPrime x)
    ((/ (- (f x) (f (- x dx))) dx))
    )
  fPrime)

(define (insertAt x i l)
  (define (insAt x i l)
    (cond ( (= i 0) (cons x l) )
          ( (null? l) (list x) )
          ( else (cons (car l) (insAt x (- i 1) (cdr l)) )))
    )
  (cond ( (< i 1) (cons x l) )
        ( (>= i (length l)) (append l (list x)) )
        ( else (insAt x i l) )
        ))

(define (range a b)
  (if (> a b) '() (cons a (range (+ a 1) b))))

(define (insertAll x l)
  (map (lambda (i) (insertAt x i l)) (range 0 (length l))))

(define (permutations l)
  (if (null? l) '( () )
      (apply append (map (lambda (p) (insertAll (car l) p)) (permutations (cdr l))))
      ))

(define (take n l)
  (cond ( (<= n 0) '() )
        ( (>= n (length l)) l)
        ( else (cons (car l) (take (- n 1) (cdr l))) )
        ))

(define (takeAll n l)
  (if (>= n (length l)) (list l)
      (cons (take n l) (takeAll n (cdr l)))))

(define (sublists l)
  (cons '() (apply append (map (lambda (i) (takeAll i l)) (range 1 (length l))))))