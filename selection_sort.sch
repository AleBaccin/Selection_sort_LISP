(define minKey
      (lambda(k L)
              (cond   (     (null? (cdr L))       (car L))
                      (     (if(<= (k (car L))   (k (minKey k (cdr L))))
                            (car L)
                            (minKey k (cdr L))))
              )
      )
)

(define removeMinKey
      (lambda(k L)
              (cond   (   (null? L)             '()           )
                      (   (if (equal? (k (car L)) (k (minKey k L))) 
                                  (cdr L)
                                  (cons (car L) (removeMinKey k (cdr L)))
                      ))
        )
    )
)




(define selectionSort
      (lambda(k L)
              (cond (   (null? L)              L                                    )
                    (   (null? (cdr L))        L                                    )
                    ( #t    (cons (minKey k L) (selectionSort k (removeMinKey k L))))
              )
      )
)

(define k (lambda(v) v))