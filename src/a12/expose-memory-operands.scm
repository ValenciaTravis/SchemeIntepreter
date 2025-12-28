(define (expose-memory-operands program) 

    (define (expose elem)
        (match elem
            [() '()]
            [(mset! ,base ,off ,tri) (guard (int32? off))
                (list 'set! (make-disp-opnd base off) tri)]
            [(mset! ,off ,base ,tri) (guard (int32? off))
                (list 'set! (make-disp-opnd base off) tri)]
            [(mset! ,base ,off ,tri) 
                (list 'set! (make-index-opnd base off) tri)]
            [(set! ,var (mref ,base ,off)) (guard (int32? off))
                (list 'set! var (make-disp-opnd base off))]
            [(set! ,var (mref ,off ,base)) (guard (int32? off))
                (list 'set! var (make-disp-opnd base off))]
            [(set! ,var (mref ,base ,off)) 
                (list 'set! var (make-index-opnd base off))]
            [,ls (guard (list? ls)) (seqmap expose ls)]
            [,temp temp]
        )
    )

    (define (expose-body body)
        (expose body)
    )

    (define (expose-bind bind*)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda (,var* ...) ,body)) (list lab (cons 'lambda  (cons '() (list (expose-body body)))))]
            )
        ) bind*)
    )

    (match program
        [(letrec (,bind* ...) ,body) (list 'letrec (expose-bind bind*) (expose-body body))]
    )


)