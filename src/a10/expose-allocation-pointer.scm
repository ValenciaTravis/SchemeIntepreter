(define (expose-allocation-pointer program)

    (define (expose elem)
        (match elem
            [() '()]
            [(set! ,var (alloc ,expr)) 
                (make-begin (list
                    (list 'set! var allocation-pointer-register)
                    (list 'set! allocation-pointer-register (list '+ allocation-pointer-register expr))
                ))]
            [,ls (guard (list? ls)) (seqmap expose ls)]
            [,temp temp]
        )
    )

    (define (expose-body body)
        (match body
            [(locals (,uv1* ...) (new-frames (,uv2* ...) ,tail))
                (list 'locals uv1* (list 'new-frames uv2* (expose tail)))
            ]
        )
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