(define (introduce-allocation-forms program)
    
    (define (introduce-body body)
        (match body
            [(locals (,uv1* ...) (frame-conflict (,graph* ...) ,tail))
                (list 'locals uv1*  
                (list 'ulocals '()
                (list 'locate '()
                (list 'frame-conflict graph* tail)
            )))]
        )
    )

    (define (introduce-bind bind)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda () ,body)) (list lab (cons 'lambda (cons '() (list (introduce-body body)))))]
            )
        ) bind)
    )

    (match program
        [(letrec (,bind* ...) ,body)
            (list 'letrec (introduce-bind bind*) (introduce-body body))])

)