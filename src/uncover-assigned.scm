(define (uncover-assigned program)

    (define (uncover-let binds elem)
        (define uvs (seqmap car binds))
        (define assigns)
        (define current)
        ; (display binds)
        (receive (ubinds assigns1) (uncover binds)
        (receive (uelem assigns2) (uncover elem)
        (set! assigns (union assigns1 assigns2))
        (set! current (intersection assigns uvs))
        (set! assigns (difference assigns current))
        (values
            (list 'let ubinds (list 'assigned current uelem))
            assigns
        )))
    )

    (define (uncover-letrec binds elem)
        (define uvs (seqmap car binds))
        (define assigns)
        (define current)
        (receive (ubinds assigns1) (uncover binds)
        (receive (uelem assigns2) (uncover elem)
        (set! assigns (union assigns1 assigns2))
        (set! current (intersection assigns uvs))
        (set! assigns (difference assigns current))
        (values
            (list 'letrec ubinds (list 'assigned current uelem))
            assigns
        )))
    )

    (define (uncover-lambda uvs elem)
        (define current)
        (receive (uelem assigns) (uncover elem)
        (set! current (intersection assigns uvs))
        (set! assigns (difference assigns current))
        (values
            (list 'lambda uvs (list 'assigned current uelem))
            assigns
        ))
    )

    (define (uncover elem)
        ; (display elem) (newline)
        (match elem
            [() (values '() '())]
            [(let ,binds ,elem1) (uncover-let binds elem1)]
            [(letrec ,binds ,elem1) (uncover-letrec binds elem1)]
            [(lambda ,uvs ,elem1) (uncover-lambda uvs elem1)]
            [(set! ,uv ,elem1) 
                (receive (uelem1 assigns) (uncover elem1)
                (values
                    (list 'set! uv uelem1)
                    (set-cons uv assigns)
                ))]
            [(,elem1 ,ls* ...) 
                (receive (uelem1 assigns1) (uncover elem1)
                (receive (uls2 assigns2) (uncover ls*)
                (values
                    (cons uelem1 uls2)
                    (union assigns1 assigns2)
                )))]
            [,temp (values temp '())]
        )
    )
    
    (receive (uprog assigns) (uncover program) uprog)
)