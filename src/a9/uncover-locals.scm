(define (uncover-locals program)

    (define (uncover-uvs uv*)
        (define uvs '())
        (define (uncover-uv pair)
            (define uv (car pair))
            (define elem (cadr pair))
            (set! uvs (append uvs (cons uv (uncover elem))))
        )
        (seqmap uncover-uv uv*)
        uvs
    )

    (define (uncover elem)
        (match elem
            [() '()]
            [(let (,uv* ...) ,elem1) (append (uncover-uvs uv*) (uncover elem1))]
            [(,elem ,ls ...) (append (uncover elem) (uncover ls))]
            [,temp '()]
        )
    )

    (define (uncover-body body)
        (list 'locals (uncover body) body)
    )

    (define (uncover-binds bind*)
        (map (lambda (bind) 
            (match bind
                [(,lab (lambda (,uv* ...) ,tail))
                    (list lab (list 'lambda uv* (uncover-body tail)))])
        ) bind*)
    )

    (match program
        [(letrec (,bind* ...) ,tail) (list 'letrec (uncover-binds bind*) (uncover-body tail))]
    )

)