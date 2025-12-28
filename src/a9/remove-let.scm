(define (remove-let program)

    (define (remove-uvs uv*)
        (define (remove-uv pair)
            (define uv (car pair))
            (define elem (cadr pair))
            (list 'set! uv (remove elem))
        )
        (seqmap remove-uv uv*)
    )

    (define (remove elem)
        (match elem
            [() '()]
            [(let (,uv* ...) ,elem1) (make-begin (append-back (remove-uvs uv*) (remove elem1)))]
            [,ls (guard (list? ls)) (seqmap remove ls)]
            [,temp temp]
        )
    )

    (define (remove-body body)
        (match body
            [(locals (,uv* ...) ,tail) (list 'locals uv* (remove tail))]
        )
        
    )

    (define (remove-binds bind*)
        (seqmap (lambda (bind) 
            (match bind
                [(,lab (lambda (,uv* ...) ,tail))
                    (list lab (list 'lambda uv* (remove-body tail)))])
        ) bind*)
    )

    (match program
        [(letrec (,bind* ...) ,tail) (list 'letrec (remove-binds bind*) (remove-body tail))]
    )
)