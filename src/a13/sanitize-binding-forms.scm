(define (sanitize-binding-forms program)

    (define (sanitize-let bind* expr)
        (define letrec-binds '())
        (define let-binds '())
        (define (loop bind)
            (match bind
                [(,uv (lambda (,uv* ...) ,expr))
                    (set! letrec-binds (cons (list uv (list 'lambda uv* (sanitize expr))) letrec-binds))]
                [,temp (set! let-binds (cons temp let-binds))]
            )
        )
        (seqmap loop bind*)
        (set! expr (sanitize expr))
        (when (not (null? let-binds)) (set! expr (list 'let let-binds expr)))
        (when (not (null? letrec-binds)) (set! expr (list 'letrec letrec-binds expr)))
        expr
    )

    (define (sanitize elem)
        (match elem
            [(let (,bind* ...) ,expr) (sanitize-let bind* expr)]
            [,ls (guard (list? ls)) (seqmap sanitize ls)]
            [,temp temp]
        )
    )

    (sanitize program)

)