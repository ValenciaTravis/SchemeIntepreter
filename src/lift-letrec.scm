(define (lift-letrec program)
    (define bind* '())
    (define (lift expr)
        (when (list? expr) (set! expr (seqmap lift expr)))
        (match expr
            [(letrec (,bind1* ...) ,expr1) (set! bind* (append bind1* bind*)) expr1]
            [,temp temp]
        )
    )
    (define lexpr (lift program))
    
    (list 'letrec bind* lexpr)
)