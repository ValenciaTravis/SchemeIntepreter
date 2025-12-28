(define (remove-anonymous-lambda program)

    (define (pred-prim? prim) 
        (or (eq? prim '<) (eq? prim '<=) (eq? prim '>) (eq? prim '>=) (eq? prim '=) 
            (eq? prim 'eq?) (eq? prim 'boolean?) (eq? prim 'fixnum?) (eq? prim 'null?) 
            (eq? prim 'pair?) (eq? prim 'vector?) (eq? prim 'procedure?)
        )
    )

    (define (val-prim? prim)
        (or (eq? prim '+) (eq? prim '-) (eq? prim '*) 
            (eq? prim 'car) (eq? prim 'cdr) (eq? prim 'cons) 
            (eq? prim 'make-vector) (eq? prim 'vector-length) (eq? prim 'vector-ref) (eq? prim 'void) 
        )
    )

    (define (eff-prim? prim) (or (eq? prim 'set-car!) (eq? prim 'set-cdr!) (eq? prim 'vector-set!)))

    (define (prim? prim) (or (val-prim? prim) (pred-prim? prim) (eff-prim? prim)))

    (define (remove-letrec bind* body)
        (define (loop bind) 
            (match bind [(,uv (lambda (,uv* ...) ,expr)) (list uv (list 'lambda uv* (remove expr)))])
        )
        (list 'letrec (seqmap loop bind*) (remove body))
    )

    (define (remove-let bind* body)
        (define (loop bind) (match bind [(,uv ,expr) (list uv (remove expr))]))
        (list 'let (seqmap loop bind*) (remove body))
    )

    (define (remove elem)
        (match elem
            [,uv (guard (uvar? uv)) uv]
            [(quote ,imm) (list 'quote imm)]
            [(if ,expr1 ,expr2 ,expr3) (list 'if (remove expr1) (remove expr2) (remove expr3))]
            [(begin ,expr* ...) (make-begin (seqmap remove expr*))]
            [(let (,bind* ...) ,body) (remove-let bind* body)]
            [(letrec (,bind* ...) ,body) (remove-letrec bind* body)]
            [(,prim ,expr* ...) (guard (prim? prim)) (cons prim (seqmap remove expr*))]
            [(lambda (,uv* ...) ,body)
                (define anon (unique-name 'anon))
                (list 'letrec (list (list anon (list 'lambda uv* (remove body)))) anon)]
            [(,uv ,expr* ...) (guard (uvar? uv)) (cons uv (seqmap remove expr*))]
            [(,expr* ...) (seqmap remove expr*)]
        )
    )

    (remove program)
)