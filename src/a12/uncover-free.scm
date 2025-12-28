(define (uncover-free program)

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

    (define (uncover-letrec bind* body)
        (define letrec-frees '())
        (define uvs (seqmap car bind*))
        (define (uncover-bind bind)
            ; (emit bind)
            (match bind 
                [(,lab (lambda (,uv* ...) ,expr))
                    (receive (frees uexpr) (uncover expr)
                    (set! frees (difference frees (intersection frees uv*)))
                    (set! letrec-frees (union letrec-frees frees))
                    (list lab (list 'lambda uv* (list 'free frees uexpr)))
                    )]))
        (set! bind* (seqmap uncover-bind bind*))
        ; (emit "uvs:") (emit uvs)
        ; (emit "letrec-frees:") (emit letrec-frees)
        (receive (frees uexpr) (uncover body)
        (set! letrec-frees (union letrec-frees frees))
        (set! letrec-frees (difference letrec-frees (intersection letrec-frees uvs)))
        (values
            letrec-frees
            (list 'letrec bind* uexpr)
        ))
    )

    (define (uncover-let bind* body)
        (define uvs (seqmap car bind*))
        (define frees '())
        (define (uncover-bind bind)
            (match bind 
                [(,uv ,expr)
                    (receive (frees1 uexpr) (uncover expr)
                        (set! frees (union frees1 frees))
                        (list uv uexpr)
                    )]))
        (receive (frees1 uexpr) (uncover body) 
        (set! frees1 (difference frees1 (intersection frees1 uvs)))
        (set! bind* (seqmap uncover-bind bind*))
        (values 
            (union frees frees1)
            (list 'let bind* uexpr)
        ))
    )   

    (define (uncover-exprs expr*)
        (define frees '())
        (set! expr* (seqmap (lambda (expr)
            (receive (frees1 uexpr) (uncover expr)
                (set! frees (union frees frees1))
                uexpr
            )
        ) expr*))
        (values frees expr*)
    )

    (define (uncover elem)
        (match elem
            [,uv (guard (uvar? uv)) (values (list uv) uv)]
            [(quote ,imm) (values '() (list 'quote imm))]
            [(if ,expr1 ,expr2 ,expr3) 
                (receive (frees1 uexpr1) (uncover expr1)
                (receive (frees2 uexpr2) (uncover expr2)
                (receive (frees3 uexpr3) (uncover expr3)
                (values
                    (union frees1 (union frees2 frees3))
                    (list 'if uexpr1 uexpr2 uexpr3)
                ))))]
            [(begin ,expr* ...) (receive (frees uexpr*) (uncover-exprs expr*) (values frees (make-begin uexpr*)))]
            [(letrec (,bind* ...) ,body) (uncover-letrec bind* body)]
            [(let (,bind* ...) ,body) (uncover-let bind* body)]
            [(,prim ,expr* ...) (guard (prim? prim)) (receive (frees uexpr*) (uncover-exprs expr*) (values frees (cons prim uexpr*)))]
            [(,uv ,expr* ...) (guard (uvar? uv)) (receive (frees uexpr*) (uncover-exprs expr*) (values (set-cons uv frees) (cons uv uexpr*)))]
            [(,expr* ...) (uncover-exprs expr*)]
        )
    )

    (receive (frees prog) (uncover program) prog)

)