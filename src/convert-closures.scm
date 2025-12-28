(define (convert-closures program)

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

    (define (convert-letrec bind* body)
        (define closures '())
        (define (loop bind)
            (match bind
                [(,uv (lambda (,uv* ...) (free (,free* ...) ,expr)))
                    (define cp (unique-name 'cp))
                    (set! closures (cons (append (list uv (unique-label uv)) free*) closures))
                    (list (unique-label uv) (list 'lambda (cons cp uv*)
                        (list 'bind-free (cons cp free*) (convert expr))
                    ))]
            )
        )
        (set! bind* (seqmap loop bind*))
        (list 'letrec bind* (list 'closures closures (convert body)))
    )

    (define (convert-let bind* body)
        (define (loop bind) (match bind [(,uv ,expr) (list uv (convert expr))]))
        (list 'let (seqmap loop bind*) (convert body))
    )

    (define (convert expr)
        (match expr
            [,uv (guard (uvar? uv)) uv]
            [(quote ,imm) (list 'quote imm)]
            [(if ,expr1 ,expr2 ,expr3) (list 'if (convert expr1) (convert expr2) (convert expr3))]
            [(begin ,expr* ...) (make-begin (seqmap convert expr*))]
            [(let (,bind* ...) ,body) (convert-let bind* body)]
            [(letrec (,bind* ...) ,body) (convert-letrec bind* body)]
            [(,prim ,expr* ...) (guard (prim? prim)) (cons prim (seqmap convert expr*))]
            [(,uv ,expr* ...) (guard (uvar? uv)) (cons uv (cons uv(seqmap convert expr*)))]
            [(,expr ,expr* ...) 
                (define uv (unique-name 'uv))
                (list 'let (list (list uv (convert expr))) (cons uv (cons uv (seqmap convert expr*))))]
        )
    )

    (convert program)
)