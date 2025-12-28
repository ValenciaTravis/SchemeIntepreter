(define (optimize-direct-call program)

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

    (define (optimize-lambda uv* val* expr)
        (define (loop uv* val*)
            (define uv)
            (define val)
            (cond 
                ((null? uv*) '())
                (else
                    (set! uv (car uv*))
                    (set! val (car val*))
                    (cons (list uv (optimize val)) (loop (cdr uv*) (cdr val*)))
                )
            )
        )
        (list 'let (loop uv* val*) (optimize expr))
    )

    (define (optimize-letrec bind* expr)
        (define (loop bind) (match bind [(,uv ,expr) (list uv (optimize expr))]))
        (list 'letrec (seqmap loop bind*) (optimize expr))
    )

    (define (optimize-let bind* expr)
        (define (loop bind) (match bind [(,uv ,expr) (list uv (optimize expr))]))
        (list 'let (seqmap loop bind*) (optimize expr))
    )

    (define (optimize elem)
        (match elem
            [,uv (guard (uvar? uv)) uv]
            [(quote ,imm) (list 'quote imm)]
            [(if ,expr1 ,expr2 ,expr3) (list 'if (optimize expr1) (optimize expr2) (optimize expr3))]
            [(begin ,expr* ...) (make-begin (seqmap optimize expr*))]
            [(let (,bind* ...) ,body) (optimize-let bind* body)]
            [(letrec (,bind* ...) ,body) (optimize-letrec bind* body)]
            [(,prim ,expr* ...) (guard (prim? prim)) (cons prim (seqmap optimize expr*))]
            [(,uv ,expr* ...) (guard (uvar? uv)) (cons uv (seqmap optimize expr*))]
            [((lambda (,uv* ...) ,expr) ,val* ...) (optimize-lambda uv* val* expr)]
            [(lambda (,uv* ...) ,expr) (list 'lambda uv* (optimize expr))]
            [(,expr* ...) (seqmap optimize expr*)]
        )
    )

    (optimize program)
    
)