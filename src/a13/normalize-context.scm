(define (normalize-context program)

    (define (pred-prim? prim) 
        (or (eq? prim '<) (eq? prim '<=) (eq? prim '>) (eq? prim '>=) (eq? prim '=) 
            (eq? prim 'eq?) (eq? prim 'boolean?) (eq? prim 'fixnum?) (eq? prim 'null?)
            (eq? prim 'pair?) (eq? prim 'vector?) (eq? prim 'procedure?)
        )
    )

    (define (val-prim? prim)
        (or (eq? prim '+) (eq? prim '-) (eq? prim '*) 
            (eq? prim 'car) (eq? prim 'cdr) (eq? prim 'cons) 
            (eq? prim 'make-procedure) (eq? prim 'procedure-ref) (eq? prim 'procedure-code) 
            (eq? prim 'make-vector) (eq? prim 'vector-length) (eq? prim 'vector-ref) (eq? prim 'void) 
        )
    )

    (define (eff-prim? prim) 
        (or (eq? prim 'set-car!) (eq? prim 'set-cdr!) 
            (eq? prim 'vector-set!) (eq? prim 'procedure-set!)))

    (define (val->pred val)
        (list 'if (list 'eq? (normalize-val val) '(quote #f)) (list 'false) (list 'true))
    )

    (define (normalize-uvs uv*)
        (define (normalize bind)
            (define uv (car bind))
            (define val (cadr bind))
            (list uv (normalize-val val))
        )
        (seqmap normalize uv*)
    )

    (define (normalize-eff eff)
        (match eff
            [(true) (list 'nop)]
            [(false) (list 'nop)]
            [(void) (list 'nop)]
            [,lab (guard (label? lab)) (list 'nop)]
            [,uv (guard (uvar? uv)) (list 'nop)]
            [(,val-prim ,val* ...) (guard (val-prim? val-prim)) (make-begin (seqmap normalize-eff val*))]
            [(,pred-prim ,val* ...) (guard (pred-prim? pred-prim)) (make-begin (seqmap normalize-eff val*))]
            [(,eff-prim ,val* ...) (guard (eff-prim? eff-prim)) (cons eff-prim (seqmap normalize-val val*))]
            [(quote ,imm) (list 'nop)]
            [(if ,pred ,eff1 ,eff2) (list 'if (normalize-pred pred) (normalize-eff eff1) (normalize-eff eff2))]
            [(begin ,eff* ...) (make-begin (seqmap normalize-eff eff*))]
            [(let (,uv* ...) ,eff1) (list 'let (normalize-uvs uv*) (normalize-eff eff1))]
            [(,val* ...) (seqmap normalize-val val*)]
        )
    )

    (define (normalize-pred pred)
        (match pred
            [(true) (list 'true)]
            [(false) (list 'false)]
            [(void) (list 'true)]
            [,lab (guard (label? lab)) (val->pred lab)]
            [,uv (guard (uvar? uv)) (val->pred uv)]
            [(,val-prim ,val* ...) (guard (val-prim? val-prim)) (val->pred pred)]
            [(,pred-prim ,val* ...) (guard (pred-prim? pred-prim)) (cons pred-prim (seqmap normalize-val val*))]
            [(,eff-prim ,val* ...) (guard (eff-prim? eff-prim)) 
                (make-begin (list (normalize-eff pred) (list 'true)))]
            [(quote ,imm) (list 'if (list 'eq? (list 'quote imm) '(quote #f)) (list 'false) (list 'true))]
            [(if ,pred1 ,pred2 ,pred3) (list 'if (normalize-pred pred1) (normalize-pred pred2) (normalize-pred pred3))]
            [(begin ,eff* ... ,pred1) (make-begin (append-back (seqmap normalize-eff eff*) (normalize-pred pred1)))]
            [(let (,uv* ...) ,pred1) (list 'let (normalize-uvs uv*) (normalize-pred pred1))]
            [(,val* ...) (val->pred pred)]
        )
    )

    (define (normalize-val val)
        (match val
            [(true) '(quote #t)]
            [(false) '(quote #f)]
            [(void) (list 'void)]
            [,lab (guard (label? lab)) lab]
            [,uv (guard (uvar? uv)) uv]
            [(,val-prim ,val* ...) (guard (val-prim? val-prim)) (cons val-prim (seqmap normalize-val val*))]
            [(,pred-prim ,val* ...) (guard (pred-prim? pred-prim))
                (list 'if (normalize-pred val) '(quote #t) '(quote #f))]
            [(,eff-prim ,val* ...) (guard (eff-prim? eff-prim)) 
                (make-begin (list (normalize-eff val) (list 'void)))]
            [(quote ,imm) (list 'quote imm)]
            [(if ,pred ,val1 ,val2) (list 'if (normalize-pred pred) (normalize-val val1) (normalize-val val2))]
            [(begin ,eff* ... ,val1) (make-begin (append-back (seqmap normalize-eff eff*) (normalize-val val1)))]
            [(let (,uv* ...) ,val1) (list 'let (normalize-uvs uv*) (normalize-val val1))]
            [(,val* ...) (seqmap normalize-val val*)]
        )
    )

    (define (normalize-binds bind*)
        (seqmap (lambda (bind) 
            (match bind
                [(,lab (lambda (,uv* ...) ,expr)) (list lab (list 'lambda uv* (normalize-val expr)))]
            )
        ) bind*)
    )

    (match program
        [(letrec (,bind* ...) ,expr) (list 'letrec (normalize-binds bind*) (normalize-val expr))]
    )
)