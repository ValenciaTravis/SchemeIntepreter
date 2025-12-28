(define (introduce-procedure-primitives program)

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

    (define (replace-uvar uv bind) 
        (cond 
            ((null? bind) uv)
            ((eq? uv (caar bind)) (cadar bind)) 
            (else (replace-uvar uv (cdr bind))))
    )

    (define (replace-all all bind)
        (define (replace all) 
            (match all
                [,ls (guard (list? ls)) (seqmap replace ls)]
                [,uv (guard (uvar? uv)) (replace-uvar uv bind)]
                [,tmp tmp]
            )
        )   
        (replace all)
    )

    (define (introduce-letrec bind* clos* body)
        (define let-binds '())
        (define (introduce-bind bind)
            (define binds '())
            (define (make-refs cp index free*)
                (match free*
                    [() #f]
                    [(,fr ,free* ...) 
                        (set! binds (cons (list fr (list 'procedure-ref cp (list 'quote index))) binds))
                        (make-refs cp (+ index 1) free*)]
                )
            )
            (match bind
                [(,lab (lambda (,uv* ...) (bind-free (,cp ,free* ...) ,expr)))
                    (make-refs cp 0 free*)
                    (list lab (list 'lambda uv* (replace-all (introduce expr) binds)))
                ]
            )
        )
        (define (introduce-clos clos)
            (define (make-sets uv index uv*)
                (match uv*
                    [() #f]
                    [(,uv1 ,uv* ...) 
                        (set! body (cons (list 'procedure-set! uv (list 'quote index) uv1) body))
                        (make-sets uv (+ index 1) uv*)
                    ]
                )
            )
            (match clos [(,uv ,lab ,uv* ...) (make-sets uv 0 uv*)
                (set! let-binds (cons (list uv (list 'make-procedure lab (list 'quote (length uv*)))) let-binds))
            ])
        )
        (set! body (list (introduce body)))
        (set! bind* (seqmap introduce-bind bind*))
        (seqmap introduce-clos clos*)
        (list 'letrec bind* (list 'let let-binds (make-begin body)))
    )

    (define (introduce-let bind* body)
        (define (loop bind) (match bind [(,uv ,expr) (list uv (introduce expr))]))
        (list 'let (seqmap loop bind*) (introduce body))
    )

    (define (introduce expr)
        (match expr
            [,uv (guard (uvar? uv)) uv]
            [(quote ,imm) (list 'quote imm)]
            [(if ,expr1 ,expr2 ,expr3) (list 'if (introduce expr1) (introduce expr2) (introduce expr3))]
            [(begin ,expr* ...) (make-begin (seqmap introduce expr*))]
            [(let (,bind* ...) ,body) (introduce-let bind* body)]
            [(letrec (,bind* ...) (closures (,clos* ...) ,body)) (introduce-letrec bind* clos* body)]
            [(,prim ,expr* ...) (guard (prim? prim)) (cons prim (seqmap introduce expr*))]
            [(,uv ,expr* ...) (guard (uvar? uv)) (cons (list 'procedure-code uv) (seqmap introduce expr*))]
        )
    )

    (introduce program)

)