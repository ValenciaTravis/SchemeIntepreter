(define (parse-scheme program)

    (define env '())

    (define (replace env var) 
        (cond 
            ((null? env) #f)
            ((eq? var (caar env)) (cadar env)) 
            (else (replace (cdr env) var)))
    )

    (define (constant? x)
        (or (boolean? x) (fixnum? x))
    )

    (define (uop? op)
        (or (eq? op 'car) (eq? op 'cdr) (eq? op 'make-vector) (eq? op 'vector-length) (eq? op 'boolean?)
            (eq? op 'fixnum?) (eq? op 'null?) (eq? op 'pair?) (eq? op 'vector?) (eq? op 'procedure?))
    )

    (define (binop? op)
        (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op 'cons) (eq? op 'vector-ref) (eq? op '<=)
            (eq? op '>=) (eq? op '<) (eq? op '>) (eq? op '=) (eq? op 'eq?) (eq? op 'set-car!) (eq? op 'set-cdr!))
    )

    (define (parse-lambda vars expr)
        (define oldenv env)
        (define varset '())
        (define uvs '())
        (define res)
        (define (change var)
            (define uv)
            (unless (symbol? var) (error 'parse-lambda "Invalid var: ~s" var))
            (when (exists? var varset) (error 'parse-lambda "Multiple vars: ~s" var))
            (set! uv (unique-name var))
            (set! varset (cons var varset))
            (set! uvs (append-back uvs uv))
            (set! env (cons (list var uv) env))
        )
        (seqmap change vars)
        (set! res (list 'lambda uvs (parse expr)))
        (set! env oldenv)
        res
    )

    (define (parse-let binds expr)
        (define oldenv env)
        (define varset '())
        (define uvs '())
        (define (change bind)
            (match bind
                [(,var ,expr) 
                    (define uv)
                    (unless (symbol? var) (error 'parse-let "Invalid var: ~s" var))
                    (when (exists? var varset) (error 'parse-let "Multiple vars: ~s" var))
                    (set! uv (unique-name var))
                    (set! varset (cons var varset))
                    (set! uvs (append-back uvs (list uv expr)))
                    (set! env (cons (list var uv) env))]
                [,temp (error 'parse-let "Invalid binding: ~s" bind)]
            )   
        )
        (define (parse-cdr bind)
            (match bind
                [(,uv ,expr) (list uv (parse expr))]
            )
        )
        (seqmap change binds)   
        (set! expr (parse expr))
        (set! env oldenv)
        (set! uvs (seqmap parse-cdr uvs))
        (list 'let uvs expr)
    )
    
    (define (parse-letrec binds expr)
        (define oldenv env)
        (define nowenv)
        (define varset '())
        (define uvs '())
        (define (change bind)
            (match bind
                [(,var ,expr) 
                    (define uv)
                    (unless (symbol? var) (error 'parse-letrec "Invalid var: ~s" var))
                    (when (exists? var varset) (error 'parse-letrec "Multiple vars: ~s" var))
                    (set! uv (unique-name var))
                    (set! varset (cons var varset))
                    (set! uvs (append-back uvs (list uv expr)))
                    (set! env (cons (list var uv) env))]
                [,temp (error 'parse-letrec "Invalid binding: ~s" bind)]
            )   
        )
        (define (parse-cdr bind)
            (match bind
                [(,uv (lambda ,uvs ,expr)) (set! env nowenv) (list uv (parse-lambda uvs expr))]
                [(,uv ,expr) (set! env oldenv) (list uv (parse expr))]
            )
        )
        (seqmap change binds)
        (set! expr (parse expr))
        (set! nowenv env)
        (set! uvs (seqmap parse-cdr uvs))
        (set! env oldenv)
        (list 'letrec uvs expr)
    )

    (define (parse expr)
        ; (emit expr)
        (match expr
            [,x (guard (constant? x)) (list 'quote x)]
            [,var (guard (replace env var)) (replace env var)]
            [(,var ,expr* ...) (guard (replace env var)) (cons (replace env var) (seqmap parse expr*))]
            [(quote ,datum) (list 'quote datum)]
            [(if ,expr1 ,expr2) (list 'if (parse expr1) (parse expr2) (list 'void))]
            [(if ,expr1 ,expr2 ,expr3) (list 'if (parse expr1) (parse expr2) (parse expr3))]
            [(not ,expr) (list 'if (parse expr) (quote '#f) (quote '#t))]
            [(and) (list 'quote #t)]
            [(and ,expr) (parse expr)]
            [(and ,expr1 ,expr* ...) (list 'if (parse expr1) (parse (cons 'and expr*)) (list 'quote #f))]
            [(or) (list 'quote #f)]
            [(or ,expr) (parse expr)]
            [(or ,expr1 ,expr* ...) 
                (define tmp (unique-name 'tmp))
                (list 'let (list (list tmp (parse expr1))) (list 'if tmp tmp (parse (cons 'or expr*))))]
            [(begin ,expr* ...) (guard (not (null? expr*))) (make-begin (seqmap parse expr*))]
            [(lambda ,vars ,expr* ...) (guard (not (null? expr*))) (parse-lambda vars (make-begin expr*))]
            [(let ,binds ,expr* ...) (guard (not (null? expr*))) (parse-let binds (make-begin expr*))]
            [(letrec ,binds ,expr* ...) (guard (not (null? expr*))) (parse-letrec binds (make-begin expr*))]
            [(set! ,var ,expr) (guard (replace env var)) (list 'set! (replace env var) (parse expr))]
            [(void) (list 'void)]
            [(,uop ,expr) (guard (uop? uop)) (list uop (parse expr))]
            [(,binop ,expr1 ,expr2) (guard (binop? binop)) (list binop (parse expr1) (parse expr2))]
            [(vector-set! ,expr1 ,expr2 ,expr3) (list 'vector-set! (parse expr1) (parse expr2) (parse expr3))]
            [(,expr* ...) (guard (not (null? expr*))) (seqmap parse expr*)]
            [,temp (error 'parse "Invalid expr: ~s" expr)]
        )
    )

    (parse program)
)