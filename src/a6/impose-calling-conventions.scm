(load "src/utils.scm")

(define (impose-calling-conventions program)

    (define (triv? var) (or (uvar? var) (int64? var) (label? var)))
    (define (binop? binop) (or (eq? binop '+) (eq? binop '-) (eq? binop '*) (eq? binop 'logand)(eq? binop 'logor) (eq? binop 'sra)))

    (define (impose-proc var*)
        (define index 0)
        (define params parameter-registers)
        (define loc* '())
        (define expr* '())
        (define (add var)
            (match params
                [() 
                    (set! expr* (cons (list 'set! (index->frame-var index) var) expr*)) 
                    (set! loc* (append-back loc* (index->frame-var index)))
                    (set! index (+ index 1))]
                [(,reg ,reg* ...) 
                    (set! expr* (append-back expr* (list 'set! reg var))) 
                    (set! loc* (append-back loc* reg))
                    (set! params reg*)
                ]
            )
        )
        (seqmap add var*)
        (values loc* expr*)
    )

    (define (impose-tail rp tail)
        (match tail
            [,tri (guard (triv? tri)) 
                (make-begin (list
                    (list 'set! return-value-register tri)
                    (list rp frame-pointer-register return-value-register)
                ))
            ]
            [(,binop ,tri1 ,tri2) (guard (binop? binop) (triv? tri1) (triv? tri2))
                (make-begin (list
                    (list 'set! return-value-register (list binop tri1 tri2))
                    (list rp frame-pointer-register return-value-register)
                ))
            ]
            [(,tri ,var* ...) (guard (triv? tri))
                (receive (loc* expr*) (impose-proc var*) 
                    (make-begin (append 
                        (append-back expr* (list 'set! return-address-register rp))
                        (list (cons tri (cons frame-pointer-register (cons return-address-register loc*))))
                    )))]
            [(if ,pred ,tail1 ,tail2) (list 'if pred (impose-tail rp tail1) (impose-tail rp tail2))]
            [(begin ,eff* ... ,tail1) (cons 'begin (append eff* (list (impose-tail rp tail1))))]
        )
    
    )

    (define (impose-vars var*)
        (define index 0)
        (define params parameter-registers)
        (define ret '())
        (define (add var)
            (match params
                [() (set! ret (append-back ret (list 'set! var (index->frame-var index)))) (set! index (+ index 1))]
                [(,reg ,reg* ...) (set! ret  (append-back ret (list 'set! var reg))) (set! params reg*)]
            )
        )
        (seqmap add var*)
        ret
    )

    (define (impose-body var* body)
        (define rp (unique-name 'rp))
        (match body
            [(locals (,uv* ...) ,tail) 
                (list 'locals (cons rp (append uv* var*)) (cons 'begin (append 
                    (list (list 'set! rp return-address-register))
                    (impose-vars var*)
                    (list (impose-tail rp tail))
                )))]
        )
    )

    (define (impose-bind bind*)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda (,var* ...) ,body)) (list lab (cons 'lambda  (cons '() (list (impose-body var* body)))))]
            )
        ) bind*)
    )

    (match program
        [(letrec (,bind* ...) ,body) (list 'letrec (impose-bind bind*) (impose-body '() body))]
    )

)