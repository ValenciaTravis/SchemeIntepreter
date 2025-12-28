(load "src/utils.scm")

(define (impose-calling-conventions program)

    (define (triv? var) (or (uvar? var) (int64? var) (label? var)))
    (define (binop? binop) (or (eq? binop '+) (eq? binop '-) (eq? binop '*) (eq? binop 'logand) (eq? binop 'logor) (eq? binop 'sra) (eq? binop 'mref)))

    (define (impose-body var* body)
        (define rp (unique-name 'rp))
        (define nfs '())

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

        (define (impose-nontail tri var*)
            (define rp-label (unique-label 'rplabel))
            (define nfvs '())
            (define params parameter-registers)
            (define loc* '())
            (define expr* '())
            (define (add var)
                (match params
                    [() 
                        (define nfv (unique-name 'nfv))
                        (set! expr* (cons (list 'set! nfv var) expr*)) 
                        (set! loc* (append-back loc* nfv))
                        (set! nfvs (append-back nfvs nfv))]
                    [(,reg ,reg* ...) 
                        (set! expr* (append-back expr* (list 'set! reg var))) 
                        (set! loc* (append-back loc* reg))
                        (set! params reg*)]
                )
            )
            (seqmap add var*)
            (set! nfs (cons nfvs nfs))
            (list 'return-point rp-label (make-begin (append-back (append-back expr* (list 'set! return-address-register rp-label)) 
                (append (list tri frame-pointer-register return-address-register allocation-pointer-register) loc*)
            )))
        )

        (define (impose-eff eff)
            (match eff
                [(nop) (list 'nop)]
                [(set! ,var ,tri) (guard (triv? tri)) (list 'set! var tri)]
                [(set! ,var (alloc ,tri)) (list 'set! var (list 'alloc tri))]
                [(set! ,var (,binop ,tri1 ,tri2)) (guard (binop? binop) (triv? tri1) (triv? tri2))
                    (list 'set! var (list binop tri1 tri2))]
                [(set! ,var (,tri ,tri* ...)) (make-begin (cons (impose-nontail tri tri*) (list (list 'set! var return-value-register))))]
                [(mset! ,tri1 ,tri2 ,tri3) (list 'mset! tri1 tri2 tri3)]
                [(if ,pred1 ,eff1 ,eff2) (list 'if (impose-pred pred1) (impose-eff eff1) (impose-eff eff2))]
                [(begin ,eff* ...) (make-begin (seqmap impose-eff eff*))]
                [(,tri ,tri* ...) (impose-nontail tri tri*)]
            )
        )

        (define (impose-pred pred)
            (match pred
                [(true) (list 'true)]
                [(false) (list 'false)]
                [(,relop ,tri1 ,tri2) (guard (triv? tri1) (triv? tri2)) (list relop tri1 tri2)]
                [(if ,pred1 ,pred2 ,pred3) (list 'if (impose-pred pred1) (impose-pred pred2) (impose-pred pred3))]
                [(begin ,eff* ... ,pred1) (make-begin (append-back (seqmap impose-eff eff*) (impose-pred pred1)))]
            )
        )

        (define (impose-tail rp tail)
            (match tail
                [,tri (guard (triv? tri)) 
                    (make-begin (list
                        (list 'set! return-value-register tri)
                        (list rp frame-pointer-register return-value-register allocation-pointer-register)
                    ))
                ]
                [(alloc ,tri1) 
                    (make-begin (list
                        (list 'set! return-value-register (list 'alloc tri1))
                        (list rp frame-pointer-register return-value-register allocation-pointer-register)
                    ))
                ]
                [(,binop ,tri1 ,tri2) (guard (binop? binop) (triv? tri1) (triv? tri2))
                    (make-begin (list
                        (list 'set! return-value-register (list binop tri1 tri2))
                        (list rp frame-pointer-register return-value-register allocation-pointer-register)
                    ))
                ]
                [(,tri ,var* ...) (guard (triv? tri))
                    (receive (loc* expr*) (impose-proc var*) 
                        (make-begin (append 
                            (append-back expr* (list 'set! return-address-register rp))
                            (list (cons tri (cons frame-pointer-register (cons return-address-register (cons allocation-pointer-register loc*)))))
                        )))]
                [(if ,pred ,tail1 ,tail2) (list 'if (impose-pred pred) (impose-tail rp tail1) (impose-tail rp tail2))]
                [(begin ,eff* ... ,tail1) (make-begin (append (seqmap impose-eff eff*) (list (impose-tail rp tail1))))]
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

    
        (match body
            [(locals (,uv* ...) ,tail) 
                (define itail (impose-tail rp tail))
                (define ivars (impose-vars var*))
                (list 'locals (cons rp (append uv* var*)) (list 'new-frames nfs (cons 'begin (append 
                    (list (list 'set! rp return-address-register))
                    ivars
                    (list itail)
                ))))]
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