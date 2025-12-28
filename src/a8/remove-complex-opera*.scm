(load "src/utils.scm")

(define (remove-complex-opera* program)

    (define (triv? var) (or (uvar? var) (int64? var) (label? var)))
    (define (binop? binop) (or (eq? binop '+) (eq? binop '-) (eq? binop '*) (eq? binop 'logand) (eq? binop 'logor) (eq? binop 'sra) (eq? binop 'mref)))

    (define (remove-values val*)
        (define uvs '())
        (define add '())
        (define rval* '())
        (define (remove val)
            (match val
                [,tri (guard (triv? tri)) (set! rval* (append-back rval* tri))]
                [,temp 
                    (define uv (unique-name 't))
                    (receive (uvs1 reff) (remove-eff (list 'set! uv val))
                    (set! uvs (cons uv (append uvs1 uvs)))
                    (set! add (append-back add reff))
                    (set! rval* (append-back rval* uv)))
                ]
            )
        )
        (seqmap remove val*)
        ; (emit "remove-values")
        ; (emit rval*) 
        (values uvs add rval*)
    )

    (define (remove-eff eff)
        ; (emit eff)
        (match eff
            [(nop) (values '() (list 'nop))]
            [(set! ,var (if ,pred1 ,tri1 ,tri2)) 
                (receive (uvs3 rpred1) (remove-pred pred1)
                (receive (uvs1 reff1) (remove-eff (list 'set! var tri1))
                (receive (uvs2 reff2) (remove-eff (list 'set! var tri2))
                (values
                    (append uvs1 uvs2 uvs3)
                    (list 'if rpred1 reff1 reff2)
                ))))]
            [(set! ,var (begin ,eff* ... ,tri))
                (receive (uvs1 reff*) (remove-effs eff*)
                (receive (uvs2 reff) (remove-eff (list 'set! var tri))
                (values
                    (append uvs1 uvs2)
                    (make-begin (append-back reff* reff))
                )))]         
            [(set! ,var ,tri) (guard (triv? tri)) (values '() (list 'set! var tri))]
            [(set! ,var (alloc ,tri)) (guard (triv? tri))
                (values '() (list 'set! var (list 'alloc tri)))]
            [(set! ,var (alloc ,tri)) 
                (define uv (unique-name 't))
                (receive (uvs reff) (remove-eff (list 'set! uv tri))
                (values 
                    (cons uv uvs)
                    (make-begin (list reff (list 'set! var (list 'alloc uv))))
                ))]
            [(set! ,var (,binop ,tri1 ,tri2))  (guard (binop? binop) (triv? tri1) (triv? tri2)) 
                (values '() (list 'set! var (list binop tri1 tri2)))]
            [(set! ,var (,binop ,tri1 ,tri2)) (guard (binop? binop) (triv? tri1)) 
                (define uv (unique-name 't))
                (receive (uvs reff) (remove-eff (list 'set! uv tri2))
                ; (emit 1145141919)
                ; (emit reff) 
                (values
                    (cons uv uvs)
                    (make-begin (list
                        reff
                        (list 'set! var (list binop tri1 uv))
                    ))
                ))]
            [(set! ,var (,binop ,tri1 ,tri2)) (guard (binop? binop) (triv? tri2)) 
                (define uv (unique-name 't))
                (receive (uvs reff) (remove-eff (list 'set! uv tri1))
                (values
                    (cons uv uvs)
                    (make-begin (list
                        reff
                        (list 'set! var (list binop uv tri2))
                    ))
                ))]
            [(set! ,var (,binop ,tri1 ,tri2)) (guard (binop? binop)) 
                (define uv1 (unique-name 't))
                (define uv2 (unique-name 't))
                (receive (uvs1 reff1) (remove-eff (list 'set! uv1 tri1))
                (receive (uvs2 reff2) (remove-eff (list 'set! uv2 tri2))
                (values
                    (cons uv1 (cons uv2 (append uvs1 uvs2)))
                    (make-begin (list
                        reff1
                        reff2
                        (list 'set! var (list binop uv1 uv2))
                    ))
                )))]
            [(set! ,var (,val* ...)) 
                (receive (uvs add rval*) (remove-values val*)
                (values
                    uvs 
                    (make-begin (append-back add (list 'set! var rval*)))
                ))]
            [(mset! ,val1 ,val2 ,val3)
                (define uv1 val1) (define uv2 val2) (define uv3 val3)
                (define uvs '())
                (define reff '())
                (unless (triv? val1)
                    (set! uv1 (unique-name 't))
                    (receive (uvs1 reff1) (remove-eff (list 'set! uv1 val1))
                    (set! uvs (append uvs1 (cons uv1 uvs)))
                    (set! reff (append-back reff reff1))))
                (unless (triv? val2)
                    (set! uv2 (unique-name 't))
                    (receive (uvs2 reff2) (remove-eff (list 'set! uv2 val2))
                    (set! uvs (append uvs2 (cons uv2 uvs)))
                    (set! reff (append-back reff reff2))))
                (unless (triv? val3)
                    (set! uv3 (unique-name 't))
                    (receive (uvs3 reff3) (remove-eff (list 'set! uv3 val3))
                    (set! uvs (append uvs3 (cons uv3 uvs)))
                    (set! reff (append-back reff reff3))))
                (values uvs (make-begin (append-back reff (list 'mset! uv1 uv2 uv3))))
            ]
            [(if ,pred ,eff1 ,eff2) 
                (receive (uvs3 rpred) (remove-pred pred)
                (receive (uvs1 reff1) (remove-eff eff1)
                (receive (uvs2 reff2) (remove-eff eff2)
                (values 
                    (append uvs1 uvs2 uvs3)
                    (list 'if rpred reff1 reff2)
                ))))]
            [(begin ,eff* ...)
                (receive (uvs reff*) (remove-effs eff*)
                (values
                    uvs
                    (make-begin reff*)
                ))]
            [(,val* ...) 
                (receive (uvs add rval*) (remove-values val*)
                (values
                    uvs
                    (make-begin (append-back add rval*))
                ))]
        )
    
    )

    (define (remove-effs eff*)
        (match eff*
            [() (values '() '())]
            [(,eff ,eff1* ...)
                (receive (uvs1 reff*) (remove-effs eff1*)
                (receive (uvs2 reff) (remove-eff eff)
                (values
                    (append uvs1 uvs2)
                    (cons reff reff*)
                )))
            ]
        )
    )

    (define (remove-pred pred)
        (match pred
            [(true) (values '() (list 'true))]
            [(false) (values '() (list 'false))]
            [(if ,pred1 ,pred2 ,pred3) 
                (receive (uvs1 rpred1) (remove-pred pred1)
                (receive (uvs2 rpred2) (remove-pred pred2)
                (receive (uvs3 rpred3) (remove-pred pred3)
                (values (append uvs1 uvs2 uvs3) (list 'if rpred1 rpred2 rpred3)))))]
            [(begin ,eff* ... ,pred)
                (receive (uvs1 reff*) (remove-effs eff*)
                (receive (uvs2 rpred) (remove-pred pred)
                (values
                    (append uvs1 uvs2)
                    (make-begin (append-back reff* pred))
                )))]
            [(,relop ,val1 ,val2) (guard (triv? val1) (triv? val2)) (values '() (list relop val1 val2))]
            [(,relop ,val1 ,val2) (guard (triv? val1)) 
                (define uv (unique-name 't))
                (receive (uvs reff) (remove-eff (list 'set! uv val2))
                (values
                    (cons uv uvs)
                    (make-begin (list reff (list relop val1 uv)))
                ))]
            [(,relop ,val1 ,val2) (guard (triv? val2)) 
                (define uv (unique-name 't))
                (receive (uvs reff) (remove-eff (list 'set! uv val1))
                (values
                    (cons uv uvs)
                    (make-begin (list reff (list relop uv val2)))
                ))]
            [(,relop ,val1 ,val2) 
                (define uv1 (unique-name 't))
                (define uv2 (unique-name 't))
                (receive (uvs1 reff1) (remove-eff (list 'set! uv1 val1))
                (receive (uvs2 reff2) (remove-eff (list 'set! uv2 val2))
                (values
                    (cons uv1 (cons uv2 (append uvs1 uvs2)))
                    (make-begin (list reff1 reff2 (list relop uv1 uv2)))
                )))]   
        )
    )

    (define (remove-tail tail)
        (match tail
            [,tri (guard (triv? tri)) (values '() tri)]
            [(alloc ,val) (guard (triv? val)) (values '() (list 'alloc val))]
            [(alloc ,val) 
                (define uv (unique-name 't))
                (receive (uvs reff) (remove-eff (list 'set! uv val))
                (values 
                    (cons uv uvs)
                    (make-begin (list reff (list 'alloc uv)))
                ))
            ]
            [(,binop ,val1 ,val2) (guard (binop? binop) (triv? val1) (triv? val2))
                (values '() (list binop val1 val2))]
            [(,binop ,val1 ,val2) (guard (binop? binop) (triv? val2))
                (define uv1 (unique-name 't))
                (receive (uvs reff) (remove-eff (list 'set! uv1 val1))
                (values 
                    (cons uv1 uvs)
                    (make-begin (list reff (list binop uv1 val2)))
                ))]
            [(,binop ,val1 ,val2) (guard (binop? binop) (triv? val1))
                (define uv2 (unique-name 't))
                (receive (uvs reff) (remove-eff (list 'set! uv2 val2))
                (values 
                    (cons uv2 uvs)
                    (make-begin (list reff (list binop val1 uv2)))
                ))]
            [(,binop ,val1 ,val2) (guard (binop? binop))
                (define uv1 (unique-name 't))
                (define uv2 (unique-name 't))
                (receive (uvs1 reff1) (remove-eff (list 'set! uv1 val1))
                (receive (uvs2 reff2) (remove-eff (list 'set! uv2 val2))
                (values 
                    (cons uv1 (cons uv2 (append uvs1 uvs2)))
                    (make-begin (list reff1 reff2 (list binop uv1 uv2)))
                )))]
            [(if ,pred ,tail1 ,tail2) 
                (receive (uvs3 rpred) (remove-pred pred)
                (receive (uvs1 rtail1) (remove-tail tail1)
                (receive (uvs2 rtail2) (remove-tail tail2)
                (values
                    (append uvs1 uvs2 uvs3)
                    (list 'if rpred rtail1 rtail2)
                ))))]
            [(begin ,eff* ... ,tail1)
                (receive (uvs1 reff*) (remove-effs eff*)
                (receive (uvs2 rtail) (remove-tail tail1)
                (values
                    (append uvs1 uvs2)
                    (make-begin (append-back reff* rtail))
                )))]
            [(,val* ...) 
                (receive (uvs add rval*) (remove-values val*)
                (values
                    uvs
                    (make-begin (append-back add rval*))
                ))]
        )
    )

    (define (remove-body body)
        (match body
            [(locals (,uv* ...) ,tail) 
                (receive (uvs rtail) (remove-tail tail) (list 'locals (append uv* uvs) rtail))]
        )
    )

    (define (remove-bind bind*)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda (,var* ...) ,body)) (list lab (cons 'lambda  (cons var* (list (remove-body body)))))]
            )
        ) bind*)
    )

    (match program
        [(letrec (,bind* ...) ,body) (list 'letrec (remove-bind bind*) (remove-body body))]
    )

)