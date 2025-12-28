(define (purify-letrec program)

    (define res )
    
    (define (purify-letrec binds assigns elem)
        (define sim '())
        (define lam '())
        (define com '())
        (define compuv '())
        (define (purify-bind bind)
            ; (display bind) (newline)
            (match bind
                [(,uv ,temp) (guard (exists? uv assigns)) 
                    (define tmp (unique-name 'tmp))
                      
                    (set! compuv (append-back compuv (list uv tmp)))
                    (set! com (append-back com (list tmp (purify temp))))
                ]
                [(,uv (quote ,imm)) (set! sim (append-back sim (list uv (list 'quote imm))))]
                [(,uv (lambda ,uvs (assigns ,assigns1 ,body))) 
                    (set! lam (append-back lam (list 'lambda uvs (list 'assigns assigns1 (purify body)))))]
                [(,uv ,temp) 
                    (define tmp (unique-name 'tmp))
                    (set! compuv (append-back compuv (list uv tmp)))
                    (set! com (append-back com (list tmp (purify temp))))]
            )
        )
        (define (make-voids elem) (match elem [(,uv ,temp) (list uv (list 'void))]))
        (define (make-sets elem) (match elem [(,uv ,temp) (list 'set! uv temp)]))
        
        (seqmap purify-bind binds)

        (list 'let sim (list 'assigned '()
            (list 'let (seqmap make-voids compuv) (list 'assigned (seqmap car compuv)
                (list 'letrec lam
                    (make-begin (list 
                    (list 'let com (list 'assigned '()
                        (if (null? compuv) (list 'void) (make-begin (seqmap make-sets compuv)))
                    ))
                    (purify elem)))
                )
            ))
        ))
    )

    (define (purify elem)
        
        (match elem
            [(letrec ,binds (assigned ,assigns ,elem1)) (purify-letrec binds assigns elem1)]
            [,ls (guard (list? ls)) (seqmap purify ls)]
            [,temp temp]
        )
    )

    (define (clean-lets elem)
        (match elem
            [(let () (assigned () ,elem1)) (clean-lets elem1)]
            [(letrec () ,elem1) (clean-lets elem1)]
            [,ls (guard (list? ls)) (seqmap clean-lets ls)]
            [,temp temp]
        )
    )

    (clean-lets (purify program))
)