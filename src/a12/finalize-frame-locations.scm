(define (finalize-frame-locations program)
    (define (replace-uvar uv bind) 
        (cond 
            ((null? bind) uv)
            ((eq? uv (caar bind)) (cadar bind)) 
            (else (replace-uvar uv (cdr bind))))
    )
    (define (check-all all)
        (match all
            [(set! ,fv1 ,fv2) (guard (eq? fv1 fv2) (frame-var? fv1) (frame-var? fv2))
                (list 'nop)
            ]
            [,ls (guard (list? ls)) (map check-all ls)]
            [,tmp tmp]
        )
    )
    (define (replace-all all bind)
        (define (replace all) 
            (match all
                [,ls (guard (list? ls)) (seqmap replace ls)]
                [,uv (guard (uvar? uv)) (replace-uvar uv bind)]
                [,tmp tmp]
            )
        )   
        (check-all (replace all))
    )
    (define (replace-body body)
        (match body
            [(locals ,uv1* (ulocals ,uv2* (locate ,bind* (frame-conflict ,graph ,tail))))
                (list 'locals uv1*
                (list 'ulocals uv2*
                (list 'locate '()
                (list 'frame-conflict graph (replace-all tail bind*)))))
            ]
            [(locate (,bind* ...) ,tail) (list 'locate bind* tail)]
        )
    )
    (define (replace-bind bind)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda () ,body)) (list lab (cons 'lambda  (cons '() (list (replace-body body)))))]
            )
        ) bind)
    )
    (match program
        [(letrec (,bind* ...) ,body) (list 'letrec (replace-bind bind*) (replace-body body))]
    )
)