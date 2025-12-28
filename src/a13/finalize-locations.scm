(define (finalize-locations program)

    (define (loc? loc) (or (register? loc) (frame-var? loc)))

    (define (replace-uvar uv bind) 
        (if (eq? uv (caar bind)) (cadar bind) (replace-uvar uv (cdr bind)))
    )
    (define (check-all all)
        (match all
            [(set! ,fv1 ,fv2) (guard (eq? fv1 fv2) (loc? fv1) (loc? fv2))
                (list 'nop)
            ]
            [,ls (guard (list? ls)) (map check-all ls)]
            [,tmp tmp]
        )
    )
    (define (replace-all all bind)
        (define (replace all) 
            (match all
                [,ls (guard (list? ls)) (map replace ls)]
                [,uv (guard (uvar? uv)) (replace-uvar uv bind)]
                [,tmp tmp]
            )
        )   
        (check-all (replace all))
    )
    (define (replace-body body)
        (match body
            [(locate (,bind* ...) ,tail) (replace-all tail bind*)]
        )
    )
    (define (replace-letrec-bind bind)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda () ,body)) (list lab (cons 'lambda  (cons '() (list (replace-body body)))))]
            )
        ) bind)
    )
    (match program
        [(letrec (,bind* ...) ,body) (list 'letrec (replace-letrec-bind bind*) (replace-body body))]
    )
)