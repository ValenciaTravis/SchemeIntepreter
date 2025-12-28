(define (finalize-locations program)
    (define (replace-uvar uv bind) 
        (if (eq? uv (caar bind)) (cadar bind) (replace-uvar uv (cdr bind)))
    )
    (define (replace-all all bind)
        (define (replace all) 
            (match all
                [,ls (guard (list? ls)) (map replace ls)]
                [,uv (guard (uvar? uv)) (replace-uvar uv bind)]
                [,tmp tmp]
            )
        )   
        (replace all)
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