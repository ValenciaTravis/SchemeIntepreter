(define (discard-call-live program)

    (define (loc? loc) (or (register? loc) (frame-var? loc)))
    (define (var? var) (or (loc? var) (uvar? var)))
    (define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))

    ; (define (discard-tail tail)
    ;     (match tail
    ;         [(,tri ,temp* ...) (guard (triv? tri)) (list tri)]
    ;         [(if ,pred ,tail1 ,tail2) (list 'if pred (discard-tail tail1) (discard-tail tail2))]
    ;         [(begin ,eff* ... ,tail) (cons 'begin (append eff* (list (discard-tail tail))))]
    ;     )
    ; )

    (define (discard elem)
        (match elem
            [(,tri ,temp* ...) (guard (triv? tri)) (list tri)]
            [,ls (guard (list? ls)) (seqmap discard ls)]
            [,temp temp]
        )
    )

    (define (discard-body body)
        (match body
            [(locate (,uv* ...) ,tail) 
                (list 'locate uv* (discard tail))
            ]
        )
    )

    (define (discard-bind bind)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda () ,body)) (list lab (cons 'lambda (cons '() (list (discard-body body)))))]
            )
        ) bind)
    )

    (match program
        [(letrec (,bind* ...) ,body) 
            (list 'letrec (discard-bind bind*) (discard-body body))])
)