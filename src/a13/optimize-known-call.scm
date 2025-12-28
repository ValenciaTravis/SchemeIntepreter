(define (optimize-known-call program)

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

    (define (optimizes expr* bind)
        (define (loop expr) (optimize expr bind))
        (seqmap loop expr*)
    )

    (define (optimize elem bind)
        (match elem
            [(closures (,clos* ...) ,expr) 
                (define bind1 (append (seqmap (lambda (bind) (list (car bind) (cadr bind))) clos*) bind))
                (list 'closures clos* (optimize expr bind1))]
            [(,uv ,expr* ...) (guard (uvar? uv)) (cons (replace-uvar uv bind) (optimizes expr* bind))]
            [,ls (guard (list? ls)) (optimizes ls bind)]
            [,temp temp]
        )
    )

    (optimize program '())

)