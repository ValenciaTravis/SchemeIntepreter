(define (flatten-program program)

    (define (loc? loc) (or (register? loc) (disp-opnd? loc)))
    (define (var? var) (or (loc? var) (uvar? var)))
    (define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))
    (define (relop? relop) (or (eq? '< relop) (eq? '> relop) (eq? '= relop) (eq? '>= relop) (eq? '<= relop)))


    (define (flat-tail tail)
        (match tail
            [(,tri) (guard (triv? tri)) (list (list 'jump tri))]
            ; [(if (,relop ,tri1 ,tri2) (,lab1) (,lab2))
            ;     (list (list 'if (list relop tri1 tri2) (list lab1) (list lab2)))
            ; ]
            [(begin ,expr* ... ,tail1) (append expr* (flat-tail tail1))]
            [,if (list if)]))

    (define (flat-bind bind)
        (match bind
            [() '()]
            [((,lab (lambda () ,tail)) . ,bind1) 
                (append (cons lab (flat-tail tail)) (flat-bind bind1))]))
    
    (define (delete-jump program)
        (define i 0)
        (define (loop program)
            (if (null? program) '()
                (let ([res (loop (cdr program))])
                    (match (car program)
                        [(if (,relop ,tri1 ,tri2) (,lab1) (,lab2)) 
                            (guard (equal? (extract-suffix lab1) i))
                            (cons (list 'if (list 'not (list relop tri1 tri2)) (list 'jump lab2)) res)
                        ]
                        [(if (,relop ,tri1 ,tri2) (,lab1) (,lab2)) 
                            (guard (equal? (extract-suffix lab2) i))
                            (cons (list 'if (list relop tri1 tri2) (list 'jump lab1)) res)
                        ]
                        [(if (,relop ,tri1 ,tri2) (,lab1) (,lab2)) 
                            (cons (list 'if (list relop tri1 tri2) (list 'jump lab1)) (cons (list 'jump lab2) res))
                        ]
                        [,lab (guard (label? lab))  (set! i (extract-suffix lab)) (cons lab res)]
                        [(jump ,lab) 
                            (guard (label? lab) (equal? (extract-suffix lab) i)) 
                            res
                        ]
                        [,tmp (cons tmp res)]
                    )
                )
            )
        )
        (loop program)
        ; program
    )

    (match program
        [(letrec (,bind* ...) ,tail) 
            (cons 'code (delete-jump (append (flat-tail tail) (flat-bind bind*))))]))
