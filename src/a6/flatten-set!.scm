(load "src/utils.scm")

(define (flatten-set! program)

    (define (triv? var) (or (uvar? var) (int64? var) (label? var)))
    (define (binop? binop) (or (eq? binop '+) (eq? binop '-) (eq? binop '*) (eq? binop 'logand)(eq? binop 'logor) (eq? binop 'sra)))

    (define (flatten-eff eff)
        (match eff
            [(nop) (list 'nop)]
            [(set! ,uv ,tri) (guard (triv? tri)) (list 'set! uv tri)]
            [(set! ,uv (,binop ,tri1 ,tri2)) (guard (binop? binop) (triv? tri1) (triv? tri2))
                (list 'set! uv (list binop tri1 tri2))]
            [(set! ,uv (if ,pred ,val1 ,val2))
                (list 'if (flatten-pred pred) 
                    (flatten-eff (list 'set! uv val1))
                    (flatten-eff (list 'set! uv val2))
                )]
            [(set! ,uv (begin ,eff* ... ,val))
                (cons 'begin (append 
                    (seqmap flatten-eff eff*) 
                    (list (flatten-eff (list 'set! uv val)))
                ))
            ]
            [(if ,pred ,eff1 ,eff2) (list 'if (flatten-pred pred) (flatten-eff eff1) (flatten-eff eff2))]
            [(begin ,eff* ...) (cons 'begin (seqmap flatten-eff eff*))]
        )
    )

    (define (flatten-pred pred)
        (match pred
            [(true) (list 'true)]
            [(false) (list 'false)]
            [(,relop ,tri1 ,tri2) (guard (triv? tri1) (triv? tri2)) (list relop tri1 tri2)]
            [(if ,pred1 ,pred2 ,pred3) (list 'if (flatten-pred pred1) (flatten-pred pred2) (flatten-pred pred3))]
            [(begin ,eff* ... ,pred1) (cons 'begin (append (seqmap flatten-eff eff*) (list (flatten-pred pred1))))]
        )
    )

    (define (flatten-tail tail)
        (match tail
            [,tri (guard (triv? tri)) tri]
            [(,binop ,tri1 ,tri2) (guard (binop? binop) (triv? tri1) (triv? tri2))
                (list binop tri1 tri2)]
            [(if ,pred ,tail1 ,tail2) (list 'if (flatten-pred pred) (flatten-tail tail1) (flatten-tail tail2))]
            [(begin ,eff* ... ,tail1) (cons 'begin (append (seqmap flatten-eff eff*) (list (flatten-tail tail1))))]
            [(,tri* ...) tri*]
        )
    )

    (define (flatten-body body)
        (match body
            [(locals (,uv* ...) ,tail) 
                (list 'locals uv* (flatten-tail tail))]
        )
    )

    (define (flatten-bind bind*)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda (,var* ...) ,body)) (list lab (cons 'lambda  (cons var* (list (flatten-body body)))))]
            )
        ) bind*)
    )

    (match program
        [(letrec (,bind* ...) ,body) (list 'letrec (flatten-bind bind*) (flatten-body body))]
    )
)