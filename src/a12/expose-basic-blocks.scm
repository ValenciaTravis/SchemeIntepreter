(define (expose-basic-blocks program)
    (define new-bind '())
    (define (make-bind lab extail) 
       (set! new-bind (cons (list lab (list 'lambda '() extail)) new-bind)))
    (define (expose-effs eff* extail)
        (define (loop eff*)
            (match eff*
                [() #f]
                [((nop) ,eff1* ...) (loop eff1*)]
                [((if ,pred ,eff1 ,eff2) ,eff3* ...) 
                    (define lab (unique-label 'extail))
                    (define lab1 (unique-label 'effT))
                    (define lab2 (unique-label 'effF))
                    (loop eff3*)
                    (make-bind lab (make-begin extail))
                    (make-bind lab2 (expose-effs (list eff2) (list (list lab))))
                    (make-bind lab1 (expose-effs (list eff1) (list (list lab))))
                    (set! extail (list (expose-pred pred lab1 lab2)))
                ]
                [((begin ,eff1* ...) ,eff2* ...) (loop eff2*) 
                    (set! extail (list (expose-effs eff1* extail)))
                ]
                [((set! ,var ,tri) ,eff1* ...) (loop eff1*) (set! extail (cons (list 'set! var tri) extail))]
                [((mset! ,tri1 ,tri2 ,tri3) ,eff1* ...) (loop eff1*) (set! extail (cons (list 'mset! tri1 tri2 tri3) extail))]
                [((return-point ,lab ,tail), eff1* ...)
                    (loop eff1*)
                    (make-bind lab (make-begin extail))
                    (set! extail (list (expose-tail tail)))
                ]
            )
        )
        (loop eff*)
        (make-begin extail)
    )
    ; (define (expose-eff eff extail)
    ;     (match eff
    ;         [(begin ,eff* ...) (expose-effs eff* extail)]
    ;         [(if ,pred1 ,eff1 ,eff2)
    ;             (define lab1 (unique-label 'effT))
    ;             (define lab2 (unique-label 'effF))
    ;             (make-bind lab2 (expose-eff eff2 extail))
    ;             (make-bind lab1 (expose-eff eff1 extail))
    ;             (expose-pred pred1 lab1 lab2)
    ;         ]
    ;         [(nop) extail]
    ;         [,set (cons eff extail)]
    ;     )
    ; )
    (define (expose-pred pred lab1 lab2)
        (match pred
            [(true) (list lab1)]
            [(false) (list lab2)]
            [(,relop ,tri1 ,tri2) (guard (relop? relop) (triv? tri1) (triv? tri2))
                (list 'if (list relop tri1 tri2) (list lab1) (list lab2))
            ]
            [(begin ,eff* ... ,pred1) 
                (define extail (expose-pred pred1 lab1 lab2))
                (expose-effs eff* (list extail))   
            ]
            [(if ,pred1 ,pred2 ,pred3)
                (define lab3 (unique-label 'predT))
                (define lab4 (unique-label 'predF))
                (make-bind lab4 (expose-pred pred3 lab1 lab2))
                (make-bind lab3 (expose-pred pred2 lab1 lab2))
                (expose-pred pred1 lab3 lab4)
            ]
        )
    )
    (define (expose-tail tail)
        (match tail
            [(if ,pred ,tail1 ,tail2) 
                (define lab1 (unique-label 'tailT))
                (define lab2 (unique-label 'tailF))
                (make-bind lab2 (expose-tail tail2))
                (make-bind lab1 (expose-tail tail1))
                (expose-pred pred lab1 lab2)
            ]
            [(begin ,eff* ... ,tail) 
                (define extail (expose-tail tail))
                (expose-effs eff* (list extail))
            ]
            [,op tail]
        )
    )
    (define (expose-bind bind)
        (match bind
            [(,lab (lambda () ,tail)) (make-bind lab (expose-tail tail))]
        )
    )

    (match program
        [(letrec (,bind* ...) ,tail) 
            (define extail (expose-tail tail))
            (map expose-bind bind*) 
            (list 'letrec new-bind extail)]
    )

)