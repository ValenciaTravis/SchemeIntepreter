(load "src/utils.scm")

(define (uncover-register-conflict program)

    (define (loc? loc) (or (register? loc) (frame-var? loc)))
    (define (var? var) (or (loc? var) (uvar? var)))
    (define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))


    (define (remove uv graph)
        (match graph
            [(,vedges ,graph* ...) (guard (eq? uv (car vedges))) graph*]
            [(,vedges ,graph* ...) (cons vedges (remove uv graph*))]
        )
    )   

    (define (add-graph graph uv set)
        (define old (assq uv graph))
        (define graph1)
        ; (emit "add-graph:")
        ; (emit uv)
        ; (emit graph)
        (set! graph1 (cons (cons uv (union (cdr old) set)) (remove uv graph)))
        ; (display graph1) (newline)
        ; graph1
        graph1
    )

    (define (merge-graph graph1 graph2)
        ; (display graph1) (newline)
        ; (display graph2) (newline)
        (map (lambda (vedges)
            (match vedges
                [(,v ,edges* ...) (set! graph1 (add-graph graph1 v edges*))]
            )
        ) graph2)
        ; (display graph1) (newline)
        graph1
    )

    (define (make-effs-set eff* graph set)
        (match eff*
            [() (values graph set)]
            [((nop) ,eff1* ...) (make-effs-set eff1* graph set)]
            [((if ,pred ,eff1 ,eff2) ,eff3* ...)
                (receive (graph3 set3) (make-effs-set eff3* graph set)
                (receive (graph1 set1) (make-effs-set (list eff1) graph3 set3)
                (receive (graph2 set2) (make-effs-set (list eff2) graph3 set3)
                (make-pred-set pred graph1 set1 graph2 set2))))
            ]
            [((begin ,eff1* ...) ,eff2* ...) 
                (receive (graph1 set1) (make-effs-set eff2* graph set)
                (make-effs-set eff1* graph1 set1))
            ]
            [((set! ,var ,tri) ,eff1* ...) 
                (guard (triv? tri))
                (receive (graph1 set1) (make-effs-set eff1* graph set)
                ; (display (list 'set! var tri ': set1)) (newline)
                ; (display (list 'graphbef: graph1)) (newline)
                (when (or (register? var) (uvar? var)) 
                    ; (display var) (newline)
                    ; (display set1) (newline)
                    (set! set1 (difference set1 (list var))) 
                    (set! graph1 (add-graph graph1 var set1)))
                (when (or (register? tri) (uvar? tri)) (set! set1 (set-cons tri set1)))
                ; (display (list 'graphaft: graph1)) (newline)
                (values graph1 set1))
            ]
            [((set! ,var (,binop ,tri1 ,tri2)) ,eff1* ... )
                (receive (graph1 set1) (make-effs-set eff1* graph set)
                
                ; (display (list 'set! var (list binop tri1 tri2) ': set1)) (newline)
                ; (display (list 'graphbef: graph1)) (newline)
                (when (or (register? var) (uvar? var)) 
                    ; (display var) (newline)
                    (set! set1 (difference set1 (list var)))
                    (set! graph1 (add-graph graph1 var set1)))
                (when (or (register? tri1) (uvar? tri1)) (set! set1 (set-cons tri1 set1)))
                (when (or (register? tri2) (uvar? tri2)) (set! set1 (set-cons tri2 set1)))
                ; (display (list 'graphaft: graph1)) (newline)
                (values graph1 set1))
            ]
            [((mset! ,tri1 ,tri2 ,tri3), eff1* ...)
                (receive (graph1 set1) (make-effs-set eff1* graph set)
                    (when (or (register? tri1) (uvar? tri1)) (set! set1 (set-cons tri1 set1)))
                    (when (or (register? tri2) (uvar? tri2)) (set! set1 (set-cons tri2 set1)))
                    (when (or (register? tri3) (uvar? tri3)) (set! set1 (set-cons tri3 set1)))
                    ; (display (list 'graphaft: graph1)) (newline)
                    (values graph1 set1))
                ]
            [((return-point ,lab ,tail) ,eff1* ...) 
                (receive (graph1 set1) (make-effs-set eff1* graph set)
                    (make-tail-set tail graph1 set1))]
        )
    )

    (define (make-pred-set pred graph1 set1 graph2 set2)
        (match pred
            [(true) (values graph1 set1)]
            [(false) (values graph2 set2)]
            [(,relop ,tri1 ,tri2)
                (guard (relop? relop) (triv? tri1) (triv? tri2))
                (define set3 (union set1 set2))
                (define graph3 (merge-graph graph1 graph2))
                (when (or (register? tri1) (uvar? tri1)) (set! set3 (set-cons tri1 set3)))
                (when (or (register? tri2) (uvar? tri2)) (set! set3 (set-cons tri2 set3)))
                (values graph3 set3) 
            ]
            [(if ,pred1 ,pred2 ,pred3)
                (receive (graph3 set3) (make-pred-set pred2 graph1 set1 graph2 set2)
                (receive (graph4 set4) (make-pred-set pred3 graph1 set1 graph2 set2)
                (make-pred-set pred1 graph3 set3 graph4 set4)))
            ]
            [(begin ,eff* ... ,pred1)
                (receive (graph3 set3) (make-pred-set pred1 graph1 set1 graph2 set2)
                (make-effs-set eff* graph3 set3))
            ]
        )
    )

    (define (make-tail-set tail graph set)
        ; (display graph) (newline)
        (match tail
            [(,tri ,loc* ...) (guard (triv? tri)) 
                (define set1 set)
                (map (lambda (uv) (when (or (register? uv) (uvar? uv)) 
                    (set! set1 (set-cons uv set1))
                )) loc*)
                (when (or (register? tri) (uvar? tri)) (set! set1 (set-cons tri set1)))
                (values graph set1)
            ]
            [(if ,pred ,tail1 ,tail2)
                (receive (graph1 set1) (make-tail-set tail1 graph set)
                (receive (graph2 set2) (make-tail-set tail2 graph set)
                (make-pred-set pred graph1 set1 graph2 set2)))
            ]
            [(begin ,eff* ... ,tail)
                (receive (graph1 set1) (make-tail-set tail graph set)
                (make-effs-set eff* graph1 set1))
            ]
        )
    )

    (define (wrapper graph)
        (define (remove-reg graph)
            (define graph1 '())
            (map (lambda (vedges) 
                (match vedges
                    [(,v ,edge* ...) (guard (register? v)) #f]
                    [,temp (set! graph1 (cons temp graph1))]
                )
            ) graph)
            graph1
        )
        (define (add-reverse graph)
            (define graph1 graph)
            (map (lambda (vedges)
                (match vedges
                    [(,v ,edges* ...) (map (lambda (u) 
                        (set! graph1 (add-graph graph1 u (list v)))) edges*)]
                )
            ) graph)
            graph1
        )
        (remove-reg (add-reverse graph))
        ; graph
    )

    (define (uncover-body body)
        (match body
            [(locals (,uv1* ...) (ulocals (,uv2* ...) (locate (,uf* ...) (frame-conflict ,graph* ,tail)))) 
                (receive (graph set) (make-tail-set tail (map (lambda (x) (list x)) (append uv1* uv2* registers)) '())
                    (list 'locals uv1*  
                    (list 'ulocals uv2*
                    (list 'locate uf*
                    (list 'frame-conflict graph*
                    (list 'register-conflict (wrapper graph) tail)
                )))))]
            [(locate (,uv* ...) ,tail) (list 'locate uv* tail)]
        )
    )

    (define (uncover-bind bind)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda () ,body)) (list lab (cons 'lambda (cons '() (list (uncover-body body)))))]
            )
        ) bind)
    )

    (match program
        [(letrec (,bind* ...) ,body)
            (list 'letrec (uncover-bind bind*) (uncover-body body))])
)
