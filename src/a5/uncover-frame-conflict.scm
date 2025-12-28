(load "src/receive.scm")

(define (uncover-frame-conflict program)

    (define (loc? loc) (or (register? loc) (frame-var? loc)))
    (define (var? var) (or (loc? var) (uvar? var)))
    (define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))


    (define (remove uv graph)
        (match graph
            [() '()]
            [(,vedges ,graph* ...) (guard (eq? uv (car vedges))) graph*]
            [(,vedges ,graph* ...) (cons vedges (remove uv graph*))]
        )
    )   

    (define (add-graph graph uv set)
        (define old (assq uv graph))
        (define graph1)
        ; (display uv) (newline)
        ; (display graph) (newline)
        (when (not old) (set! old (list uv)))
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
                (when (or (frame-var? var) (uvar? var)) 
                    ; (display var) (newline)
                    (set! set1 (difference set1 (list var))) 
                    (set! graph1 (add-graph graph1 var set1)))
                (when (or (frame-var? tri) (uvar? tri)) (set! set1 (set-cons tri set1)))
                ; (display (list 'graphaft: graph1)) (newline)
                (values graph1 set1))
            ]
            [((set! ,var (,binop ,tri1 ,tri2)) ,eff1* ... )
                (receive (graph1 set1) (make-effs-set eff1* graph set)
                
                ; (display (list 'set! var (list binop tri1 tri2) ': set1)) (newline)
                ; (display (list 'graphbef: graph1)) (newline)
                (when (or (frame-var? var) (uvar? var)) 
                    ; (display var) (newline)
                    (set! set1 (difference set1 (list var)))
                    (set! graph1 (add-graph graph1 var set1)))
                (when (or (frame-var? tri1) (uvar? tri1)) (set! set1 (set-cons tri1 set1)))
                (when (or (frame-var? tri2) (uvar? tri2)) (set! set1 (set-cons tri2 set1)))
                ; (display (list 'graphaft: graph1)) (newline)
                (values graph1 set1))
            ]
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
                (when (or (frame-var? tri1) (uvar? tri1)) (set! set3 (set-cons tri1 set3)))
                (when (or (frame-var? tri2) (uvar? tri2)) (set! set3 (set-cons tri2 set3)))
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
                (map (lambda (uv) (when (or (frame-var? uv) (uvar? uv)) 
                    (set! set1 (set-cons uv set1))
                )) loc*)
                (when (or (frame-var? tri) (uvar? tri)) (set! set1 (set-cons tri set1)))
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
        (define (remove-frame graph)
            (define graph1 '())
            (map (lambda (vedges) 
                (match vedges
                    [(,v ,edge* ...) (guard (frame-var? v)) #f]
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
        (remove-frame (add-reverse graph))
        ; graph
    )

    (define (uncover-body body)
        (match body
            [(locals (,uv* ...) ,tail) 
                (receive (graph set) (make-tail-set tail (map (lambda (x) (list x)) uv*) '())
                (list 'locals uv* (list 'frame-conflict (wrapper graph) tail)))
            ]
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
