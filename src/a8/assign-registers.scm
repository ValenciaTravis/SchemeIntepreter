(define (assign-registers program)
    (define (loc? loc) (or (register? loc) (frame-var? loc)))
    (define (var? var) (or (loc? var) (uvar? var)))
    (define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))

    (define (exists? elem lst)
        (cond
            ((null? lst) #f) ; 如果列表为空，返回 #f
            ((equal? elem (car lst)) #t) ; 如果找到元素，返回 #t
            (else (exists? elem (cdr lst))))) ; 递归检查剩余列表

    (define (find-min-deg uvs graph)
        (define uv (car uvs))
        (define len (length (assq uv graph)))
        (map (lambda (uv1)
            (define len1 (length (assq uv1 graph)))
            (when (< len1 len) (set! uv uv1) (set! len len1))
        ) (cdr uvs))
        ; (display (map length graph)) (newline)
        ; (display len) (newline)
        uv
    )

    (define (remove1 graph uv)
        ; (display (list 'graph: graph)) (newline)
        (match graph
            [(,vedges ,graph* ...) (guard (eq? uv (car vedges))) graph*]
            [(,vedges ,graph* ...) (cons vedges (remove1 graph* uv))]
        )
    )   
    (define (remove2 ls uv)
        ; (display (list 'remove2 ls uv)) (newline)
        (match ls
            [() '()]
            [(,uv1 ,uv* ...) (guard (eq? uv1 uv)) uv*]
            [(,uv1 ,uv* ...) (cons uv1 (remove2 uv* uv))]
        )
    )

    (define (remove-uv graph uv)
        (define graph1 (remove1 graph uv))
        ; (display graph1) (newline)
        (map (lambda (ls) (remove2 ls uv)) graph1)
    )

    (define (remove-all-uv graph uvs) 
        (define graph1 graph)
        (map (lambda (uv) (set! graph (remove-uv graph uv))) uvs)
    )

    (define (find-reg spills assigns edges)
        (define regs registers) 
        (define (remove uv)
            (cond
                ((register? uv) (set! regs (remove2 regs uv)))
                ((assq uv assigns) (set! regs (remove2 regs (cadr (assq uv assigns)))))
                (else #f)
            )
        )
        ; (display (list 'spills: spills)) (newline)
        ; (display (list 'assigns: assigns)) (newline)
        ; (display (list 'edges: edges)) (newline)
        (map remove edges)
        ; (display regs)
        (if (null? regs) #f (car regs))
    )

    (define (assign uvs graph spills assigns)
        (match uvs
            [() (values spills assigns)]
            [,temp
                (define uv (find-min-deg uvs graph))
                (define graph1 (remove-uv graph uv))
                (define reg)
                (receive (spills1 assigns1) (assign (remove2 uvs uv) graph1 spills assigns)
                    (set! reg (find-reg spills1 assigns1 (cdr (assq uv graph))))
                    (if reg
                        (values spills1 (cons (list uv reg) assigns1))
                        (values (cons uv spills1) assigns1)
                    )
                )
            ]
        )
    )

    (define (assign-graph unspills uvs graph)
        ; (display graph) (newline)
        (receive (spills assigns) (assign unspills graph '() '())
            (assign uvs graph spills assigns)
        )
    )

    (define (assign-body body)
        (match body
            [(locals (,uv1* ...) 
                (ulocals (,uv2* ...) 
                (locate (,uf* ...) 
                (frame-conflict ,graph1
                (register-conflict ,graph ,tail))))) 
                (receive (spills assigns) (assign-graph uv2* uv1* graph)
                (if (null? spills)
                    (list 'locate assigns tail)
                    (list 'locals (difference uv1* spills)
                        (list 'ulocals uv2*
                        (list 'spills spills
                        (list 'locate uf*
                        (list 'frame-conflict graph1 tail)))))))
            ]
            [(locate (,uv* ...) ,tail) (list 'locate uv* tail)]
        )
    )

    (define (assign-bind bind)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda () ,body)) (list lab (cons 'lambda (cons '() (list (assign-body body)))))]
            )
        ) bind)
    )

    (match program
        [(letrec (,bind* ...) ,body) 
            (list 'letrec (assign-bind bind*) (assign-body body))])
)