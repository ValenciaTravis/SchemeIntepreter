(define (assign-registers program)
    (define (loc? loc) (or (register? loc) (frame-var? loc)))
    (define (var? var) (or (loc? var) (uvar? var)))
    (define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))

    (define (find-min-deg graph)
        (define uv (caar graph))
        (define len (length (car graph)))
        (map (lambda (vedges)
            (define uv1 (car vedges))
            (define len1 (length vedges))
            (when (< len1 len) (set! uv uv1) (set! len len1))
        ) (cdr graph))
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

    (define (find-reg ret edges)
        (define regs registers) 
        (define (remove uv)
            (if (register? uv) (set! regs (remove2 regs uv))
                (set! regs (remove2 regs (cadr (assq uv ret))))
            )
        )
        (map remove edges)
        ; (display ret) (newline)
        ; (display edges) (newline)
        ; (display regs) (newline)
        (if (null? regs) (emit "Impossible") (car regs))
    )

    (define (assign-graph graph)
        ; (display graph) (newline)
        (match graph 
            [() '()]
            [,temp
                (define uv (find-min-deg graph))
                (define graph1 (remove-uv graph uv))
                (define ret (assign-graph graph1))
                (define reg (find-reg ret (cdr (assq uv graph))))
                (cons (list uv reg) ret)    
            ]
        )
    )

    (define (assign-body body)
        (match body
            [(locals (,uv* ...) (register-conflict ,graph ,tail)) 
                (list 'locate (assign-graph graph) tail)
            ]
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