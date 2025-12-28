(load "src/utils.scm")

(define (assign-frame program)
    (define (loc? loc) (or (register? loc) (frame-var? loc)))
    (define (var? var) (or (loc? var) (uvar? var)))
    (define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))

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

    (define (find-frame index edges)
        (define fvar (index->frame-var index))
        (if (exists? fvar edges) (find-frame (+ index 1) edges) fvar)
    )

    (define (replace-with-assigns edges assigns)
        (define (replace elem)
            (define fvar (assq elem assigns))
            (if fvar (cadr fvar) elem)
        )
        (map replace edges)
    )

    (define (add-graph graph edges fvar)
        (define (add elem)
            (define vedges (assq elem graph))
            (when (and vedges (not (exists? fvar vedges)))
                (set! graph (cons (append vedges (list fvar)) (remove1 graph elem)))
            )
        )
        (map add edges)
        graph
    )

    (define (assign uvs graph assigns)
        (match uvs
            [() (values graph assigns)]
            [,temp
                (define uv (find-min-deg uvs graph))
                (define graph1 (remove-uv graph uv))
                (define edges (cdr (assq uv graph)))
                (define fvar)
                (receive (graph2 assigns1) (assign (remove2 uvs uv) graph1 assigns)
                    (set! edges (replace-with-assigns edges assigns1))
                    ; (display (list 'edges: edges)) (newline)
                    (set! fvar (find-frame 0 edges))
                    (values 
                        (add-graph graph2 edges fvar)
                        (cons (list uv fvar) assigns1)
                    )
                )
            ]
        )
    )

    (define (assign-body body)
        (match body
            [(locals (,uv1* ...) 
                (ulocals (,uv2* ...) 
                (spills (,uv3* ...)
                (locate (,uf* ...) 
                (frame-conflict ,graph ,tail)))))
                (receive (graph1 assigns) (assign uv3* graph '())
                    (list 'locals uv1*
                    (list 'ulocals uv2*
                    (list 'locate assigns
                    (list 'frame-conflict graph1 tail)))))
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