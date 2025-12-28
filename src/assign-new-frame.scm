(load "src/utils.scm")

(define (assign-new-frame program)

    (define (assign-nfs max-index new-frames)
        (define nfs '())
        (define (assign-list ls) 
            (define index max-index)
            (define (assign nf) 
                (set! nfs (cons (list nf (index->frame-var index)) nfs))
                (set! index (+ index 1))    
            )
            (seqmap assign ls)
        )
        (seqmap assign-list new-frames)
        nfs
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

    (define (get-max elem)
        ; (emit elem)
        (match elem
            [() -1]
            [,ls (guard (list? ls)) (get-maxs ls)]
            [,fv (guard (frame-var? fv)) (frame-var->index fv)]
            [,temp -1]
        )
    )

    (define (get-maxs ls)
        (define mx -1)
        (define (loop elem)
            (set! mx (max mx (get-max elem)))
        )
        (seqmap loop ls)
        mx
    )    

    (define (remove1 graph uv)
        ; (display (list 'graph: graph)) (newline)
        (match graph
            [(,vedges ,graph* ...) (guard (eq? uv (car vedges))) graph*]
            [(,vedges ,graph* ...) (cons vedges (remove1 graph* uv))]
        )
    )  

    (define (modify-graph graph ls)
        (define (modify elem)
            (define uv (car elem))
            (define fv (cadr elem))
            (define edges (cdr (assq uv graph)))
            (set! graph (remove1 graph uv))
            (set! graph (add-graph graph edges fv))
        )
        (seqmap modify ls)
        graph
    )

    (define (assign-body body)
        (define max-index)

        (define (modify elem)
            (match elem
                [() '()]
                [(return-point ,lab ,tail) 
                    (make-begin (list
                        (list 'set! frame-pointer-register (list '+ frame-pointer-register (ash max-index align-shift)))
                        (list 'return-point lab tail)
                        (list 'set! frame-pointer-register (list '- frame-pointer-register (ash max-index align-shift)))
                    ))
                ]
                [,ls (guard (list? ls)) (seqmap modify ls)]
                [,temp temp]
            )
        )

        (match body
            [(locals (,uv1* ...) 
                (new-frames (,uv2* ...) 
                (locate (,uv3* ...)
                (frame-conflict ,graph 
                (call-live (,call* ...) ,tail)))))
                (define uv4*)
                (set! max-index (+ 1 (max (get-max call*) (get-max uv3*))))
                (set! uv4* (assign-nfs max-index uv2*))
                (set! graph (modify-graph graph uv4*))
                (list 'locals (difference uv1* (intersection uv1* call*))
                (list 'ulocals '()
                (list 'locate (append uv3* uv4*)
                (list 'frame-conflict graph (modify tail)))))
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