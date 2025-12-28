(define (convert-complex-datum program)
    (define tmps '())

    (define (convert-quote ls)
        (define veccnt 0)

        (define (convert-list ls)
            (match ls
                [() (quote '())]
                [(,elem ,ls* ...) (list 'cons (convert-quote-elem elem) (convert-list ls*))]
            )
        )

        (define (convert-vector ls)
            (define tmp (unique-name 'tmp))
            (define pos -1)
            (define res)
            (define (assign-vec elem)
                (set! pos (+ pos 1))
                (list 'vector-set! tmp (list 'quote pos) (convert-quote-elem elem))
            )
            (set! veccnt (+ veccnt 1))
            (set! res (list 'let (list (list tmp (list 'make-vector (list 'quote (length ls)))))
                (make-begin (append-back (seqmap assign-vec ls) tmp))
            ))
            (set! veccnt (- veccnt 1))
            res
        )

        (define (convert-pair p)
            (match p
                [(,elem1 . ,elem2) (list 'cons (convert-quote-elem elem1) (convert-quote-elem elem2))]
            )
        )

        (define (convert-quote-elem ls)
            (match ls 
                [,vec (guard (vector? vec)) 
                    (define tmp (unique-name 'tmp)) 
                    (set! tmps (append-back tmps (list tmp (convert-vector (vector->list vec)))))
                    tmp]
                [,ls (guard (list? ls)) 
                    (define tmp (unique-name 'tmp)) 
                    (set! tmps (append-back tmps (list tmp (convert-list ls))))
                    tmp]
                [,p (guard (pair? p)) 
                    (define tmp (unique-name 'tmp)) 
                    (set! tmps (append-back tmps (list tmp (convert-pair p))))
                    tmp]
                ; [,elem (guard (> veccnt 0)) elem]
                [,elem (list 'quote elem)]
            )
        )

        (convert-quote-elem ls)
    )

    (define (convert-let binds elem)
        (define binds1 '())
        (define (convert-bind bind)
            (match bind
                [(,uv ,tmp) (set! binds1 (append-back binds1 (list uv (convert tmp))))]
            )
        )
        (seqmap convert-bind binds)
        (list 'let binds1 (convert elem))
    )

    (define (convert-letrec binds elem)
        (define binds1 '())
        (define (convert-bind bind)
            (match bind
                [(,uv ,tmp) (set! binds1 (append-back binds1 (list uv (convert tmp))))]
            )
        )
        (seqmap convert-bind binds)
        (list 'letrec binds1 (convert elem))
    )

    (define (convert elem)
        (match elem
            [(quote ,ls) (convert-quote ls)]
            [(let ,binds ,elem) (convert-let binds elem)]
            [(letrec ,binds ,elem) (convert-letrec binds elem)]
            [,ls (guard (list? ls)) (seqmap convert ls)]
            [,tmp tmp]
        )
    )

    (define (make-lets bind) (set! program (list 'let (list bind) program)))
    
    (set! program (convert program))

    (revmap make-lets tmps)

    program   
)