(define (convert-assignments program)

    (define replace '())

    (define (convert-let binds assigns elem)
        (define binds1 '())
        (define binds2 '())
        (define (convert-bind bind)
            ; (display bind) (newline)
            (match bind
                [(,uv ,temp) (guard (exists? uv assigns)) 
                    (define tmp (unique-name 'tmp))
                    (set! binds1 (append-back binds1 (list tmp (convert temp))))
                    (set! binds2 (append-back binds2 (list uv (list 'cons tmp (list 'void)))))
                    (set! replace (set-cons uv replace))
                ]
                [(,uv ,temp)
                    (set! binds1 (append-back binds1 (list uv (convert temp))))
                ]
            )
        )
        (seqmap convert-bind binds)
        (list 'let binds1 (list 'let binds2 (convert elem)))
    )

    (define (convert-lambda uvs assigns elem)
        (define uvs1 '())
        (define binds2 '())
        (define (convert-uv uv)
            ; (display bind) (newline)
            (match uv
                [,uv (guard (exists? uv assigns)) 
                    (define tmp (unique-name 'tmp))
                    (set! uvs1 (append-back uvs1 tmp))
                    (set! binds2 (append-back binds2 (list uv (list 'cons tmp (list 'void)))))
                    (set! replace (set-cons uv replace))
                ]
                [,uv (set! uvs1 (append-back uvs1 uv))]
            )
        )
        (seqmap convert-uv uvs)
        (list 'lambda uvs1 (list 'let binds2 (convert elem)))
    )

    (define (convert elem)
        (match elem
            [(let ,binds (assigned ,assigns ,elem1)) (convert-let binds assigns elem1)]
            [(lambda ,uvs (assigned ,assigns ,elem1)) (convert-lambda uvs assigns elem1)]
            [(set! ,uv ,temp) (guard (exists? uv replace)) (list 'set-car! uv (convert temp))]
            [,uv (guard (exists? uv replace)) (list 'car uv)]
            [,ls (guard (list? ls)) (seqmap convert ls)]
            [,temp temp]
        )
    )

    (convert program)
)