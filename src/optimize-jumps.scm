(define (optimize-jumps program)

    (define (remove1 graph uv)
        ; (display (list 'graph: graph)) (newline)
        (match graph
            [(,vedges ,graph* ...) (guard (eq? uv (car vedges))) graph*]
            [(,vedges ,graph* ...) (cons vedges (remove1 graph* uv))]
        )
    )   

    (define (optimize-binds bind*)
        (define visited '())
        (define edges '())
        (define dests '())
       
        (define (find-final lab)
            (define labs (list lab))
            (define (loop now)
                (define res (assq now edges))
                ; (emit now)
                (cond 
                    ((not res) now)
                    ((eq? lab now) #f)
                    (else (set! labs (cons (cdr res) labs)) (loop (cdr res)))
                )
            )
            (define final (loop (cdr (assq lab edges))))
            (emit final)
            (cond 
                ((exists? lab visited) (void))
                ((not final) 
                    (set! labs (car labs))
                    (set! visited (append visited labs))
                    (seqmap (lambda (x) (set! dests (cons (list x lab) dests))) labs)
                    (set! bind* (cons (list lab (list 'lambda '() (list lab))) bind*))
                )
                (else (set! dests (cons (list lab final) dests)))
            )
        )
        (seqmap (lambda (bind) (match bind
            [(,lab1 (lambda () (,lab2))) (guard (label? lab1) (label? lab2)) 
                (set! bind* (remove1 bind* lab1))
                (set! edges (cons (cons lab1 lab2) edges))]
            [,temp temp]
        )) bind*)
        (seqmap (lambda (bind) (find-final (car bind))) edges)
        ; (emit "bind*:")
        ; (emit bind*)
        (values dests bind*)
    )

    (define (replace-label lab bind) 
        (cond 
            ((null? bind) lab)
            ((eq? lab (caar bind)) (cadar bind)) 
            (else (replace-label lab (cdr bind))))
    )

    (define (replace-all all bind)
        (define (replace all) 
            (match all
                [,ls (guard (list? ls)) (map replace ls)]
                [,lab (guard (label? lab)) (replace-label lab bind)]
                [,tmp tmp]
            )
        )   
        (replace all)
    )

    (match program
        [(letrec (,bind* ...) ,tail) 
            (receive (dests bind1*) (optimize-binds bind*)
                (list 'letrec (replace-all bind1* dests) (replace-all tail dests))
            )])
)