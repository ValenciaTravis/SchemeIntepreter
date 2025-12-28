(define (specify-representation program)

    (define offset-car (- disp-car tag-pair))
    (define offset-cdr (- disp-cdr tag-pair))
    (define offset-vector-data (- disp-vector-data tag-vector))
    (define offset-vector-length (- disp-vector-length tag-vector))
    (define offset-procedure-code (- disp-procedure-code tag-procedure))
    (define offset-procedure-data (- disp-procedure-data tag-procedure))

    (define (binop? binop) (or (eq? binop '+) (eq? binop '-) (eq? binop '*)))
    (define (relop? relop) (or (eq? relop '<) (eq? relop '<=) (eq? relop '>) (eq? relop '>=) (eq? relop '=)))

    (define (calcbin binop e1 e2)
        (match binop
            [+ (+ e1 e2)]
            [- (- e1 e2)]
            [* (* (ash e1 (- 0 shift-fixnum)) e2)]
        )
    )
    
    (define (calcrel relop e1 e2)
        (if (match relop
                [< (< e1 e2)]
                [<= (<= e1 e2)]
                [> (> e1 e2)]
                [>= (>= e1 e2)]
                [= (= e1 e2)])
            (list 'true)
            (list 'false)
        )
    )

    (define (ask e mask tag) (list '= (list 'logand e mask) tag))

    (define (specify-imm imm)
        (match imm
            [#t $true]
            [#f $false]
            [() $nil]
            [,fix (guard (fixnum? fix)) (ash fix shift-fixnum)]
        )
    )

    ; (define (specify-pred pred)
    ;     (match pred
    ;         [(true) (list 'true)]
    ;         [(false) (list 'false)]
    ;         [(if ,pred1 ,pred2 ,pred3) (list 'if (specify-pred pred1) (specify-pred pred2) (specify-pred pred3))]
    ;         [(begin ,eff* ,pred1) (make-begin (seqmap specify-eff eff*) (specify-pred pred1))]
    ;         [(let)]
    ;     )
    ; )

    ; (define (specify-value val)
    ;     (match val
    ;         [,lab (guard (label? lab)) lab]
    ;         [,uv (guard (uvar? uv)) uv]
    ;         [(quote ,imm) (specify-imm imm)]
    ;         [(car ,val) (list 'mref val (- disp-car tag-pair))]

    ;     )
    ; )

    (define (specify elem)
        (when (list? elem) (set! elem (seqmap specify elem)))
        ; (emit elem)
        (match elem
            [(quote ,imm) (specify-imm imm)]
            [(void) $void]
            [(car ,e) (list 'mref e offset-car)]
            [(cdr ,e) (list 'mref e offset-cdr)]
            [(set-car! ,e1 ,e2) (list 'mset! e1 offset-car e2)]
            [(set-cdr! ,e1 ,e2) (list 'mset! e1 offset-cdr e2)]
            [(cons ,e1 ,e2) 
                (define tmp-car (unique-name 'tmp-car))
                (define tmp-cdr (unique-name 'tmp-cdr))
                (define tmp (unique-name 'tmp))
                (list 'let (list (list tmp-car e1) (list tmp-cdr e2))
                    (list 'let (list (list tmp (list '+ (list 'alloc size-pair) tag-pair)))
                        (make-begin (list
                            (list 'mset! tmp offset-car tmp-car)
                            (list 'mset! tmp offset-cdr tmp-cdr)
                            tmp
                        ))))]
            [(vector-ref ,e1 ,k) (guard (int64? k))  (list 'mref e1 (+ offset-vector-data k))]
            [(vector-ref ,e1 ,e2) (list 'mref e1 (list '+ offset-vector-data e2))]
            [(vector-set! ,e1 ,k ,e3) (guard (int64? k)) (list 'mset! e1 (+ offset-vector-data k) e3)]
            [(vector-set! ,e1 ,e2 ,e3) (list 'mset! e1 (list '+ offset-vector-data e2) e3)]
            [(make-vector ,k) (guard (int64? k))
                (define tmp (unique-name 'tmp))
                (list 'let (list (list tmp (list '+ (list 'alloc (+ disp-vector-data k)) tag-vector)))
                    (make-begin (list
                        (list 'mset! tmp offset-vector-length k)
                        tmp
                    )))]
            [(make-vector ,e) 
                (define tmp1 (unique-name 'tmp))
                (define tmp2 (unique-name 'tmp))
                (list 'let (list (list tmp1 e))
                (list 'let (list (list tmp2 (list '+ (list 'alloc (list '+ disp-vector-data e)) tag-vector)))
                    (make-begin (list
                        (list 'mset! tmp2 offset-vector-length tmp1)
                        tmp2
                    ))))]
            [(,binop ,e1 ,e2) (guard (binop? binop) (int64? e1) (int64? e2)) (calcbin binop e1 e2)]
            [(,relop ,e1 ,e2) (guard (relop? relop) (int64? e1) (int64? e2)) (calcrel relop e1 e2)]
            [(* ,e1 ,e2) (guard (int64? e1)) (list '* (ash e1 (- 0 shift-fixnum)) e2)]
            [(* ,e1 ,e2) (guard (int64? e2)) (list '* e1 (ash e2 (- 0 shift-fixnum)))]
            [(* ,e1 ,e2) (list '* (list 'sra e1 shift-fixnum) e2)]
            [(vector-length ,e) (list 'mref e offset-vector-length)]
            ((boolean? ,e) (ask e mask-boolean tag-boolean))
            [(fixnum? ,e) (ask e mask-fixnum tag-fixnum)]
            [(null? ,e) (specify (list 'eq? e (list 'quote '())))]
            [(pair? ,e) (ask e mask-pair tag-pair)]
            [(vector? ,e) (ask e mask-vector tag-vector)]
            [(procedure? ,e) (ask e mask-procedure tag-procedure)]
            [(eq? ,e1 ,e2) (list '= e1 e2)]
            [(make-procedure ,lab ,n) (guard (int64? n)) 
                (define tmp (unique-name 'tmp))
                (list 'let (list (list tmp (list '+ (list 'alloc (+ disp-procedure-data n)) tag-procedure)))
                    (make-begin (list
                        (list 'mset! tmp offset-procedure-code lab)
                        tmp
                    )))]
            [(procedure-ref ,proc ,n) (guard (int64? n)) (list 'mref proc (+ offset-procedure-data n))]
            [(procedure-code ,proc) (list 'mref proc offset-procedure-code)]
            [(procedure-set! ,proc ,n ,expr) (list 'mset! proc (+ offset-procedure-data n) expr)]
            [,temp temp]
        )
    )

    (define (specify-binds bind*)
        (seqmap (lambda (bind)
            (match bind
                [(,lab (lambda (,uv* ...) ,val)) (list lab (list 'lambda uv* (specify val)))]
            )
        ) bind*)
    )

    (match program
        [(letrec (,bind* ...) ,val) (list 'letrec (specify-binds bind*) (specify val))]
    )

)