(load "src/utils.scm")

(define (select-instructions program)

    (define (loc? loc) (or (register? loc) (frame-var? loc)))
    (define (var? var) (or (loc? var) (uvar? var)))
    (define (maybe-reg? var) (or (register? var) (uvar? var)))
    (define (triv? tri) (or (label? tri) (int64? tri) (var? tri)))
    (define (imm64? imm) (and (int64? imm) (not (int32? imm))))
    (define (binop? binop) (or (eq? binop '+) (eq? binop '-) (eq? binop '*) (eq? binop 'logand) (eq? binop 'logor) (eq? binop 'sra)))
    
    (define (commu? binop)
        (match binop
            [+ #t]
            [* #t]
            [logand #t]
            [logor #t]
            [,temp #f]
        )
    )
    (define (rev-relop relop)
        (match relop
            [< (quote >)]
            [> (quote <)]
            [= (quote =)]
            [!= (quote !=)]
            [<= (quote >=)]
            [>= (quote <=)]
        )
    )

    (define (make-x86-binop-set binop-set) 
        ; (emit binop-set)
        (match binop-set
            [(set! ,var (,binop ,tri1 ,tri2)) 
                (guard (imm64? tri1))
                ; tri1 是 int64
                (define uv (unique-name 't))
                (receive (uvs1 eff*) (make-x86-binop-set (list 'set! var (list binop uv tri2)))
                (values 
                    (cons uv uvs1)
                    (list (make-begin (cons (list 'set! uv tri1) eff*)))   
                ))
            ]
            [(set! ,var (,binop ,tri1 ,tri2)) 
                (guard (imm64? tri2) (binop? binop))
                ; tri2 是 int64
                (define uv (unique-name 't))
                (receive (uvs1 eff*) (make-x86-binop-set (list 'set! var (list binop tri1 uv)))
                (values 
                    (cons uv uvs1)
                    (list (make-begin (cons (list 'set! uv tri2) eff*)))
                ))
            ]
            [(set! ,var (,binop ,tri1 ,tri2)) 
                (guard (binop? binop) (eq? var tri1) (or (maybe-reg? var) (maybe-reg? tri2) (int32? tri2)))
                ; var = triv1 且 (至少有一者是寄存器 或 triv2是立即数)
                (values '() (list (list 'set! var (list binop tri1 tri2))))
            ]
            [(set! ,var (,binop ,tri1 ,tri2))
                (guard (binop? binop) (eq? var tri2) (commu? binop) (or (maybe-reg? var) (maybe-reg? tri1) (int32? tri1)))
                ; var = triv2 且 (至少有一者是寄存器 或 triv2是立即数) 且 binop可交换
                (values '() (list (list 'set! var (list binop tri2 tri1))))]
            ; [(set! ,var (,binop ,tri1 ,tri2))
            ;     (guard (var? var) (triv? tri1) (triv? tri2) (maybe-reg? var) (not (eq? var tri2)))
            ;     ; var是寄存器 且 var 不是 triv2
            ;     (values 
            ;         '()
            ;         (list (make-begin (list 
            ;             (list 'set! var tri1)
            ;             (list 'set! var (list binop var tri2)))))
            ;     )]
            ; [(set! ,var (,binop ,tri1 ,tri2))
            ;     (guard (var? var) (triv? tri1) (triv? tri2) (commu? binop) (maybe-reg? var) (not (eq? var tri1)))
            ;     ; var是寄存器 且 var 不是 triv1 且 binop可交换
            ;     (values 
            ;         '()
            ;         (list (make-begin (list 
            ;             (list 'set! var tri2)
            ;             (list 'set! var (list binop var tri1)))))
            ;     )]
            ; [(set! ,var (,binop ,tri1 ,tri2))
            ;     (guard (var? var) (triv? tri1) (triv? tri2) (not (eq? var tri2))
            ;            (or (maybe-reg? tri1) (int32? tri1)) (or (maybe-reg?  tri2) (int32? tri2)))
            ;     ; (triv1是寄存器 或 立即数) 且 (triv2是寄存器 或立即数) 且 (var 不是 triv2)
            ;     (values 
            ;         '()
            ;         (list (make-begin (list 
            ;             (list 'set! var tri1)
            ;             (list 'set! var (list binop var tri2)))))
            ;     )]

            [(set! ,var (,binop ,tri1 ,tri2))
                (guard (var? var) (triv? tri1) (triv? tri2))
                ; 必须引入一个中间变量
                (define uv (unique-name 't))
                (values 
                    (list uv)
                    (list (make-begin (list 
                        (list 'set! uv tri1)
                        (list 'set! uv (list binop uv tri2))
                        (list 'set! var uv))))
                )]
        )
    )

    (define (select-effs eff*)
        (match eff*
            [() (values '() '())]
            [((nop) ,eff1* ...)
                (receive (uvs seff1*) (select-effs eff1*)
                (values
                    uvs
                    (cons (list 'nop) seff1*)    
                ))]
            [((if ,pred ,eff1 ,eff2) ,eff3* ...)
                (receive (uvs1 seff1*) (select-effs (list eff1))
                (receive (uvs2 seff2*) (select-effs (list eff2))
                (receive (uvs3 seff3*) (select-effs eff3*)
                (receive (uvs4 spred) (select-pred pred)
                (values
                    (append uvs1 uvs2 uvs3 uvs4)
                    (cons (list 'if spred (car seff1*) (car seff2*)) seff3*)
                )))))]
            [((begin ,eff1* ...) ,eff2* ...)
                (receive (uvs1 seff1*) (select-effs eff1*)
                (receive (uvs2 seff2*) (select-effs eff2*)
                (values
                    (append uvs1 uvs2)
                    (cons (make-begin seff1*) seff2*)
                )))]
            [((set! ,var ,tri), eff1* ...) (guard (var? var) (triv? tri) 
                (not (maybe-reg? var)) (not (maybe-reg? tri)) (not (int64? tri)))
                ; 全部都不是寄存器 triv也不是立即数
                (define uv (unique-name 't))
                (receive (uvs1 seff1*) (select-effs eff1*)
                (values 
                    (cons uv uvs1)
                    (cons (make-begin (list 
                        (list 'set! uv tri)
                        (list 'set! var uv)
                    )) seff1*)))]
            [((set! ,var ,tri), eff1* ...) (guard (var? var) (triv? tri) 
                (not (maybe-reg? var)) (not (maybe-reg? tri)) (imm64? tri))
                ; 全部都不是寄存器 triv是立即数 但是是 64位
                (define uv (unique-name 't))
                (receive (uvs1 seff1*) (select-effs eff1*)
                (values 
                    (cons uv uvs1)
                    (cons (make-begin (list 
                        (list 'set! uv tri)
                        (list 'set! var uv)
                    )) seff1*)))]
            [((set! ,var ,tri), eff1* ...) (guard (var? var) (triv? tri))
                ; 至少有一者是寄存器 或者 triv是立即数
                (receive (uvs1 seff1*) (select-effs eff1*)
                (values 
                    uvs1
                    (cons (list 'set! var tri) seff1*)
                ))]
            [((set! ,var (* ,tri1 ,tri2)) ,eff1* ...)
                (guard (maybe-reg? var))
                ; binop = * 且 var是寄存器
                (receive (uvs1 seff1*) (select-effs eff1*)
                (receive (uvs2 seff2*) (make-x86-binop-set (list 'set! var (list '* tri1 tri2)))
                (values 
                    (append uvs2 uvs1)
                    (append seff2* seff1*)
                )))]
            [((set! ,var (* ,tri1 ,tri2)) ,eff1* ...)
                (guard (not (maybe-reg? var)))
                ; binop = * 且 var不是寄存器
                (define uv (unique-name 't))
                (receive (uvs1 seff1*) (select-effs eff1*)
                (receive (uvs2 seff2*) (make-x86-binop-set (list 'set! uv (list '* tri1 tri2)))
                (values 
                    (cons uv (append uvs2 uvs1))
                    (cons (make-begin (append seff2* (list (list 'set! var uv)))) seff1*)
                )))]
            [((set! ,var (,binop ,tri1 ,tri2)) ,eff1* ...) (guard (binop? binop))
                ; 其他所有情况
                (receive (uvs1 seff1*) (select-effs eff1*)
                (receive (uvs2 seff2*) (make-x86-binop-set (list 'set! var (list binop tri1 tri2)))
                (values 
                    (append uvs2 uvs1)
                    (append seff2* seff1*))
                ))]
            [((set! ,var (mref ,tri1 ,tri2)) ,eff1* ...)
                (define uv1 tri1)
                (define uv2 tri2)
                (define uv3 var)
                (define uvs '())
                (define seff* '())
                (define add '())
                (unless (or (int32? tri1) (maybe-reg? tri1))
                    (set! uv1 (unique-name 't))
                    (set! uvs (cons uv1 uvs))
                    (set! seff* (cons (list 'set! uv1 tri1) seff*)))
                (unless (or (int32? tri2) (maybe-reg? tri2))
                    (set! uv2 (unique-name 't))
                    (set! uvs (cons uv2 uvs))
                    (set! seff* (cons (list 'set! uv2 tri2) seff*)))
                (unless (or (int32? var) (maybe-reg? var))
                    (set! uv3 (unique-name 't))
                    (set! uvs (cons uv3 uvs))
                    (set! add (list (list 'set! var uv3))))
                (set! seff* (append-back seff* (list 'set! uv3 (list 'mref uv1 uv2))))
                (receive (uvs1 seff1*) (select-effs eff1*)
                (values 
                    (append uvs uvs1)
                    (append seff* add seff1*))
                )
            ]
            [((mset! ,tri1 ,tri2 ,tri3) ,eff1* ...)
                (define uv1 tri1)
                (define uv2 tri2)
                (define uv3 tri3)
                (define uvs '())
                (define seff* '())
                (unless (or (int32? tri1) (maybe-reg? tri1))
                    (set! uv1 (unique-name 't))
                    (set! uvs (cons uv1 uvs))
                    (set! seff* (cons (list 'set! uv1 tri1) seff*)))
                (unless (or (int32? tri2) (maybe-reg? tri2))
                    (set! uv2 (unique-name 't))
                    (set! uvs (cons uv2 uvs))
                    (set! seff* (cons (list 'set! uv2 tri2) seff*)))
                (unless (or (int32? tri3) (maybe-reg? tri3))
                    (set! uv3 (unique-name 't))
                    (set! uvs (cons uv3 uvs))
                    (set! seff* (cons (list 'set! uv3 tri3) seff*)))
                (set! seff* (append-back seff* (list 'mset! uv1 uv2 uv3)))
                (receive (uvs1 seff1*) (select-effs eff1*)
                (values 
                    (append uvs uvs1)
                    (append seff* seff1*))
                )]
            [((return-point ,lab ,tail) ,eff1* ...)
                (receive (uvs1 seff1*) (select-effs eff1*)
                (receive (uvs2 stail) (select-tail tail)
                (values 
                    (append uvs1 uvs2)
                    (cons (list 'return-point lab stail) seff1*)
                )))]
        )
    )

    (define (select-pred pred)
        (match pred
            [(true) (values '() (list 'true))]
            [(false) (values '() (list 'false))]


            [(,relop ,tri1 ,tri2) (guard (imm64? tri1) (imm64? tri2))
                (define uv1 (unique-name 't))
                (define uv2 (unique-name 't))
                (values (list uv1 uv2) (make-begin (list 
                    (list 'set! uv1 tri1)
                    (list 'set! uv2 tri2)
                    (list relop uv1 uv2)
                )))
            ]
            [(,relop ,tri1 ,tri2) (guard (imm64? tri1))
                (define uv (unique-name 't))
                (values (list uv) (make-begin (list 
                    (list 'set! uv tri1)
                    (list relop uv tri2)
                )))
            ]
            [(,relop ,tri1 ,tri2) (guard (imm64? tri2))
                (define uv (unique-name 't))
                (values (list uv) (make-begin (list 
                    (list 'set! uv tri2)
                    (list (rev-relop relop) uv tri1)
                )))
            ]
            [(,relop ,tri1 ,tri2) (guard (int32? tri1) (int32? tri2))
                (define uv (unique-name 't))
                (values 
                    (list uv)
                    (make-begin (list (list 'set! uv tri1) (list relop uv tri2)))
                )
            ]
            [(,relop ,tri1 ,tri2) (guard (int32? tri2))
                (values '() (list relop tri1 tri2))
            ]
            [(,relop ,tri1 ,tri2) (guard (int32? tri1))
                (values '() (list (rev-relop relop) tri2 tri1))
            ]
            [(,relop ,tri1 ,tri2) (guard (frame-var? tri1) (frame-var? tri2))
                (define uv (unique-name 't))
                (values 
                    (list uv)
                    (make-begin (list (list 'set! uv tri1) (list relop uv tri2)))
                )
            ]
            [(,relop ,tri1 ,tri2) (guard (triv? tri1) (triv? tri2))
                (values '() (list relop tri1 tri2))
            ]
            
            [(if ,pred1 ,pred2 ,pred3) 
                (receive (uvs1 spred1) (select-pred pred1)
                (receive (uvs2 spred2) (select-pred pred2)
                (receive (uvs3 spred3) (select-pred pred3)
                (values
                    (append uvs1 uvs2 uvs3)
                    (list 'if spred1 spred2 spred3)       
                ))))
            ]
            [(begin ,eff* ... ,pred1)
                (receive (uvs1 seff*) (select-effs eff*)
                (receive (uvs2 spred1) (select-pred pred1)
                (values 
                    (append uvs1 uvs2)
                    (make-begin (append seff* (list spred1)))
                )))
            ]
        )
    )

    (define (select-tail tail)
        (match tail
            [(,tri ,temp* ...) (guard (triv? tri)) (values '() (cons tri temp*))]
            [(if ,pred ,tail1 ,tail2) 
                (receive (uvs1 spred) (select-pred pred)
                (receive (uvs2 stail1) (select-tail tail1)
                (receive (uvs3 stail2) (select-tail tail2)
                (values
                    (append uvs1 uvs2 uvs3)
                    (list 'if spred stail1 stail2)
                ))))]
            [(begin ,eff* ... ,tail) 
                (receive (uvs1 seff*) (select-effs eff*)
                (receive (uvs2 stail) (select-tail tail)
                (values 
                    (append uvs1 uvs2)
                    (make-begin (append seff* (list stail)))
                )))]
        )
    )

    (define (select-body body)
        (match body
            [(locals (,uv1* ...) (ulocals (,uv2* ...) (locate (,uf* ...) (frame-conflict (,graph* ...) ,tail)))) 
                (receive (uvs stail) (select-tail tail)
                (list 'locals uv1* 
                (list 'ulocals (append uv2* uvs)
                (list 'locate uf*
                (list 'frame-conflict graph* stail)))))]
            [(locate (,uv* ...) ,tail) (list 'locate uv* tail)]
        )
    )

    (define (select-bind bind)
        (map (lambda (bind)
            (match bind
                [(,lab (lambda () ,body)) (list lab (cons 'lambda (cons '() (list (select-body body)))))]
            )
        ) bind)
    )

    (match program
        [(letrec (,bind* ...) ,body) 
            (list 'letrec (select-bind bind*) (select-body body))])

)