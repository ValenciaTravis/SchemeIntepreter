(define (loc? loc) (or (register? loc) (disp-opnd? loc) (index-opnd? loc)))
(define (var? var) (or (loc? var) (uvar? var)))
(define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))
(define (relop? relop) (or (eq? '< relop) (eq? '> relop) (eq? '= relop) (eq? '>= relop) (eq? '<= relop)))

(define (set!-x86 op)
        (match op
            [(,op1 ,op2) (guard (var? op1) (label? op2)) (emit "leaq" op2 op1)]
            [(,op1 ,op2) (guard (var? op1) (triv? op2)) (emit "movq" op2 op1)] 
            ; [(,op1 ,op2) (guard (var? op1) (int64? op2)) (emit "movq" op2 op1)]
            [(,op1 (+ ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3))  (emit "addq" op3 op1)]
            [(,op1 (- ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "subq" op3 op1)]
            [(,op1 (* ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "imulq" op3 op1)]
            [(,op1 (logand ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "andq" op3 op1)]
            [(,op1 (logor ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "orq" op3 op1)]
            [(,op1 (sra ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "sarq" op3 op1)]
        ))
            

(define (jump-x86 lab) (emit-jump "jmp" lab))

(define (if-x86 relop tri1 tri2 lab)
    (emit "cmpq" tri2 tri1)
    (match relop
        [= (emit-jump "je" lab)]
        [!= (emit-jump  "jne" lab)]
        [< (emit-jump "jl" lab)]
        [<= (emit-jump  "jle" lab)]
        [> (emit-jump "jg" lab)]
        [>= (emit-jump  "jge" lab)]
    )
)

(define (if-not-x86 relop tri1 tri2 lab)
    (emit "cmpq" tri2 tri1)
    (match relop
        [= (emit-jump "jne" lab)]
        [!= (emit-jump  "je" lab)]
        [< (emit-jump "jge" lab)]
        [<= (emit-jump  "jg" lab)]
        [> (emit-jump "jle" lab)]
        [>= (emit-jump  "jl" lab)]
    )
)

(define (mset!-x86 tri1 tri2 tri3)
    (define mem)
    (cond 
        ((int32? tri1) (set! mem (make-disp-opnd tri2 tri1)))
        ((int32? tri2) (set! mem (make-disp-opnd tri1 tri2)))
        (else (set! mem (make-index-opnd tri1 tri2)))
    )
    (if (label? tri3) (emit "leaq" tri3 mem) (emit "movq" tri3 mem))
)

(define (def-x86 lab) (emit-label lab))

(define (generate-x86-64 program)
    (define gen-x86
        (lambda (op)
            (match op
                [code 'code]
                [(set! ,op1 ,op2) (set!-x86 (list op1 op2))]
                [(if (,relop ,tri1 ,tri2) (jump ,lab)) (if-x86 relop tri1 tri2 lab)]
                [(if (not (,relop ,tri1 ,tri2)) (jump ,lab)) (if-not-x86 relop tri1 tri2 lab)]
                [(jump ,lab) (guard (label? lab)) (jump-x86 lab)]    
                [(jump ,disp) (guard (disp-opnd? disp)) (jump-x86 disp)]
                [(jump ,reg) (guard (register? reg)) (jump-x86 reg)]              
                [,lab (guard (label? lab)) (def-x86 lab)]
            )
        )
    )
    (define (loop prog)
        (match prog
            [() #f]
            [(,op ,prog ...) (gen-x86 op) (loop prog)]
        )
    )
    (emit-program (loop program))
)