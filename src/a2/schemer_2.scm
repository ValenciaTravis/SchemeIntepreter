(define verify-scheme
    (lambda (program) program)
)

(define var? (lambda (var) (or (register? var) (disp-opnd? var))))
(define triv? (lambda (tri) (or (label? tri) (var? tri) (int64? tri))))

(define set!-x86 
    (lambda (op)
        (define check2 (lambda (op) (or (register? op) (int32? op) (disp-opnd? op))))
        (match op
            [(,op1 ,op2) (guard (var? op1) (label? op2)) (emit "leaq" op2 op1)]
            [(,op1 ,op2) (guard (var? op1) (triv? op2)) (emit "movq" op2 op1)] 
            ; [(,op1 ,op2) (guard (var? op1) (int64? op2)) (emit "movq" op2 op1)]
            [(,op1 (+ ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3))  (emit "addq" op3 op1)]
            [(,op1 (- ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "subq" op3 op1)]
            [(,op1 (* ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "imulq" op3 op1)]
            [(,op1 (logand ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "and" op3 op1)]
            [(,op1 (logor ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "or" op3 op1)]
            [(,op1 (sra ,op2 ,op3)) (guard (var? op1) (triv? op2) (triv? op3)) (emit "sarq" op3 op1)]
            
            )))
            

(define jump-x86 (lambda (lab) (emit-jump "jmp" lab)))

(define def-x86 (lambda (lab) (emit-label lab)))

(define expose-frame-var 
    (lambda (program)
    
        (match program
            [() '()]
            [(,fr . ,ls1) (guard (frame-var? fr)) 
                (cons (make-disp-opnd 'rbp (* 8 (frame-var->index fr))) 
                (expose-frame-var ls1))]        
            [(,ls1 . ,ls2) (guard (list? ls1)) (cons (expose-frame-var ls1) (expose-frame-var ls2))]
            [(,op . ,ls1) (cons op (expose-frame-var ls1))]
        )

    )
)


(define flatten-program
    (lambda (program)

        (define flat-tail
            (lambda (tail)
                (match tail
                    [(,tri) (guard (triv? tri)) (list (list 'jump tri))]
                    [(begin ,expr* ... ,tail1) (append expr* (flat-tail tail1))]))) 

        (define flat-bind
            (lambda (bind)
                (match bind
                    [() '()]
                    [((,lab (lambda () ,tail)) . ,bind1) 
                        (append (cons lab (flat-tail tail)) (flat-bind bind1))]))) 
        

        (match program
            [(letrec (,bind* ...) ,tail) 
                (cons 'code (append (flat-tail tail) (flat-bind bind*)))])))

(define generate-x86-64
    (lambda (program)
        (define gen-x86
            (lambda (op)
                (match op
                    [code 'code]
                    [(set! ,op1 ,op2) (set!-x86 (list op1 op2))]
                    [(jump ,lab) (guard (label? lab)) (jump-x86 lab)]    
                    [(jump ,disp) (guard (disp-opnd? disp)) (jump-x86 disp)]
                    [(jump ,reg) (guard (register? reg)) (jump-x86 reg)]              
                    [,lab (guard (label? lab)) (def-x86 lab)]
                )
            )
        )
        (define loop
            (lambda (ls)
                (match ls
                    [() 0]
                    [(,op . ,ls) (gen-x86 op) (loop ls)])))
        (emit-program (loop program))
    )
)

; (define temp
;     (lambda (ls)
;         (match ls
;             [(,(number? a) ,(number? b)) (+ a b)]
;         )
;     )
; )

; (define translate
;   (lambda (x)
;     (match x
;       [(let ((,var* ,expr*) ...) ,body ,body* ...)
;        `((lambda ,var* ,body ,@body*) ,@expr*)]
;       [,x (error 'translate "invalid expression: ~s" x)])))


; (translate '(let ((x 3) (y 4)) (+ x y)))

; (flatten-program (quote (letrec ([double$0 (lambda () (begin (set! rax (+ rax rax)) (r15)))]) (begin (set! rax 10) (double$0)))))

; (flatten-program (quote (begin (set! rax rax) (begin (set! rax rax) (r15)))))

; (cons (quote code) (quote ((set! rax 5) (r15))))


; (generate-x86-64 (quote (code
;   (set! rax 10)
;   (double$0)
;   double$0
;   (set! rax (+ rax rax))
;   (r15))))