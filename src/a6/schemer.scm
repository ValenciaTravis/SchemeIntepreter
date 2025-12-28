(define-syntax receive
  (syntax-rules ()
    ((_ (vars ...) producer body ...)
     (call-with-values (lambda () producer)
       (lambda (vars ...)
         body ...)))))

(define (verify-scheme program) program)

(define (loc? loc) (or (register? loc) (disp-opnd? loc)))
(define (var? var) (or (loc? var) (uvar? var)))
(define (triv? tri) (or (label? tri) (var? tri) (int64? tri)))
(define (relop? relop) (or (eq? '< relop) (eq? '> relop) (eq? '= relop) (eq? '>= relop) (eq? '<= relop)))

(load "src/remove-complex-opera*.scm")
(load "src/flatten-set!.scm")
(load "src/impose-calling-conventions.scm")
(load "src/uncover-frame-conflict.scm")
(load "src/introduce-allocation-forms.scm")
(load "src/uncover-register-conflict.scm")
(load "src/select-instructions.scm")
(load "src/assign-frame.scm")
(load "src/finalize-frame-locations.scm")
(load "src/assign-registers.scm")
(load "src/discard-call-live.scm")
(load "src/finalize-locations.scm")

(define (expose-frame-var program)
    
        (match program
            [() '()]
            [(,fr . ,ls1) (guard (frame-var? fr)) 
                (cons (make-disp-opnd frame-pointer-register (ash (frame-var->index fr) align-shift)) 
                (expose-frame-var ls1))]        
            [(,ls1 . ,ls2) (guard (list? ls1)) (cons (expose-frame-var ls1) (expose-frame-var ls2))]
            [(,op . ,ls1) (cons op (expose-frame-var ls1))]
        )

    )

(define (expose-basic-blocks program)
    (define new-bind '())
    (define (make-bind lab extail) 
       (set! new-bind (cons (list lab (list 'lambda '() extail)) new-bind)))
    (define (expose-effs eff* extail)
        (define (loop eff*)
            (match eff*
                [() #f]
                [((nop) ,eff1* ...) (loop eff1*)]
                [((if ,pred ,eff1 ,eff2) ,eff3* ...) 
                    (define lab (unique-label 'extail))
                    (define lab1 (unique-label 'effT))
                    (define lab2 (unique-label 'effF))
                    (loop eff3*)
                    (make-bind lab (make-begin extail))
                    (make-bind lab2 (expose-effs (list eff2) (list (list lab))))
                    (make-bind lab1 (expose-effs (list eff1) (list (list lab))))
                    (set! extail (list (expose-pred pred lab1 lab2)))
                ]
                [((begin ,eff1* ...) ,eff2* ...) (loop eff2*) 
                    (set! extail (list (expose-effs eff1* extail)))
                ]
                [(,set ,eff1* ...) (loop eff1*) (set! extail (cons set extail))]
            )
        )
        (loop eff*)
        (make-begin extail)
    )
    ; (define (expose-eff eff extail)
    ;     (match eff
    ;         [(begin ,eff* ...) (expose-effs eff* extail)]
    ;         [(if ,pred1 ,eff1 ,eff2)
    ;             (define lab1 (unique-label 'effT))
    ;             (define lab2 (unique-label 'effF))
    ;             (make-bind lab2 (expose-eff eff2 extail))
    ;             (make-bind lab1 (expose-eff eff1 extail))
    ;             (expose-pred pred1 lab1 lab2)
    ;         ]
    ;         [(nop) extail]
    ;         [,set (cons eff extail)]
    ;     )
    ; )
    (define (expose-pred pred lab1 lab2)
        (match pred
            [(true) (list lab1)]
            [(false) (list lab2)]
            [(,relop ,tri1 ,tri2) (guard (relop? relop) (triv? tri1) (triv? tri2))
                (list 'if (list relop tri1 tri2) (list lab1) (list lab2))
            ]
            [(begin ,eff* ... ,pred1) 
                (define extail (expose-pred pred1 lab1 lab2))
                (expose-effs eff* (list extail))   
            ]
            [(if ,pred1 ,pred2 ,pred3)
                (define lab3 (unique-label 'predT))
                (define lab4 (unique-label 'predF))
                (make-bind lab4 (expose-pred pred3 lab1 lab2))
                (make-bind lab3 (expose-pred pred2 lab1 lab2))
                (expose-pred pred1 lab3 lab4)
            ]
        )
    )
    (define (expose-tail tail)
        (match tail
            [(if ,pred ,tail1 ,tail2) 
                (define lab1 (unique-label 'tailT))
                (define lab2 (unique-label 'tailF))
                (make-bind lab2 (expose-tail tail2))
                (make-bind lab1 (expose-tail tail1))
                (expose-pred pred lab1 lab2)
            ]
            [(begin ,eff* ... ,tail) 
                (define extail (expose-tail tail))
                (expose-effs eff* (list extail))
            ]
            [,op tail]
        )
    )
    (define (expose-bind bind)
        (match bind
            [(,lab (lambda () ,tail)) (make-bind lab (expose-tail tail))]
        )
    )

    (match program
        [(letrec (,bind* ...) ,tail) 
            (define extail (expose-tail tail))
            (map expose-bind bind*) 
            (list 'letrec new-bind extail)]
    )

)

(define (flatten-program program)

        (define (flat-tail tail)
            (match tail
                [(,tri) (guard (triv? tri)) (list (list 'jump tri))]
                ; [(if (,relop ,tri1 ,tri2) (,lab1) (,lab2))
                ;     (list (list 'if (list relop tri1 tri2) (list lab1) (list lab2)))
                ; ]
                [(begin ,expr* ... ,tail1) (append expr* (flat-tail tail1))]
                [,if (list if)]))

        (define (flat-bind bind)
            (match bind
                [() '()]
                [((,lab (lambda () ,tail)) . ,bind1) 
                    (append (cons lab (flat-tail tail)) (flat-bind bind1))]))
        
        (define (delete-jump program)
            (define i 0)
            (define (loop program)
                (if (null? program) '()
                    (let ([res (loop (cdr program))])
                        (match (car program)
                            [(if (,relop ,tri1 ,tri2) (,lab1) (,lab2)) 
                                (guard (equal? (extract-suffix lab1) i))
                                (cons (list 'if (list 'not (list relop tri1 tri2)) (list 'jump lab2)) res)
                            ]
                            [(if (,relop ,tri1 ,tri2) (,lab1) (,lab2)) 
                                (guard (equal? (extract-suffix lab2) i))
                                (cons (list 'if (list relop tri1 tri2) (list 'jump lab1)) res)
                            ]
                            [(if (,relop ,tri1 ,tri2) (,lab1) (,lab2)) 
                                (cons (list 'if (list relop tri1 tri2) (list 'jump lab1)) (cons (list 'jump lab2) res))
                            ]
                            [,lab (guard (label? lab))  (set! i (extract-suffix lab)) (cons lab res)]
                            [(jump ,lab) 
                                (guard (label? lab) (equal? (extract-suffix lab) i)) 
                                res
                            ]
                            [,tmp (cons tmp res)]
                        )
                    )
                )
            )
            (loop program)
            ; program
        )

        (match program
            [(letrec (,bind* ...) ,tail) 
                (cons 'code (delete-jump (append (flat-tail tail) (flat-bind bind*))))]))

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




; (expose-frame-var
;     (finalize-locations (quote (letrec ([f$1 (lambda ()
;         (locate ([x.1 r8] [y.2 r9])
;         (if (if (= x.1 1) (true) (> y.2 1000))
;         (begin (set! rax y.2) (r15))
;         (begin
;         (set! y.2 (* y.2 2))
;         (set! rax x.1)
;         (set! rax (logand rax 1))
;         (if (= rax 0) (set! y.2 (+ y.2 1)) (nop))
;         (set! x.1 (sra x.1 1))
;         (f$1)))))])
;         (locate () (begin (set! r8 3) (set! r9 10) (f$1))))))
; )

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