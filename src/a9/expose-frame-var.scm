(define (expose-frame-var program)

    (define (expose off elem)
        (match elem
            [() (values off '())]
            [(,fr . ,ls1) (guard (frame-var? fr)) 
                (receive (off1 els1) (expose off ls1)
                (values
                    off
                    (cons (make-disp-opnd frame-pointer-register (+ off (ash (frame-var->index fr) align-shift))) els1)
                ))]    
            [((set! ,reg1 (+ ,reg2 ,n)) . ,ls1) (guard (eq? reg1 frame-pointer-register))
                (define off1 (- off n))
                (receive (off2 els2) (expose off1 ls1)
                (values off2 (cons (list 'set! reg1 (list '+ reg2 n)) els2)))]   
            [((set! ,reg1 (- ,reg2 ,n)) . ,ls1) (guard (eq? reg1 frame-pointer-register))
                (define off1 (+ off n))
                (receive (off2 els2) (expose off1 ls1)
                (values off2 (cons (list 'set! reg1 (list '- reg2 n)) els2)))]    
            [(,ls1 . ,ls2) (guard (list? ls1)) 
                (receive (off1 els1) (expose off ls1)
                (receive (off2 els2) (expose off1 ls2)
                (values off2 (cons els1 els2))))]
            [(,op . ,ls1)
                (receive (off1 els1) (expose off ls1)
                (values off1 (cons op els1)))]
        )
    )
    
    (receive (off prog) (expose 0 program) prog)

)