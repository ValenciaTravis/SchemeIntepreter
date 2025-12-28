(define-syntax receive
  (syntax-rules ()
    ((_ (vars ...) producer body ...)
     (call-with-values (lambda () producer)
       (lambda (vars ...)
         body ...)))))

(define (revmap func lst)
  (if (null? lst)
        '()
        (let ([res (revmap func (cdr lst))])
            (append res (list (func (car lst))))
        ))) ; 将函数应用于当前元素并添加到结果列表中

(define (seqmap func lst)
    (if (null? lst)
      '()
      (let ([res (list (func (car lst)))]) ; 将函数应用于当前元素并添加到结果列表中
        (append  
              res
              (seqmap func (cdr lst))
        )))) ; 递归处理剩余的元素

(define (append-back ls elem)
    (append ls (list elem))
)

(define (exists? elem lst)
        (cond
            ((null? lst) #f) ; 如果列表为空，返回 #f
            ((equal? elem (car lst)) #t) ; 如果找到元素，返回 #t
            (else (exists? elem (cdr lst))))) ; 递归检查剩余列表
