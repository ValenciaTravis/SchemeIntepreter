(define-syntax receive
  (syntax-rules ()
    ((_ (vars ...) producer body ...)
     (call-with-values (lambda () producer)
       (lambda (vars ...)
         body ...)))))

(define (revmap func lst)
  (if (null? lst)
      '()
      (append (revmap func (cdr lst)) ; 递归处理剩余的元素
              (list (func (car lst)))))) ; 将函数应用于当前元素并添加到结果列表中

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