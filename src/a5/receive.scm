(define-syntax receive
  (syntax-rules ()
    ((_ (vars ...) producer body ...)
     (call-with-values (lambda () producer)
       (lambda (vars ...)
         body ...)))))