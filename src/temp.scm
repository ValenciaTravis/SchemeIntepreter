; (uncover-locals
; (specify-representation
(normalize-context
(lift-letrec
(introduce-procedure-primitives
(optimize-known-call
(convert-closures
(uncover-free
(sanitize-binding-forms
(remove-anonymous-lambda
(optimize-direct-call
(convert-assignments
(purify-letrec
(uncover-assigned
(convert-complex-datum
(verify-scheme
(quote

    (letrec ([div.69 (lambda (d.71 n.70)
                        (letrec ([f.72 (lambda (d.75 n.74 q.73)
                                         (if (> n.74 d.75)
                                             q.73
                                             (f.72
                                               (- d.75 n.74)
                                               n.74
                                               (+ q.73 '1))))])
                          (f.72 d.71 n.70 '0)))])
       (letrec ([alloc.78 (lambda (n.84)
                            (make-vector (div.69 n.84 '8)))]
                [mref.77 (lambda (x.83 y.82)
                           (if (vector? x.83)
                               (vector-ref x.83 (div.69 y.82 '8))
                               (vector-ref y.82 (div.69 x.83 '8))))]
                [mset!.76 (lambda (x.81 y.80 z.79)
                            (begin
                              (if (vector? x.81)
                                  (vector-set! x.81 (div.69 y.80 '8) z.79)
                                  (vector-set! y.80 (div.69 x.81 '8) z.79))
                              (if '#f '#f (void))))])
         (letrec ([stack-push.87 (lambda (self.91 val.90)
                                   (begin
                                     (mset!.76
                                       (mref.77 self.91 '16)
                                       (* (mref.77 self.91 '8) '8)
                                       val.90)
                                     (mset!.76
                                       self.91
                                       '8
                                       (+ (mref.77 self.91 '8) '1))
                                     self.91))]
                  [stack-pop.86 (lambda (self.89)
                                  (begin
                                    (mset!.76
                                      self.89
                                      '8
                                      (- (mref.77 '8 self.89) '1))
                                    (mref.77
                                      (mref.77 self.89 '16)
                                      (* (mref.77 self.89 '8) '8))))]
                  [stack-top.85 (lambda (self.88)
                                  (mref.77
                                    (mref.77 self.88 '16)
                                    (* (- (mref.77 '8 self.88) '1) '8)))])
           (letrec ([stack-new.93 (let ([meths.96 (alloc.78
                                                    (* '3 '8))])
                                    (begin
                                      (mset!.76 meths.96 '0 stack-push.87)
                                      (mset!.76 meths.96 '8 stack-pop.86)
                                      (mset!.76 meths.96 '16 stack-top.85)
                                      (lambda (size.97)
                                        (let ([self.98 (alloc.78
                                                         (* '3 '8))])
                                          (begin
                                            (mset!.76 self.98 '0 meths.96)
                                            (mset!.76 self.98 '8 '0)
                                            (mset!.76
                                              self.98
                                              '16
                                              (alloc.78 (* '8 size.97)))
                                            self.98)))))]
                    [invoke.92 (lambda (obj.95 meth-idx.94)
                                 (mref.77
                                   (mref.77 obj.95 '0)
                                   (* meth-idx.94 '8)))])
             (let ([s1.99 (stack-new.93 '10)])
               (begin
                 ((invoke.92 s1.99 '0) s1.99 '10)
                 ((invoke.92 s1.99 '0) s1.99 '20)
                 ((invoke.92 s1.99 '0) s1.99 '30)
                 ((invoke.92 s1.99 '0) s1.99 '40)
                 ((invoke.92 s1.99 '0) s1.99 '0)
                 ((invoke.92 s1.99 '0) s1.99 '60)
                 ((invoke.92 s1.99 '0) s1.99 '70)
                 ((invoke.92 s1.99 '0) s1.99 '80)
                 ((invoke.92 s1.99 '0) s1.99 '90)
                 ((invoke.92 s1.99 '0) s1.99 '100)
                 (let ([s2.100 (stack-new.93 '6)])
                   (begin
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s1.99 '1) s1.99)
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s1.99 '1) s1.99)
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s1.99 '1) s1.99)
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s1.99 '1) s1.99)
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     (let ([x.101 (+ ((invoke.92 s2.100 '1) s2.100)
                                     ((invoke.92 s2.100 '1) s2.100))])
                       (* (+ (let ([x.103 (+ ((invoke.92 s2.100 '2) s2.100)
                                             ((invoke.92 s2.100 '2)
                                               s2.100))])
                               (- x.103
                                  (+ ((invoke.92 s2.100 '1) s2.100)
                                     ((invoke.92 s2.100 '1) s2.100))))
                             (let ([x.102 (+ ((invoke.92 s2.100 '2) s2.100)
                                             ((invoke.92 s2.100 '2)
                                               s2.100))])
                               (- (+ ((invoke.92 s2.100 '1) s2.100)
                                     ((invoke.92 s2.100 '1) s2.100))
                                  x.102)))
                          x.101))))))))))

)))))))))))))))))


(let ([tmp.3 (void)])
  (let ([tmp.1 (cons (tmp.3 void))])
    (begin
      (let ([tmp.2 (cons '1 (cons '2 (cons '3 (cons '4 '()))))])
        (let () (set-car! tmp.1 tmp.2)))
      (car tmp.1))))

(let ([tmp.301 (void)])
  (let ([div.69 (cons tmp.301 (void))])
    (begin
      (let ([tmp.291 (letrec ([anon.311 (lambda (d.71 n.70)
                                          (free ()
                                            (let ()
                                              (let ([tmp.302 (void)])
                                                (let ([f.72 (cons
                                                              tmp.302
                                                              (void))])
                                                  (begin
                                                    (let ([tmp.292 (letrec ([anon.312 (lambda (d.75
                                                                                               n.74
                                                                                               q.73)
                                                                                        (free (f.72)
                                                                                          (let ()
                                                                                            (if (> n.74
                                                                                                   d.75)
                                                                                                q.73
                                                                                                ((car f.72)
                                                                                                  (- d.75
                                                                                                     n.74)
                                                                                                  n.74
                                                                                                  (+ q.73
                                                                                                     '1))))))])
                                                                     anon.312)])
                                                      (let ()
                                                        (set-car!
                                                          f.72
                                                          tmp.292)))
                                                    ((car f.72)
                                                      d.71
                                                      n.70
                                                      '0)))))))])
                       anon.311)])
        (set-car! div.69 tmp.291))
      (let ([tmp.305 (void)] [tmp.304 (void)] [tmp.303 (void)])
        (let ([mset!.76 (cons tmp.305 (void))]
              [mref.77 (cons tmp.304 (void))]
              [alloc.78 (cons tmp.303 (void))])
          (begin
            (let ([tmp.295 (letrec ([anon.315 (lambda (x.81 y.80 z.79)
                                                (free (div.69)
                                                  (let ()
                                                    (begin
                                                      (if (vector? x.81)
                                                          (vector-set!
                                                            x.81
                                                            ((car div.69)
                                                              y.80
                                                              '8)
                                                            z.79)
                                                          (vector-set!
                                                            y.80
                                                            ((car div.69)
                                                              x.81
                                                              '8)
                                                            z.79))
                                                      (if '#f
                                                          '#f
                                                          (void))))))])
                             anon.315)]
                  [tmp.294 (letrec ([anon.314 (lambda (x.83 y.82)
                                                (free (div.69)
                                                  (let ()
                                                    (if (vector? x.83)
                                                        (vector-ref
                                                          x.83
                                                          ((car div.69)
                                                            y.82
                                                            '8))
                                                        (vector-ref
                                                          y.82
                                                          ((car div.69)
                                                            x.83
                                                            '8))))))])
                             anon.314)]
                  [tmp.293 (letrec ([anon.313 (lambda (n.84)
                                                (free (div.69)
                                                  (let ()
                                                    (make-vector
                                                      ((car div.69)
                                                        n.84
                                                        '8)))))])
                             anon.313)])
              (begin
                (set-car! alloc.78 tmp.293)
                (set-car! mref.77 tmp.294)
                (set-car! mset!.76 tmp.295)))
            (let ([tmp.308 (void)] [tmp.307 (void)] [tmp.306 (void)])
              (let ([stack-top.85 (cons tmp.308 (void))]
                    [stack-pop.86 (cons tmp.307 (void))]
                    [stack-push.87 (cons tmp.306 (void))])
                (begin
                  (let ([tmp.298 (letrec ([anon.318 (lambda (self.88)
                                                      (free (mref.77)
                                                        (let ()
                                                          ((car mref.77)
                                                            ((car mref.77)
                                                              self.88
                                                              '16)
                                                            (* (- ((car mref.77)
                                                                    '8
                                                                    self.88)
                                                                  '1)
                                                               '8)))))])
                                   anon.318)]
                        [tmp.297 (letrec ([anon.317 (lambda (self.89)
                                                      (free (mset!.76
                                                              mref.77)
                                                        (let ()
                                                          (begin
                                                            ((car mset!.76)
                                                              self.89
                                                              '8
                                                              (- ((car mref.77)
                                                                   '8
                                                                   self.89)
                                                                 '1))
                                                            ((car mref.77)
                                                              ((car mref.77)
                                                                self.89
                                                                '16)
                                                              (* ((car mref.77)
                                                                   self.89
                                                                   '8)
                                                                 '8))))))])
                                   anon.317)]
                        [tmp.296 (letrec ([anon.316 (lambda (self.91
                                                             val.90)
                                                      (free (mset!.76
                                                              mref.77)
                                                        (let ()
                                                          (begin
                                                            ((car mset!.76)
                                                              ((car mref.77)
                                                                self.91
                                                                '16)
                                                              (* ((car mref.77)
                                                                   self.91
                                                                   '8)
                                                                 '8)
                                                              val.90)
                                                            ((car mset!.76)
                                                              self.91
                                                              '8
                                                              (+ ((car mref.77)
                                                                   self.91
                                                                   '8)
                                                                 '1))
                                                            self.91))))])
                                   anon.316)])
                    (begin
                      (set-car! stack-push.87 tmp.296)
                      (set-car! stack-pop.86 tmp.297)
                      (set-car! stack-top.85 tmp.298)))
                  (let ([tmp.310 (void)] [tmp.309 (void)])
                    (let ([invoke.92 (cons tmp.310 (void))]
                          [stack-new.93 (cons tmp.309 (void))])
                      (begin
                        (let ([tmp.300 (letrec ([anon.320 (lambda (obj.95
                                                                   meth-idx.94)
                                                            (free (mref.77)
                                                              (let ()
                                                                ((car mref.77)
                                                                  ((car mref.77)
                                                                    obj.95
                                                                    '0)
                                                                  (* meth-idx.94
                                                                     '8)))))])
                                         anon.320)]
                              [tmp.299 (let ([meths.96 ((car alloc.78)
                                                         (* '3 '8))])
                                         (let ()
                                           (begin
                                             ((car mset!.76)
                                               meths.96
                                               '0
                                               (car stack-push.87))
                                             ((car mset!.76)
                                               meths.96
                                               '8
                                               (car stack-pop.86))
                                             ((car mset!.76)
                                               meths.96
                                               '16
                                               (car stack-top.85))
                                             (letrec ([anon.319 (lambda (size.97)
                                                                  (free (meths.96
                                                                          mset!.76
                                                                          alloc.78)
                                                                    (let ()
                                                                      (let ([self.98 ((car alloc.78)
                                                                                       (* '3
                                                                                          '8))])
                                                                        (let ()
                                                                          (begin
                                                                            ((car mset!.76)
                                                                              self.98
                                                                              '0
                                                                              meths.96)
                                                                            ((car mset!.76)
                                                                              self.98
                                                                              '8
                                                                              '0)
                                                                            ((car mset!.76)
                                                                              self.98
                                                                              '16
                                                                              ((car alloc.78)
                                                                                (* '8
                                                                                   size.97)))
                                                                            self.98))))))])
                                               anon.319))))])
                          (begin
                            (set-car! stack-new.93 tmp.299)
                            (set-car! invoke.92 tmp.300)))
                        (let ([s1.99 ((car stack-new.93) '10)])
                          (begin
                            (((car invoke.92) s1.99 '0) s1.99 '10)
                            (((car invoke.92) s1.99 '0) s1.99 '20)
                            (((car invoke.92) s1.99 '0) s1.99 '30)
                            (((car invoke.92) s1.99 '0) s1.99 '40)
                            (((car invoke.92) s1.99 '0) s1.99 '0)
                            (((car invoke.92) s1.99 '0) s1.99 '60)
                            (((car invoke.92) s1.99 '0) s1.99 '70)
                            (((car invoke.92) s1.99 '0) s1.99 '80)
                            (((car invoke.92) s1.99 '0) s1.99 '90)
                            (((car invoke.92) s1.99 '0) s1.99 '100)
                            (let ([s2.100 ((car stack-new.93) '6)])
                              (begin
                                (((car invoke.92) s2.100 '0)
                                  s2.100
                                  (((car invoke.92) s1.99 '1) s1.99))
                                (((car invoke.92) s1.99 '1) s1.99)
                                (((car invoke.92) s2.100 '0)
                                  s2.100
                                  (((car invoke.92) s1.99 '1) s1.99))
                                (((car invoke.92) s1.99 '1) s1.99)
                                (((car invoke.92) s2.100 '0)
                                  s2.100
                                  (((car invoke.92) s1.99 '1) s1.99))
                                (((car invoke.92) s1.99 '1) s1.99)
                                (((car invoke.92) s2.100 '0)
                                  s2.100
                                  (((car invoke.92) s1.99 '1) s1.99))
                                (((car invoke.92) s1.99 '1) s1.99)
                                (((car invoke.92) s2.100 '0)
                                  s2.100
                                  (((car invoke.92) s1.99 '1) s1.99))
                                (((car invoke.92) s2.100 '0)
                                  s2.100
                                  (((car invoke.92) s1.99 '1) s1.99))
                                (let ([x.101 (+ (((car invoke.92)
                                                   s2.100
                                                   '1)
                                                  s2.100)
                                                (((car invoke.92)
                                                   s2.100
                                                   '1)
                                                  s2.100))])
                                  (* (+ (let ([x.103 (+ (((car invoke.92)
                                                           s2.100
                                                           '2)
                                                          s2.100)
                                                        (((car invoke.92)
                                                           s2.100
                                                           '2)
                                                          s2.100))])
                                          (- x.103
                                             (+ (((car invoke.92)
                                                   s2.100
                                                   '1)
                                                  s2.100)
                                                (((car invoke.92)
                                                   s2.100
                                                   '1)
                                                  s2.100))))
                                        (let ([x.102 (+ (((car invoke.92)
                                                           s2.100
                                                           '2)
                                                          s2.100)
                                                        (((car invoke.92)
                                                           s2.100
                                                           '2)
                                                          s2.100))])
                                          (- (+ (((car invoke.92)
                                                   s2.100
                                                   '1)
                                                  s2.100)
                                                (((car invoke.92)
                                                   s2.100
                                                   '1)
                                                  s2.100))
                                             x.102)))
                                     x.101))))))))))))))))))


146: (letrec ([div.69 (lambda (d.71 n.70)
                        (letrec ([f.72 (lambda (d.75 n.74 q.73)
                                         (if (> n.74 d.75)
                                             q.73
                                             (f.72
                                               (- d.75 n.74)
                                               n.74
                                               (+ q.73 '1))))])
                          (f.72 d.71 n.70 '0)))])
       (letrec ([alloc.78 (lambda (n.84)
                            (make-vector (div.69 n.84 '8)))]
                [mref.77 (lambda (x.83 y.82)
                           (if (vector? x.83)
                               (vector-ref x.83 (div.69 y.82 '8))
                               (vector-ref y.82 (div.69 x.83 '8))))]
                [mset!.76 (lambda (x.81 y.80 z.79)
                            (begin
                              (if (vector? x.81)
                                  (vector-set! x.81 (div.69 y.80 '8) z.79)
                                  (vector-set! y.80 (div.69 x.81 '8) z.79))
                              (if '#f '#f (void))))])
         (letrec ([stack-push.87 (lambda (self.91 val.90)
                                   (begin
                                     (mset!.76
                                       (mref.77 self.91 '16)
                                       (* (mref.77 self.91 '8) '8)
                                       val.90)
                                     (mset!.76
                                       self.91
                                       '8
                                       (+ (mref.77 self.91 '8) '1))
                                     self.91))]
                  [stack-pop.86 (lambda (self.89)
                                  (begin
                                    (mset!.76
                                      self.89
                                      '8
                                      (- (mref.77 '8 self.89) '1))
                                    (mref.77
                                      (mref.77 self.89 '16)
                                      (* (mref.77 self.89 '8) '8))))]
                  [stack-top.85 (lambda (self.88)
                                  (mref.77
                                    (mref.77 self.88 '16)
                                    (* (- (mref.77 '8 self.88) '1) '8)))])
           (letrec ([stack-new.93 (let ([meths.96 (alloc.78
                                                    (* '3 '8))])
                                    (begin
                                      (mset!.76 meths.96 '0 stack-push.87)
                                      (mset!.76 meths.96 '8 stack-pop.86)
                                      (mset!.76 meths.96 '16 stack-top.85)
                                      (lambda (size.97)
                                        (let ([self.98 (alloc.78
                                                         (* '3 '8))])
                                          (begin
                                            (mset!.76 self.98 '0 meths.96)
                                            (mset!.76 self.98 '8 '0)
                                            (mset!.76
                                              self.98
                                              '16
                                              (alloc.78 (* '8 size.97)))
                                            self.98)))))]
                    [invoke.92 (lambda (obj.95 meth-idx.94)
                                 (mref.77
                                   (mref.77 obj.95 '0)
                                   (* meth-idx.94 '8)))])
             (let ([s1.99 (stack-new.93 '10)])
               (begin
                 ((invoke.92 s1.99 '0) s1.99 '10)
                 ((invoke.92 s1.99 '0) s1.99 '20)
                 ((invoke.92 s1.99 '0) s1.99 '30)
                 ((invoke.92 s1.99 '0) s1.99 '40)
                 ((invoke.92 s1.99 '0) s1.99 '0)
                 ((invoke.92 s1.99 '0) s1.99 '60)
                 ((invoke.92 s1.99 '0) s1.99 '70)
                 ((invoke.92 s1.99 '0) s1.99 '80)
                 ((invoke.92 s1.99 '0) s1.99 '90)
                 ((invoke.92 s1.99 '0) s1.99 '100)
                 (let ([s2.100 (stack-new.93 '6)])
                   (begin
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s1.99 '1) s1.99)
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s1.99 '1) s1.99)
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s1.99 '1) s1.99)
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s1.99 '1) s1.99)
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     ((invoke.92 s2.100 '0)
                       s2.100
                       ((invoke.92 s1.99 '1) s1.99))
                     (let ([x.101 (+ ((invoke.92 s2.100 '1) s2.100)
                                     ((invoke.92 s2.100 '1) s2.100))])
                       (* (+ (let ([x.103 (+ ((invoke.92 s2.100 '2) s2.100)
                                             ((invoke.92 s2.100 '2)
                                               s2.100))])
                               (- x.103
                                  (+ ((invoke.92 s2.100 '1) s2.100)
                                     ((invoke.92 s2.100 '1) s2.100))))
                             (let ([x.102 (+ ((invoke.92 s2.100 '2) s2.100)
                                             ((invoke.92 s2.100 '2)
                                               s2.100))])
                               (- (+ ((invoke.92 s2.100 '1) s2.100)
                                     ((invoke.92 s2.100 '1) s2.100))
                                  x.102)))
                          x.101))))))))))

Error occurred running output of pass uncover-locals on test 146
uncover-locals input:
(letrec ([anon$132 (lambda (cp.169 size.97)
                     (let ()
                       (let ([self.98 (let ([uv.170 (mref
                                                      (mref cp.169 22)
                                                      -1)])
                                        ((mref uv.170 -2) uv.170 192))])
                         (let ()
                           (begin
                             (let ([uv.171 (mref (mref cp.169 14) -1)])
                               ((mref uv.171 -2)
                                 uv.171
                                 self.98
                                 0
                                 (mref cp.169 6)))
                             (let ([uv.172 (mref (mref cp.169 14) -1)])
                               ((mref uv.172 -2) uv.172 self.98 64 0))
                             (let ([uv.173 (mref (mref cp.169 14) -1)])
                               ((mref uv.173 -2)
                                 uv.173
                                 self.98
                                 128
                                 (let ([uv.174 (mref (mref cp.169 22) -1)])
                                   ((mref uv.174 -2)
                                     uv.174
                                     (* 8 size.97)))))
                             self.98)))))]
         [anon$133 (lambda (cp.162 obj.95 meth-idx.94)
                     (let ()
                       (let ([uv.163 (mref (mref cp.162 6) -1)])
                         ((mref uv.163 -2)
                           uv.163
                           (let ([uv.164 (mref (mref cp.162 6) -1)])
                             ((mref uv.164 -2) uv.164 obj.95 0))
                           (* meth-idx.94 8)))))]
         [anon$129 (lambda (cp.156 self.91 val.90)
                     (let ()
                       (begin
                         (let ([uv.157 (mref (mref cp.156 6) -1)])
                           ((mref uv.157 -2)
                             uv.157
                             (let ([uv.158 (mref (mref cp.156 14) -1)])
                               ((mref uv.158 -2) uv.158 self.91 128))
                             (* (let ([uv.159 (mref (mref cp.156 14) -1)])
                                  ((mref uv.159 -2) uv.159 self.91 64))
                                8)
                             val.90))
                         (let ([uv.160 (mref (mref cp.156 6) -1)])
                           ((mref uv.160 -2)
                             uv.160
                             self.91
                             64
                             (+ (let ([uv.161 (mref (mref cp.156 14) -1)])
                                  ((mref uv.161 -2) uv.161 self.91 64))
                                8)))
                         self.91)))]
         [anon$130 (lambda (cp.150 self.89)
                     (let ()
                       (begin
                         (let ([uv.151 (mref (mref cp.150 6) -1)])
                           ((mref uv.151 -2)
                             uv.151
                             self.89
                             64
                             (- (let ([uv.152 (mref (mref cp.150 14) -1)])
                                  ((mref uv.152 -2) uv.152 64 self.89))
                                8)))
                         (let ([uv.153 (mref (mref cp.150 14) -1)])
                           ((mref uv.153 -2)
                             uv.153
                             (let ([uv.154 (mref (mref cp.150 14) -1)])
                               ((mref uv.154 -2) uv.154 self.89 128))
                             (* (let ([uv.155 (mref (mref cp.150 14) -1)])
                                  ((mref uv.155 -2) uv.155 self.89 64))
                                8))))))]
         [anon$131 (lambda (cp.146 self.88)
                     (let ()
                       (let ([uv.147 (mref (mref cp.146 6) -1)])
                         ((mref uv.147 -2)
                           uv.147
                           (let ([uv.148 (mref (mref cp.146 6) -1)])
                             ((mref uv.148 -2) uv.148 self.88 128))
                           (* (- (let ([uv.149 (mref (mref cp.146 6) -1)])
                                   ((mref uv.149 -2) uv.149 64 self.88))
                                 8)
                              8)))))]
         [anon$126 (lambda (cp.144 n.84)
                     (let ()
                       (let ([tmp.249 (let ([uv.145 (mref
                                                      (mref cp.144 6)
                                                      -1)])
                                        ((mref uv.145 -2)
                                          uv.145
                                          n.84
                                          64))])
                         (let ([tmp.250 (+ (alloc
                                             (+ 8
                                                (let ([uv.145 (mref
                                                                (mref
                                                                  cp.144
                                                                  6)
                                                                -1)])
                                                  ((mref uv.145 -2)
                                                    uv.145
                                                    n.84
                                                    64))))
                                           3)])
                           (begin (mset! tmp.250 -3 tmp.249) tmp.250)))))]
         [anon$127 (lambda (cp.141 x.83 y.82)
                     (let ()
                       (if (= (logand x.83 7) 3)
                           (mref
                             x.83
                             (+ 5
                                (let ([uv.142 (mref (mref cp.141 6) -1)])
                                  ((mref uv.142 -2) uv.142 y.82 64))))
                           (mref
                             y.82
                             (+ 5
                                (let ([uv.143 (mref (mref cp.141 6) -1)])
                                  ((mref uv.143 -2) uv.143 x.83 64)))))))]
         [anon$128 (lambda (cp.138 x.81 y.80 z.79)
                     (let ()
                       (begin
                         (if (= (logand x.81 7) 3)
                             (mset!
                               x.81
                               (+ 5
                                  (let ([uv.139 (mref (mref cp.138 6) -1)])
                                    ((mref uv.139 -2) uv.139 y.80 64)))
                               z.79)
                             (mset!
                               y.80
                               (+ 5
                                  (let ([uv.140 (mref (mref cp.138 6) -1)])
                                    ((mref uv.140 -2) uv.140 x.81 64)))
                               z.79))
                         (if (if (= 6 6) (false) (true)) 6 30))))]
         [anon$124 (lambda (cp.134 d.71 n.70)
                     (let ()
                       (let ([tmp.115 30])
                         (let ([f.72 (let ([tmp-car.251 tmp.115]
                                           [tmp-cdr.252 30])
                                       (let ([tmp.253 (+ (alloc 16) 1)])
                                         (begin
                                           (mset! tmp.253 -1 tmp-car.251)
                                           (mset! tmp.253 7 tmp-cdr.252)
                                           tmp.253)))])
                           (begin
                             (let ([tmp.105 (let ([anon.125 (let ([tmp.254 (+ (alloc
                                                                                16)
                                                                              2)])
                                                              (begin
                                                                (mset!
                                                                  tmp.254
                                                                  -2
                                                                  anon$125)
                                                                tmp.254))])
                                              (begin
                                                (mset! anon.125 6 f.72)
                                                anon.125))])
                               (let () (mset! f.72 -1 tmp.105)))
                             (let ([uv.137 (mref f.72 -1)])
                               ((mref uv.137 -2) uv.137 d.71 n.70 0)))))))]
         [anon$125 (lambda (cp.135 d.75 n.74 q.73)
                     (let ()
                       (if (> n.74 d.75)
                           q.73
                           (let ([uv.136 (mref (mref cp.135 6) -1)])
                             ((mref uv.136 -2)
                               uv.136
                               (- d.75 n.74)
                               n.74
                               (+ q.73 8))))))])
  (let ([tmp.114 30])
    (let ([div.69 (let ([tmp-car.255 tmp.114] [tmp-cdr.256 30])
                    (let ([tmp.257 (+ (alloc 16) 1)])
                      (begin
                        (mset! tmp.257 -1 tmp-car.255)
                        (mset! tmp.257 7 tmp-cdr.256)
                        tmp.257)))])
      (begin
        (let ([tmp.104 (let ([anon.124 (let ([tmp.258 (+ (alloc 8)
                                                         2)])
                                         (begin
                                           (mset! tmp.258 -2 anon$124)
                                           tmp.258))])
                         anon.124)])
          (mset! div.69 -1 tmp.104))
        (let ([tmp.118 30] [tmp.117 30] [tmp.116 30])
          (let ([mset!.76 (let ([tmp-car.259 tmp.118]
                                [tmp-cdr.260 30])
                            (let ([tmp.261 (+ (alloc 16) 1)])
                              (begin
                                (mset! tmp.261 -1 tmp-car.259)
                                (mset! tmp.261 7 tmp-cdr.260)
                                tmp.261)))]
                [mref.77 (let ([tmp-car.262 tmp.117] [tmp-cdr.263 30])
                           (let ([tmp.264 (+ (alloc 16) 1)])
                             (begin
                               (mset! tmp.264 -1 tmp-car.262)
                               (mset! tmp.264 7 tmp-cdr.263)
                               tmp.264)))]
                [alloc.78 (let ([tmp-car.265 tmp.116] [tmp-cdr.266 30])
                            (let ([tmp.267 (+ (alloc 16) 1)])
                              (begin
                                (mset! tmp.267 -1 tmp-car.265)
                                (mset! tmp.267 7 tmp-cdr.266)
                                tmp.267)))])
            (begin
              (let ([tmp.108 (let ([anon.128 (let ([tmp.268 (+ (alloc 16)
                                                               2)])
                                               (begin
                                                 (mset!
                                                   tmp.268
                                                   -2
                                                   anon$128)
                                                 tmp.268))])
                               (begin (mset! anon.128 6 div.69) anon.128))]
                    [tmp.107 (let ([anon.127 (let ([tmp.269 (+ (alloc 16)
                                                               2)])
                                               (begin
                                                 (mset!
                                                   tmp.269
                                                   -2
                                                   anon$127)
                                                 tmp.269))])
                               (begin (mset! anon.127 6 div.69) anon.127))]
                    [tmp.106 (let ([anon.126 (let ([tmp.270 (+ (alloc 16)
                                                               2)])
                                               (begin
                                                 (mset!
                                                   tmp.270
                                                   -2
                                                   anon$126)
                                                 tmp.270))])
                               (begin
                                 (mset! anon.126 6 div.69)
                                 anon.126))])
                (begin
                  (mset! alloc.78 -1 tmp.106)
                  (mset! mref.77 -1 tmp.107)
                  (mset! mset!.76 -1 tmp.108)))
              (let ([tmp.121 30] [tmp.120 30] [tmp.119 30])
                (let ([stack-top.85 (let ([tmp-car.271 tmp.121]
                                          [tmp-cdr.272 30])
                                      (let ([tmp.273 (+ (alloc 16) 1)])
                                        (begin
                                          (mset! tmp.273 -1 tmp-car.271)
                                          (mset! tmp.273 7 tmp-cdr.272)
                                          tmp.273)))]
                      [stack-pop.86 (let ([tmp-car.274 tmp.120]
                                          [tmp-cdr.275 30])
                                      (let ([tmp.276 (+ (alloc 16) 1)])
                                        (begin
                                          (mset! tmp.276 -1 tmp-car.274)
                                          (mset! tmp.276 7 tmp-cdr.275)
                                          tmp.276)))]
                      [stack-push.87 (let ([tmp-car.277 tmp.119]
                                           [tmp-cdr.278 30])
                                       (let ([tmp.279 (+ (alloc 16) 1)])
                                         (begin
                                           (mset! tmp.279 -1 tmp-car.277)
                                           (mset! tmp.279 7 tmp-cdr.278)
                                           tmp.279)))])
                  (begin
                    (let ([tmp.111 (let ([anon.131 (let ([tmp.280 (+ (alloc
                                                                       16)
                                                                     2)])
                                                     (begin
                                                       (mset!
                                                         tmp.280
                                                         -2
                                                         anon$131)
                                                       tmp.280))])
                                     (begin
                                       (mset! anon.131 6 mref.77)
                                       anon.131))]
                          [tmp.110 (let ([anon.130 (let ([tmp.281 (+ (alloc
                                                                       24)
                                                                     2)])
                                                     (begin
                                                       (mset!
                                                         tmp.281
                                                         -2
                                                         anon$130)
                                                       tmp.281))])
                                     (begin
                                       (mset! anon.130 14 mref.77)
                                       (mset! anon.130 6 mset!.76)
                                       anon.130))]
                          [tmp.109 (let ([anon.129 (let ([tmp.282 (+ (alloc
                                                                       24)
                                                                     2)])
                                                     (begin
                                                       (mset!
                                                         tmp.282
                                                         -2
                                                         anon$129)
                                                       tmp.282))])
                                     (begin
                                       (mset! anon.129 14 mref.77)
                                       (mset! anon.129 6 mset!.76)
                                       anon.129))])
                      (begin
                        (mset! stack-push.87 -1 tmp.109)
                        (mset! stack-pop.86 -1 tmp.110)
                        (mset! stack-top.85 -1 tmp.111)))
                    (let ([tmp.123 30] [tmp.122 30])
                      (let ([invoke.92 (let ([tmp-car.283 tmp.123]
                                             [tmp-cdr.284 30])
                                         (let ([tmp.285 (+ (alloc 16) 1)])
                                           (begin
                                             (mset! tmp.285 -1 tmp-car.283)
                                             (mset! tmp.285 7 tmp-cdr.284)
                                             tmp.285)))]
                            [stack-new.93 (let ([tmp-car.286 tmp.122]
                                                [tmp-cdr.287 30])
                                            (let ([tmp.288 (+ (alloc 16)
                                                              1)])
                                              (begin
                                                (mset!
                                                  tmp.288
                                                  -1
                                                  tmp-car.286)
                                                (mset!
                                                  tmp.288
                                                  7
                                                  tmp-cdr.287)
                                                tmp.288)))])
                        (begin
                          (let ([tmp.113 (let ([anon.133 (let ([tmp.289 (+ (alloc
                                                                             16)
                                                                           2)])
                                                           (begin
                                                             (mset!
                                                               tmp.289
                                                               -2
                                                               anon$133)
                                                             tmp.289))])
                                           (begin
                                             (mset! anon.133 6 mref.77)
                                             anon.133))]
                                [tmp.112 (let ([meths.96 (let ([uv.165 (mref
                                                                         alloc.78
                                                                         -1)])
                                                           ((mref
                                                              uv.165
                                                              -2)
                                                             uv.165
                                                             192))])
                                           (let ()
                                             (begin
                                               (let ([uv.166 (mref
                                                               mset!.76
                                                               -1)])
                                                 ((mref uv.166 -2)
                                                   uv.166
                                                   meths.96
                                                   0
                                                   (mref
                                                     stack-push.87
                                                     -1)))
                                               (let ([uv.167 (mref
                                                               mset!.76
                                                               -1)])
                                                 ((mref uv.167 -2)
                                                   uv.167
                                                   meths.96
                                                   64
                                                   (mref stack-pop.86 -1)))
                                               (let ([uv.168 (mref
                                                               mset!.76
                                                               -1)])
                                                 ((mref uv.168 -2)
                                                   uv.168
                                                   meths.96
                                                   128
                                                   (mref stack-top.85 -1)))
                                               (let ([anon.132 (let ([tmp.290 (+ (alloc
                                                                                   32)
                                                                                 2)])
                                                                 (begin
                                                                   (mset!
                                                                     tmp.290
                                                                     -2
                                                                     anon$132)
                                                                   tmp.290))])
                                                 (begin
                                                   (mset!
                                                     anon.132
                                                     22
                                                     alloc.78)
                                                   (mset!
                                                     anon.132
                                                     14
                                                     mset!.76)
                                                   (mset!
                                                     anon.132
                                                     6
                                                     meths.96)
                                                   anon.132)))))])
                            (begin
                              (mset! stack-new.93 -1 tmp.112)
                              (mset! invoke.92 -1 tmp.113)))
                          (let ([s1.99 (let ([uv.175 (mref
                                                       stack-new.93
                                                       -1)])
                                         ((mref uv.175 -2) uv.175 80))])
                            (begin
                              (let ([uv.176 (let ([uv.177 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.177 -2)
                                                uv.177
                                                s1.99
                                                0))])
                                ((mref uv.176 -2) uv.176 s1.99 80))
                              (let ([uv.178 (let ([uv.179 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.179 -2)
                                                uv.179
                                                s1.99
                                                0))])
                                ((mref uv.178 -2) uv.178 s1.99 160))
                              (let ([uv.180 (let ([uv.181 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.181 -2)
                                                uv.181
                                                s1.99
                                                0))])
                                ((mref uv.180 -2) uv.180 s1.99 240))
                              (let ([uv.182 (let ([uv.183 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.183 -2)
                                                uv.183
                                                s1.99
                                                0))])
                                ((mref uv.182 -2) uv.182 s1.99 320))
                              (let ([uv.184 (let ([uv.185 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.185 -2)
                                                uv.185
                                                s1.99
                                                0))])
                                ((mref uv.184 -2) uv.184 s1.99 0))
                              (let ([uv.186 (let ([uv.187 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.187 -2)
                                                uv.187
                                                s1.99
                                                0))])
                                ((mref uv.186 -2) uv.186 s1.99 480))
                              (let ([uv.188 (let ([uv.189 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.189 -2)
                                                uv.189
                                                s1.99
                                                0))])
                                ((mref uv.188 -2) uv.188 s1.99 560))
                              (let ([uv.190 (let ([uv.191 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.191 -2)
                                                uv.191
                                                s1.99
                                                0))])
                                ((mref uv.190 -2) uv.190 s1.99 640))
                              (let ([uv.192 (let ([uv.193 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.193 -2)
                                                uv.193
                                                s1.99
                                                0))])
                                ((mref uv.192 -2) uv.192 s1.99 720))
                              (let ([uv.194 (let ([uv.195 (mref
                                                            invoke.92
                                                            -1)])
                                              ((mref uv.195 -2)
                                                uv.195
                                                s1.99
                                                0))])
                                ((mref uv.194 -2) uv.194 s1.99 800))
                              (let ([s2.100 (let ([uv.196 (mref
                                                            stack-new.93
                                                            -1)])
                                              ((mref uv.196 -2)
                                                uv.196
                                                48))])
                                (begin
                                  (let ([uv.197 (let ([uv.198 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.198 -2)
                                                    uv.198
                                                    s2.100
                                                    0))])
                                    ((mref uv.197 -2)
                                      uv.197
                                      s2.100
                                      (let ([uv.199 (let ([uv.200 (mref
                                                                    invoke.92
                                                                    -1)])
                                                      ((mref uv.200 -2)
                                                        uv.200
                                                        s1.99
                                                        8))])
                                        ((mref uv.199 -2) uv.199 s1.99))))
                                  (let ([uv.201 (let ([uv.202 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.202 -2)
                                                    uv.202
                                                    s1.99
                                                    8))])
                                    ((mref uv.201 -2) uv.201 s1.99))
                                  (let ([uv.203 (let ([uv.204 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.204 -2)
                                                    uv.204
                                                    s2.100
                                                    0))])
                                    ((mref uv.203 -2)
                                      uv.203
                                      s2.100
                                      (let ([uv.205 (let ([uv.206 (mref
                                                                    invoke.92
                                                                    -1)])
                                                      ((mref uv.206 -2)
                                                        uv.206
                                                        s1.99
                                                        8))])
                                        ((mref uv.205 -2) uv.205 s1.99))))
                                  (let ([uv.207 (let ([uv.208 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.208 -2)
                                                    uv.208
                                                    s1.99
                                                    8))])
                                    ((mref uv.207 -2) uv.207 s1.99))
                                  (let ([uv.209 (let ([uv.210 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.210 -2)
                                                    uv.210
                                                    s2.100
                                                    0))])
                                    ((mref uv.209 -2)
                                      uv.209
                                      s2.100
                                      (let ([uv.211 (let ([uv.212 (mref
                                                                    invoke.92
                                                                    -1)])
                                                      ((mref uv.212 -2)
                                                        uv.212
                                                        s1.99
                                                        8))])
                                        ((mref uv.211 -2) uv.211 s1.99))))
                                  (let ([uv.213 (let ([uv.214 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.214 -2)
                                                    uv.214
                                                    s1.99
                                                    8))])
                                    ((mref uv.213 -2) uv.213 s1.99))
                                  (let ([uv.215 (let ([uv.216 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.216 -2)
                                                    uv.216
                                                    s2.100
                                                    0))])
                                    ((mref uv.215 -2)
                                      uv.215
                                      s2.100
                                      (let ([uv.217 (let ([uv.218 (mref
                                                                    invoke.92
                                                                    -1)])
                                                      ((mref uv.218 -2)
                                                        uv.218
                                                        s1.99
                                                        8))])
                                        ((mref uv.217 -2) uv.217 s1.99))))
                                  (let ([uv.219 (let ([uv.220 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.220 -2)
                                                    uv.220
                                                    s1.99
                                                    8))])
                                    ((mref uv.219 -2) uv.219 s1.99))
                                  (let ([uv.221 (let ([uv.222 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.222 -2)
                                                    uv.222
                                                    s2.100
                                                    0))])
                                    ((mref uv.221 -2)
                                      uv.221
                                      s2.100
                                      (let ([uv.223 (let ([uv.224 (mref
                                                                    invoke.92
                                                                    -1)])
                                                      ((mref uv.224 -2)
                                                        uv.224
                                                        s1.99
                                                        8))])
                                        ((mref uv.223 -2) uv.223 s1.99))))
                                  (let ([uv.225 (let ([uv.226 (mref
                                                                invoke.92
                                                                -1)])
                                                  ((mref uv.226 -2)
                                                    uv.226
                                                    s2.100
                                                    0))])
                                    ((mref uv.225 -2)
                                      uv.225
                                      s2.100
                                      (let ([uv.227 (let ([uv.228 (mref
                                                                    invoke.92
                                                                    -1)])
                                                      ((mref uv.228 -2)
                                                        uv.228
                                                        s1.99
                                                        8))])
                                        ((mref uv.227 -2) uv.227 s1.99))))
                                  (let ([x.101 (+ (let ([uv.229 (let ([uv.230 (mref
                                                                                invoke.92
                                                                                -1)])
                                                                  ((mref
                                                                     uv.230
                                                                     -2)
                                                                    uv.230
                                                                    s2.100
                                                                    8))])
                                                    ((mref uv.229 -2)
                                                      uv.229
                                                      s2.100))
                                                  (let ([uv.231 (let ([uv.232 (mref
                                                                                invoke.92
                                                                                -1)])
                                                                  ((mref
                                                                     uv.232
                                                                     -2)
                                                                    uv.232
                                                                    s2.100
                                                                    8))])
                                                    ((mref uv.231 -2)
                                                      uv.231
                                                      s2.100)))])
                                    (* (sra (+ (let ([x.103 (+ (let ([uv.233 (let ([uv.234 (mref
                                                                                             invoke.92
                                                                                             -1)])
                                                                               ((mref
                                                                                  uv.234
                                                                                  -2)
                                                                                 uv.234
                                                                                 s2.100
                                                                                 16))])
                                                                 ((mref
                                                                    uv.233
                                                                    -2)
                                                                   uv.233
                                                                   s2.100))
                                                               (let ([uv.235 (let ([uv.236 (mref
                                                                                             invoke.92
                                                                                             -1)])
                                                                               ((mref
                                                                                  uv.236
                                                                                  -2)
                                                                                 uv.236
                                                                                 s2.100
                                                                                 16))])
                                                                 ((mref
                                                                    uv.235
                                                                    -2)
                                                                   uv.235
                                                                   s2.100)))])
                                                 (- x.103
                                                    (+ (let ([uv.237 (let ([uv.238 (mref
                                                                                     invoke.92
                                                                                     -1)])
                                                                       ((mref
                                                                          uv.238
                                                                          -2)
                                                                         uv.238
                                                                         s2.100
                                                                         8))])
                                                         ((mref uv.237 -2)
                                                           uv.237
                                                           s2.100))
                                                       (let ([uv.239 (let ([uv.240 (mref
                                                                                     invoke.92
                                                                                     -1)])
                                                                       ((mref
                                                                          uv.240
                                                                          -2)
                                                                         uv.240
                                                                         s2.100
                                                                         8))])
                                                         ((mref uv.239 -2)
                                                           uv.239
                                                           s2.100)))))
                                               (let ([x.102 (+ (let ([uv.241 (let ([uv.242 (mref
                                                                                             invoke.92
                                                                                             -1)])
                                                                               ((mref
                                                                                  uv.242
                                                                                  -2)
                                                                                 uv.242
                                                                                 s2.100
                                                                                 16))])
                                                                 ((mref
                                                                    uv.241
                                                                    -2)
                                                                   uv.241
                                                                   s2.100))
                                                               (let ([uv.243 (let ([uv.244 (mref
                                                                                             invoke.92
                                                                                             -1)])
                                                                               ((mref
                                                                                  uv.244
                                                                                  -2)
                                                                                 uv.244
                                                                                 s2.100
                                                                                 16))])
                                                                 ((mref
                                                                    uv.243
                                                                    -2)
                                                                   uv.243
                                                                   s2.100)))])
                                                 (- (+ (let ([uv.245 (let ([uv.246 (mref
                                                                                     invoke.92
                                                                                     -1)])
                                                                       ((mref
                                                                          uv.246
                                                                          -2)
                                                                         uv.246
                                                                         s2.100
                                                                         8))])
                                                         ((mref uv.245 -2)
                                                           uv.245
                                                           s2.100))
                                                       (let ([uv.247 (let ([uv.248 (mref
                                                                                     invoke.92
                                                                                     -1)])
                                                                       ((mref
                                                                          uv.248
                                                                          -2)
                                                                         uv.248
                                                                         s2.100
                                                                         8))])
                                                         ((mref uv.247 -2)
                                                           uv.247
                                                           s2.100)))
                                                    x.102)))
                                            3)
                                       x.101)))))))))))))))))))
========
uncover-locals output:
(letrec ([anon$132 (lambda (cp.169 size.97)
                     (locals (self.98 uv.170 uv.171 uv.172 uv.173 uv.174)
                       (let ()
                         (let ([self.98 (let ([uv.170 (mref
                                                        (mref cp.169 22)
                                                        -1)])
                                          ((mref uv.170 -2) uv.170 192))])
                           (let ()
                             (begin
                               (let ([uv.171 (mref (mref cp.169 14) -1)])
                                 ((mref uv.171 -2)
                                   uv.171
                                   self.98
                                   0
                                   (mref cp.169 6)))
                               (let ([uv.172 (mref (mref cp.169 14) -1)])
                                 ((mref uv.172 -2) uv.172 self.98 64 0))
                               (let ([uv.173 (mref (mref cp.169 14) -1)])
                                 ((mref uv.173 -2)
                                   uv.173
                                   self.98
                                   128
                                   (let ([uv.174 (mref
                                                   (mref cp.169 22)
                                                   -1)])
                                     ((mref uv.174 -2)
                                       uv.174
                                       (* 8 size.97)))))
                               self.98))))))]
         [anon$133 (lambda (cp.162 obj.95 meth-idx.94)
                     (locals (uv.163 uv.164)
                       (let ()
                         (let ([uv.163 (mref (mref cp.162 6) -1)])
                           ((mref uv.163 -2)
                             uv.163
                             (let ([uv.164 (mref (mref cp.162 6) -1)])
                               ((mref uv.164 -2) uv.164 obj.95 0))
                             (* meth-idx.94 8))))))]
         [anon$129 (lambda (cp.156 self.91 val.90)
                     (locals (uv.157 uv.158 uv.159 uv.160 uv.161)
                       (let ()
                         (begin
                           (let ([uv.157 (mref (mref cp.156 6) -1)])
                             ((mref uv.157 -2)
                               uv.157
                               (let ([uv.158 (mref (mref cp.156 14) -1)])
                                 ((mref uv.158 -2) uv.158 self.91 128))
                               (* (let ([uv.159 (mref
                                                  (mref cp.156 14)
                                                  -1)])
                                    ((mref uv.159 -2) uv.159 self.91 64))
                                  8)
                               val.90))
                           (let ([uv.160 (mref (mref cp.156 6) -1)])
                             ((mref uv.160 -2)
                               uv.160
                               self.91
                               64
                               (+ (let ([uv.161 (mref
                                                  (mref cp.156 14)
                                                  -1)])
                                    ((mref uv.161 -2) uv.161 self.91 64))
                                  8)))
                           self.91))))]
         [anon$130 (lambda (cp.150 self.89)
                     (locals (uv.151 uv.152 uv.153 uv.154 uv.155)
                       (let ()
                         (begin
                           (let ([uv.151 (mref (mref cp.150 6) -1)])
                             ((mref uv.151 -2)
                               uv.151
                               self.89
                               64
                               (- (let ([uv.152 (mref
                                                  (mref cp.150 14)
                                                  -1)])
                                    ((mref uv.152 -2) uv.152 64 self.89))
                                  8)))
                           (let ([uv.153 (mref (mref cp.150 14) -1)])
                             ((mref uv.153 -2)
                               uv.153
                               (let ([uv.154 (mref (mref cp.150 14) -1)])
                                 ((mref uv.154 -2) uv.154 self.89 128))
                               (* (let ([uv.155 (mref
                                                  (mref cp.150 14)
                                                  -1)])
                                    ((mref uv.155 -2) uv.155 self.89 64))
                                  8)))))))]
         [anon$131 (lambda (cp.146 self.88)
                     (locals (uv.147 uv.148 uv.149)
                       (let ()
                         (let ([uv.147 (mref (mref cp.146 6) -1)])
                           ((mref uv.147 -2)
                             uv.147
                             (let ([uv.148 (mref (mref cp.146 6) -1)])
                               ((mref uv.148 -2) uv.148 self.88 128))
                             (* (- (let ([uv.149 (mref
                                                   (mref cp.146 6)
                                                   -1)])
                                     ((mref uv.149 -2) uv.149 64 self.88))
                                   8)
                                8))))))]
         [anon$126 (lambda (cp.144 n.84)
                     (locals (tmp.249 uv.145 tmp.250 uv.145)
                       (let ()
                         (let ([tmp.249 (let ([uv.145 (mref
                                                        (mref cp.144 6)
                                                        -1)])
                                          ((mref uv.145 -2)
                                            uv.145
                                            n.84
                                            64))])
                           (let ([tmp.250 (+ (alloc
                                               (+ 8
                                                  (let ([uv.145 (mref
                                                                  (mref
                                                                    cp.144
                                                                    6)
                                                                  -1)])
                                                    ((mref uv.145 -2)
                                                      uv.145
                                                      n.84
                                                      64))))
                                             3)])
                             (begin
                               (mset! tmp.250 -3 tmp.249)
                               tmp.250))))))]
         [anon$127 (lambda (cp.141 x.83 y.82)
                     (locals (uv.142 uv.143)
                       (let ()
                         (if (= (logand x.83 7) 3)
                             (mref
                               x.83
                               (+ 5
                                  (let ([uv.142 (mref (mref cp.141 6) -1)])
                                    ((mref uv.142 -2) uv.142 y.82 64))))
                             (mref
                               y.82
                               (+ 5
                                  (let ([uv.143 (mref (mref cp.141 6) -1)])
                                    ((mref uv.143 -2)
                                      uv.143
                                      x.83
                                      64))))))))]
         [anon$128 (lambda (cp.138 x.81 y.80 z.79)
                     (locals (uv.139 uv.140)
                       (let ()
                         (begin
                           (if (= (logand x.81 7) 3)
                               (mset!
                                 x.81
                                 (+ 5
                                    (let ([uv.139 (mref
                                                    (mref cp.138 6)
                                                    -1)])
                                      ((mref uv.139 -2) uv.139 y.80 64)))
                                 z.79)
                               (mset!
                                 y.80
                                 (+ 5
                                    (let ([uv.140 (mref
                                                    (mref cp.138 6)
                                                    -1)])
                                      ((mref uv.140 -2) uv.140 x.81 64)))
                                 z.79))
                           (if (if (= 6 6) (false) (true)) 6 30)))))]
         [anon$124 (lambda (cp.134 d.71 n.70)
                     (locals (tmp.115 f.72 tmp-car.251 tmp-cdr.252 tmp.253 tmp.105 anon.125 tmp.254 uv.137)
                       (let ()
                         (let ([tmp.115 30])
                           (let ([f.72 (let ([tmp-car.251 tmp.115]
                                             [tmp-cdr.252 30])
                                         (let ([tmp.253 (+ (alloc 16) 1)])
                                           (begin
                                             (mset! tmp.253 -1 tmp-car.251)
                                             (mset! tmp.253 7 tmp-cdr.252)
                                             tmp.253)))])
                             (begin
                               (let ([tmp.105 (let ([anon.125 (let ([tmp.254 (+ (alloc
                                                                                  16)
                                                                                2)])
                                                                (begin
                                                                  (mset!
                                                                    tmp.254
                                                                    -2
                                                                    anon$125)
                                                                  tmp.254))])
                                                (begin
                                                  (mset! anon.125 6 f.72)
                                                  anon.125))])
                                 (let () (mset! f.72 -1 tmp.105)))
                               (let ([uv.137 (mref f.72 -1)])
                                 ((mref uv.137 -2)
                                   uv.137
                                   d.71
                                   n.70
                                   0))))))))]
         [anon$125 (lambda (cp.135 d.75 n.74 q.73)
                     (locals (uv.136)
                       (let ()
                         (if (> n.74 d.75)
                             q.73
                             (let ([uv.136 (mref (mref cp.135 6) -1)])
                               ((mref uv.136 -2)
                                 uv.136
                                 (- d.75 n.74)
                                 n.74
                                 (+ q.73 8)))))))])
  (locals (tmp.114 div.69 tmp-car.255 tmp-cdr.256 tmp.257 tmp.104 anon.124 tmp.258 tmp.118 tmp.117 tmp.116 mset!.76 tmp-car.259 tmp-cdr.260 tmp.261 mref.77 tmp-car.262 tmp-cdr.263 tmp.264 alloc.78 tmp-car.265 tmp-cdr.266 tmp.267 tmp.108 anon.128 tmp.268 tmp.107 anon.127 tmp.269 tmp.106 anon.126 tmp.270 tmp.121 tmp.120 tmp.119 stack-top.85 tmp-car.271 tmp-cdr.272 tmp.273 stack-pop.86 tmp-car.274 tmp-cdr.275 tmp.276 stack-push.87 tmp-car.277 tmp-cdr.278 tmp.279 tmp.111 anon.131 tmp.280 tmp.110 anon.130 tmp.281 tmp.109 anon.129 tmp.282 tmp.123 tmp.122 invoke.92 tmp-car.283 tmp-cdr.284 tmp.285 stack-new.93 tmp-car.286 tmp-cdr.287 tmp.288 tmp.113 anon.133 tmp.289 tmp.112 meths.96 uv.165 uv.166 uv.167 uv.168 anon.132 tmp.290 s1.99 uv.175 uv.176 uv.177 uv.178 uv.179 uv.180 uv.181 uv.182 uv.183 uv.184 uv.185 uv.186 uv.187 uv.188 uv.189 uv.190 uv.191 uv.192 uv.193 uv.194 uv.195 s2.100 uv.196 uv.197 uv.198 uv.199 uv.200 uv.201 uv.202 uv.203 uv.204 uv.205 uv.206 uv.207 uv.208 uv.209 uv.210 uv.211 uv.212 uv.213 uv.214 uv.215 uv.216 uv.217 uv.218 uv.219 uv.220 uv.221 uv.222 uv.223 uv.224 uv.225 uv.226 uv.227 uv.228 x.101 uv.229 uv.230 uv.231 uv.232 x.103 uv.233 uv.234 uv.235 uv.236 uv.237 uv.238 uv.239 uv.240 x.102 uv.241 uv.242 uv.243 uv.244 uv.245 uv.246 uv.247 uv.248)
    (let ([tmp.114 30])
      (let ([div.69 (let ([tmp-car.255 tmp.114] [tmp-cdr.256 30])
                      (let ([tmp.257 (+ (alloc 16) 1)])
                        (begin
                          (mset! tmp.257 -1 tmp-car.255)
                          (mset! tmp.257 7 tmp-cdr.256)
                          tmp.257)))])
        (begin
          (let ([tmp.104 (let ([anon.124 (let ([tmp.258 (+ (alloc 8)
                                                           2)])
                                           (begin
                                             (mset! tmp.258 -2 anon$124)
                                             tmp.258))])
                           anon.124)])
            (mset! div.69 -1 tmp.104))
          (let ([tmp.118 30] [tmp.117 30] [tmp.116 30])
            (let ([mset!.76 (let ([tmp-car.259 tmp.118]
                                  [tmp-cdr.260 30])
                              (let ([tmp.261 (+ (alloc 16) 1)])
                                (begin
                                  (mset! tmp.261 -1 tmp-car.259)
                                  (mset! tmp.261 7 tmp-cdr.260)
                                  tmp.261)))]
                  [mref.77 (let ([tmp-car.262 tmp.117] [tmp-cdr.263 30])
                             (let ([tmp.264 (+ (alloc 16) 1)])
                               (begin
                                 (mset! tmp.264 -1 tmp-car.262)
                                 (mset! tmp.264 7 tmp-cdr.263)
                                 tmp.264)))]
                  [alloc.78 (let ([tmp-car.265 tmp.116] [tmp-cdr.266 30])
                              (let ([tmp.267 (+ (alloc 16) 1)])
                                (begin
                                  (mset! tmp.267 -1 tmp-car.265)
                                  (mset! tmp.267 7 tmp-cdr.266)
                                  tmp.267)))])
              (begin
                (let ([tmp.108 (let ([anon.128 (let ([tmp.268 (+ (alloc 16)
                                                                 2)])
                                                 (begin
                                                   (mset!
                                                     tmp.268
                                                     -2
                                                     anon$128)
                                                   tmp.268))])
                                 (begin
                                   (mset! anon.128 6 div.69)
                                   anon.128))]
                      [tmp.107 (let ([anon.127 (let ([tmp.269 (+ (alloc 16)
                                                                 2)])
                                                 (begin
                                                   (mset!
                                                     tmp.269
                                                     -2
                                                     anon$127)
                                                   tmp.269))])
                                 (begin
                                   (mset! anon.127 6 div.69)
                                   anon.127))]
                      [tmp.106 (let ([anon.126 (let ([tmp.270 (+ (alloc 16)
                                                                 2)])
                                                 (begin
                                                   (mset!
                                                     tmp.270
                                                     -2
                                                     anon$126)
                                                   tmp.270))])
                                 (begin
                                   (mset! anon.126 6 div.69)
                                   anon.126))])
                  (begin
                    (mset! alloc.78 -1 tmp.106)
                    (mset! mref.77 -1 tmp.107)
                    (mset! mset!.76 -1 tmp.108)))
                (let ([tmp.121 30] [tmp.120 30] [tmp.119 30])
                  (let ([stack-top.85 (let ([tmp-car.271 tmp.121]
                                            [tmp-cdr.272 30])
                                        (let ([tmp.273 (+ (alloc 16) 1)])
                                          (begin
                                            (mset! tmp.273 -1 tmp-car.271)
                                            (mset! tmp.273 7 tmp-cdr.272)
                                            tmp.273)))]
                        [stack-pop.86 (let ([tmp-car.274 tmp.120]
                                            [tmp-cdr.275 30])
                                        (let ([tmp.276 (+ (alloc 16) 1)])
                                          (begin
                                            (mset! tmp.276 -1 tmp-car.274)
                                            (mset! tmp.276 7 tmp-cdr.275)
                                            tmp.276)))]
                        [stack-push.87 (let ([tmp-car.277 tmp.119]
                                             [tmp-cdr.278 30])
                                         (let ([tmp.279 (+ (alloc 16) 1)])
                                           (begin
                                             (mset! tmp.279 -1 tmp-car.277)
                                             (mset! tmp.279 7 tmp-cdr.278)
                                             tmp.279)))])
                    (begin
                      (let ([tmp.111 (let ([anon.131 (let ([tmp.280 (+ (alloc
                                                                         16)
                                                                       2)])
                                                       (begin
                                                         (mset!
                                                           tmp.280
                                                           -2
                                                           anon$131)
                                                         tmp.280))])
                                       (begin
                                         (mset! anon.131 6 mref.77)
                                         anon.131))]
                            [tmp.110 (let ([anon.130 (let ([tmp.281 (+ (alloc
                                                                         24)
                                                                       2)])
                                                       (begin
                                                         (mset!
                                                           tmp.281
                                                           -2
                                                           anon$130)
                                                         tmp.281))])
                                       (begin
                                         (mset! anon.130 14 mref.77)
                                         (mset! anon.130 6 mset!.76)
                                         anon.130))]
                            [tmp.109 (let ([anon.129 (let ([tmp.282 (+ (alloc
                                                                         24)
                                                                       2)])
                                                       (begin
                                                         (mset!
                                                           tmp.282
                                                           -2
                                                           anon$129)
                                                         tmp.282))])
                                       (begin
                                         (mset! anon.129 14 mref.77)
                                         (mset! anon.129 6 mset!.76)
                                         anon.129))])
                        (begin
                          (mset! stack-push.87 -1 tmp.109)
                          (mset! stack-pop.86 -1 tmp.110)
                          (mset! stack-top.85 -1 tmp.111)))
                      (let ([tmp.123 30] [tmp.122 30])
                        (let ([invoke.92 (let ([tmp-car.283 tmp.123]
                                               [tmp-cdr.284 30])
                                           (let ([tmp.285 (+ (alloc 16)
                                                             1)])
                                             (begin
                                               (mset!
                                                 tmp.285
                                                 -1
                                                 tmp-car.283)
                                               (mset!
                                                 tmp.285
                                                 7
                                                 tmp-cdr.284)
                                               tmp.285)))]
                              [stack-new.93 (let ([tmp-car.286 tmp.122]
                                                  [tmp-cdr.287 30])
                                              (let ([tmp.288 (+ (alloc 16)
                                                                1)])
                                                (begin
                                                  (mset!
                                                    tmp.288
                                                    -1
                                                    tmp-car.286)
                                                  (mset!
                                                    tmp.288
                                                    7
                                                    tmp-cdr.287)
                                                  tmp.288)))])
                          (begin
                            (let ([tmp.113 (let ([anon.133 (let ([tmp.289 (+ (alloc
                                                                               16)
                                                                             2)])
                                                             (begin
                                                               (mset!
                                                                 tmp.289
                                                                 -2
                                                                 anon$133)
                                                               tmp.289))])
                                             (begin
                                               (mset! anon.133 6 mref.77)
                                               anon.133))]
                                  [tmp.112 (let ([meths.96 (let ([uv.165 (mref
                                                                           alloc.78
                                                                           -1)])
                                                             ((mref
                                                                uv.165
                                                                -2)
                                                               uv.165
                                                               192))])
                                             (let ()
                                               (begin
                                                 (let ([uv.166 (mref
                                                                 mset!.76
                                                                 -1)])
                                                   ((mref uv.166 -2)
                                                     uv.166
                                                     meths.96
                                                     0
                                                     (mref
                                                       stack-push.87
                                                       -1)))
                                                 (let ([uv.167 (mref
                                                                 mset!.76
                                                                 -1)])
                                                   ((mref uv.167 -2)
                                                     uv.167
                                                     meths.96
                                                     64
                                                     (mref
                                                       stack-pop.86
                                                       -1)))
                                                 (let ([uv.168 (mref
                                                                 mset!.76
                                                                 -1)])
                                                   ((mref uv.168 -2)
                                                     uv.168
                                                     meths.96
                                                     128
                                                     (mref
                                                       stack-top.85
                                                       -1)))
                                                 (let ([anon.132 (let ([tmp.290 (+ (alloc
                                                                                     32)
                                                                                   2)])
                                                                   (begin
                                                                     (mset!
                                                                       tmp.290
                                                                       -2
                                                                       anon$132)
                                                                     tmp.290))])
                                                   (begin
                                                     (mset!
                                                       anon.132
                                                       22
                                                       alloc.78)
                                                     (mset!
                                                       anon.132
                                                       14
                                                       mset!.76)
                                                     (mset!
                                                       anon.132
                                                       6
                                                       meths.96)
                                                     anon.132)))))])
                              (begin
                                (mset! stack-new.93 -1 tmp.112)
                                (mset! invoke.92 -1 tmp.113)))
                            (let ([s1.99 (let ([uv.175 (mref
                                                         stack-new.93
                                                         -1)])
                                           ((mref uv.175 -2) uv.175 80))])
                              (begin
                                (let ([uv.176 (let ([uv.177 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.177 -2)
                                                  uv.177
                                                  s1.99
                                                  0))])
                                  ((mref uv.176 -2) uv.176 s1.99 80))
                                (let ([uv.178 (let ([uv.179 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.179 -2)
                                                  uv.179
                                                  s1.99
                                                  0))])
                                  ((mref uv.178 -2) uv.178 s1.99 160))
                                (let ([uv.180 (let ([uv.181 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.181 -2)
                                                  uv.181
                                                  s1.99
                                                  0))])
                                  ((mref uv.180 -2) uv.180 s1.99 240))
                                (let ([uv.182 (let ([uv.183 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.183 -2)
                                                  uv.183
                                                  s1.99
                                                  0))])
                                  ((mref uv.182 -2) uv.182 s1.99 320))
                                (let ([uv.184 (let ([uv.185 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.185 -2)
                                                  uv.185
                                                  s1.99
                                                  0))])
                                  ((mref uv.184 -2) uv.184 s1.99 0))
                                (let ([uv.186 (let ([uv.187 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.187 -2)
                                                  uv.187
                                                  s1.99
                                                  0))])
                                  ((mref uv.186 -2) uv.186 s1.99 480))
                                (let ([uv.188 (let ([uv.189 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.189 -2)
                                                  uv.189
                                                  s1.99
                                                  0))])
                                  ((mref uv.188 -2) uv.188 s1.99 560))
                                (let ([uv.190 (let ([uv.191 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.191 -2)
                                                  uv.191
                                                  s1.99
                                                  0))])
                                  ((mref uv.190 -2) uv.190 s1.99 640))
                                (let ([uv.192 (let ([uv.193 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.193 -2)
                                                  uv.193
                                                  s1.99
                                                  0))])
                                  ((mref uv.192 -2) uv.192 s1.99 720))
                                (let ([uv.194 (let ([uv.195 (mref
                                                              invoke.92
                                                              -1)])
                                                ((mref uv.195 -2)
                                                  uv.195
                                                  s1.99
                                                  0))])
                                  ((mref uv.194 -2) uv.194 s1.99 800))
                                (let ([s2.100 (let ([uv.196 (mref
                                                              stack-new.93
                                                              -1)])
                                                ((mref uv.196 -2)
                                                  uv.196
                                                  48))])
                                  (begin
                                    (let ([uv.197 (let ([uv.198 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.198 -2)
                                                      uv.198
                                                      s2.100
                                                      0))])
                                      ((mref uv.197 -2)
                                        uv.197
                                        s2.100
                                        (let ([uv.199 (let ([uv.200 (mref
                                                                      invoke.92
                                                                      -1)])
                                                        ((mref uv.200 -2)
                                                          uv.200
                                                          s1.99
                                                          8))])
                                          ((mref uv.199 -2)
                                            uv.199
                                            s1.99))))
                                    (let ([uv.201 (let ([uv.202 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.202 -2)
                                                      uv.202
                                                      s1.99
                                                      8))])
                                      ((mref uv.201 -2) uv.201 s1.99))
                                    (let ([uv.203 (let ([uv.204 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.204 -2)
                                                      uv.204
                                                      s2.100
                                                      0))])
                                      ((mref uv.203 -2)
                                        uv.203
                                        s2.100
                                        (let ([uv.205 (let ([uv.206 (mref
                                                                      invoke.92
                                                                      -1)])
                                                        ((mref uv.206 -2)
                                                          uv.206
                                                          s1.99
                                                          8))])
                                          ((mref uv.205 -2)
                                            uv.205
                                            s1.99))))
                                    (let ([uv.207 (let ([uv.208 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.208 -2)
                                                      uv.208
                                                      s1.99
                                                      8))])
                                      ((mref uv.207 -2) uv.207 s1.99))
                                    (let ([uv.209 (let ([uv.210 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.210 -2)
                                                      uv.210
                                                      s2.100
                                                      0))])
                                      ((mref uv.209 -2)
                                        uv.209
                                        s2.100
                                        (let ([uv.211 (let ([uv.212 (mref
                                                                      invoke.92
                                                                      -1)])
                                                        ((mref uv.212 -2)
                                                          uv.212
                                                          s1.99
                                                          8))])
                                          ((mref uv.211 -2)
                                            uv.211
                                            s1.99))))
                                    (let ([uv.213 (let ([uv.214 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.214 -2)
                                                      uv.214
                                                      s1.99
                                                      8))])
                                      ((mref uv.213 -2) uv.213 s1.99))
                                    (let ([uv.215 (let ([uv.216 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.216 -2)
                                                      uv.216
                                                      s2.100
                                                      0))])
                                      ((mref uv.215 -2)
                                        uv.215
                                        s2.100
                                        (let ([uv.217 (let ([uv.218 (mref
                                                                      invoke.92
                                                                      -1)])
                                                        ((mref uv.218 -2)
                                                          uv.218
                                                          s1.99
                                                          8))])
                                          ((mref uv.217 -2)
                                            uv.217
                                            s1.99))))
                                    (let ([uv.219 (let ([uv.220 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.220 -2)
                                                      uv.220
                                                      s1.99
                                                      8))])
                                      ((mref uv.219 -2) uv.219 s1.99))
                                    (let ([uv.221 (let ([uv.222 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.222 -2)
                                                      uv.222
                                                      s2.100
                                                      0))])
                                      ((mref uv.221 -2)
                                        uv.221
                                        s2.100
                                        (let ([uv.223 (let ([uv.224 (mref
                                                                      invoke.92
                                                                      -1)])
                                                        ((mref uv.224 -2)
                                                          uv.224
                                                          s1.99
                                                          8))])
                                          ((mref uv.223 -2)
                                            uv.223
                                            s1.99))))
                                    (let ([uv.225 (let ([uv.226 (mref
                                                                  invoke.92
                                                                  -1)])
                                                    ((mref uv.226 -2)
                                                      uv.226
                                                      s2.100
                                                      0))])
                                      ((mref uv.225 -2)
                                        uv.225
                                        s2.100
                                        (let ([uv.227 (let ([uv.228 (mref
                                                                      invoke.92
                                                                      -1)])
                                                        ((mref uv.228 -2)
                                                          uv.228
                                                          s1.99
                                                          8))])
                                          ((mref uv.227 -2)
                                            uv.227
                                            s1.99))))
                                    (let ([x.101 (+ (let ([uv.229 (let ([uv.230 (mref
                                                                                  invoke.92
                                                                                  -1)])
                                                                    ((mref
                                                                       uv.230
                                                                       -2)
                                                                      uv.230
                                                                      s2.100
                                                                      8))])
                                                      ((mref uv.229 -2)
                                                        uv.229
                                                        s2.100))
                                                    (let ([uv.231 (let ([uv.232 (mref
                                                                                  invoke.92
                                                                                  -1)])
                                                                    ((mref
                                                                       uv.232
                                                                       -2)
                                                                      uv.232
                                                                      s2.100
                                                                      8))])
                                                      ((mref uv.231 -2)
                                                        uv.231
                                                        s2.100)))])
                                      (* (sra (+ (let ([x.103 (+ (let ([uv.233 (let ([uv.234 (mref
                                                                                               invoke.92
                                                                                               -1)])
                                                                                 ((mref
                                                                                    uv.234
                                                                                    -2)
                                                                                   uv.234
                                                                                   s2.100
                                                                                   16))])
                                                                   ((mref
                                                                      uv.233
                                                                      -2)
                                                                     uv.233
                                                                     s2.100))
                                                                 (let ([uv.235 (let ([uv.236 (mref
                                                                                               invoke.92
                                                                                               -1)])
                                                                                 ((mref
                                                                                    uv.236
                                                                                    -2)
                                                                                   uv.236
                                                                                   s2.100
                                                                                   16))])
                                                                   ((mref
                                                                      uv.235
                                                                      -2)
                                                                     uv.235
                                                                     s2.100)))])
                                                   (- x.103
                                                      (+ (let ([uv.237 (let ([uv.238 (mref
                                                                                       invoke.92
                                                                                       -1)])
                                                                         ((mref
                                                                            uv.238
                                                                            -2)
                                                                           uv.238
                                                                           s2.100
                                                                           8))])
                                                           ((mref
                                                              uv.237
                                                              -2)
                                                             uv.237
                                                             s2.100))
                                                         (let ([uv.239 (let ([uv.240 (mref
                                                                                       invoke.92
                                                                                       -1)])
                                                                         ((mref
                                                                            uv.240
                                                                            -2)
                                                                           uv.240
                                                                           s2.100
                                                                           8))])
                                                           ((mref
                                                              uv.239
                                                              -2)
                                                             uv.239
                                                             s2.100)))))
                                                 (let ([x.102 (+ (let ([uv.241 (let ([uv.242 (mref
                                                                                               invoke.92
                                                                                               -1)])
                                                                                 ((mref
                                                                                    uv.242
                                                                                    -2)
                                                                                   uv.242
                                                                                   s2.100
                                                                                   16))])
                                                                   ((mref
                                                                      uv.241
                                                                      -2)
                                                                     uv.241
                                                                     s2.100))
                                                                 (let ([uv.243 (let ([uv.244 (mref
                                                                                               invoke.92
                                                                                               -1)])
                                                                                 ((mref
                                                                                    uv.244
                                                                                    -2)
                                                                                   uv.244
                                                                                   s2.100
                                                                                   16))])
                                                                   ((mref
                                                                      uv.243
                                                                      -2)
                                                                     uv.243
                                                                     s2.100)))])
                                                   (- (+ (let ([uv.245 (let ([uv.246 (mref
                                                                                       invoke.92
                                                                                       -1)])
                                                                         ((mref
                                                                            uv.246
                                                                            -2)
                                                                           uv.246
                                                                           s2.100
                                                                           8))])
                                                           ((mref
                                                              uv.245
                                                              -2)
                                                             uv.245
                                                             s2.100))
                                                         (let ([uv.247 (let ([uv.248 (mref
                                                                                       invoke.92
                                                                                       -1)])
                                                                         ((mref
                                                                            uv.248
                                                                            -2)
                                                                           uv.248
                                                                           s2.100
                                                                           8))])
                                                           ((mref
                                                              uv.247
                                                              -2)
                                                             uv.247
                                                             s2.100)))
                                                      x.102)))
                                              3)
                                         x.101))))))))))))))))))))
Exception: duplicate bound variable uv.145 in (let ((tmp.249 0) (uv.145 0) (tmp.250 0) (uv.145 0)) (let () (let (...) (...))))
Type (debug) to enter the debugger.