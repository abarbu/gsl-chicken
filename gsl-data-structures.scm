;; TODO equal? on matrix/vector, maybe matrix?/vector?
;;  map, for-each

(foreign-declare "#include <gsl/gsl_matrix_double.h>")
(foreign-declare "#include <gsl/gsl_math.h>")
(foreign-declare "#include <gsl/gsl_blas.h>")
(foreign-declare "#include <gsl/gsl_eigen.h>")
(foreign-declare "#include <gsl/gsl_linalg.h>")

;;; General

(define-syntax define-gsl-binary-operator
 (er-macro-transformer
  (lambda (form rename compare)
   (let* ((%a (rename 'a)) (%b (rename 'b))
          (%let (rename 'let)) (%obj (rename 'obj))
          (flip? (and (> (length form) 5) (equal? (last form) 'flip)))
          (flip (lambda (a b) (if flip? (list b a) (list a b))))
          (type-prefix (lambda (a) (if (member a '(matrix vector)) (string->symbol (conc "gsl:" a)) a)))
          (to-type (lambda (a) (cond ((member a '(matrix vector)) (type-prefix a))
                                ((member a '(double)) 'number)
                                (else (error "unknown type")))))
          (call (lambda (a b)
                 `(,%let ((,%obj ,a))
                         ((foreign-lambda int ,(conc "gsl_" (car (flip (fourth form) (fifth form))) "_" (third form))
                                          ,@(map type-prefix (flip (fourth form) (fifth form))))
                          ,%obj ,b)
                          ,%obj))))
    `(begin (define (,(string->symbol (conc (second form) "!")) ,@(flip %a %b))
             ,(call %a %b))
            (define (,(second form) ,@(flip %a %b))
             (if (and ,@(map (lambda (t n) `(,(string->symbol (conc (to-type (t form)) "?")) ,n))
                           (flip fourth fifth)
                           (list %a %b)))
                 ,(call
                   `(,(string->symbol (conc "gsl-copy-" ((car (flip fourth fifth)) form))) ,%a) %b)
                 (,(string->symbol (conc "la-" (second form))) ,@(flip %a %b)))))))))

(define (gsl-> obj)
 (cond ((gsl:matrix? obj) (gsl->matrix obj))
       ((gsl:vector? obj) (gsl->vector obj))
       (else (error "Cannot convert GSL object to a scheme object"))))

(define (->gsl obj)
 (cond ((matrix? obj) (matrix->gsl obj))
       ((vector? obj) (vector->gsl obj))
       (else (error "Cannot convert scheme object to a GSL object"))))

(define (gsl-copy a)
 (cond ((gsl:vector? a) (gsl-copy-vector a))
       ((gsl:matrix? a) (gsl-copy-matrix a))
       (else (error "Incompatible gsl copy types"))))

(define (vector->f64vector v) (list->f64vector (vector->list v)))

(define (for-each-vector f v . &rest)
 (if (gsl:vector? v)
     (for-each-n
       (lambda (i) (apply f (gsl-vector-ref v i) (map (lambda (v) (gsl-vector-ref v i)) &rest)))
      (gsl-vector-length v))
     (apply tr-for-each-vector f v &rest)))

(define (for-each-vector1 f v)
 (if (gsl:vector? v)
     (for-each-n (lambda (i) (f (gsl-vector-ref v i))) (gsl-vector-length v))
     (tr-for-each-vector f v)))

(define (for-each-indexed-vector f v)
 (if (gsl:vector? v)
     (for-each-n (lambda (i) (f (gsl-vector-ref v i) i)) (gsl-vector-length v))
     (tr-for-each-indexed-vector f v)))

(define (map-vector f v . &rest)
 (if (gsl:vector? v)
     (let ((new-v (gsl-vector-alloc (gsl-vector-length v))))
      (for-each-n (lambda (i)
                   (gsl-vector-set!
                    new-v i (apply f
                                   (gsl-vector-ref v i)
                                   (map (lambda (a) (gsl-vector-ref a i)) &rest))))
       (gsl-vector-length v))
      new-v)
     (apply tr-map-vector f v &rest)))

(define (map-vector1 f v)
 (if (gsl:vector? v)
     (let ((new-v (gsl-vector-alloc (gsl-vector-length v))))
      (for-each-n (lambda (i) (gsl-vector-set! new-v i (f (gsl-vector-ref v i))))
       (gsl-vector-length v))
      new-v)
     (apply tr-map-vector f v)))

(define (map-indexed-vector f v)
 (if (gsl:vector? v)
     (let ((new-v (gsl-vector-alloc (gsl-vector-length v))))
      (for-each-n (lambda (i) (gsl-vector-set! new-v i (f (gsl-vector-ref v i) i)))
       (gsl-vector-length v))
      new-v)
     (apply tr-map-indexed-vector f v)))

(define (map-n-gsl-vector f n)
 (let ((new-v (gsl-vector-alloc n)))
  (for-each-n (lambda (i) (gsl-vector-set! new-v i (f i))) n)
  new-v))

(define (enumerate-gsl-vector n) (map-n-gsl-vector (lambda (a) a) n))

(define (vector-length v)
 (if (gsl:vector? v)
     (gsl-vector-length v)
     (sc-vector-length v)))

(define (some-vector p v . &rest)
 (if (gsl:vector? v)
     (call-with-current-continuation
      (lambda (k) (apply for-each-vector (lambda e (when (apply p e) (k #t))) v &rest) #f))
     (apply tr-some-vector p v &rest)))

(define (some-vector1 p v)
 (if (gsl:vector? v)
     (call-with-current-continuation
      (lambda (k) (for-each-vector1 (lambda (e) (when (p e) (k #t))) v) #f))
     (tr-some-vector p v)))

(define (every-vector p v . &rest)
 (if (gsl:vector? v)
     (call-with-current-continuation
      (lambda (k) (apply for-each-vector (lambda e (unless (apply p e) (k #f))) v &rest) #t))
     (apply tr-some-vector p v &rest)))

(define (every-vector1 p v)
 (if (gsl:vector? v)
     (call-with-current-continuation
      (lambda (k) (for-each-vector1 (lambda (e) (unless (p e) (k #f))) v) #t))
     (tr-some-vector p v)))

(define (one-vector p v . &rest)
 (if (gsl:vector? v)
     (let ((f #f))
      (call-with-current-continuation
       (lambda (k) (apply for-each-vector (lambda e (when (apply p e) (if f (k #f) (set! f #t)))) v &rest) f)))
     (apply tr-some-vector p v &rest)))

(define (one-vector1 p v . &rest)
 (if (gsl:vector? v)
     (let ((f #f))
      (call-with-current-continuation
       (lambda (k) (for-each-vector1 (lambda (e) (when (p e) (if f (k #f) (set! f #t)))) v) f)))
     (tr-some-vector p v)))

(define (reduce-vector f i v)
 (if (gsl:vector? v)
     (let ((n (gsl-vector-length v)))
      (cond ((zero? n) i)
            ((= n 1) (gsl-vector-ref v 0))
            (else (let loop ((i 1) (c (gsl-vector-ref v 0)))
                   (if (= i n) c (loop (+ i 1) (f (gsl-vector-ref v i) c)))))))
     (tr-reduce-vector f i v)))

(define (map-reduce-vector g i f v . vs)
 (if (gsl:vector? v)
     (let ((n (gsl-vector-length v)))
      (let loop ((j 0) (result i))
       (if (= j n)
           result
           (loop (+ j 1)
                 (g (apply f
                           (gsl-vector-ref v j)
                           (map (lambda (v) (gsl-vector-ref v j)) vs))
                    result)))))
     (apply tr-map-reduce-vector g i f v vs)))

(define (map-reduce-vector1 g i f v)
 (if (gsl:vector? v)
     (let ((n (gsl-vector-length v)))
      (let loop ((j 0) (result i))
       (if (= j n)
           result
           (loop (+ j 1) (g (f (gsl-vector-ref v j)) result)))))
     (tr-map-reduce-vector g i f v)))

(define-syntax define-gsl-subview-binding
 (er-macro-transformer
  (lambda (form rename compare)
   (let ((type (conc "gsl_" (second form))))
    `(define ,(string->symbol (conc "gsl-" (second form) "-" (third form)))
      (foreign-lambda*
       ,(string->symbol (conc "gsl:" (fourth form)))
       ,(fifth form)
       ,(conc type " *p0 = malloc(sizeof(" type "));")
       ,(conc "gsl_" (fourth form) "_view p1 = gsl_" (second form) "_" (third form) "("
              (string-join (map (lambda (a) (symbol->string (second a))) (fifth form)) ",")
              ");")
       ,(conc "memcpy(p0, &p1." (fourth form) ", sizeof(" type "));")
       "C_return(p0);"))))))

;;; Vectors

(define-structure gsl:vector handle)
(define-foreign-type gsl:vector (c-pointer "gsl_vector")
 gsl:vector-handle make-gsl:vector)

(define-reader-ctor 'gsl-vector
 (lambda (v) (unless (and (vector? v) (not (matrix? v)))
         (error "can't convert to a GSL vector, not a vector of numbers"))
    (vector->gsl v)))

(define-record-printer
 (gsl:vector obj out)
 (fprintf out "#,(gsl-vector ")
 (pp-without-newline (gsl->vector obj) out)
 (fprintf out ")"))

(define (make-gsl-vector n #!optional fill)
 (let ((v (gsl-vector-alloc n)))
  (when fill ((foreign-lambda void "gsl_vector_set_all" gsl:vector double) v fill))
  v))

(define (gsl-vector-alloc a)
 (set-finalizer! ((foreign-lambda gsl:vector "gsl_vector_alloc" int) a)
                 gsl-vector-free))
(define gsl-vector-free
 (foreign-lambda void "gsl_vector_free" gsl:vector))

(define (vector->gsl obj)
 (unless (vector? obj) (error "not a vector"))
 (let ((gv (gsl-vector-alloc (vector-length obj))))
  ((foreign-lambda* void ((f64vector sv) (gsl:vector gv))
                    "int i; for(i = 0; i < gv->size; ++i)
                             gsl_vector_set(gv, i, sv[i]);
                            C_return(0);") (vector->f64vector obj) gv)
  gv))

(define (gsl->vector obj)
 (unless (gsl:vector? obj) (error "not a gsl vector"))
 (let* ((size ((foreign-lambda* unsigned-int ((gsl:vector gv))
                                "C_return(gv->size);") 
               obj))
        (v (make-vector size)))
  (let loop ((i 0))
   (if (= i size)
       v
       (begin
        (vector-set! v i ((foreign-lambda double "gsl_vector_get" 
                                          gsl:vector unsigned-int) 
                          obj i))
        (loop (+ i 1)))))))

(define gsl-vector-ref (foreign-lambda double "gsl_vector_get" gsl:vector unsigned-int))
(define gsl-vector-set! (foreign-lambda void "gsl_vector_set" gsl:vector unsigned-int double))
(define gsl-vector-pointer (foreign-lambda c-pointer "gsl_vector_ptr" gsl:vector unsigned-int))

(define gsl-vector-memcpy (foreign-lambda int "gsl_vector_memcpy" gsl:vector gsl:vector))

(define (gsl-copy-vector v) (let ((new-v (gsl-vector-alloc (gsl-vector-length v))))
                        (gsl-vector-memcpy new-v v)
                        new-v))

(define-gsl-binary-operator v+ "add" vector vector)
(define-gsl-binary-operator v- "sub" vector vector)
(define-gsl-binary-operator v* "mul" vector vector)
(define-gsl-binary-operator v/ "div" vector vector)
(define-gsl-binary-operator v*k "scale" vector double)
(define-gsl-binary-operator v+k "add_constant" vector double)

(define (k*v k v) (v*k v k))
(define (v/k v k) (k*v (/ 1 k) v))
(define (k+v k v) (v+k v k))

(define gsl-vector-null? (foreign-lambda int "gsl_vector_isnull" gsl:vector))
(define gsl-vector-positive? (foreign-lambda int "gsl_vector_ispos" gsl:vector))
(define gsl-vector-negative? (foreign-lambda int "gsl_vector_isneg" gsl:vector))
(define gsl-vector-non-negative? (foreign-lambda int "gsl_vector_isnonneg" gsl:vector))

(define gsl-dot
 (foreign-lambda* double
                  ((gsl:vector x) (gsl:vector y))
                  "double d0;"
                  "gsl_blas_ddot(x,y,&d0);"
                  "C_return(d0);"))

(define (dot x y)
 (if (and (gsl:vector? x) (gsl:vector? y))
     (gsl-dot x y)
     (la-dot x y)))

;;; Matrices

(define-structure gsl:matrix handle)
(define-foreign-type gsl:matrix (c-pointer "gsl_matrix")
 gsl:matrix-handle make-gsl:matrix)

(define-reader-ctor 'gsl-matrix
 (lambda (v) (unless (matrix? v)
         (error "can't convert to a GSL matrix, not a matrix of numbers"))
    (matrix->gsl v)))

(define-record-printer
 (gsl:matrix obj out)
 (fprintf out "#,(gsl-matrix ")
 (pp-without-newline (gsl->matrix obj) out)
 (fprintf out ")"))

(define (make-gsl-matrix rows columns #!optional fill)
 (let ((m (gsl-matrix-alloc rows columns)))
  (when fill ((foreign-lambda void "gsl_matrix_set_all" gsl:matrix double) m fill))
  m))

(define gsl-matrix-rows (foreign-lambda* unsigned-int ((gsl:matrix gm)) "C_return(gm->size1);"))
(define gsl-matrix-columns (foreign-lambda* unsigned-int ((gsl:matrix gm)) "C_return(gm->size2);"))
(define gsl-vector-length (foreign-lambda* unsigned-int ((gsl:vector gv)) "C_return(gv->size);"))

(define (gsl-matrix-alloc a b)
 (set-finalizer! ((foreign-lambda gsl:matrix "gsl_matrix_alloc" int int) a b)
                 gsl-matrix-free))
(define gsl-matrix-free (foreign-lambda void "gsl_matrix_free" gsl:matrix))

(define (matrix->gsl obj)
 (unless (matrix? obj) (error "not a matrix"))
 (let ((gm (gsl-matrix-alloc (matrix-rows obj) (matrix-columns obj)))
       (size (matrix-rows obj)))
  (let loop ((i 0))
   (if (= i size)
       gm
       (begin
        ((foreign-lambda* void ((f64vector sm) (gsl:matrix gm) (int i))
                          "int j; for(j = 0; j < gm->size2; ++j)
                                   gsl_matrix_set(gm, i, j, sm[j]);
                                  C_return(0);")
         (vector->f64vector (vector-ref obj i)) gm i)
        (loop (+ i 1)))))))

(define (gsl->matrix obj)
 (unless (gsl:matrix? obj) (error "not a gsl matrix"))
 (let* ((size1 ((foreign-lambda* unsigned-int ((gsl:matrix gm))
                                 "C_return(gm->size1);") obj))
        (size2 ((foreign-lambda* unsigned-int ((gsl:matrix gm))
                                 "C_return(gm->size2);") obj))
        (m (make-matrix size1 size2)))
  (let loopi ((i 0))
   (if (= i size1)
       m
       (let loopj ((j 0))
        (if (= j size2)
            (loopi (+ i 1))
            (begin 
             (matrix-set! m i j 
                          ((foreign-lambda double "gsl_matrix_get" 
                                           gsl:matrix unsigned-int 
                                           unsigned-int) 
                           obj i j))
             (loopj (+ j 1)))))))))

(define gsl-matrix-get
 (foreign-lambda double "gsl_matrix_get" gsl:matrix unsigned-int unsigned-int))
(define gsl-matrix-set
 (foreign-lambda void "gsl_matrix_set" gsl:matrix unsigned-int unsigned-int double))
(define gsl-matrix-transpose
 (foreign-lambda int "gsl_matrix_transpose" gsl:matrix))
(define gsl-matrix-swap-rows
 (foreign-lambda int "gsl_matrix_swap_rows" gsl:matrix unsigned-int unsigned-int))
(define gsl-matrix-swap-columns
 (foreign-lambda int "gsl_matrix_swap_columns" gsl:matrix unsigned-int unsigned-int))

(define gsl-matrix-get-row
 (foreign-lambda int "gsl_matrix_get_row" gsl:vector gsl:matrix unsigned-int))
(define gsl-matrix-get-col
 (foreign-lambda int "gsl_matrix_get_col" gsl:vector gsl:matrix unsigned-int))
(define gsl-matrix-set-row!
 (foreign-lambda int "gsl_matrix_set_row" gsl:matrix unsigned-int gsl:vector))
(define gsl-matrix-set-col!
 (foreign-lambda int "gsl_matrix_set_col" gsl:matrix unsigned-int gsl:vector))

(define gsl-matrix-memcpy
 (foreign-lambda int "gsl_matrix_memcpy" gsl:matrix gsl:matrix))
(define (gsl-memcpy a b)
 (cond ((and (gsl:vector? a) (gsl:vector? b)) (gsl-vector-memcpy a b))
       ((and (gsl:matrix? a) (gsl:matrix? b)) (gsl-matrix-memcpy a b))
       (else (error "Incompatible gsl memcpy types"))))

(define (gsl-copy-matrix m)
 (let ((new-m (gsl-matrix-alloc (gsl-matrix-rows m)
				(gsl-matrix-columns m))))
  (gsl-matrix-memcpy new-m m)
  new-m))

(define-gsl-binary-operator m+ "add" matrix matrix)
(define-gsl-binary-operator m- "sub" matrix matrix)
(define-gsl-binary-operator m*. "mul_elements" matrix matrix)
(define-gsl-binary-operator m/. "div_elements" matrix matrix)
(define-gsl-binary-operator m*k "scale" matrix double)
(define-gsl-binary-operator m+k "add_constant" matrix double)

(define (k*m k m) (m*k m k))
(define (m/k m k) (k*m (/ 1 k) m))
(define (k+m k m) (m+k m k))

(define gsl-matrix-all!
 (foreign-lambda void "gsl_matrix_set_all" gsl:matrix double))
(define gsl-matrix-zero!
 (foreign-lambda void "gsl_matrix_set_zero" gsl:matrix))
(define gsl-matrix-identity!
 (foreign-lambda void "gsl_matrix_set_identity" gsl:matrix))
(define gsl-matrix-null?
 (foreign-lambda int "gsl_matrix_isnull" gsl:matrix))
(define gsl-matrix-positive?
 (foreign-lambda int "gsl_matrix_ispos" gsl:matrix))
(define gsl-matrix-negative?
 (foreign-lambda int "gsl_matrix_isneg" gsl:matrix))
(define gsl-matrix-non-negative?
 (foreign-lambda int "gsl_matrix_isnonneg" gsl:matrix))

(define-gsl-subview-binding matrix row vector ((gsl:matrix m) (unsigned-int i)))
(define-gsl-subview-binding matrix column vector ((gsl:matrix m) (unsigned-int i)))
(define-gsl-subview-binding matrix submatrix matrix
 ((gsl:matrix m) (unsigned-int i) (unsigned-int j) (unsigned-int n1) (unsigned-int n2)))
(define-gsl-subview-binding matrix diagonal vector ((gsl:matrix m)))
(define-gsl-subview-binding matrix subdiagonal vector ((gsl:matrix m) (unsigned-int k)))
(define-gsl-subview-binding matrix superdiagonal vector ((gsl:matrix m) (unsigned-int k)))
(define-gsl-subview-binding matrix subrow vector ((gsl:matrix m) (unsigned-int i) (unsigned-int offset) (unsigned-int n)))
(define-gsl-subview-binding matrix subcolumn vector ((gsl:matrix m) (unsigned-int i) (unsigned-int offset) (unsigned-int n)))
(define-gsl-subview-binding matrix view_array matrix ((c-pointer p) (unsigned-int n1) (unsigned-int n2)))
(define-gsl-subview-binding matrix view_array_with_tda matrix ((c-pointer p) (unsigned-int n1) (unsigned-int n2) (unsigned-int tda)))
