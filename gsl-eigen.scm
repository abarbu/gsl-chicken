(define gsl-svd-decomp!
 ;; (gsl_matrix * A, gsl_matrix * V, gsl_vector * S, gsl_vector * work) 
 (foreign-lambda int "gsl_linalg_SV_decomp" gsl:matrix gsl:matrix gsl:vector gsl:vector))

(define (gsl-svd a)
 (let* ((u (gsl-copy-matrix a))
	(v (gsl-matrix-alloc (gsl-matrix-columns a)
			     (gsl-matrix-columns a)))
	(s (gsl-vector-alloc (gsl-matrix-columns a)))
	(scratch (gsl-vector-alloc (gsl-matrix-columns a))))
  (gsl-svd-decomp! u v s scratch)
  `(,u ,s ,v)))

(define (gsl-eigen-symmv-workspace a)
 (set-finalizer! ((foreign-lambda c-pointer "gsl_eigen_symmv_alloc" int) a)
                 gsl-eigen-symmv-free))
(define gsl-eigen-symmv-free
 (foreign-lambda void "gsl_eigen_symmv_free" c-pointer))
(define gsl-eigen-symmv!
 (foreign-lambda int "gsl_eigen_symmv" gsl:matrix gsl:vector gsl:matrix c-pointer))
(define gsl-eigen-symmv-sort!
 (foreign-lambda int "gsl_eigen_symmv_sort" gsl:vector gsl:matrix int))

(define gsl-eigen-sort-val-asc
 (foreign-value "GSL_EIGEN_SORT_VAL_ASC" int))
(define gsl-eigen-sort-val-desc
 (foreign-value "GSL_EIGEN_SORT_VAL_DESC" int))

(define (gsl-eigen-symm a)
 (let* ((m (gsl-copy-matrix a))
	(eval (gsl-vector-alloc (gsl-matrix-columns m)))
	(evec (gsl-matrix-alloc (gsl-matrix-rows m)
				(gsl-matrix-columns m)))
	(scratch (gsl-eigen-symmv-workspace
		  (gsl-matrix-columns m))))
  (gsl-eigen-symmv! m eval evec scratch)
  (gsl-eigen-symmv-sort! eval evec gsl-eigen-sort-val-asc)
  `(,eval ,evec)))
