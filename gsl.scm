(module gsl *
(import chicken scheme foreign)
(use traversal linear-algebra
      foreigners define-structure srfi-4)

(foreign-declare "#include <gsl/gsl_matrix_double.h>")
(foreign-declare "#include <gsl/gsl_math.h>")
(foreign-declare "#include <gsl/gsl_eigen.h>")
(foreign-declare "#include <gsl/gsl_linalg.h>")
(foreign-declare "#include <gsl/gsl_rng.h>")
(foreign-declare "#include <gsl/gsl_randist.h>")
(foreign-declare "#include <gsl/gsl_cdf.h>")
(foreign-declare "#include <gsl/gsl_fft.h>")
(foreign-declare "#include <gsl/gsl_fft_complex.h>")

(define-structure gsl:rng handle)
(define-foreign-type gsl:rng (c-pointer "gsl_rng") gsl:rng-handle make-gsl:rng)
(define-structure gsl:rng-type handle)
(define-foreign-type gsl:rng-type (c-pointer "gsl_rng_type") 
 gsl:rng-type-handle make-gsl:rng-type)
(define-structure gsl:matrix handle)
(define-foreign-type gsl:matrix (c-pointer "gsl_matrix")
 gsl:matrix-handle make-gsl:matrix)
(define-structure gsl:vector handle)
(define-foreign-type gsl:vector (c-pointer "gsl_vector")
 gsl:vector-handle make-gsl:vector)

(define gsl-matrix-size1 (foreign-lambda* unsigned-int ((gsl:matrix gm))
                                     "C_return(gm->size1);"))
(define gsl-matrix-size2 (foreign-lambda* unsigned-int ((gsl:matrix gm))
                                     "C_return(gm->size2);"))
(define gsl-vector-size (foreign-lambda* unsigned-int ((gsl:vector gv))
                                    "C_return(gv->size);"))

;;; Default RNG

(define *gsl-rng* #f)
(define (setup-default-gsl-rng!)
 ;; Note that the default seed is 0
 (unless *gsl-rng*
  (gsl:rng-env-setup)
  (set! *gsl-rng* (gsl:rng-alloc gsl:rng-default))
  *gsl-rng*))

;;; Matrices

(define (gsl-matrix-alloc a b)
 (set-finalizer! ((foreign-lambda gsl:matrix "gsl_matrix_alloc" int int) a b)
                 gsl-matrix-free))
(define gsl-matrix-free
 (foreign-lambda void "gsl_matrix_free" gsl:matrix))

(define (gsl-vector-alloc a)
 (set-finalizer! ((foreign-lambda gsl:vector "gsl_vector_alloc" int) a)
                 gsl-vector-free))
(define gsl-vector-free
 (foreign-lambda void "gsl_vector_free" gsl:vector))

(define (vector->f64vector v) (list->f64vector (vector->list v)))

(define (scheme->gsl obj)
 (cond ((matrix? obj)
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
       ((vector? obj)
        (let ((gv (gsl-vector-alloc (vector-length obj))))
         ((foreign-lambda* void ((f64vector sv) (gsl:vector gv))
                           "int i; for(i = 0; i < gv->size; ++i)
                             gsl_vector_set(gv, i, sv[i]);
                            C_return(0);") (vector->f64vector obj) gv)
         gv))
       (else (error "Cannot convert scheme object to a GSL object"))))

(define (gsl->scheme obj)
 (cond ((gsl:matrix? obj)
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
       ((gsl:vector? obj)
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
       (else (error "Cannot convert GSL object to a scheme object"))))

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

(define (gsl-matrix-get-row a v i) (let ((m (gsl-copy-matrix a)))
                               (gsl-matrix-get-row m v i)
                               m))
(define (gsl-matrix-get-col a v i) (let ((m (gsl-copy-matrix a)))
                               (gsl-matrix-get-col m v i)
                               m))
(define (gsl-matrix-set-row a i v) (let ((m (gsl-copy-matrix a)))
                               (gsl-matrix-set-row! m i v)
                               m))
(define (gsl-matrix-set-col a i v) (let ((m (gsl-copy-matrix a)))
                               (gsl-matrix-set-col! m i v)
                               m))

(define gsl-vector-memcpy
 (foreign-lambda int "gsl_vector_memcpy" gsl:vector gsl:vector))
(define gsl-matrix-memcpy
 (foreign-lambda int "gsl_matrix_memcpy" gsl:matrix gsl:matrix))
(define (gsl-memcpy a b)
 (cond ((and (gsl:vector? a) (gsl:vector? b)) (gsl-vector-memcpy a b))
       ((and (gsl:matrix? a) (gsl:matrix? b)) (gsl-matrix-memcpy a b))
       (else (error "Incompatible gsl memcpy types"))))

(define (gsl-copy-vector v)
 (let ((new-v (gsl-vector-alloc (gsl-vector-size v))))
  (gsl-vector-memcpy new-v v)
  new-v))
(define (gsl-copy-matrix m)
 (let ((new-m (gsl-matrix-alloc (gsl-matrix-size1 m)
				(gsl-matrix-size2 m))))
  (gsl-matrix-memcpy new-m m)
  new-m))
(define (gsl-copy a)
 (cond ((gsl:vector? a) (gsl-copy-vector a))
       ((gsl:matrix? a) (gsl-copy-matrix a))
       (else (error "Incompatible gsl copy types"))))

(define gsl-vector-add!
 (foreign-lambda int "gsl_vector_add" gsl:vector gsl:vector))
(define gsl-vector-sub!
 (foreign-lambda int "gsl_vector_sub" gsl:vector gsl:vector))
(define gsl-vector-mul!
 (foreign-lambda int "gsl_vector_mul" gsl:vector gsl:vector))
(define gsl-vector-div!
 (foreign-lambda int "gsl_vector_div" gsl:vector gsl:vector))
(define gsl-vector-scale!
 (foreign-lambda int "gsl_vector_scale" gsl:vector double))
(define gsl-vector-add-constant!
 (foreign-lambda int "gsl_vector_add_constant" gsl:vector double))

(define (gsl-v+ a b)
 (gsl-vector-add! (gsl-copy-vector a) b))
(define (gsl-v- a b)
 (gsl-vector-sub! (gsl-copy-vector a) b))
(define (gsl-v* a b)
 (gsl-vector-mul! (gsl-copy-vector a) b))
(define (gsl-v/ a b)
 (gsl-vector-div! (gsl-copy-vector a) b))
(define (gsl-k*v a b)
 (gsl-vector-scale! (gsl-copy-vector a) b))
(define (gsl-k+v a b)
 (gsl-vector-add-constant! (gsl-copy-vector a) b))

(define gsl-matrix-add!
 (foreign-lambda int "gsl_matrix_add" gsl:matrix gsl:matrix))
(define gsl-matrix-sub!
 (foreign-lambda int "gsl_matrix_sub" gsl:matrix gsl:matrix))
(define gsl-matrix-mul-elements!
 (foreign-lambda int "gsl_matrix_mul_elements" gsl:matrix gsl:matrix))
(define gsl-matrix-div-elements!
 (foreign-lambda int "gsl_matrix_div_elements" gsl:matrix gsl:matrix))
(define gsl-matrix-scale!
 (foreign-lambda int "gsl_matrix_scale" gsl:matrix double))
(define gsl-matrix-add-constant!
 (foreign-lambda int "gsl_matrix_add_constant" gsl:matrix double))

(define (gsl-m+ a b)
 (gsl-matrix-add! (gsl-copy-matrix a) b))
(define (gsl-m- a b)
 (gsl-matrix-sub! (gsl-copy-matrix a) b))
(define (gsl-k+m k m)
 (gsl-matrix-add-constant! (gsl-copy-matrix m) k))
(define (gsl-k*m k m)
 (gsl-matrix-scale! (gsl-copy-matrix m) k))
(define (gsl-m*-elements a b)
 (gsl-matrix-mul-elements! (gsl-copy-matrix a) b))
(define (gsl-m/-elements a b)
 (gsl-matrix-div-elements! (gsl-copy-matrix a) b))

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

(define gsl-vector-null?
 (foreign-lambda int "gsl_vector_isnull" gsl:vector))
(define gsl-vector-positive?
 (foreign-lambda int "gsl_vector_ispos" gsl:vector))
(define gsl-vector-negative?
 (foreign-lambda int "gsl_vector_isneg" gsl:vector))
(define gsl-vector-non-negative?
 (foreign-lambda int "gsl_vector_isnonneg" gsl:vector))

(define gsl-svd-decomp!
 ;; (gsl_matrix * A, gsl_matrix * V, gsl_vector * S, gsl_vector * work) 
 (foreign-lambda int "gsl_linalg_SV_decomp" gsl:matrix gsl:matrix gsl:vector gsl:vector))

(define (gsl-svd a)
 (let* ((u (gsl-copy-matrix a))
	(v (gsl-matrix-alloc (gsl-matrix-size2 a)
			     (gsl-matrix-size2 a)))
	(s (gsl-vector-alloc (gsl-matrix-size2 a)))
	(scratch (gsl-vector-alloc (gsl-matrix-size2 a))))
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
	(eval (gsl-vector-alloc (gsl-matrix-size2 m)))
	(evec (gsl-matrix-alloc (gsl-matrix-size1 m)
				(gsl-matrix-size2 m)))
	(scratch (gsl-eigen-symmv-workspace
		  (gsl-matrix-size2 m))))
  (gsl-eigen-symmv! m eval evec scratch)
  (gsl-eigen-symmv-sort! eval evec gsl-eigen-sort-val-asc)
  `(,eval ,evec)))

;;; Distributions, sampling and pdfs

(define gsl:ran-bernoulli (foreign-lambda unsigned-int "gsl_ran_bernoulli" gsl:rng double))
(define gsl:ran-bernoulli-pdf (foreign-lambda double "gsl_ran_bernoulli_pdf" unsigned-int double))

(define gsl:ran-beta (foreign-lambda double "gsl_ran_beta" gsl:rng double double))
(define gsl:ran-beta-pdf (foreign-lambda double "gsl_ran_beta_pdf" double double double))

(define gsl:ran-binomial (foreign-lambda unsigned-int "gsl_ran_binomial" gsl:rng double unsigned-int))
(define gsl:ran-binomial-knuth (foreign-lambda unsigned-int "gsl_ran_binomial_knuth" gsl:rng double unsigned-int))
(define gsl:ran-binomial-tpe (foreign-lambda unsigned-int "gsl_ran_binomial_tpe" gsl:rng double unsigned-int))
(define gsl:ran-binomial-pdf (foreign-lambda double "gsl_ran_binomial_pdf" unsigned-int double unsigned-int))

(define gsl:ran-exponential (foreign-lambda double "gsl_ran_exponential" gsl:rng double))
(define gsl:ran-exponential-pdf (foreign-lambda double "gsl_ran_exponential_pdf" double double))

(define gsl:ran-exppow (foreign-lambda double "gsl_ran_exppow" gsl:rng double double))
(define gsl:ran-exppow-pdf (foreign-lambda double "gsl_ran_exppow_pdf" double double double))

(define gsl:ran-cauchy (foreign-lambda double "gsl_ran_cauchy" gsl:rng double))
(define gsl:ran-cauchy-pdf (foreign-lambda double "gsl_ran_cauchy_pdf" double double))

(define gsl:ran-chisq (foreign-lambda double "gsl_ran_chisq" gsl:rng double))
(define gsl:ran-chisq-pdf (foreign-lambda double "gsl_ran_chisq_pdf" double double))

(define gsl:ran-dirichlet (foreign-lambda void "gsl_ran_dirichlet" gsl:rng unsigned-int c-pointer c-pointer))
(define gsl:ran-dirichlet-pdf (foreign-lambda double "gsl_ran_dirichlet_pdf" unsigned-int c-pointer c-pointer))
(define gsl:ran-dirichlet-lnpdf (foreign-lambda double "gsl_ran_dirichlet_lnpdf" unsigned-int c-pointer c-pointer))

(define gsl:ran-erlang (foreign-lambda double "gsl_ran_erlang" gsl:rng double double))
(define gsl:ran-erlang-pdf (foreign-lambda double "gsl_ran_erlang_pdf" double double double))

(define gsl:ran-fdist (foreign-lambda double "gsl_ran_fdist" gsl:rng double double))
(define gsl:ran-fdist-pdf (foreign-lambda double "gsl_ran_fdist_pdf" double double double))

(define gsl:ran-flat (foreign-lambda double "gsl_ran_flat" gsl:rng double double))
(define gsl:ran-flat-pdf (foreign-lambda double "gsl_ran_flat_pdf" double double double))

(define gsl:ran-gamma (foreign-lambda double "gsl_ran_gamma" gsl:rng double double))
(define gsl:ran-gamma-int (foreign-lambda double "gsl_ran_gamma_int" gsl:rng unsigned-int))
(define gsl:ran-gamma-pdf (foreign-lambda double "gsl_ran_gamma_pdf" double double double))
(define gsl:ran-gamma-mt (foreign-lambda double "gsl_ran_gamma_mt" gsl:rng double double))
(define gsl:ran-gamma-knuth (foreign-lambda double "gsl_ran_gamma_knuth" gsl:rng double double))

(define gsl:ran-gaussian (foreign-lambda double "gsl_ran_gaussian" gsl:rng double))
(define gsl:ran-gaussian-ratio-method (foreign-lambda double "gsl_ran_gaussian_ratio_method" gsl:rng double))
(define gsl:ran-gaussian-ziggurat (foreign-lambda double "gsl_ran_gaussian_ziggurat" gsl:rng double))
(define gsl:ran-gaussian-pdf (foreign-lambda double "gsl_ran_gaussian_pdf" double double))

(define gsl:ran-ugaussian (foreign-lambda double "gsl_ran_ugaussian" gsl:rng))
(define gsl:ran-ugaussian-ratio-method (foreign-lambda double "gsl_ran_ugaussian_ratio_method" gsl:rng))
(define gsl:ran-ugaussian-pdf (foreign-lambda double "gsl_ran_ugaussian_pdf" double))

(define gsl:ran-gaussian-tail (foreign-lambda double "gsl_ran_gaussian_tail" gsl:rng double double))
(define gsl:ran-gaussian-tail-pdf (foreign-lambda double "gsl_ran_gaussian_tail_pdf" double double double))

(define gsl:ran-ugaussian-tail (foreign-lambda double "gsl_ran_ugaussian_tail" gsl:rng double))
(define gsl:ran-ugaussian-tail-pdf (foreign-lambda double "gsl_ran_ugaussian_tail_pdf" double double))

(define gsl:ran-bivariate-gaussian (foreign-lambda void "gsl_ran_bivariate_gaussian" gsl:rng double double double c-pointer c-pointer))
(define gsl:ran-bivariate-gaussian-pdf (foreign-lambda double "gsl_ran_bivariate_gaussian_pdf" double double double double double))

(define gsl:ran-landau (foreign-lambda double "gsl_ran_landau" gsl:rng))
(define gsl:ran-landau-pdf (foreign-lambda double "gsl_ran_landau_pdf" double))

(define gsl:ran-geometric (foreign-lambda unsigned-int "gsl_ran_geometric" gsl:rng double))
(define gsl:ran-geometric-pdf (foreign-lambda double "gsl_ran_geometric_pdf" unsigned-int double))

(define gsl:ran-hypergeometric (foreign-lambda unsigned-int "gsl_ran_hypergeometric" gsl:rng unsigned-int unsigned-int unsigned-int))
(define gsl:ran-hypergeometric-pdf (foreign-lambda double "gsl_ran_hypergeometric_pdf" unsigned-int unsigned-int unsigned-int unsigned-int))

(define gsl:ran-gumbel1 (foreign-lambda double "gsl_ran_gumbel1" gsl:rng double double))
(define gsl:ran-gumbel1-pdf (foreign-lambda double "gsl_ran_gumbel1_pdf" double double double))

(define gsl:ran-gumbel2 (foreign-lambda double "gsl_ran_gumbel2" gsl:rng double double))
(define gsl:ran-gumbel2-pdf (foreign-lambda double "gsl_ran_gumbel2_pdf" double double double))

(define gsl:ran-logistic (foreign-lambda double "gsl_ran_logistic" gsl:rng double))
(define gsl:ran-logistic-pdf (foreign-lambda double "gsl_ran_logistic_pdf" double double))

(define gsl:ran-lognormal (foreign-lambda double "gsl_ran_lognormal" gsl:rng double double))
(define gsl:ran-lognormal-pdf (foreign-lambda double "gsl_ran_lognormal_pdf" double double double))

(define gsl:ran-logarithmic (foreign-lambda unsigned-int "gsl_ran_logarithmic" gsl:rng double))
(define gsl:ran-logarithmic-pdf (foreign-lambda double "gsl_ran_logarithmic_pdf" unsigned-int double))

(define gsl:ran-multinomial (foreign-lambda void "gsl_ran_multinomial" gsl:rng unsigned-int unsigned-int c-pointer c-pointer))
(define gsl:ran-multinomial-pdf (foreign-lambda double "gsl_ran_multinomial_pdf" unsigned-int c-pointer c-pointer))
(define gsl:ran-multinomial-lnpdf (foreign-lambda double "gsl_ran_multinomial_lnpdf" unsigned-int c-pointer c-pointer))

(define gsl:ran-negative-binomial (foreign-lambda unsigned-int "gsl_ran_negative_binomial" gsl:rng double double))
(define gsl:ran-negative-binomial-pdf (foreign-lambda double "gsl_ran_negative_binomial_pdf" unsigned-int double double))

(define gsl:ran-pascal (foreign-lambda unsigned-int "gsl_ran_pascal" gsl:rng double unsigned-int))
(define gsl:ran-pascal-pdf (foreign-lambda double "gsl_ran_pascal_pdf" unsigned-int double unsigned-int))

(define gsl:ran-pareto (foreign-lambda double "gsl_ran_pareto" gsl:rng double double))
(define gsl:ran-pareto-pdf (foreign-lambda double "gsl_ran_pareto_pdf" double double double))

(define gsl:ran-poisson (foreign-lambda unsigned-int "gsl_ran_poisson" gsl:rng double))
(define gsl:ran-poisson-array (foreign-lambda void "gsl_ran_poisson_array" gsl:rng unsigned-int c-pointer double))
(define gsl:ran-poisson-pdf (foreign-lambda double "gsl_ran_poisson_pdf" unsigned-int double))

(define gsl:ran-rayleigh (foreign-lambda double "gsl_ran_rayleigh" gsl:rng double))
(define gsl:ran-rayleigh-pdf (foreign-lambda double "gsl_ran_rayleigh_pdf" double double))

(define gsl:ran-rayleigh-tail (foreign-lambda double "gsl_ran_rayleigh_tail" gsl:rng double double))
(define gsl:ran-rayleigh-tail-pdf (foreign-lambda double "gsl_ran_rayleigh_tail_pdf" double double double))

(define gsl:ran-tdist (foreign-lambda double "gsl_ran_tdist" gsl:rng double))
(define gsl:ran-tdist-pdf (foreign-lambda double "gsl_ran_tdist_pdf" double double))

(define gsl:ran-laplace (foreign-lambda double "gsl_ran_laplace" gsl:rng double))
(define gsl:ran-laplace-pdf (foreign-lambda double "gsl_ran_laplace_pdf" double double))

(define gsl:ran-levy (foreign-lambda double "gsl_ran_levy" gsl:rng double double))
(define gsl:ran-levy-skew (foreign-lambda double "gsl_ran_levy_skew" gsl:rng double double double))

(define gsl:ran-weibull (foreign-lambda double "gsl_ran_weibull" gsl:rng double double))
(define gsl:ran-weibull-pdf (foreign-lambda double "gsl_ran_weibull_pdf" double double double))

(define gsl:ran-dir-2d (foreign-lambda void "gsl_ran_dir_2d" gsl:rng c-pointer c-pointer))
(define gsl:ran-dir-2d-trig-method (foreign-lambda void "gsl_ran_dir_2d_trig_method" gsl:rng c-pointer c-pointer))
(define gsl:ran-dir-3d (foreign-lambda void "gsl_ran_dir_3d" gsl:rng c-pointer c-pointer c-pointer))
(define gsl:ran-dir-nd (foreign-lambda void "gsl_ran_dir_nd" gsl:rng unsigned-int c-pointer))

;;; Distributions cdfs

(define gsl:cdf-ugaussian-P (foreign-lambda double "gsl_cdf_ugaussian_P" double))
(define gsl:cdf-ugaussian-Q (foreign-lambda double "gsl_cdf_ugaussian_Q" double))

(define gsl:cdf-ugaussian-P-inv (foreign-lambda double "gsl_cdf_ugaussian_Pinv" double))
(define gsl:cdf-ugaussian-Q-inv (foreign-lambda double "gsl_cdf_ugaussian_Qinv" double))

(define gsl:cdf-gaussian-P (foreign-lambda double "gsl_cdf_gaussian_P" double double))
(define gsl:cdf-gaussian-Q (foreign-lambda double "gsl_cdf_gaussian_Q" double double))

(define gsl:cdf-gaussian-P-inv (foreign-lambda double "gsl_cdf_gaussian_Pinv" double double))
(define gsl:cdf-gaussian-Q-inv (foreign-lambda double "gsl_cdf_gaussian_Qinv" double double))

(define gsl:cdf-gamma-P (foreign-lambda double "gsl_cdf_gamma_P" double double double))
(define gsl:cdf-gamma-Q (foreign-lambda double "gsl_cdf_gamma_Q" double double double))

(define gsl:cdf-gamma-P-inv (foreign-lambda double "gsl_cdf_gamma_Pinv" double double double))
(define gsl:cdf-gamma-Q-inv (foreign-lambda double "gsl_cdf_gamma_Qinv" double double double))

(define gsl:cdf-cauchy-P (foreign-lambda double "gsl_cdf_cauchy_P" double double))
(define gsl:cdf-cauchy-Q (foreign-lambda double "gsl_cdf_cauchy_Q" double double))

(define gsl:cdf-cauchy-P-inv (foreign-lambda double "gsl_cdf_cauchy_Pinv" double double))
(define gsl:cdf-cauchy-Q-inv (foreign-lambda double "gsl_cdf_cauchy_Qinv" double double))

(define gsl:cdf-laplace-P (foreign-lambda double "gsl_cdf_laplace_P" double double))
(define gsl:cdf-laplace-Q (foreign-lambda double "gsl_cdf_laplace_Q" double double))

(define gsl:cdf-laplace-P-inv (foreign-lambda double "gsl_cdf_laplace_Pinv" double double))
(define gsl:cdf-laplace-Q-inv (foreign-lambda double "gsl_cdf_laplace_Qinv" double double))

(define gsl:cdf-rayleigh-P (foreign-lambda double "gsl_cdf_rayleigh_P" double double))
(define gsl:cdf-rayleigh-Q (foreign-lambda double "gsl_cdf_rayleigh_Q" double double))

(define gsl:cdf-rayleigh-P-inv (foreign-lambda double "gsl_cdf_rayleigh_Pinv" double double))
(define gsl:cdf-rayleigh-Q-inv (foreign-lambda double "gsl_cdf_rayleigh_Qinv" double double))

(define gsl:cdf-chisq-P (foreign-lambda double "gsl_cdf_chisq_P" double double))
(define gsl:cdf-chisq-Q (foreign-lambda double "gsl_cdf_chisq_Q" double double))

(define gsl:cdf-chisq-P-inv (foreign-lambda double "gsl_cdf_chisq_Pinv" double double))
(define gsl:cdf-chisq-Q-inv (foreign-lambda double "gsl_cdf_chisq_Qinv" double double))

(define gsl:cdf-exponential-P (foreign-lambda double "gsl_cdf_exponential_P" double double))
(define gsl:cdf-exponential-Q (foreign-lambda double "gsl_cdf_exponential_Q" double double))

(define gsl:cdf-exponential-P-inv (foreign-lambda double "gsl_cdf_exponential_Pinv" double double))
(define gsl:cdf-exponential-Q-inv (foreign-lambda double "gsl_cdf_exponential_Qinv" double double))

(define gsl:cdf-exppow-P (foreign-lambda double "gsl_cdf_exppow_P" double double double))
(define gsl:cdf-exppow-Q (foreign-lambda double "gsl_cdf_exppow_Q" double double double))

(define gsl:cdf-tdist-P (foreign-lambda double "gsl_cdf_tdist_P" double double))
(define gsl:cdf-tdist-Q (foreign-lambda double "gsl_cdf_tdist_Q" double double))

(define gsl:cdf-tdist-P-inv (foreign-lambda double "gsl_cdf_tdist_Pinv" double double))
(define gsl:cdf-tdist-Q-inv (foreign-lambda double "gsl_cdf_tdist_Qinv" double double))

(define gsl:cdf-fdist-P (foreign-lambda double "gsl_cdf_fdist_P" double double double))
(define gsl:cdf-fdist-Q (foreign-lambda double "gsl_cdf_fdist_Q" double double double))

(define gsl:cdf-fdist-P-inv (foreign-lambda double "gsl_cdf_fdist_Pinv" double double double))
(define gsl:cdf-fdist-Q-inv (foreign-lambda double "gsl_cdf_fdist_Qinv" double double double))

(define gsl:cdf-beta-P (foreign-lambda double "gsl_cdf_beta_P" double double double))
(define gsl:cdf-beta-Q (foreign-lambda double "gsl_cdf_beta_Q" double double double))

(define gsl:cdf-beta-P-inv (foreign-lambda double "gsl_cdf_beta_Pinv" double double double))
(define gsl:cdf-beta-Q-inv (foreign-lambda double "gsl_cdf_beta_Qinv" double double double))

(define gsl:cdf-flat-P (foreign-lambda double "gsl_cdf_flat_P" double double double))
(define gsl:cdf-flat-Q (foreign-lambda double "gsl_cdf_flat_Q" double double double))

(define gsl:cdf-flat-P-inv (foreign-lambda double "gsl_cdf_flat_Pinv" double double double))
(define gsl:cdf-flat-Q-inv (foreign-lambda double "gsl_cdf_flat_Qinv" double double double))

(define gsl:cdf-lognormal-P (foreign-lambda double "gsl_cdf_lognormal_P" double double double))
(define gsl:cdf-lognormal-Q (foreign-lambda double "gsl_cdf_lognormal_Q" double double double))

(define gsl:cdf-lognormal-P-inv (foreign-lambda double "gsl_cdf_lognormal_Pinv" double double double))
(define gsl:cdf-lognormal-Q-inv (foreign-lambda double "gsl_cdf_lognormal_Qinv" double double double))

(define gsl:cdf-gumbel1-P (foreign-lambda double "gsl_cdf_gumbel1_P" double double double))
(define gsl:cdf-gumbel1-Q (foreign-lambda double "gsl_cdf_gumbel1_Q" double double double))

(define gsl:cdf-gumbel1-P-inv (foreign-lambda double "gsl_cdf_gumbel1_Pinv" double double double))
(define gsl:cdf-gumbel1-Q-inv (foreign-lambda double "gsl_cdf_gumbel1_Qinv" double double double))

(define gsl:cdf-gumbel2-P (foreign-lambda double "gsl_cdf_gumbel2_P" double double double))
(define gsl:cdf-gumbel2-Q (foreign-lambda double "gsl_cdf_gumbel2_Q" double double double))

(define gsl:cdf-gumbel2-P-inv (foreign-lambda double "gsl_cdf_gumbel2_Pinv" double double double))
(define gsl:cdf-gumbel2-Q-inv (foreign-lambda double "gsl_cdf_gumbel2_Qinv" double double double))

(define gsl:cdf-weibull-P (foreign-lambda double "gsl_cdf_weibull_P" double double double))
(define gsl:cdf-weibull-Q (foreign-lambda double "gsl_cdf_weibull_Q" double double double))

(define gsl:cdf-weibull-P-inv (foreign-lambda double "gsl_cdf_weibull_Pinv" double double double))
(define gsl:cdf-weibull-Q-inv (foreign-lambda double "gsl_cdf_weibull_Qinv" double double double))

(define gsl:cdf-pareto-P (foreign-lambda double "gsl_cdf_pareto_P" double double double))
(define gsl:cdf-pareto-Q (foreign-lambda double "gsl_cdf_pareto_Q" double double double))

(define gsl:cdf-pareto-P-inv (foreign-lambda double "gsl_cdf_pareto_Pinv" double double double))
(define gsl:cdf-pareto-Q-inv (foreign-lambda double "gsl_cdf_pareto_Qinv" double double double))

(define gsl:cdf-logistic-P (foreign-lambda double "gsl_cdf_logistic_P" double double))
(define gsl:cdf-logistic-Q (foreign-lambda double "gsl_cdf_logistic_Q" double double))

(define gsl:cdf-logistic-P-inv (foreign-lambda double "gsl_cdf_logistic_Pinv" double double))
(define gsl:cdf-logistic-Q-inv (foreign-lambda double "gsl_cdf_logistic_Qinv" double double))

(define gsl:cdf-binomial-P (foreign-lambda double "gsl_cdf_binomial_P" unsigned-int double unsigned-int))
(define gsl:cdf-binomial-Q (foreign-lambda double "gsl_cdf_binomial_Q" unsigned-int double unsigned-int))

(define gsl:cdf-poisson-P (foreign-lambda double "gsl_cdf_poisson_P" unsigned-int double))
(define gsl:cdf-poisson-Q (foreign-lambda double "gsl_cdf_poisson_Q" unsigned-int double))

(define gsl:cdf-geometric-P (foreign-lambda double "gsl_cdf_geometric_P" unsigned-int double))
(define gsl:cdf-geometric-Q (foreign-lambda double "gsl_cdf_geometric_Q" unsigned-int double))

(define gsl:cdf-negative-binomial-P (foreign-lambda double "gsl_cdf_negative_binomial_P" unsigned-int double double))
(define gsl:cdf-negative-binomial-Q (foreign-lambda double "gsl_cdf_negative_binomial_Q" unsigned-int double double))

(define gsl:cdf-pascal-P (foreign-lambda double "gsl_cdf_pascal_P" unsigned-int double unsigned-int))
(define gsl:cdf-pascal-Q (foreign-lambda double "gsl_cdf_pascal_Q" unsigned-int double unsigned-int))

(define gsl:cdf-hypergeometric-P (foreign-lambda double "gsl_cdf_hypergeometric_P" unsigned-int unsigned-int unsigned-int unsigned-int))
(define gsl:cdf-hypergeometric-Q (foreign-lambda double "gsl_cdf_hypergeometric_Q" unsigned-int unsigned-int unsigned-int unsigned-int))

;;; Random number generators

(define gsl:rng-borosh13         (foreign-value "gsl_rng_borosh13" gsl:rng-type))
(define gsl:rng-coveyou          (foreign-value "gsl_rng_coveyou" gsl:rng-type))
(define gsl:rng-cmrg             (foreign-value "gsl_rng_cmrg" gsl:rng-type))
(define gsl:rng-fishman18        (foreign-value "gsl_rng_fishman18" gsl:rng-type))
(define gsl:rng-fishman20        (foreign-value "gsl_rng_fishman20" gsl:rng-type))
(define gsl:rng-fishman2x        (foreign-value "gsl_rng_fishman2x" gsl:rng-type))
(define gsl:rng-gfsr4            (foreign-value "gsl_rng_gfsr4" gsl:rng-type))
(define gsl:rng-knuthran         (foreign-value "gsl_rng_knuthran" gsl:rng-type))
(define gsl:rng-knuthran2        (foreign-value "gsl_rng_knuthran2" gsl:rng-type))
(define gsl:rng-knuthran2002     (foreign-value "gsl_rng_knuthran2002" gsl:rng-type))
(define gsl:rng-lecuyer21        (foreign-value "gsl_rng_lecuyer21" gsl:rng-type))
(define gsl:rng-minstd           (foreign-value "gsl_rng_minstd" gsl:rng-type))
(define gsl:rng-mrg              (foreign-value "gsl_rng_mrg" gsl:rng-type))
(define gsl:rng-mt19937          (foreign-value "gsl_rng_mt19937" gsl:rng-type))
(define gsl:rng-mt19937-1999     (foreign-value "gsl_rng_mt19937_1999" gsl:rng-type))
(define gsl:rng-mt19937-1998     (foreign-value "gsl_rng_mt19937_1998" gsl:rng-type))
(define gsl:rng-r250             (foreign-value "gsl_rng_r250" gsl:rng-type))
(define gsl:rng-ran0             (foreign-value "gsl_rng_ran0" gsl:rng-type))
(define gsl:rng-ran1             (foreign-value "gsl_rng_ran1" gsl:rng-type))
(define gsl:rng-ran2             (foreign-value "gsl_rng_ran2" gsl:rng-type))
(define gsl:rng-ran3             (foreign-value "gsl_rng_ran3" gsl:rng-type))
(define gsl:rng-rand             (foreign-value "gsl_rng_rand" gsl:rng-type))
(define gsl:rng-rand48           (foreign-value "gsl_rng_rand48" gsl:rng-type))
(define gsl:rng-random128-bsd    (foreign-value "gsl_rng_random128_bsd" gsl:rng-type))
(define gsl:rng-random128-glibc2 (foreign-value "gsl_rng_random128_glibc2" gsl:rng-type))
(define gsl:rng-random128-libc5  (foreign-value "gsl_rng_random128_libc5" gsl:rng-type))
(define gsl:rng-random256-bsd    (foreign-value "gsl_rng_random256_bsd" gsl:rng-type))
(define gsl:rng-random256-glibc2 (foreign-value "gsl_rng_random256_glibc2" gsl:rng-type))
(define gsl:rng-random256-libc5  (foreign-value "gsl_rng_random256_libc5" gsl:rng-type))
(define gsl:rng-random32-bsd     (foreign-value "gsl_rng_random32_bsd" gsl:rng-type))
(define gsl:rng-random32-glibc2  (foreign-value "gsl_rng_random32_glibc2" gsl:rng-type))
(define gsl:rng-random32-libc5   (foreign-value "gsl_rng_random32_libc5" gsl:rng-type))
(define gsl:rng-random64-bsd     (foreign-value "gsl_rng_random64_bsd" gsl:rng-type))
(define gsl:rng-random64-glibc2  (foreign-value "gsl_rng_random64_glibc2" gsl:rng-type))
(define gsl:rng-random64-libc5   (foreign-value "gsl_rng_random64_libc5" gsl:rng-type))
(define gsl:rng-random8-bsd      (foreign-value "gsl_rng_random8_bsd" gsl:rng-type))
(define gsl:rng-random8-glibc2   (foreign-value "gsl_rng_random8_glibc2" gsl:rng-type))
(define gsl:rng-random8-libc5    (foreign-value "gsl_rng_random8_libc5" gsl:rng-type))
(define gsl:rng-random-bsd       (foreign-value "gsl_rng_random_bsd" gsl:rng-type))
(define gsl:rng-random-glibc2    (foreign-value "gsl_rng_random_glibc2" gsl:rng-type))
(define gsl:rng-random-libc5     (foreign-value "gsl_rng_random_libc5" gsl:rng-type))
(define gsl:rng-randu            (foreign-value "gsl_rng_randu" gsl:rng-type))
(define gsl:rng-ranf             (foreign-value "gsl_rng_ranf" gsl:rng-type))
(define gsl:rng-ranlux           (foreign-value "gsl_rng_ranlux" gsl:rng-type))
(define gsl:rng-ranlux389        (foreign-value "gsl_rng_ranlux389" gsl:rng-type))
(define gsl:rng-ranlxd1          (foreign-value "gsl_rng_ranlxd1" gsl:rng-type))
(define gsl:rng-ranlxd2          (foreign-value "gsl_rng_ranlxd2" gsl:rng-type))
(define gsl:rng-ranlxs0          (foreign-value "gsl_rng_ranlxs0" gsl:rng-type))
(define gsl:rng-ranlxs1          (foreign-value "gsl_rng_ranlxs1" gsl:rng-type))
(define gsl:rng-ranlxs2          (foreign-value "gsl_rng_ranlxs2" gsl:rng-type))
(define gsl:rng-ranmar           (foreign-value "gsl_rng_ranmar" gsl:rng-type))
(define gsl:rng-slatec           (foreign-value "gsl_rng_slatec" gsl:rng-type))
(define gsl:rng-taus             (foreign-value "gsl_rng_taus" gsl:rng-type))
(define gsl:rng-taus2            (foreign-value "gsl_rng_taus2" gsl:rng-type))
(define gsl:rng-taus113          (foreign-value "gsl_rng_taus113" gsl:rng-type))
(define gsl:rng-transputer       (foreign-value "gsl_rng_transputer" gsl:rng-type))
(define gsl:rng-tt800            (foreign-value "gsl_rng_tt800" gsl:rng-type))
(define gsl:rng-uni              (foreign-value "gsl_rng_uni" gsl:rng-type))
(define gsl:rng-uni32            (foreign-value "gsl_rng_uni32" gsl:rng-type))
(define gsl:rng-vax              (foreign-value "gsl_rng_vax" gsl:rng-type))
(define gsl:rng-waterman14       (foreign-value "gsl_rng_waterman14" gsl:rng-type))
(define gsl:rng-zuf              (foreign-value "gsl_rng_zuf" gsl:rng-type))

(define gsl:rng-env-setup (foreign-lambda gsl:rng-type "gsl_rng_env_setup"))
(define gsl:rng-default (foreign-value "gsl_rng_default" gsl:rng-type))
(define gsl:rng-default-seed (foreign-value "gsl_rng_default_seed" long))

(define (gsl:rng-alloc type) 
 (set-finalizer! 
  ((foreign-lambda gsl:rng "gsl_rng_alloc" gsl:rng-type) type)
  gsl:rng-free))
(define gsl:rng-memcpy (foreign-lambda int "gsl_rng_memcpy" gsl:rng gsl:rng))
(define (gsl:rng-clone rng)
 (set-finalizer! ((foreign-lambda gsl:rng "gsl_rng_clone" gsl:rng) rng)
                 gsl:rng-free))
(define gsl:rng-free (foreign-lambda void "gsl_rng_free" gsl:rng))

(define gsl:rng-set (foreign-lambda void "gsl_rng_set" gsl:rng long))
(define gsl:rng-max (foreign-lambda long "gsl_rng_max" gsl:rng))
(define gsl:rng-min (foreign-lambda long "gsl_rng_min" gsl:rng))
(define gsl:rng-name (foreign-lambda c-string "gsl_rng_name" gsl:rng))

(define gsl:rng-get (foreign-lambda long "gsl_rng_get" gsl:rng))
(define gsl:rng-uniform (foreign-lambda double "gsl_rng_uniform" gsl:rng))
(define gsl:rng-uniform-pos (foreign-lambda double "gsl_rng_uniform_pos" gsl:rng))
(define gsl:rng-uniform-int (foreign-lambda long "gsl_rng_uniform_int" gsl:rng long))
;; high-level API

(define (eigen-symmetric matrix)
 (map gsl->scheme (gsl-eigen-symm (scheme->gsl matrix))))

(define (power-of-two? n) (= (expt (/ (log n) (log 2)) 2) n))

(define (fft-with-direction _data direction)
 (let ((direction (cond ((equal? direction 'forward)
                         (foreign-value "gsl_fft_forward" int))
                        ((equal? direction 'backward)
                         (foreign-value "gsl_fft_backward" int))
                        (else (error "unknown direction"))))
       (data 
        (cond ((f64vector? _data) _data)
              ((vector? _data)
               (let ((d (make-f64vector (vector-length _data))))
                (for-each-n (lambda (i) (f64vector-set! d i (vector-ref _data i)))
                 (vector-length _data))
                d))
              (else (error "fft unknown data type")))))
  (if (power-of-two? (/ (f64vector-length data) 2))
      ((foreign-lambda void "gsl_fft_complex_radix2_transform"
                       nonnull-f64vector integer integer integer)
       data 1 (/ (f64vector-length data) 2) direction)
      ((foreign-lambda* void ((nonnull-f64vector data) (integer nn) (integer direction))
                        "gsl_fft_complex_wavetable * wavetable = gsl_fft_complex_wavetable_alloc(nn);"
                        "gsl_fft_complex_workspace * workspace = gsl_fft_complex_workspace_alloc (nn);"
                        "gsl_fft_complex_transform (data, 1, nn, wavetable, workspace, direction);"
                        "gsl_fft_complex_wavetable_free (wavetable);"
                        "gsl_fft_complex_workspace_free (workspace);")
       data (/ (f64vector-length data) 2) direction))
  (when (equal? direction (foreign-value "gsl_fft_backward" int))
   ((foreign-lambda* void ((nonnull-f64vector data) (integer nn))
                     "for(unsigned int i=0; i<2*nn; ++i) data[i] /= nn;")
    data (/ (f64vector-length data) 2)))
  (cond ((f64vector? _data) data)
        ((vector? _data)
         (let ((d (make-vector (vector-length _data))))
          (for-each-n (lambda (i) (vector-set! d i (f64vector-ref data i)))
           (vector-length _data))
          d))
        (else (error "fft unknown data type")))))

(define (fft data) (fft-with-direction data 'forward))
(define (ifft data) (fft-with-direction data 'backward))

(define (real->complex v)
 (let ((c (make-vector (* 2 (vector-length v)))))
  (for-each-n (lambda (n)
               (vector-set! c (* n 2) (vector-ref v n))
               (vector-set! c (+ (* n 2) 1) 0))
   (vector-length v))
  c))
(define (complex->real-part v)
 (let ((c (make-vector (inexact->exact (/ (vector-length v) 2)))))
  (for-each-n (lambda (n) (vector-set! c n (vector-ref v (* n 2))))
   (/ (vector-length v) 2))
  c))
(define (complex->complex-part v)
 (let ((c (make-vector (/ (vector-length v) 2))))
  (for-each-n (lambda (n) (vector-set! c n (vector-ref v (+ (* n 2) 1))))
   (/ (vector-length v) 2))
  c))
)
