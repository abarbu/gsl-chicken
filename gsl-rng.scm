;;; Default RNG

(foreign-declare "#include <gsl/gsl_rng.h>")
(foreign-declare "#include <gsl/gsl_randist.h>")
(foreign-declare "#include <gsl/gsl_cdf.h>")

(define-structure gsl:rng handle)
(define-foreign-type gsl:rng (c-pointer "gsl_rng") gsl:rng-handle make-gsl:rng)
(define-structure gsl:rng-type handle)
(define-foreign-type gsl:rng-type (c-pointer "gsl_rng_type") 
 gsl:rng-type-handle make-gsl:rng-type)

(define *gsl-rng* #f)
(define (setup-default-gsl-rng!)
 ;; Note that the default seed is 0
 (unless *gsl-rng*
  (gsl:rng-env-setup)
  (set! *gsl-rng* (gsl:rng-alloc gsl:rng-default))
  *gsl-rng*))

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
