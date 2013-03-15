(module gsl *
(import chicken scheme foreign)
(use foreigners define-structure srfi-4)
(use (except linear-algebra
             v+ v- v*  v/  k*v k+v v/k v*k v+k
             m+ m- m*. m/. k*m m+k m/k m*k k+m
             for-each-matrix)
     (except traversal for-each-vector))
(use (prefix traversal tr-)
     (prefix linear-algebra la-))

(include "gsl-data-structures.scm")
(include "gsl-eigen.scm")
(include "gsl-rng.scm")
(include "gsl-fft.scm")
)
