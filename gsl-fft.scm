;; FFT

(foreign-declare "#include <gsl/gsl_fft.h>")
(foreign-declare "#include <gsl/gsl_fft_complex.h>")

(define (eigen-symmetric matrix)
 (map gsl->matrix (gsl-eigen-symm (matrix->gsl matrix))))

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
