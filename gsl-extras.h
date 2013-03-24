
#define GSL_VECTOR_VECTOR_OP3(name, op)                                 \
  int name (gsl_vector *a, const gsl_vector *b, const gsl_vector *c)    \
  {                                                                     \
    const size_t N = a->size;                                            \
    if (b->size != N && c->size != N)                                        \
      GSL_ERROR ("vectors must have same length", GSL_EBADLEN);         \
    else                                                                \
      {                                                                 \
        const size_t stride_a = a->stride;                               \
        const size_t stride_b = b->stride;                               \
        const size_t stride_c = c->stride;                               \
                                                                        \
        size_t i;                                                       \
                                                                        \
        for (i = 0; i < N; i++)                                         \
          {                                                             \
            a->data[i * stride_a] =                                      \
              b->data[i * stride_b] op c->data[i * stride_c];             \
          }                                                             \
                                                                        \
        return GSL_SUCCESS;                                             \
      }                                                                 \
  }

#define GSL_VECTOR_SCALAR_OP3(name, op)                                  \
  int name (gsl_vector *a, const gsl_vector *b, const double c)         \
  {                                                                     \
    const size_t N = a->size;                                            \
    if (b->size != N)                                                     \
      GSL_ERROR ("vectors must have same length", GSL_EBADLEN);         \
    else                                                                \
      {                                                                 \
        const size_t stride_a = a->stride;                               \
        const size_t stride_b = b->stride;                               \
                                                                        \
        size_t i;                                                       \
                                                                        \
        for (i = 0; i < N; i++)                                         \
          {                                                             \
            a->data[i * stride_a] = b->data[i * stride_b] op c;           \
          }                                                             \
                                                                        \
        return GSL_SUCCESS;                                             \
      }                                                                 \
  }

#define GSL_MATRIX_MATRIX_OP3(name, op)                                 \
  int name (gsl_matrix *a, const gsl_matrix *b, const gsl_matrix *c)    \
  {                                                                     \
    const size_t M = a->size1;                                           \
    const size_t N = a->size2;                                           \
                                                                        \
    if (b->size1 != M || b->size2 != N || c->size1 != M || c->size2 != N)          \
      {                                                                 \
        GSL_ERROR ("matrices must have same dimensions", GSL_EBADLEN);  \
      }                                                                 \
    else                                                                \
      {                                                                 \
        const size_t tda_a = a->tda;                                     \
        const size_t tda_b = b->tda;                                     \
        const size_t tda_c = c->tda;                                     \
                                                                        \
        size_t i, j;                                                    \
                                                                        \
        for (i = 0; i < M; i++)                                         \
          {                                                             \
            for (j = 0; j < N; j++)                                     \
              {                                                         \
                a->data[i * tda_a + j] =                                 \
                  b->data[i * tda_b + j] op c->data[i * tda_c + j];       \
              }                                                         \
          }                                                             \
                                                                        \
        return GSL_SUCCESS;                                             \
      }                                                                 \
  }

#define GSL_MATRIX_SCALAR_OP3(name, op)                                 \
  int name (gsl_matrix *a, const gsl_matrix *b, const double c)         \
  {                                                                     \
    const size_t M = a->size1;                                           \
    const size_t N = a->size2;                                           \
                                                                        \
    if (b->size1 != M || b->size2 != N)                                      \
      {                                                                 \
        GSL_ERROR ("matrices must have same dimensions", GSL_EBADLEN);  \
      }                                                                 \
    else                                                                \
      {                                                                 \
        const size_t tda_a = a->tda;                                     \
        const size_t tda_b = b->tda;                                     \
                                                                        \
        size_t i, j;                                                    \
                                                                        \
        for (i = 0; i < M; i++)                                         \
          {                                                             \
            for (j = 0; j < N; j++)                                     \
              {                                                         \
                a->data[i * tda_a + j] = b->data[i * tda_b + j] op c;     \
              }                                                         \
          }                                                             \
                                                                        \
        return GSL_SUCCESS;                                             \
      }                                                                 \
  }

#define GSL_MATRIX_SCALAR_DIAGONAL_OP3(name, op)                \
  int name (gsl_matrix *a, gsl_matrix *b, const double x)       \
  {                                                             \
    const size_t M = a->size1;                                   \
    const size_t N = a->size2;                                   \
    const size_t tda = a->tda;                                   \
    const size_t tdb = a->tda;                                   \
    const size_t loop_lim = ( M < N ? M : N );                  \
    size_t i;                                                           \
    if (b->size1 != M || b->size2 != N)                                      \
      {                                                                 \
        GSL_ERROR ("matrices must have same dimensions", GSL_EBADLEN);  \
      }                                                                 \
    else                                                                \
      {                                                                 \
        for (i = 0; i < loop_lim; i++)                                  \
          {                                                             \
            a->data[i * tda + i] = b->data[i * tdb + i] op x;             \
          }                                                             \
                                                                        \
        return GSL_SUCCESS;                                             \
      }                                                                 \
  }
