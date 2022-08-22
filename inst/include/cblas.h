#ifndef CBLAS_H
#define CBLAS_H

#include <stddef.h>
#include <Rconfig.h>
#include <R_ext/BLAS.h>
#include <R.h>
#ifndef FCONE
# define FCONE
#endif

#ifdef __cplusplus
extern "C" { /* Assume C declarations for C++ */
#endif       /* __cplusplus */

/*
 * Enumerated and derived types
 */
#ifdef WeirdNEC
#define CBLAS_INDEX long
#else
#define CBLAS_INDEX int
#endif

typedef enum { CblasRowMajor = 101, CblasColMajor = 102 } CBLAS_LAYOUT;
typedef enum {
  CblasNoTrans = 111,
  CblasTrans = 112,
  CblasConjTrans = 113
} CBLAS_TRANSPOSE;
typedef enum { CblasUpper = 121, CblasLower = 122 } CBLAS_UPLO;
typedef enum { CblasNonUnit = 131, CblasUnit = 132 } CBLAS_DIAG;
typedef enum { CblasLeft = 141, CblasRight = 142 } CBLAS_SIDE;

typedef CBLAS_LAYOUT
    CBLAS_ORDER; /* this for backward compatibility with CBLAS_ORDER */

#define F77_INT int

#include "cblas/cblas_globals.h"
#include "cblas/cblas_xerbla.h"
#include "cblas/xerbla.h"

/* Double Precision Level 3 BLAS */

#include "cblas/cblas_dgemm.h"

#ifdef __cplusplus
}
#endif

#endif
