/*****************************************************************************
  Copyright (c) 2014, Intel Corp.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Intel Corporation nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
  THE POSSIBILITY OF SUCH DAMAGE.
******************************************************************************
* Contents: Native C interface to LAPACK utility functions
* Author: Intel Corporation
* Created in January, 2010
*****************************************************************************/

#ifndef _LAPACKE_UTILS_H_
#define _LAPACKE_UTILS_H_

// #include "lapacke.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#ifndef ABS
#define ABS(x) (((x) < 0) ? -(x) : (x))
#endif
#ifndef MAX
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif
#ifndef MIN
#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#endif
#ifndef MAX3
#define MAX3(x, y, z) (((x) > MAX(y, z)) ? (x) : MAX(y, z))
#endif
#ifndef MIN3
#define MIN3(x, y, z) (((x) < MIN(y, z)) ? (x) : MIN(y, z))
#endif

#define IS_S_NONZERO(x) ((x) < 0 || (x) > 0)
#define IS_D_NONZERO(x) ((x) < 0 || (x) > 0)
#define IS_C_NONZERO(x) \
  (IS_S_NONZERO(*((float *)&x)) || IS_S_NONZERO(*(((float *)&x) + 1)))
#define IS_Z_NONZERO(x) \
  (IS_D_NONZERO(*((double *)&x)) || IS_D_NONZERO(*(((double *)&x) + 1)))

/* Error handler */
void LAPACKE_xerbla(const char *name, lapack_int info);

/* Compare two chars (case-insensitive) */
lapack_logical LAPACKE_lsame(char ca, char cb);

/* Functions to convert column-major to row-major 2d arrays and vice versa. */
void LAPACKE_dsy_trans(int matrix_layout, char uplo, lapack_int n,
                       const double *in, lapack_int ldin, double *out,
                       lapack_int ldout);
void LAPACKE_dge_trans(int matrix_layout, lapack_int m, lapack_int n,
                       const double *in, lapack_int ldin, double *out,
                       lapack_int ldout);
void LAPACKE_dtr_trans(int matrix_layout, char uplo, char diag,
                       lapack_int n, const double *in, lapack_int ldin,
                       double *out, lapack_int ldout);
/* NaN checkers */
#define LAPACK_SISNAN(x) (x != x)
#define LAPACK_DISNAN(x) (x != x)
#define LAPACK_CISNAN(x) \
  (LAPACK_SISNAN(*((float *)&x)) || LAPACK_SISNAN(*(((float *)&x) + 1)))
#define LAPACK_ZISNAN(x) \
  (LAPACK_DISNAN(*((double *)&x)) || LAPACK_DISNAN(*(((double *)&x) + 1)))

/* NaN checkers for vectors */
lapack_logical LAPACKE_d_nancheck(lapack_int n, const double *x,
                                  lapack_int incx);

/* NaN checkers for matrices */
lapack_logical LAPACKE_dsy_nancheck(int matrix_layout, char uplo, lapack_int n,
                                    const double *a, lapack_int lda);

lapack_logical LAPACKE_dge_nancheck(int matrix_layout, lapack_int m,
                                    lapack_int n, const double *a,
                                    lapack_int lda);

lapack_logical LAPACKE_dtr_nancheck(int matrix_layout, char uplo,
                                    char diag, lapack_int n,
                                    const double *a, lapack_int lda);
/* Error handler */
#include "lapacke_utils/lapacke_xerbla.h"

/* Compare two chars (case-insensitive) */
#include "lapacke_utils/lapacke_lsame.h"

/* Functions to convert column-major to row-major 2d arrays and vice versa. */
#include "lapacke_utils/lapacke_dsy_trans.h"
#include "lapacke_utils/lapacke_dge_trans.h"
#include "lapacke_utils/lapacke_dtr_trans.h"

/* NaN checkers for vectors */
#include "lapacke_utils/lapacke_d_nancheck.h"

/* NaN checkers for matrices */
#include "lapacke_utils/lapacke_dsy_nancheck.h"
#include "lapacke_utils/lapacke_dge_nancheck.h"
#include "lapacke_utils/lapacke_dtr_nancheck.h"

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _LAPACKE_UTILS_H_ */
