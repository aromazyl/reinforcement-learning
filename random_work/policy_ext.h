/*
 * policy_ext.h
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#ifndef POLICY_EXT_H
#define POLICY_EXT_H
#include "matrix.h"
#include <time.h>
#include <stdlib.h>

namespace rl {
  namespace policy {
    template <typename Matrix>
      struct Policy {
        typedef Matrix MatrixT;
        virtual int TakeAction(const Matrix& matrix, int current_state) = 0;
        virtual ~Policy() {}
      };

    template <typename Matrix>
      struct FullyRandom : Policy<Matrix> {
        ~FullyRandom() {}
        FullyRandom() {
          srand(time(NULL));
        }
        int TakeAction(const Matrix& matrix, int current_state) {
          int res = int((double)rand() / (double)RAND_MAX > 0.5);
          return res * 2 - 1;
        }
      };
  }
}


#endif /* !POLICY_EXT_H */
