/*
 * matrix.h
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#ifndef MATRIX_H
#define MATRIX_H
#include <stdio.h>
#include <string.h>

namespace rl {
  namespace matrix {
    template <int x, int y, typename T>
      struct Matrix {
        Matrix() {
          memset(val, 0, sizeof(T) * x * y);
        }
        T val[x][y];
        typedef T type;
        enum {
          row = x,
          col = y
        };
        void print(const char* name) {
          for (int i = 0; i < x; ++i) {
            for (int j = 0; j < y; ++j) {
              printf("%s[%d][%d]=%f\t", name, i, j, val[i][j]);
            }
            printf("\n");
          }
        }
        T* operator[](int k) {
          return val[k];
        }
      };
  }
}


#endif /* !MATRIX_H */
