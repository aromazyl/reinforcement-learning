/*
 * control.h
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#ifndef CONTROL_H
#define CONTROL_H

namespace rl {
  namespace control {
    template <typename Matrix>
      class QualityLearning {
        public:
          typedef Matrix MatrixT;

        public:
          virtual ~QualityLearning() {}

        public:
          virtual 

        private:
            Matrix _mat;
      };
  }
}

#endif /* !CONTROL_H */
