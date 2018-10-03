/*
 * policy.h
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#ifndef POLICY_H
#define POLICY_H

#include <time.h>
#include <stdlib.h>

namespace rl {
  namespace td0 {
    struct Policy {
      // -1 1, left, right
      virtual int take_action(int state, float* value, int N) = 0;
      virtual ~Policy() {}
    };

    class EpisilonGreedy : public Policy {
    public:
      EpisilonGreedy() {
        srand(time(NULL));
      }
      ~EpisilonGreedy() {
      }
      int take_action(int state, float* value, int N) {
        float epsilon = 0.9;
        float seed = double(rand()) / (double)RAND_MAX;
        if (seed < epsilon) {
          return value[state-1] < value[state+1] ? -1 : 1;
        } else {
          return value[state-1] < value[state+1] ? 1 : -1;
        }
      }
    };

    class FullyRandom : public Policy {
    public:
      FullyRandom() {
        srand(time(NULL));
      };
      ~FullyRandom() {}
      int take_action(int state, float* value, int N) {
        float seed = double(rand()) / (double)RAND_MAX;
        return seed > 0.5 ? -1 : 1;
      }
    };

  }
}


#endif /* !POLICY_H */
