/*
 * sarsa.h
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#ifndef SARSA_H
#define SARSA_H

#include "matrix.h"
#include "policy_ext.h"

namespace rl {
  namespace sarsa {
    struct CommonSettings {
      static float learning_rate;
      static float discounting;
    };

    template <typename MatrixT>
    class Sarsa {
    public:
      enum { x = MatrixT::row, y = MatrixT::col };
      int RunOneEpisode(policy::Policy<MatrixT>* policy) {
        int state = x / 2;
        int next_state = 0;
        int action = 0;
        int action2 = 0;
        int walks = 0;
        while (state != 0 && state != x - 1) {
          action = policy->TakeAction(_matrix, state);
          next_state = state + action;
          float reward = (next_state == (x - 1));
          action2 = policy->TakeAction(_matrix, next_state);
          _matrix[state][action == -1] += CommonSettings::learning_rate * (
              reward + CommonSettings::discounting *
              _matrix[next_state][action2 == -1] - _matrix[state][action == -1]);
          state = next_state;
          ++walks;
        }
        return walks;
      }

      void Run(int turns, policy::Policy<MatrixT>* policy) {
        int walks = 0;
        for (int i = 0; i < turns; ++i) {
          walks += this->RunOneEpisode(policy);
        }
        _matrix.print("SarsaQ");
        printf("walks / episodes:%d\n", walks / turns);
      }

    private:
      MatrixT _matrix;
    };

    template <typename MatrixT>
      class NStepSarsa {
      };
  }
}

#endif /* !SARSA_H */
