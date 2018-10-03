/*
 * tabular_tdn.h
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#ifndef TABULAR_TDN_H
#define TABULAR_TDN_H

#include <math.h>
#include <assert.h>
#include <limits.h>
#include <algorithm>
#include "policy_ext.h"


namespace rl {
  namespace td {
    struct CommonSettings {
      static float learning_rate;
      static float discounting;
    };

    template <typename T, int step>
    struct Queue {
      int size = 0;
      T mem[step + 1];
      int head = 0;
      int front = 1;

      void enque(float x) {
        assert(this->size <= step);
        head += 1;
        head %= step + 1;
        mem[head] = x;
        size += 1;
      }
      T deque() {
        assert(this->size > 0);
        const T& ret = mem[front];
        front = (front + 1) % (step + 1);
        size -= 1;
        return ret;
      }

    };

    template <typename MatrixT, int step>
      class NStepTD {
        public:
          int RunOneEpisode(policy::Policy<MatrixT>* policy) {
            Queue<float, step> que;
            Queue<int, step> state_que;
            int state = MatrixT::row / 2;
            int next_state = 0;
            int action;
            int t = 0;
            float G = 0;
            int walk = 0;
            double prev_add_v = 0;
            int T = INT_MAX;
            int tau = 0;
            float reward_vec[10000];
            int state_vec[10000];
            while (true) {
              if (tau == T - 1) break;
              walk += 1;
              if (t < T) {
                action = policy->TakeAction(_matrix, state);
                next_state = action + state;
                float reward = (next_state == (MatrixT::row-1));
                reward_vec[t+1] = reward;
                state_vec[t] = state;
                state_vec[t+1] = next_state;
                if (next_state == 0 || next_state == MatrixT::row - 1)
                  T = t + 1;
              }
              tau = t - step + 1;
              if (tau >= 0) {
                G = 0;
                if (tau + step < T) {
                  G += _matrix[state_vec[tau + step]][0];
                }
                for (int i = std::min(tau+step, T); i >= tau + 1; --i) {
                  G = G * CommonSettings::discounting + reward_vec[i];
                }
                _matrix[state_vec[tau]][0] += CommonSettings::learning_rate * (G - _matrix[state_vec[tau]][0]);
              }

              state = next_state;
              t += 1;
            }
            return walk;
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

  }
}

#endif /* !TABULAR_TDN_H */
