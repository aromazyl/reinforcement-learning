#pragma once

#include <stdio.h>
#include "policy.h"

namespace rl {
  namespace td0 {
    struct CommonSettings {
      static float learning_rate;
      static float discounting;
    };

    class TD0 {
    public:
      TD0() {
        _value[6] = 1;
      }
      int RunOneEpisode(Policy* policy);
      void DumpValue();
      void Run(int turns, Policy* policy) {
        int walks = 0;
        for (int i = 0; i < turns; ++i) {
          walks += this->RunOneEpisode(policy);
        }
        this->DumpValue();
        printf("walks / episode:%d\n", walks / turns);
      }
    private:
      float _value[7] = {0};
    };
  }
}
