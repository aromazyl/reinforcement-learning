/*
 * tabular_td0.cc
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#include <stdio.h>
#include "tabular_td0.h"


namespace rl {
namespace td0 {
  float CommonSettings::learning_rate = 0.1;
  float CommonSettings::discounting = 0.1;
  int TD0::RunOneEpisode(Policy* policy) {
    int current_state = 2; // C
    int next_state = 0;
    int reward = 0;
    int walks = 0;
    while (current_state != 0 && current_state != 6) {
      walks += 1;
      int action = policy->take_action(current_state, _value, 6);
      next_state = current_state + action;
      float reward = (next_state == 6 ? 1 : 0);
      _value[current_state] += CommonSettings::learning_rate *
        (reward + CommonSettings::discounting * _value[next_state]
         - _value[current_state]);
      current_state = next_state;
    }
    return walks;
  }

  void TD0::DumpValue() {
    for (int i = 0; i < 7; ++i) {
      printf("V(%d)=%f\t", i, _value[i]);
    }
    printf("\n");
  }
}
}
