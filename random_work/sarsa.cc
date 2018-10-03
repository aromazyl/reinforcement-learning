/*
 * sarsa.cc
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#include "sarsa.h"



namespace rl {
  namespace sarsa {
    float CommonSettings::learning_rate = 0.1;
    float CommonSettings::discounting = 0.1;
  }
}
