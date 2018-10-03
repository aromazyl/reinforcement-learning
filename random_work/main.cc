/*
 * main.cc
 * Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
 *
 * Distributed under terms of the MIT license.
 */

#include "matrix.h"
#include "tabular_tdn.h"


using namespace rl;

int main(int argc, char* argv[]) {
  rl::td::CommonSettings::learning_rate = .01;
  rl::td::CommonSettings::discounting = 0.8;
  typedef matrix::Matrix<20, 1, double> QMatrix;
  policy::Policy<QMatrix>* policy_ptr = new policy::FullyRandom<QMatrix>;
  td::NStepTD<QMatrix, 5> solver;
  solver.Run(100000, policy_ptr);
  delete policy_ptr;
  return 0;
}
