/*
  This is a comment
*/
# This is a comment
// this is a comment
functions {
  void f0(real a) {
    target += a;
  }
  real[,] f1(real[,,] a) {
    return sum(a);
  }
  row_vector[] f2(real a, int b, matrix[] c) {
    real d;
    d = a + b;
    return d;
  }
}
data {
  real delta;
}
transformed data {
  real charlie;
  charlie = 1 * 2 / 3;
}
parameters {
  real bravo;
}
transformed parameters {
  #include something
}
model {
  real foo;
  vector[1] baz;
  row_vector[2] qux;
  matrix[2, 3] quux;
  cov_matrix[2] alpha;
  corr_matrix[2] beta;
  cholesky_factor_corr[2] gamma;
  cholesky_factor_cov[2] delta;
  real eps[5, 5];
  real phi = 1;
  positive_ordered[3] xi;
  ordered[4] zeta;
  unit_vector[3] omega;
  simplex[5] theta;

  target += normal_lpdf(0.5 | 0, 1);
  foo = 0.5;
  foo <- 0.5;
  foo ~ normal(0, 1);
  print("Hello, World!");
  reject("Error");
  foo <- bar[1][2];
}
generated quantities {
  real bravo;
  bravo = 1 * 2 / 3;
}
