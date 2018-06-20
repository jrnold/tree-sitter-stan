/*
  This is a comment
*/
# This is a comment
// this is a comment
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
  //int<lower=0> bar1;
  //int<upper=0> bar2;
  // int<lower=0, upper=1> bar3;
  // int<> bar4;
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
