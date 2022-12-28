# brms_test_cases

## Test cases for latent AR/ARMA models in brms

This repository contains three scripts with test cases for a latent variable formulation
of ARMA(p,q) models in `brms`. This uses the fork of `brms` available at the 
`mpdylan/brms` branch `autocor_ranef`. One test script uses simulated data,
while the other two use real-world data sets obtained from Kaggle and the UC
Irvine Machine Learning Repository.

* `synthetic_test.R` contains several test cases using synthetic data. Simple
time series for "true" underlying parameters are generated using `arima.sim`.
Observations are generated using `rnorm` or `rbinom` and then models are fit.
This demonstrates the use of an explicit time variable to enable interpolation
as well as handling multiple observations at a single time point.
* `bike_share_test.R` contains several test cases based on data from the Capital
Bike Share system in Washington, DC. Here, we use weather conditions
(temperature and wind speed) to predict ridership, while modeling the change in
ridership over time with an AR(1) process. The model is fit once without any
grouping factors, and once with a grouping factor indicating whether a given
day is a working day or a non-working day. Then, the model is re-fit with 
observations aggregated by week to pool observations at the same time point.
* `chickenpox_test.R` contains several test cases based on counts of chickenpox
cases in several cities in Hungary over a period of about one year. The purpose
of this test case is to demonstrate the use of a non-Gaussian likelihood (in
this case, Poisson) as well as to demonstrate backcasting as well as 
forecasting. There are also secondary test cases to demonstrate the latent
variable formulation in the absence of an explicit time variable or grouping
factor.