## Repository Structure

BTC_Granger_Causality_test.R – main script performing the analysis  
BTC_Causality_test_description.pdf – project description and methodology

This project investigates whether mining difficulty exerts a causal effect on Bitcoin’s USD exchange rate by acting as a short-term supply friction.
The null hypothesis tested is that difficulty does not Granger-cause Bitcoin returns, implying a purely passive role in the pricing process. If the null is not rejected, this may indicate that mining difficulty plays no causal role—or that the protocol is functioning as intended:
stabilizing quantity and letting prices absorb demand shocks, but with no immediate reflection in the USD exchange rate if miners delay converting rewards into fiat.
The empirical analysis proceeds in four steps: First, an ARIMA model establishes a benchmark for Bitcoin returns. Second, an ARIMAX model incorporates macroeconomic, behavioral, and blockchain variables to test their predictive power. Third, Granger causality is tested between mining difficulty and returns. Finally, a GARCH(1,1) model evaluates volatility clustering and the explanatory power of conditional variance. 
