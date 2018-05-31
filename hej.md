Inspiration for ALM assignment
========================================================
author: Silas Furu Friby
date: 2018-04-22
autosize: true

Pension Company
========================================================

We consider a pension company with a simplified balance consisting of

*Assets*

- Present value of assets at time $t$, $A(t)$ (market value)

*Liabilities*
- Present value of liabilites at time $t$, $L(t)$ (market value, different from article)
- Free capital at time $t$, $B(t)$

Financial market - P-dynamics
========================================================


$$
\begin{aligned}
d\beta(t) &= r(t) \beta(t) dt \\
dr(t) &= \kappa (\theta^P - r(t)) dt + \sigma_r dW^P_1(t) dt \\
dS(t) &= \mu S(t) dt + \sigma_s S(t) dW^P_3(t)
\end{aligned}
$$

$dW^P_3(t) = \rho dW^P_1(t) + \sqrt{1 - \rho ^ 2} dW^P_2(t)$


Financial market - Q-dynamics
========================================================


$$
\begin{aligned}
dr(t) &= \kappa (\theta - r(t)) dt + \sigma_r dW_1(t) dt \\
dS(t) &= r(t) S(t) dt + \sigma_s S(t) dW_3(t)
\end{aligned}
$$

$\theta = \theta^P - \frac{\lambda \sigma_r}{\kappa}$

$dW_3(t) = \rho dW_1(t) + \sqrt{1 - \rho ^ 2} dW_2(t)$


Financial market - ZCB dynamics
========================================================

P-dynamics

$$dp(t, T) = p(t, T) (r(t) - \lambda \sigma_r B(t,T)) dt - \sigma_r B(t,T) dW^P_1(t) dt$$

Q-dynamics

$$dp(t, T) = p(t, T) r(t) dt - \sigma_r B(t,T) dW_1(t) dt$$

$B(t,T) = \frac{1}{\kappa}\left(1 - e^{-\kappa (T - t)} \right)$


Financial market - Asset dynamics
========================================================

Investment in bank, stocks and ZCBs

$$dA(t) = c_\beta(t) d\beta(t) + c_s(t) dS(t) + \sum_{j = 1}^T c_{ij}(t) dp(t, i + j) $$

for $t \in [i, i + 1)$.

Note: We need only to simulate under $P$ in order to obtain $A(t)$ for $t \in [0, T]$. Why no need to simulate under $Q$?


Financial market - Liability model
========================================================

Simple savings contract.

$$L(T) = premium * (1 + q)^T$$

Where $premium$ is initial payment and $q$ is a constant "guaranteed" interest rate.

$$
\begin{aligned}
L(t) &= E^Q \left[ e^{\int_t^T r(s) ds} L(T) | \mathcal{F}_t \right] \\ 
&= E^Q \left[ e^{\int_t^T r(s) ds} | \mathcal{F}_t \right] L(T) \\
&= p(t,T) L(T)
\end{aligned}
$$


Simulated asset paths - 100% Stock
========================================================

<img src="../plots/asset_paths_stocks.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" width="800px" height="500px" />

Simulated asset paths - 100% Bank
========================================================

<img src="../plots/asset_paths_bank.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="800px" height="500px" />

Simulated asset paths - 100% Bonds 
========================================================

<img src="../plots/asset_paths_bonds.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="800px" height="500px" />

Asset distribution - 100% Stock 
========================================================

<img src="../plots/asset_distribution_stocks.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="800px" height="500px" />


Asset distribution - 100% Bank 
========================================================

<img src="../plots/asset_distribution_bank.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="800px" height="500px" />

Asset distribution - 100% Bonds
========================================================

<img src="../plots/asset_distribution_bonds.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="800px" height="500px" />


Remarks on previous plots
========================================================

Combination of bank, stocks and bonds influence

- Skewness of asset distribution at different times (median vs mean)
- $P(A(T) < L(T))$

Results are very sensitive to changes in model parameters

- given current parameters: approx 10% stocks and 90% to minimize $P(A(T) < L(T))$
- increasing $\kappa$: approx 100% bank to minimize $P(A(T) < L(T))$

However, usually we also wish to create som profit!


Risk management - Solvency 2 and VaR
========================================================


Solvency 2 SCR = free capital today must insure that risk of default during next year is less than 99.5%

In ohter words, Solvency 2 SCR = 99.5% quantile of 1-year loss distribution

Solvency 2 risk measure: *Value at Risk* (VaR)

$$ Loss(t) = B(t) - e^{-\int_t^{t+1} r (s) ds} B(t + 1) $$

$$ CR(t) = \frac{B(t)}{SCR(t)} $$

FSA supervision of company if $CR$ falls below regulatory level


Risk management - Example
========================================================

Initial premium = 1000

"Guaranteed" interest rate = 2.5%

B(0) = 100 

10% stocks 90% bonds




Risk management - example: CR paths
========================================================

<img src="../plots/CR.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="800px" height="500px" />


Risk management - example: insolvency prob
========================================================

<img src="../plots/insol.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="800px" height="500px" />


The End
========================================================

Thank you!
© 2018 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
API
Training
Shop
Blog
About
