# [<img src="https://github.com/roflecopter/qs-analysis/blob/main/Deep%20Sleep%20Stims/regression.png">](https://github.com/roflecopter/qs-analysis/)

```
Call:
lm(formula = DEEP ~ Number.of.Stimulations, data = dreem)

Residuals:
    Min      1Q  Median      3Q     Max 
-43.055 -14.494  -2.857  12.111  54.287 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)            92.81139    3.55987  26.072  < 2e-16 ***
Number.of.Stimulations  0.03528    0.01002   3.521 0.000577 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 20.94 on 143 degrees of freedom
Multiple R-squared:  0.07978,	Adjusted R-squared:  0.07335 
F-statistic:  12.4 on 1 and 143 DF,  p-value: 0.0005771

[1] "weak"

                           47.5 %      52.5 %
(Intercept)            92.5877661 93.03500637
Number.of.Stimulations  0.0346501  0.03590891
```
