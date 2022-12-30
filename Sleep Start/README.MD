# [<img src="https://github.com/roflecopter/qs-analysis/blob/main/Sleep%20Start/regression.png">](https://github.com/roflecopter/qs-analysis/)

```Response TST :

Call:
lm(formula = TST ~ StartTime, data = dreem)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.6040 -0.3347  0.1767  0.4709  1.4574 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  19.5134     2.9110   6.703 4.37e-10 ***
StartTime    -0.5601     0.1325  -4.228 4.19e-05 ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7744 on 143 degrees of freedom
Multiple R-squared:  0.1111,	Adjusted R-squared:  0.1049 
F-statistic: 17.88 on 1 and 143 DF,  p-value: 4.187e-05



Response DEEP :

Call:
lm(formula = DEEP ~ StartTime, data = dreem)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.86080 -0.25705 -0.07327  0.21778  1.00792 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  3.35478    1.36085   2.465   0.0149 *
StartTime   -0.07400    0.06193  -1.195   0.2341  
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.362 on 143 degrees of freedom
Multiple R-squared:  0.009885,	Adjusted R-squared:  0.002961 
F-statistic: 1.428 on 1 and 143 DF,  p-value: 0.2341


Response REM :

Call:
lm(formula = REM ~ StartTime, data = dreem)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.15452 -0.36894  0.00812  0.33504  1.38979 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  2.83648    1.98570   1.428    0.155
StartTime   -0.05841    0.09037  -0.646    0.519

Residual standard error: 0.5283 on 143 degrees of freedom
Multiple R-squared:  0.002913,	Adjusted R-squared:  -0.00406 
F-statistic: 0.4178 on 1 and 143 DF,  p-value: 0.5191


Response SOL :

Call:
lm(formula = SOL ~ StartTime, data = dreem)

Residuals:
    Min      1Q  Median      3Q     Max 
-15.576  -7.216  -2.670   4.163  61.907 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -42.466     42.645  -0.996    0.321
StartTime      2.826      1.941   1.456    0.148

Residual standard error: 11.35 on 143 degrees of freedom
Multiple R-squared:  0.01461,	Adjusted R-squared:  0.007717 
F-statistic:  2.12 on 1 and 143 DF,  p-value: 0.1476


Response WASO :

Call:
lm(formula = WASO ~ StartTime, data = dreem)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.47806 -0.23204 -0.11823  0.08589  1.76773 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.21103    1.41582  -0.149    0.882
StartTime    0.03066    0.06443   0.476    0.635

Residual standard error: 0.3767 on 143 degrees of freedom
Multiple R-squared:  0.001581,	Adjusted R-squared:  -0.005401 
F-statistic: 0.2264 on 1 and 143 DF,  p-value: 0.6349


Response Number.of.awakenings :

Call:
lm(formula = Number.of.awakenings ~ StartTime, data = dreem)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.5554 -1.4558 -0.3744  1.6138  8.5154 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   1.5446     9.7739   0.158    0.875
StartTime     0.1742     0.4448   0.392    0.696

Residual standard error: 2.6 on 143 degrees of freedom
Multiple R-squared:  0.001072,	Adjusted R-squared:  -0.005914 
F-statistic: 0.1535 on 1 and 143 DF,  p-value: 0.6958


Response Position.Changes :

Call:
lm(formula = Position.Changes ~ StartTime, data = dreem)

Residuals:
     Min       1Q   Median       3Q      Max 
-16.5874  -4.2643  -0.2878   4.0179  16.9139 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   1.3390    25.2407   0.053    0.958
StartTime     0.9568     1.1487   0.833    0.406

Residual standard error: 6.715 on 143 degrees of freedom
Multiple R-squared:  0.004829,	Adjusted R-squared:  -0.002131 
F-statistic: 0.6938 on 1 and 143 DF,  p-value: 0.4063

StartTime    StartTime    StartTime    StartTime 
0.0002930587 0.5462725477 0.6958398243 0.6958398243 
   StartTime    StartTime    StartTime 
0.5165718622 0.6958398243 0.6958398243```
