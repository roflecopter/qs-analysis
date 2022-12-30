library(effectsize)
library(lubridate)
library(ggplot2)


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


dreem <- read.csv("https://blog.kto.to/uploads/dreem.csv", skip = 5, sep = ';', header = TRUE)
dreem <- dreem[!is.na(dreem$Type),]

starttime = vector(); stims = vector(); deeptime = vector(); remtime = vector(); waso = vector(); sol = vector(); lighttime = vector()
for (i in 1:nrow(dreem)) {
  sleepstart <- strsplit(dreem$Start.Time[i], "T")[[1]][2]
  sleepstarttime <- strsplit(sleepstart, "\\+")[[1]][1]
  starttime[i] <- period_to_seconds(hms(sleepstarttime))
  deeptime[i] <- period_to_seconds(hms(dreem$Deep.Sleep.Duration[i]))
  remtime[i] <- period_to_seconds(hms(dreem$REM.Duration[i]))
  lighttime[i] <- period_to_seconds(hms(dreem$Light.Sleep.Duration[i]))
  sol[i] <- period_to_seconds(hms(dreem$Sleep.Onset.Duration[i]))
  waso[i]  <- period_to_seconds(hms(dreem$Wake.After.Sleep.Onset.Duration[i]))
}
dreem$StartTime <- (starttime/3600)
dreem$DEEP <- (deeptime/3600)
dreem$REM <- (remtime/3600)
dreem$LIGHT <- (lighttime/3600)
dreem$TST <- (deeptime + remtime + lighttime)/3600
dreem$WASO <- (waso/3600)
dreem$SOL <- (sol/60)

l <- lm(cbind(TST, DEEP, REM, SOL, WASO, Number.of.awakenings, Position.Changes) ~ StartTime, data=dreem)
summary(dreem)
summary(anova(l))
s <- summary(l); s
interpret_r2(s$`Response TST`$adj.r.squared[1])
interpret_r2(s$`Response DEEP`$adj.r.squared[1])
interpret_r2(s$`Response REM`$adj.r.squared[1])
interpret_r2(s$`Response WASO`$adj.r.squared[1])
interpret_r2(s$`Response SOL`$adj.r.squared[1])
interpret_r2(s$`Response Number.of.awakenings`$adj.r.squared[1])
interpret_r2(s$`Response Position.Changes`$adj.r.squared[1])

p.adjust(c(
  s$`Response TST`$coefficients[,4][2],
  s$`Response DEEP`$coefficients[,4][2],
  s$`Response REM`$coefficients[,4][2],
  s$`Response WASO`$coefficients[,4][2],
  s$`Response SOL`$coefficients[,4][2],
  s$`Response Number.of.awakenings`$coefficients[,4][2],
  s$`Response Position.Changes`$coefficients[,4][2]), method="BH") #< 0.05
confint(lm(TST ~ StartTime, data=dreem), level = 0.05)

ggplotRegression(lm(cbind(dreem$TST) ~ dreem$StartTime, data=dreem))
