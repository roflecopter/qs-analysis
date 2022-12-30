library(effectsize)
library(lubridate)
library(ggplot2)
ggplotRegression <- function (fit) { require(ggplot2); ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + geom_point() + stat_smooth(method = "lm", col = "red") + labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),"Intercept =",signif(fit$coef[[1]],5 )," Slope =",signif(fit$coef[[2]], 5)," P =",signif(summary(fit)$coef[2,4], 5))) }

dreem <- read.csv("https://blog.kto.to/uploads/dreem.csv", skip = 5, sep = ';', header = TRUE)
dreem <- dreem[!is.na(dreem$Type),]

starttime = vector(); stims = vector(); deeptime = vector(); remtime = vector(); waso = vector(); sol = vector(); lighttime = vector()
for (i in 1:nrow(dreem)) {
  deeptime[i] <- period_to_seconds(hms(dreem$Deep.Sleep.Duration[i]))
}

dreem$DEEP <- (deeptime/60)

l <- lm(DEEP ~ Number.of.Stimulations, data=dreem)
summary(dreem)
length(dreem$Number.of.Stimulations[dreem$Number.of.Stimulations>0])
length(dreem$Number.of.Stimulations[dreem$Number.of.Stimulations==0])
summary(anova(l))
s <- summary(l); s
interpret_r2(s$adj.r.squared[1])
confint(l , level = 0.05)

ggplotRegression(l)