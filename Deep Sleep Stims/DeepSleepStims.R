library(effectsize)
library(lubridate)
library(ggplot2)
alpha <- .05; crit <- 1.96
ggplotRegression <- function (fit) { require(ggplot2); ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + geom_point() + stat_smooth(method = "lm", col = "red") + labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),"Intercept =",signif(fit$coef[[1]],5 )," Slope =",signif(fit$coef[[2]], 5)," P =",signif(summary(fit)$coef[2,4], 5))) }

dreem <- read.csv("https://blog.kto.to/uploads/dreem-2022-12-30.csv", skip = 5, sep = ';', header = TRUE)
dreem <- dreem[!is.na(dreem$Type),]

dreem <- dreem[dreem$Start.Time != "2021-08-18T23:01:04+07:00",] #battery went out in the middle of night
dreem <- dreem[dreem$Start.Time != "2021-10-01T00:15:49+07:00",] #battery went out in the middle of night
dreem <- dreem[dreem$Start.Time != "2021-10-14T23:14:08+07:00",] #battery went out in the middle of night
dreem <- dreem[dreem$Start.Time != "2022-01-13T23:27:38+07:00",] #battery went out in the middle of night
dreem <- dreem[dreem$Start.Time != "2021-12-17T23:00:51+07:00",] #battery went out in the middle of night
dreem <- dreem[dreem$Start.Time != "2022-01-22T23:09:39+07:00",] #double sleep

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
confint(l , level = alpha)

ggplotRegression(l)

if(length(dreem[,"Number.of.Stimulations"]) < 5000) st <-shapiro.test(dreem[,"Number.of.Stimulations"]) else st <- ad.test(dreem[,"Number.of.Stimulations"])
if(st$p.value < alpha) "Non-normal" else "Normal"

library("boot")
bca_reps <- 1000
Bmean <- function(data, i){d <- data[i,]; return(mean(d[d$group == 1,]$value)-mean(d[d$group == 2,]$value))}

dreem[dreem$Number.of.Stimulations == 0,"group"] <- 1
dreem[dreem$Number.of.Stimulations != 0,"group"] <- 2
dreem$value <- dreem$DEEP
df <- dreem

if(st$p.value < alpha) { #data is not normally distributed, using bootstrap
  set.seed(1)
  results <- boot(data=df, statistic=Bmean, R = bca_reps)
  result <- boot.ci(results, type = c("bca"), conf = 1 - alpha)
  ci <- result$bca[c(4,5)]
} else { #data is normally distributed, using T-test
  result <- t.test(df[df$group == 1,"value"], df[df$group == 2,"value"])
  ci <- result$conf.int[c(1,2)]
}

m <- mean(df[df$group == 1,"value"])/mean(df[df$group == 2,"value"])
ci <- 1 + ci / mean(df[df$group == 1,"value"])
n <- 1
type <- "norm"

sd <- (ci[2]-ci[1])*sqrt(n)/(2*crit)
desc <- paste0(round(m,3),"±",round(abs(sd),3)) 

pdf1 <- function(x) dnorm(x, mean = m, sd = sd) # pdf
qdf1 <- function(x) qnorm(x, mean = m, sd = sd) # qf
cdf1 <- function(x) pnorm(x, mean = m, sd = sd) # cdf

surv.function <- function(x) return (1-cdf1(x))
x.times <- cdf1(1) / surv.function(1)

if(x.times < 1) odds.t <- paste0("Odds 1:", round(1/x.times,1)) else odds.t <- paste0("Odds ", round(x.times,1),":1 to have effect less than 1")

g.pad <- .1
print(
  ggplot(data.frame(x = c(ci[1]-g.pad, ci[2]+g.pad)), aes(x = x)) + xlim(ci[1]-g.pad, ci[2]+g.pad) + ylim(0, pdf1(m)) +
    stat_function(fun = pdf1, geom = "area", fill = "blue", alpha = 0.25) + 
    stat_function(fun = pdf1) + 
    geom_text(data = data.frame(x = c(1+g.pad/2),y = c(pdf1(1)/2), l = c(paste0(round(100*(1-cdf1(1)),2),"%"))), aes(x = x, y = y, label = l), size = 6) +
    geom_text(data = data.frame(x = c(m),y = c(pdf1(m)/2), l = c(paste0(round(100*cdf1(1),2),"%"))), aes(x = x, y = y, label = l), size = 6) +
    geom_vline(xintercept = 1, color = "orange", size = 1) + 
    labs(x = "\n x", y = "f(x) \n", title = paste0("Prob Density Function\n", desc, ", ",type,"\n",odds.t)) + 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face="bold", colour="blue", size = 12),
          axis.title.y = element_text(face="bold", colour="blue", size = 12))
)
