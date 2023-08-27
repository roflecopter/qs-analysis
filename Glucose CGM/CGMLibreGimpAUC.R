#set dir with this file as working dir
source("../Functions/Default.R"); 
source("../Functions/f.R")

url.filename <- 'GlicemiaMisurazioni.csv'; cache.filename <- 'GlicemiaMisurazioni.Rda'; 
cache.folder <- '../Data/'; cache.file <- paste0(cache.folder, cache.filename)
if(file.exists(cache.file)) { load(cache.file) } else {
  url <- 'https://blog.kto.to/uploads/'
  # url <- "Data/"
  gimp <- read.csv(paste0(url, url.filename), sep = ';', header = F, skipNul = T)
  save(gimp, file = cache.file)
}

data.csv <- gimp
data.csv <- data.csv[,c("V3","V6","V7")]
colnames(data.csv) <- c("datetime","raw","calibrated")
data.csv$datetime <- as.POSIXct(data.csv$datetime, origin = "1970-01-01")

date.start <- as.POSIXct("2021-06-04", timezone = "UTC")
date.end <- as.POSIXct("2021-06-05", timezone = "UTC")

data.csv <-  data.csv[data.csv$datetime >= date.start & data.csv$datetime <= date.end,]

# check for NA data
library(mice)
md.pattern(data.csv, rotate.names = T)
summary(data.csv)

df <- data.csv[,c("datetime","calibrated")]

describe.data(df)

# visualize missing data
library(naniar)
vis_miss(df, sort_miss = TRUE)

library(ggplot2); library(scales)
ggplot(df, aes(x = datetime, y = calibrated)) + 
  geom_point() + scale_x_datetime(breaks = "1 hour") + 
  theme(axis.text.x = element_text(angle=90))

period.after <- 120 # in minutes
period.pre <- 30 # in minutes

df.f <- df
df.f$iAUC <- NA

n <- nrow(df.f)

for(i in 1:n) {
  point <- df.f[i,]
  ds <- point$datetime; de <- point$datetime + period.after*60; dpre <- point$datetime - period.pre*60
  # get points for next 2 hours (postprandial), excluding start one
  points.after <- df.f[df.f$datetime >= ds & df.f$datetime <= de,]
  # get points for previous 30 mins to calc a baseline
  points.pre <- df.f[df.f$datetime >= dpre & df.f$datetime <= ds,]
  # get baseline as mean from previous 30 minutes
  baseline <- mean(points.pre$calibrated)
  
  points.after$iAUC <- NULL
  date.prev <- ds
  g.prev <- baseline
  n.a <- nrow(points.after)
  iAUC <- 0
  if(n.a > 3) {
    for(j in 1:n.a) {
      point.j <- points.after[j,]
      time.diff <- difftime(point.j$datetime, date.prev, units = "mins")
      point.iAUC <- 0
      if(points.after[j,]$calibrated > baseline) {
        # calc iAUC https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2769714/
        # Si = dt*((Gi - G0) + (Gi-1 - G0))/2; iAUC = sum(S)/120
        point.iAUC <- time.diff*((point.j$calibrated-baseline)+(g.prev-baseline))/2
        iAUC <- iAUC + point.iAUC
        #print(paste0(point.j$datetime, " (",time.diff,") > ", points.after[j,]$iAUC, " bl: ", baseline, " cur.p: ", point.j$calibrated, " g.p: ", g.prev))
      }
      g.prev <- point.j$calibrated
      date.prev <- point.j$datetime
    }
    df.f[i,]$iAUC <- round(iAUC / 120,1)
  }
}

df.f <- na.omit(df.f)

library(lubridate)
df.p <- aggregate(cbind(iAUC = df.f$iAUC, glucose = df.f$calibrated), by = list(datetime = floor_date(df.f$datetime, "10 minute")), FUN = max)
q <- quantile(df.p$iAUC,.9)
df.p$label <- ""; df.p[df.p$iAUC > q, "label"] <- df.p[df.p$iAUC > q, "iAUC"]
ggplot(df.p, aes(x = datetime, y = iAUC, label=label)) + 
  geom_bar(stat = "identity") + 
  geom_point(aes(x=datetime, y=glucose/2)) + 
  geom_text(hjust=0, vjust=0) + 
  theme(axis.text.x = element_text(angle=45))

df.tsv <- data.frame("GlucoseDisplayTime" = df$datetime, "GlucoseValue" = df$calibrated)
df.tsv <- aggregate(cbind(GlucoseValue = df.tsv$GlucoseValue), by = list(GlucoseDisplayTime = floor_date(df.tsv$GlucoseDisplayTime, "5 minute")), FUN = mean)
df.tsv$GlucoseValue <- round(df.tsv$GlucoseValue)
write.table(df.tsv, file="classify.tsv", quote = F,sep = "\t", row.names = F)


