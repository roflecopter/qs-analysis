cache.df <- function(key,df, m.refresh = F, cache.dir = "../Data/df.cache/") {
  library(digest); hash <- digest(key, algo = "md5", serialize = F)
  cached.f <- paste0(cache.dir ,hash,".Rda")
  if(file.exists(cached.f) & !m.refresh) load(cached.f) else save(df, file = cached.f)
  return(df)
}

read.json.dir <- function(path)
{
  files <- dir(path, pattern = "*.json")
  library(jsonlite)
  library(purrr)
  data <- files %>% map_df(~fromJSON(file.path(path, .), flatten = TRUE))
}

describe.data <- function(df, digits = 2) {
  for(i in 1:length(colnames(df))) {
    x <- df[,colnames(df)[i]]
    if(is.numeric(x)) {
      print(paste0(colnames(df)[i], "(", length(x) , "): ",round(mean(x),digits),"±",round(sd(x),digits)," ", 
                   round(median(x),digits),"[",round(quantile(x,.25),digits),",",round(quantile(x,.75), digits),"] ", 
                   round(min(x),digits),"-",round(max(x),digits))) 
    }
  }
}


ggplotRegression <- function (fit, title = "") {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(alpha = 0.3) + 
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste(title, 
                       "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " p =",signif(summary(fit)$coef[2,4], 5))) +  geom_point(alpha = 0.05)
}

ggplotRegression.v2 <- function (df, formula, res.x = 1000, grid.x = 10, grid.y = 10, f.x.rnd = 2, f.y.rnd = 2, title = "") {
  require(ggplot2)
  
  cols <- colnames(df)
  x.n <- cols[1]
  y.n <- cols[2]
  colnames(df) <- c("x","y")
  
  fit <- lm(as.formula(formula), df)
  
  p.by <- round((max(df$x)-min(df$x))/res.x,4)
  df.px <- data.frame(x = seq(min(df$x),max(df$x), by = p.by))
  df.p <- predict(fit, df.px)
  df.p <- as.data.frame(df.p)
  colnames(df.p) <- c("y")
  df.p[,"x"] <- df.px$x
  
  residuals <- df$y - predict(fit, data.frame(x = df$x))
  residuals.p <- 100*(df$y - predict(fit, data.frame(x = df$x)))/df$y
  rmse <- sqrt(mean(residuals^2))
  rmspe <- sqrt(mean(residuals.p^2))
  rss <- sum(residuals^2)
  g <- ggplot(df, aes(x = x, y = y)) + 
    geom_point(alpha = 0.3) + 
    geom_point(alpha = 0.05) + 
    geom_line(data = df.p, aes(x = x, y = y), color = "red") +
    labs(title = paste(title, 
                       "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),"\n",
                       "Slope =",signif(fit$coef[[2]], 5),
                       " p =",signif(summary(fit)$coef[2,4], 5), "\n",
                       "RMSPE = ",signif(rmspe,4),"% | RSS ", round(rss,2))) + 
    scale_x_continuous(name = x.n, limits = c(min(df$x, na.rm = T), max(df$x, na.rm = T)), breaks = seq(round(min(df$x, na.rm = T), f.x.rnd), max(df$x, na.rm = T), by = round((max(df$x, na.rm = T)-min(df$x, na.rm = T))/grid.x))) +
    scale_y_continuous(name = y.n, limits = c(min(df$y,df.p$y, na.rm = T), max(df$y,df.p$y, na.rm = T)), breaks = seq(round(min(df$y,df.p$y, na.rm = T), f.y.rnd), max(df$y,df.p$y, na.rm = T), by = round((max(df$y,df.p$y)-min(df$y,df.p$y))/grid.y,f.y.rnd)))
  return(list(fit = fit, g = g))
}

roll.mean.period <- function(df, col.dt, cols.roll, secs.roll, min.cover = .65, type = "mean") {
  df.res <- foreach(i=1:nrow(df), .combine = 'rbind', .packages = c("lubridate")) %dopar% {
    p <- df[i,]
    t.diff <- difftime(p[1,col.dt],df[,col.dt],units = "secs")
    roll.range <- df[t.diff < secs.roll & t.diff >= 0,]
    if(nrow(na.omit(roll.range[,c(col.dt,cols.roll)])) > round(secs.roll*min.cover)) {
      for(j in 1:length(cols.roll)) {
        if(type == 'mean') df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- mean(roll.range[,cols.roll[j]], na.rm = T)
        if(type == 'median') df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- median(roll.range[,cols.roll[j]], na.rm = T)
        if(type == 'min') df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- min(roll.range[,cols.roll[j]], na.rm = T)
        if(type == 'max') df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- max(roll.range[,cols.roll[j]], na.rm = T)
      }
    } else {
      for(j in 1:length(cols.roll)) {
        df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- NA
      }
    }
    return(df[i,])
  }
  return(df.res)
}

mean.period.v2 <- function(df, col.dt, cols.roll, secs.roll, min.cover = .65, type = "mean") {
  ds <- min(df[,col.dt]); de <- max(df[,col.dt]); range <- as.numeric(difftime(de,ds,units="secs"))
  p.num <- ceiling(range/secs.roll)
  df.res <- foreach(i=1:p.num, .combine = 'rbind', .packages = c("lubridate")) %do% {
    p.ds <- ds + (i-1) * secs.roll; p.de <- p.ds + secs.roll
    roll.range <- df[df[,col.dt] >= p.ds & df[,col.dt] < p.de ,]
    df.p <- data.frame()
    df.p[1,col.dt] <- p.de
    if(nrow(na.omit(roll.range[,c(col.dt,cols.roll)])) > round(secs.roll*min.cover)) {
      for(j in 1:length(cols.roll)) {
        if(type == 'mean') df.p[1,paste0(cols.roll[j],".",secs.roll,"s")] <- mean(roll.range[,cols.roll[j]], na.rm = T)
        if(type == 'median') df.p[1,paste0(cols.roll[j],".",secs.roll,"s")] <- median(roll.range[,cols.roll[j]], na.rm = T)
      }
    } else {
      for(j in 1:length(cols.roll)) {
        df.p[1,paste0(cols.roll[j],".",secs.roll,"s")] <- NA
      }
    }
    return(df.p[1,])
  }
  return(df.res)
}

mean.period <- function(df, col.dt, cols.roll, secs.roll, min.cover = .65, type = "mean") {
  df.res <- foreach(i=1:nrow(df), .combine = 'rbind', .packages = c("lubridate")) %dopar% {
    p <- df[i,]
    if(!is.na(df[i,col.dt]) && (as.numeric(df[i,col.dt]) %% secs.roll) == (as.numeric(df[1,col.dt]) %% 60)) {
      t.diff <- difftime(p[1,col.dt],df[,col.dt],units = "secs")
      roll.range <- df[t.diff < secs.roll & t.diff >= 0,]
      if(nrow(na.omit(roll.range[,c(col.dt,cols.roll)])) > round(secs.roll*min.cover)) {
        for(j in 1:length(cols.roll)) {
          if(type == 'mean') df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- mean(roll.range[,cols.roll[j]], na.rm = T)
          if(type == 'median') df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- median(roll.range[,cols.roll[j]], na.rm = T)
        }
      } else {
        for(j in 1:length(cols.roll)) {
          df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- NA
        }
      }
      return(df[i,])
    }
  }
  return(df.res)
}

mean.period.0 <- function(df, col.dt, cols.roll, secs.roll, min.cover = .65, type = "mean") {
  df.res <- foreach(i=1:nrow(df), .combine = 'rbind', .packages = c("lubridate")) %dopar% {
    p <- df[i,]
    if(!is.na(df[i,"dt"]) && (as.numeric(df[i,"dt"]) %% secs.roll) == 0) {
      t.diff <- difftime(p[1,col.dt],df[,col.dt],units = "secs")
      roll.range <- df[t.diff < secs.roll & t.diff >= 0,]
      if(nrow(na.omit(roll.range[,c(col.dt,cols.roll)])) > round(secs.roll*min.cover)) {
        for(j in 1:length(cols.roll)) {
          if(type == 'mean') df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- mean(roll.range[,cols.roll[j]], na.rm = T)
          if(type == 'median') df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- median(roll.range[,cols.roll[j]], na.rm = T)
        }
      } else {
        for(j in 1:length(cols.roll)) {
          df[i,paste0(cols.roll[j],".",secs.roll,"s")] <- NA
        }
      }
      return(df[i,])
    }
  }
  return(df.res)
}
