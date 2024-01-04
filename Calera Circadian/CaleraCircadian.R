library(lubridate) 
library(hms) 
library(cosinor)
library(ggsci)
library(dplyr)
library(ggetho)
library(zeitgebr)
library(ggplot2) 
library(ggtext)
library(scales)
library(circacompare)
h24 = 3600*24

days_p = 14; 
per_l = 3600*1.5; 
de = as.POSIXct("2024-01-01 15:00:00"); ds =  de - (days_p+1)*h24
min_quality = 3 # Calera have quality from 1-4, use 3 as minimum

files = c(
  "2023-12-25T14-20-25.042Z_FE5B48B9E8BE.csv",
  "2023-12-26T13-16-58.301Z_FE5B48B9E8BE.csv",
  "2023-12-28T02-43-09.857Z_FE5B48B9E8BE.csv",
  "2023-12-31T10-29-37.720Z_FE5B48B9E8BE.csv")
files_dir = "/path/to/files"

fdf = NULL
for(f in 1:length(files)) {
  file.name = paste0(files_dir, "/", files[f])
  file.info <- file(file.name,"r")
  info = readLines(file.info, n=16)
  close(file.info)
  tdf <- read.csv(file.name, header = F, skip = 16)
  cols = info[16]
  cols = as.character(str_split(info[16], ",", simplify = T))
  colnames(tdf) = cols
  time_info = as.character(str_split(cols[1], "=", simplify = T))
  time_info_z = as.numeric(gsub("\\]", "", gsub("\\+","",time_info[2])))
  timezone.secs = 3600 * time_info_z/100 + 60 * time_info_z %% 100
  tdf$dt = as.POSIXct(tdf[,1])
  tdf$cbt = tdf$`cbt [mC]`/1000
  tdf$skin = tdf$`temp_a0 [mC]`/1000
  tdf$quality = sapply(paste0("0x",as.character(as.hexmode(tdf$status))), function(x) as.numeric(substr(x, nchar(x),nchar(x))))
  fdf = rbind(fdf, tdf)
}

period_secs = 10 # average data to 10 second means
features = aggregate(cbind(cbt = as.numeric(fdf$cbt), skin = as.numeric(fdf$skin), quality = fdf$quality), 
                   by = list(dt=round_date(fdf$dt,unit=paste0(period_secs, " seconds"))), mean)
features = features[features$quality >= min_quality,]

f_col = "cbt"
th = quantile(features[,f_col], .995) # exclude extreme values
features = features[features[,f_col] < th,]
th = quantile(features[,f_col], .005) # exclude extreme values
features = features[features[,f_col] > th,]
circadian_df = features[,c("dt",f_col)]
circadian_df[,"value"] = circadian_df[,f_col]

# filter by date
circadian_df = circadian_df[(circadian_df$dt >= ds) & (circadian_df$dt <= de),]

# periodograms
by_days = NA
if(!is.na(by_days)) circadian_df$id = paste0(gsub("value\\|","", paste0(f_col,"|")),cut(circadian_df$dt, breaks = paste0(by_days, " days")),":",by_days,"d")
if(is.na(by_days)) circadian_df$id = paste0(f_col,"|",1)
circadian_df$t = as.numeric(circadian_df$dt) + timezone.secs
circadian_df$activity = circadian_df$value
metadata = data.table(id = unique(circadian_df$id), period_group = as.factor("N")) %>% setkey(id)
circadian_df = data.table(circadian_df) %>% setkey(id)
cbe = behavr(circadian_df, metadata =, data.table(metadata))

periodogram_type = cwt_periodogram
per_range = c(hours(1), hours(36))

per_df <- periodogram(activity, cbe, period_range = per_range, FUN = periodogram_type)
per_df <- find_peaks(per_df)
per_max = per_df[which.max(per_df$power - per_df$signif_threshold),]
per_len_h = floor(per_max$period / 3600)
per_len_m = round((per_max$period - 3600*per_len_h) / 60)

ggperio(per_df) + 
  geom_line(aes(group = id, colour=id)) + 
  geom_peak(col = "black") + 
  geom_area(aes(y = signif_threshold), size = .1, fill = "grey") + 
  geom_hline(yintercept = .5, color = "black", size = .1) + 
  facet_wrap(~ id, ncol = 4)
Sys.sleep(1)

ggperio(per_df, aes(
  y = power - signif_threshold,
  colour=period_group)) + 
  stat_pop_etho() + 
  ggtitle(paste0("Period Duration ",per_len_h,"h",per_len_m, "m"))
Sys.sleep(1)

spect_dt <- spectrogram(activity, cbe)
ggspectro(spect_dt) +
  stat_tile_etho() +
  scale_y_log10() +
  facet_wrap(~ id)
Sys.sleep(1)

circadian_df$sm = as.numeric(seconds(as_hms(format(circadian_df$dt, "%H:%M:%S"))))
circadian_df$d = as.Date(circadian_df$dt)
fit = cosinor.lm(value ~ time(sm), data = circadian_df, period = per_max$period)
ggplot_cosinor.lm(fit)
circadian_df$phase = fit$fit$fitted.values

circadian_df$dt_sm = round_date(circadian_df$dt[1], unit = "day") + circadian_df$sm
ggplot(circadian_df, aes(x=dt_sm + timezone.secs,y=phase, group=d, color = d)) + 
  geom_line()  + 
  scale_x_datetime(name = "", breaks = date_breaks("60 mins"), labels = date_format("%H\n%M"), expand = c(0,0))
Sys.sleep(1)

cmax = circadian_df[which.max(circadian_df$phase),]
cmin = circadian_df[which.min(circadian_df$phase),]

per_min_h = floor(cmin$sm / 3600)
per_min_m = round((cmin$sm - 3600*per_min_h) / 60)
per_max_h = floor(cmax$sm / 3600)
per_max_m = round((cmax$sm - 3600*per_max_h) / 60)

circadian_df$dt_smr = round_date(circadian_df$dt_sm, "5 mins")
df_summary <- data.frame(dt_sm = tapply(circadian_df$dt_smr, circadian_df$dt_smr, mean), n=tapply(circadian_df$activity, circadian_df$dt_smr, length), mean=tapply(circadian_df$activity, circadian_df$dt_smr, median))
df_summary$sd <- tapply(circadian_df$activity, circadian_df$dt_smr, sd)
df_summary$CI_lower <- df_summary$mean + df_summary$sd
df_summary$CI_upper <- df_summary$mean - df_summary$sd
df_summary$dt_sm = as.POSIXct(df_summary$dt_sm)

#white theme
bg.color = "white"
ax.color = "#201B42"
label.color = "black"
main.color = "black"

library(ggtext)
g.theme = theme(plot.title = element_markdown(hjust = 0.5, size = 11, color = main.color),
                plot.subtitle = element_markdown(hjust = 0.5, size = 9, color = label.color),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
                panel.background = element_rect(fill = bg.color), plot.background = element_rect(fill = bg.color, size = 0),
                axis.ticks = element_line(color = label.color), axis.title = element_text(colour = main.color),
                axis.text = element_text(colour = label.color, size = 6), axis.line = element_line(color= ax.color, size = .5))



ggplot(df_summary, aes(x=dt_sm + timezone.secs,y=mean)) + 
  ggtitle(paste0("Batyphase: ", per_min_h,"h",ifelse(per_min_m < 10, paste0("0",per_min_m), per_min_m), "m",
                 ", Acrophase: ", per_max_h,"h",ifelse(per_max_m < 10, paste0("0",per_max_m), per_max_m), "m", 
                 ", Period: ", per_len_h,"h",ifelse(per_len_m < 10, paste0("0",per_len_m), per_len_m), "m",
                 "<br>Amplitude: ", round(fit$coefficients['amp'],1),"°C",
                 ", Mesor: ", round(mean(circadian_df$phase),1),"°C"),
          subtitle = paste0(circadian_df$dt[1]," - ", circadian_df$dt[length(circadian_df$dt)])) + 
  geom_point(data = circadian_df, aes(x = dt_sm + timezone.secs, y = activity), size = .05, color = "grey") + 
  geom_line(size = .5, color = "blue") +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), fill="blue", alpha=0.3) +
  geom_line(data = circadian_df, aes(x=dt_sm + timezone.secs,y=phase), color = "red", size = 1) + 
  geom_vline(xintercept = cmin$dt_sm + timezone.secs, color = "darkorange", size = 1) + 
  geom_vline(xintercept = cmax$dt_sm + timezone.secs, color = "darkorange", size = 1) + 
  geom_hline(yintercept = mean(circadian_df$phase), size = .2, color = "grey30", linetype = "dashed") +
  geom_hline(yintercept = mean(circadian_df$phase) + fit$coefficients['amp'], size = .2, color = "grey30", linetype = "dashed") +
  geom_hline(yintercept = mean(circadian_df$phase) - fit$coefficients['amp'], size = .2, color = "grey30", linetype = "dashed") +
  scale_y_continuous(name = "Core Body Temp, °C") + 
  scale_x_datetime(name = "", breaks = date_breaks("60 mins"), labels = date_format("%H\n%M"), expand = c(0,0)) + 
  g.theme
Sys.sleep(3)


per = per_max$period + per_l; cdfs = NULL; cdres = NULL
min_coverage = h24 * .8 / 10
for(i in days_p:1) {
  back = (i-1)*h24
  dt_max = max(circadian_df$dt, na.rm = T)
  circadian_df$dtz = circadian_df$dt + timezone.secs
  circadian_df$tsz = as.numeric(circadian_df$dt) - as.numeric(as.POSIXct(format(circadian_df$dt[1], format = "%Y-%m-%d 00:00:00")))
  cdf_s = dt_max - per - back
  cdf_e = dt_max - back
  cdf = circadian_df[(circadian_df$dt >= cdf_s) & (circadian_df$dt <= cdf_e),]
  if(nrow(cdf) > min_coverage) {
    cdf$g = i
    cdf$metric = runmed(cdf$value, 7)
    cdf$metric_n = -1*runmed(cdf$value, 7)
    cdf$tsz = as.numeric(cdf$dt) - as.numeric(as.POSIXct(format(cdf$dt[1], format = "%Y-%m-%d 00:00:00")))
    cdf$sm = as.numeric(seconds(as_hms(format(cdf$dt, "%H:%M:%S"))))
    cs = circa_single(x=cdf, col_time = "tsz", col_outcome = "metric", period = per_max$period)
    cs_n = circa_single(x=cdf, col_time = "tsz", col_outcome = "metric_n", period = per_max$period)
    cdf$fitted = cs$fit$m$fitted()
    cdf$fitted_n = cs_n$fit$m$fitted()
    cdfs = rbind(cdfs, cdf)
    mesor = cs$summary$value[which(cs$summary$parameter == 'mesor')]
    amp = cs$summary$value[which(cs$summary$parameter == 'amplitude')]
    phase = cs$summary$value[which(cs$summary$parameter == 'phase_radians')]
    peak_h = cs$summary$value[which(cs$summary$parameter == 'peak_time_hours')]
    min_h = cs_n$summary$value[which(cs$summary$parameter == 'peak_time_hours')]
    pval = round(cs$summary$value[which(cs$summary$parameter == 'rhythmic_p')],5)
    angle = phase * 180 / pi
    cdres = rbind(cdres, data.frame(g = i, mesor = mesor, amp = amp, phase = phase, peak_h = peak_h, min_h = min_h, pval = pval))
    print(paste0('Batyphase ', as_hms(round(min_h)) ,' Acrophase ', as_hms(round(peak_h)) ,' (m:', round(mesor), ', a:', round(amp), ", p-val:", pval,")"))
    if(T) {
      c_days = unique(round_date(cdf$dt, unit = "days"))
      c_peaks = c_days + peak_h
      g = ggplot(cdf, aes(x=dt+timezone.secs,y=fitted)) + geom_line() +
        ggtitle(paste0(max(c_days),' Batyphase ', as_hms(round(min_h)) ,' Acrophase ', as_hms(round(peak_h)) ,' (m:', round(mesor), ', a:', round(amp), ", p-val:", pval,")")) +
        geom_point(data =cdf, aes(x=dt+timezone.secs, y=metric), size = .2) +
        geom_vline(xintercept = c_peaks + timezone.secs) +
        geom_vline(xintercept = c_days + timezone.secs, linetype="dashed") +
        geom_hline(yintercept = mesor) +
        geom_hline(linetype="dashed", yintercept = mesor - amp) +
        geom_hline(linetype="dashed", yintercept = mesor + amp) +
        scale_x_datetime(name = "Hour", breaks = date_breaks("30 min"), labels = date_format("%H\n%M"), expand = c(0,0))
      print(g)
      Sys.sleep(.5)
    }
  }
}

if(F) {
  # devtools::install_github("matiasandina/ggethos")
  library(ggethos)
  ggplot(cdfs, aes(y=g, behaviour=fitted, color=metric)) + geom_ethogram()
  ggplot(cdfs, aes(y=g, behaviour=fitted, color=fitted)) + geom_ethogram()
}

circadian_df$id = paste0(f_col,"|",1)
circadian_df$t = as.numeric(circadian_df$dt) + timezone.secs
circadian_df$activity = circadian_df$value
circadian_df$activity = circadian_df$activity - quantile(circadian_df$activity, .05, na.rm = T)
circadian_df = data.table(circadian_df) %>% setkey(id)
be = behavr(circadian_df, metadata = data.table(metadata))

ggetho(be, aes(z = activity), multiplot = 2)  + stat_bar_tile_etho() + facet_wrap( ~ id, ncol = 8)
Sys.sleep(1)
ggetho(be, aes(z = activity), multiplot = 2)  + stat_tile_etho() + facet_wrap( ~ id, ncol = 8)
Sys.sleep(1)

cdfs$id = paste0(f_col,"|",1)
cdfs$tsz = as.numeric(cdfs$dt) - as.numeric(as.POSIXct(format(cdfs$dt[1], format = "%Y-%m-%d 00:00:00")))
cdfs$t = as.numeric(cdfs$dt) + timezone.secs
cdfs$activity = cdfs$metric
cdfs$activity = cdfs$activity - quantile(cdfs$activity, .05, na.rm = T)
cdfs$activity = cdfs$fitted - abs(min(cdfs$fitted))
cdfs = data.table(cdfs) %>% setkey(id)
metadata = data.table(metadata) %>% setkey(id)
be = behavr(cdfs, metadata = data.table(metadata))
ggetho(be, aes(z = activity), multiplot = 2)  + stat_bar_tile_etho() + facet_wrap( ~ id, ncol = 8)
Sys.sleep(1)

cdres = cdres[cdres$min_h < .5*h24,]
ggplot(cdres, aes(x=g, y=as_hms(min_h))) + geom_col() + ggtitle(paste0("±", as_hms(round(sd(cdres$min_h)))))
