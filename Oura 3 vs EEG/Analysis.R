library(jsonlite)
library(doParallel)
registerDoParallel(cores=10)

bg.color = "#27214C"
ax.color = "#201B42"
label.color = "#7C76A2"
main.color = "#FCFBFF"
n3.color = "#533DD6"

dt_timezone = 'Antarctica/Davis'
timezone_secs = 420 * 60

# dts_compare = as.POSIXct("2023-11-01 12:00:00")
# dte_compare = dts_compare + 24 * 3600
dts_compare = as.Date("2023-12-24"); rm(dte_compare)
# dte_compare = as.Date("2023-12-31")
oura_sleep_algo = "2.1"
# oura_sleep_algo = "v2"
test_g = paste0('Oura V3 (SA: ', oura_sleep_algo,")")
ref_g = 'OpenBCI'
# ref_g = 'ZMax'


rawoura<-fromJSON('/Volumes/Data/Storage/Downloads/oura_sleep_2024-01-01T02-53-16.json')

oura_nights <- flatten(data.frame(rawoura[['sleep']]))
oura_nights$datetime <- ymd_hms(oura_nights$bedtime_start)
oura_nights$ds <- as.Date(oura_nights$bedtime_start)

oura_nights = oura_nights[oura_nights$sleep_algorithm_version == oura_sleep_algo,]
if(exists("dte_compare")) {
  oura_nights = oura_nights[oura_nights$ds >= dts_compare,]
  oura_nights = oura_nights[oura_nights$ds <= dte_compare,]
} else {
  oura_nights = oura_nights[oura_nights$ds == dts_compare,]
  oura_nights = oura_nights[nrow(oura_nights),]
}

oura_results <- foreach(i=1:nrow(oura_nights), .combine='rbind', .multicombine=TRUE, .packages = "lubridate") %dopar% {
  odf = NULL
  if(!is.na(oura_nights$sleep_phase_5_min[i])) {
    for(j in 1:nchar(oura_nights$sleep_phase_5_min[i])) {
      stage <- as.numeric(substring(oura_nights$sleep_phase_5_min[i], j, j)[1])
      for(k in 1:10) { # downsample to 30-sec epochs 
        datetime <- oura_nights$datetime[i] + 300 * (j - 1) + (k - 1)*30
        res <- c(datetime = datetime, stage = stage, bedtime_start = oura_nights$bedtime_start[i])
        odf = rbind(odf, res)
      }
    } 
  }
  return(odf)
}

test_hypnos = as.data.frame(oura_results)
test_hypnos$dt = as.POSIXct(test_hypnos$datetime)
test_hypnos$bs = as.POSIXct(test_hypnos$bedtime_start)
test_hypnos$ds = as.Date(test_hypnos$bs)
test_hypno = test_hypnos
# test_hypno = test_hypnos[test_hypnos$ds == dts_compare, ]
# ggplot(test_hypno, aes(x = datetime, y = stage)) + geom_line()

# openbci
library(DBI); library(RClickhouse); library(RMariaDB)
sql.con <- NULL; sql.con <- s.q(sql.con)

if(ref_g == 'OpenBCI') {
  q <- "select * from openbci_features order by datetime asc limit 100000"; sql.con <- s.q(sql.con); rs <- dbSendQuery(sql.con, q); dbRows <- dbFetch(rs); dbClearResult(rs)
  ref_hypnos = dbRows; ref_hypnos$dt =  force_tz(ref_hypnos$datetime, dt_timezone)
  ref_hypno = ref_hypnos[(ref_hypnos$category == 'hypno_max') & (ref_hypnos$dt >= min(test_hypno$dt)) & (ref_hypnos$dt <= max(test_hypno$dt)),]
} else {
  # zmax
  ch = 'F7'
  q <- paste0("select * from zmax_raw where channel = '", ch,"' order by datetime asc limit 100000"); sql.con <- s.q(sql.con); rs <- dbSendQuery(sql.con, q); dbRows <- dbFetch(rs); dbClearResult(rs)
  ref_hypnos = dbRows; ref_hypnos$dt =  force_tz(ref_hypnos$datetime, dt_timezone)
  ref_hypno = ref_hypnos[(ref_hypnos$dt >= min(test_hypno$dt)) & (ref_hypnos$dt <= max(test_hypno$dt)),]
  ref_hypno[,"value"] = ref_hypno[,"level"]
  ref_g = paste0(ref_g, " ", ch)
}

ref_hypno[ref_hypno$value == 3,"value"] = 2
ref_hypno[ref_hypno$value == 4,"value"] = 3
ref_hypno[ref_hypno$value == 5,"value"] = 4
dt.by = "30 mins"
dt.format = "%H:%M"

ref_hypno$group = ref_g
for(ob in 1:nrow(ref_hypno)) {
  if(ob > 11) {
    stages = ref_hypno$value[(ob-12):ob]
    ref_hypno[ob,"stage"] = as.numeric(names(sort(table(stages),decreasing=TRUE))[1])
  }
}
ref_hypno$stage_plot = ref_hypno$stage

test_hypno$group = test_g
test_hypno$value = test_hypno$stage
test_up = 4
test_hypno$stage_plot = test_hypno$stage + test_up

hypno = rbind(test_hypno[,c("dt","stage","stage_plot","group")], ref_hypno[,c("dt","stage","stage_plot","group")])

g.theme = theme(plot.title = element_markdown(hjust = 0.5, size = 11, color = main.color),
                plot.subtitle = element_markdown(hjust = 0.5, size = 9, color = label.color),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                legend.position = "top", legend.key = element_rect(fill = bg.color), legend.title = element_blank(),
                legend.background = element_rect(fill = bg.color), legend.text = element_text(colour = label.color, size = 10),
                panel.background = element_rect(fill = bg.color), plot.background = element_rect(fill = bg.color, size = 0),
                axis.ticks = element_line(color = label.color), axis.title = element_text(colour = main.color),
                axis.text = element_text(colour = label.color, size = 6), axis.line = element_line(color= ax.color, size = 1))

test_color = "cyan"
ref_color = "white"
stage_size = .5
color_scale = c()
color_scale[test_g] = test_color
color_scale[ref_g] = ref_color

ggplot() + 
  ggtitle(dts_compare) + 
  geom_line(data = hypno, aes(x = dt + timezone_secs, y = stage_plot, group = group, color = group), size = .5) + 
  geom_point(data = hypno[hypno$group == ref_g & hypno$stage_plot == 3,], aes(x = dt + timezone_secs, y = stage_plot), color = "red", size = stage_size) +
  geom_point(data = hypno[hypno$group == ref_g & hypno$stage_plot == 1,], aes(x = dt + timezone_secs, y = stage_plot), color = n3.color, size = stage_size) +
  geom_point(data = hypno[hypno$group == test_g & hypno$stage_plot == 3 + test_up,], aes(x = dt + timezone_secs, y = stage_plot), color = "red", size = stage_size) +
  geom_point(data = hypno[hypno$group == test_g & hypno$stage_plot == 1 + test_up,], aes(x = dt + timezone_secs, y = stage_plot), color = n3.color, size = stage_size) +
  scale_x_datetime(name = "Time", breaks = date_breaks(dt.by), labels = date_format(dt.format), expand = c(0,0)) +
  scale_y_continuous(name = "Stage", expand = c(0,0), limits = c(.8,8.1)) +
  scale_color_manual(values = color_scale) + 
  g.theme

period <-  "30secs"
test_hypno$period <- round_date(test_hypno$dt, period)
ref_hypno$period <- round_date(ref_hypno$dt, period)

merged_hypnogram <- inner_join(as.data.frame(test_hypno[,c("period","stage")]), as.data.frame(ref_hypno[,c("period","stage")]) , by = c("period" = "period"))
colnames(merged_hypnogram) <- c("datetime",test_g,ref_g)

merged_hypnogram <- merged_hypnogram[!is.na(merged_hypnogram[,test_g]),]
merged_hypnogram <- merged_hypnogram[!is.na(merged_hypnogram[,ref_g]),]

merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 1] <- "DEEP";
merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 2] <- "LIGHT";
merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 3] <- "REM";
merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 4] <- "AWAKE";

merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 1] <- "DEEP";
merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 2] <- "LIGHT";
merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 3] <- "REM";
merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 4] <- "AWAKE";

category_order <- c("DEEP", "LIGHT", "REM", "AWAKE")
merged_hypnogram[,test_g] = factor(merged_hypnogram[,test_g], levels = category_order)
merged_hypnogram[,ref_g] = factor(merged_hypnogram[,ref_g], levels = category_order)

cm_metric <- confusion_matrix(targets = as.factor(merged_hypnogram[,test_g]), predictions = as.factor(merged_hypnogram[,ref_g]))

cm <- table(merged_hypnogram[,test_g],merged_hypnogram[,ref_g])
cm <- cm / rowSums(cm)
cm <- as.data.frame(cm, stringsAsFactors = TRUE)
cm$Var2 <- factor(cm$Var2, rev(levels(cm$Var2)))

total_hours = round((nrow(merged_hypnogram) / 2) / 60,1)
g.title = paste0(dts_compare, ifelse(exists("dte_compare"), paste0(" to ",dte_compare), ""), "<br>", 
                 "Overall Accuracy: ", round(100*cm_metric$`Overall Accuracy`,1), "%, ",
                 "F1 score:", round(cm_metric$F1,2), "<br>", total_hours, " hours analyzed")
ggplot(cm, aes(Var1, Var2, fill = round(100*Freq,1))) +
  ggtitle(g.title) + 
  geom_tile() +
  geom_text(aes(label = paste(" ", round(100*Freq,1),"%")),  size=6) +
  scale_x_discrete(expand = c(0, 0),position = 'top') +
  scale_y_discrete(expand = c(0, 0),position = 'left') +
  scale_fill_gradient(low = "white", high = "#3575b5") +
  labs(x = test_g, y = ref_g, fill = "Agreement, %") +
  theme(plot.title = element_markdown(hjust = 0.5, size = 15),
        plot.subtitle = element_markdown(hjust = 0.5, size = 12),
        legend.title = element_text(size = 12, margin = margin(0, 20, 10, 0)),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 18),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 18))
