bg.color = "#27214C"
ax.color = "#201B42"
label.color = "#7C76A2"
main.color = "#FCFBFF"
n3.color = "#533DD6"

dt_timezone = 'Antarctica/Davis'
timezone_secs = 420 * 60

library(DBI); library(RClickhouse); library(RMariaDB)
if(F) {
  sql.con <- NULL; sql.con <- s.q(sql.con)
  q <- "select * from openbci_features order by datetime asc limit 1000000"; sql.con <- s.q(sql.con); rs <- dbSendQuery(sql.con, q); dbRows <- dbFetch(rs); dbClearResult(rs)
}

dts_compare = as.Date("2024-01-15"); rm(dte_compare)
dte_compare = as.Date("2024-02-16")
test_g = paste0('OpenBCI EOG')
ref_g = 'OpenBCI'
channel = 'F7-T4'

if(test_g == 'OpenBCI EOG') {
  test_hypnos = dbRows; test_hypnos$dt =  force_tz(test_hypnos$datetime, dt_timezone)
  test_hypno = test_hypnos[(test_hypnos$category == paste0("hypno_",channel,"-EOG-RL")),]
  test_hypno = test_hypno[(test_hypno$dt >= as.POSIXct(dts_compare)+12*3600) & (test_hypno$dt <= as.POSIXct(dte_compare)+12*3600),]
  test_g = paste0(test_g, " ", channel)
}

if(ref_g == 'OpenBCI') {
  ref_hypnos = dbRows; ref_hypnos$dt =  force_tz(ref_hypnos$datetime, dt_timezone)
  ref_hypno = ref_hypnos[(ref_hypnos$category == paste0("hypno_",channel)),]
  ref_hypno = ref_hypno[(ref_hypno$dt >= min(test_hypno$dt)) & (ref_hypno$dt <= max(test_hypno$dt)),]
  ref_g = paste0(ref_g, " ", channel)
}

dt.by = "30 mins"
dt.format = "%H:%M"

ref_hypno$group = ref_g
ref_hypno$stage = ref_hypno$value
ref_hypno$stage_plot = ref_hypno$stage

test_hypno$group = test_g
test_hypno$stage = test_hypno$value
test_up = 5
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
  geom_point(data = hypno[hypno$group == ref_g & hypno$stage_plot == 4,], aes(x = dt + timezone_secs, y = stage_plot), color = "red", size = stage_size) +
  geom_point(data = hypno[hypno$group == ref_g & hypno$stage_plot == 1,], aes(x = dt + timezone_secs, y = stage_plot), color = n3.color, size = stage_size) +
  geom_point(data = hypno[hypno$group == test_g & hypno$stage_plot == 4 + test_up,], aes(x = dt + timezone_secs, y = stage_plot), color = "red", size = stage_size) +
  geom_point(data = hypno[hypno$group == test_g & hypno$stage_plot == 1 + test_up,], aes(x = dt + timezone_secs, y = stage_plot), color = n3.color, size = stage_size) +
  scale_x_datetime(name = "Time", breaks = date_breaks(dt.by), labels = date_format(dt.format), expand = c(0,0)) +
  scale_y_continuous(name = "Stage", expand = c(0,0), limits = c(.8,10.1)) +
  scale_color_manual(values = color_scale) + 
  g.theme

period <-  "30secs"
test_hypno$period <- round_date(test_hypno$dt, period)
ref_hypno$period <- round_date(ref_hypno$dt, period)

merged_hypnogram <- inner_join(as.data.frame(test_hypno[,c("period","stage")]), as.data.frame(ref_hypno[,c("period","stage")]) , by = c("period" = "period"))
colnames(merged_hypnogram) <- c("datetime",test_g,ref_g)

merged_hypnogram <- merged_hypnogram[!is.na(merged_hypnogram[,test_g]),]
merged_hypnogram <- merged_hypnogram[!is.na(merged_hypnogram[,ref_g]),]

merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 1] <- "N3";
merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 2] <- "N2";
merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 3] <- "N1";
merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 4] <- "REM";
merged_hypnogram[,ref_g][merged_hypnogram[,ref_g] == 5] <- "AWAKE";

merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 1] <- "N3";
merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 2] <- "N2";
merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 3] <- "N1";
merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 4] <- "REM";
merged_hypnogram[,test_g][merged_hypnogram[,test_g] == 5] <- "AWAKE";

category_order <- c("N3", "N2", "N1", "REM", "AWAKE")
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
