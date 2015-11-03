#----------------------------------------------------------------
# Using Wikipedia Page View Statistics
# to Measure Public Issue Attention
# Simon Munzert
# 2015
#----------------------------------------------------------------

source("packages.r")
source("functions.r")


# Quelle, Paket laden -------------------------------------------
browseURL("http://stats.grok.se")
library(wikipediatrend)


# Beispiel 1: Pegida --------------------------------------------
pegida_ts <- wp_trend("Pegida", from = "2014-10-01", to = "2015-11-02", lang = "de")
head(pegida_ts)
tail(pegida_ts)
View(pegida_ts)
ggplot(pegida_ts, aes(x=date, y=count)) + geom_line(colour="steelblue") 



# Beispiel 2: Flüchtlingskrise ----------------------------------


# Artikelnamen sammeln
artikel_fluechtlinge <- c("Flüchtling", "Einwanderung", "Ausländer", "Lampedusa", "Bootsunglück_vor_Lampedusa_2013", "Mare_Nostrum_(Marineoperation)", "Illegale_Einwanderung", "Einwanderung_über_das_Mittelmeer_in_die_EU") # beispielhaft weiteren Artikel hinzufügen, um Live-Download zu demonstrieren; Bsp: "Flüchtlingskrise_in_Europa_2015"

# Aufrufstatistiken herunterladen
wiki_ger <- list()
wiki_filenames_raw <- vector()
for (i in artikel_fluechtlinge) {
  folder <- "./data/wikipediatrend/"
  filename <- paste0("wp_", i, "_de.csv")
  if (!file.exists(paste0(folder, filename))) {
    wiki_ger[[i]] <- wp_trend(i, from = "2008-01-01", to = "2015-11-02", lang = "de")
    write.csv(wiki_ger[[i]], file = paste0(folder, filename), row.names = FALSE)    
  }
  wiki_filenames_raw[i] <- filename
}

# Daten importieren
wiki_files <- paste0("./data/wikipediatrend/", wiki_filenames_raw)
wiki_filenames <- basename(wiki_files) %>% str_replace("\\.csv", "") %>% str_replace("wp_", "") %>% str_replace("_de", "")
wiki_ger <- list()
for (i in seq_along(wiki_files)) {
  foo <- read.csv(wiki_files[i])
  dat <- select(foo, date, count)
  names(dat) <- c("date", wiki_filenames[i])
  dat$date <- wp_date(dat$date)
  wiki_ger[[i]] <- dat 
}

# Data Frame generieren
wiki_ger_df <- join_all(wiki_ger, by = 'date', type = 'full')
wiki_ger_df <- arrange(wiki_ger_df, date)
wiki_ger_df <- filter(wiki_ger_df, date >= "2008-02-01")
head(wiki_ger_df)

# Zeitreihen aufräumen
wiki_ger_df[wiki_ger_df==0] <- NA
wiki_ger_df <- na.locf(wiki_ger_df, na.rm = FALSE)
wiki_ger_df[is.na(wiki_ger_df)] <- 0 # set leading NAs (`page did not exist yet') zero
cols_num <- names(wiki_ger_df)[!(names(wiki_ger_df) %in% "date")]
wiki_ger_df[cols_num] <- sapply(wiki_ger_df[cols_num],as.numeric)
wiki_ger_df$date <- ymd(wiki_ger_df$date)

# kumulierte Zeitreihe erzeugen
article_names_ger <- names(wiki_ger_df)[-1]
wiki_ger_df_agg <- data.frame(wiki_fluechtlinge = rowSums(wiki_ger_df[,article_names_ger]))

# Zeitreihen-Objekte erzeugen
wiki_ger_xts_daily <- xts(wiki_ger_df[,-1], wiki_ger_df$date)
wiki_ger_weekly <- apply(wiki_ger_xts_daily, 2, apply.weekly, sum)
weeks_dates <- apply.weekly(wiki_ger_df$date, sum) %>% index()
wiki_ger_xts_weekly <- xts(wiki_ger_weekly, weeks_dates)

wiki_ger_xts_daily_agg <- xts(wiki_ger_df_agg , wiki_ger_df$date)
wiki_ger_weekly_agg  <- apply(wiki_ger_xts_daily_agg , 2, apply.weekly, sum)
weeks_dates <- apply.weekly(wiki_ger_df$date, sum) %>% index()
wiki_ger_xts_weekly_agg  <- xts(wiki_ger_weekly_agg , weeks_dates)

# Problem: Saisonalität
plot(wiki_ger_xts_daily_agg, xlim = ymd(c("2015-01-01", "2015-11-02")))
plot(wiki_ger_xts_daily_agg, xlim = ymd(c("2015-10-01", "2015-11-02")), ylim = c(0, 2000))

# Seasonal Decomposition of Time Series by LOESS, aggregated time series
articles_names_agg <- names(wiki_ger_xts_weekly_agg)

ts_obj <- ts(as.numeric(wiki_ger_xts_weekly_agg$wiki_fluechtlinge), start=c(2008,5), frequency = 52) # Beginn im Februar 2008, also in der 5. Woche
ts_obj_stl <- stats::stl(ts_obj, s.window = "periodic")
plot(ts_obj_stl)

# Bereinigung der Zeitreihe um Saisonalität
wiki_ts_adj_agg <- ts_obj_stl$time.series[ ,2] + ts_obj_stl$time.series[ ,3]
plot(wiki_ger_xts_weekly_agg)
plot(wiki_ts_adj_agg)

# Vergleich mit Google Trends/Google News!

# Darstellung individueller Zeitreihen
par(mfrow = c(3, 3))
for (i in seq_along(names(wiki_ger_xts_daily))) {
  plot(wiki_ger_xts_daily[,i], xlim = ymd(c("2015-01-01", "2015-11-02")), main = names(wiki_ger_xts_daily)[i])
}

# Deskriptive Statistiken erstellen

# Tabelle basteln
tab_views_count <- data.frame(article_name_ger = names(apply(wiki_ger_df[,2:9], 2, sum)), 
                              count = apply(wiki_ger_df[,2:9], 2, sum), 
                              avg = round(apply(wiki_ger_df[,2:9], 2, mean), 1), 
                              max = apply(wiki_ger_df[,2:9], 2, max))
tab_views_count <- arrange(tab_views_count, desc(avg))
names(tab_views_count) <- c("Artikel", "Zugriffe, total", "Zugriffe, Durchschnitt", "Zugriffe, Maximum")

# Tabelle nach LaTeX exportieren
print(xtable(tab_views_count, align = c("l","l","r", "r", "r"), digits=1, caption = "Zugriffsstatistiken.\\label{tab:wikiarticles1}"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "h!", include.rownames=FALSE, file = "wiki-sumstats.tex")



# Vergleich mit Flüchtlingsdaten

# Daten verfügbar auf http://www.themigrantsfiles.com/

# import migrants' files data
refugee_df <- readWorksheet(loadWorkbook("./data/refugee-deaths.xlsx"),sheet=1, colTypes=rep("character", 20))
refugee_df$date <- str_sub(refugee_df$date, 1,10) %>% ymd()
refugee_df$dead_and_missing <- num(refugee_df$dead_and_missing)
refugee_df <- arrange(refugee_df, desc(date))
View(refugee_df)

refugee_xts <- xts(refugee_df$dead_and_missing, refugee_df$date)
refugee_xts <- refugee_xts[!is.na(time(refugee_xts)),]
names(refugee_xts) <- c("count")
refugee_xts <- apply.daily(refugee_xts, sum)
refugee_xts <- refugee_xts["2008-02-01/2015-11-01"]
plot(refugee_xts)

wiki_ger_xts_daily_agg <- 1000 - normalize1000(wiki_ger_xts_daily_agg$wiki_fluechtlinge)

xts_merge <- merge(refugee_xts, wiki_ger_xts_daily_agg)
xts_merge[is.na(xts_merge)] <- 0


# Flüchtlingstote
plot(xts_merge$count, ylim = c(0,1000), xlim = ymd(c("2008-02-01", "2015-11-01")), main = c("","Flüchtlingskatastrophen, Aufmerksamkeit"))
# Wikipedia-Artikel zur Flüchtlingsthematik
lines(xts_merge$wiki_fluechtlinge, col = "red")







