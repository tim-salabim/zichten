library(googlesheets4)
library(ggplot2)
library(data.table)
library(lubridate)

options(gargle_oauth_email = TRUE)

sht = read_sheet("https://docs.google.com/spreadsheets/d/1UNUKMf1Zw5eryhTJLgDyp4nKmKwi_Udg_dik56dpWIA/edit#gid=0")
sht = as.data.table(sht)
sht[, days := seq_along(n)]
sht[, week := lubridate::week(date)]
sht[, week_ave := round(mean(n, na.rm = TRUE), 2), by = week]


lm1 = lm(n ~ days, data = sht)
eta0 = round((0 - lm1$coefficients[1]) / lm1$coefficients[2], 0) - sum(!is.na(sht$n))
eta5 = round((5 - lm1$coefficients[1]) / lm1$coefficients[2], 0) - sum(!is.na(sht$n))
zero_date0 = Sys.Date() + eta0
zero_date5 = Sys.Date() + eta5

clr = ifelse(sht$n_max - sht$n >= 0, "#007600", "#760000")

p = ggplot(
  data = sht
  , mapping = aes(
    x = date
    , y = n
  )
) + 
  geom_area(
    mapping = aes(
      x = date
      , y = n_max
    )
    , fill = "#6c936c50"
  ) +
  geom_bar(
    mapping = aes(
      x = date
      , y = c(0, diff(tobacco))
    )
    , stat = "identity"
    , fill = "#5e2f0d"
  ) +
  geom_line(
    aes(x = date, y = week_ave)
    , colour = "cornflowerblue"
    , lwd = 1.5
    , alpha = 0.6
  ) +
  geom_line(colour = "grey50") +
  geom_point(colour = clr) +
  geom_smooth(method = lm, colour = "grey40") +
  geom_text(
    label = sprintf(
      "ETA (0 Zichten): %s days from today, i.e. %s"
      , eta0
      , zero_date0
    )
    , x = sht$date[11]
    , y = 20
  ) +
  geom_text(
    label = sprintf(
      "ETA (5 Zichten): %s days from today, i.e. %s"
      , eta5
      , zero_date5
    )
    , x = sht$date[11]
    , y = 19
  ) +
  ylim(c(0, 20)) +
  ylab("# cigarettes smoked") +
  theme_minimal()

p

mean(sht$n, na.rm = TRUE)

plotly::ggplotly(p)
