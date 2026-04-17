### Zgony rocznie
setwd("~/Projekty/BDO/EuroStat/cause_of_death")
library('eurostat')
library('tidyverse')

## Dane roczne
z0 <- get_eurostat("demo_magec")

z1 <- z0 |>
  filter (geo == 'PL' ) |>
  filter (sex == 'T' & age == 'TOTAL') |>
  mutate (year = substr(TIME_PERIOD, 1, 4),
          date = as.Date(sprintf("%s-07-01", year) )) |>
  select (date, year, zgony=values)

p1 <- z1 |>
  filter (year > 1999) |>
  ggplot(mapping=aes(x=date, y=zgony )) +
  geom_line(color='brown4', alpha=.3) +
  geom_point(color='red') +
  ylab('') +
  xlab('lata') +
  ggtitle('Liczba zgonów w Polsce w latach 2000-2025',
          subtitle = 'Źródło: Eurostat/demo_magec')
p1

z2 <- z1 |> mutate(t = row_number())
t1 <- lm(zgony ~ t, data=z2)
summary(t1)

## Dodajeym dummy
## https://www.studocu.com/pl/document/politechnika-gdanska/ekonometria/ekonometria-wyklad-12-13-modele-sezonowosci-i-zmienne-zerojedynkowe/126703806
z3 <- z2 |> mutate(dummy = if_else(year >= 2020 & year <= 2022, 1, 0))
t2 <- lm(zgony ~ t + dummy, data=z3)
summary(t2)

##
## średnie tempo wzrostu (średnia geometryczna)
z3 %>%
  arrange(year) %>%
  summarise(
    cagr = (last(zgony) / first(zgony))^(1/(n() - 1)) - 1
  ) %>%
  mutate(cagr = cagr * 100)
## Przeciętny roczny wzrost
## 0,942%
##
## Dane miesięczne
z0 <- get_eurostat("demo_mmonth")

z1 <- z0 |>
  filter (geo == 'PL' ) |>
  filter (month != 'TOTAL' & month != 'UNK') |>
  select (tp=TIME_PERIOD, zgony=values, month) |>
  mutate (year = substr(tp, 1, 4), year = as.numeric(year),
          monthno = as.numeric(substr(month, 2,3)),
          date = as.Date(sprintf ("%s-%s-01", year, monthno))
          ) |>
  arrange(date)

p1 <- z1 |>
  filter (year > 1999) |>
  ggplot(mapping=aes(x=date, y=zgony )) +
  geom_line(color='brown4', alpha=.3) +
 ## geom_point(color='red') +
  geom_point(aes(color = monthno == 1)) +
  scale_color_manual(values = c("FALSE" = "grey70", "TRUE" = "blue")) +
  ylab('') +
  xlab('lata') +
  ggtitle('Liczba zgonów w Polsce w latach 2000-2025',
          subtitle = 'Źródło: Eurostat/mmonth')

p1

z2 <- z1 |> mutate(t = row_number())
t1 <- lm(zgony ~ t, data=z2)
summary(t1)

##
z3 <- z2 |> mutate(dummy = if_else(year >= 2020 & year <= 2022, 1, 0))
t2 <- lm(zgony ~ t + dummy, data=z3)
summary(t2)

factor(z3$month)

## 0/1 dla miesięcy
t3 <- lm(zgony ~ t + factor(month), data = z3)

summary(t3)
##

z3 |> group_by(month) |>
  summarise(m = mean(zgony))
###
## Średnie tempo wzrostu
## https://www.wawrowski.edu.pl/stat/07-szereg-dynamiczny

##y <- c(100, 110, 121, 115, 130)
##
##n <- length(y)
##
##cagr <- (tail(y, 1) / head(y, 1))^(1/(n-1)) ##- 1
##cagr * 100

z3 |>

