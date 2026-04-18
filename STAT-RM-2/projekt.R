library('tidyverse')
library('moments')
library('knitr')

## Przykład analiza struktury
## „Otyłość wśród dorosłych, BMI ≥ 30, częstość występowania (surowe oszacowanie) (%)
## Obesity among adults, BMI >= 30, prevalence (crude estimate) (%)
## https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-obesity-among-adults-bmi-=-30-(crude-estimate)-(-)
o0 <- read.csv("./otyli_who.csv",
              dec = ".", sep = ";", header=T, na.strings ="NA")
o.eur <- o0 |>
  filter (ParentLocationCode == 'EUR') |> ## Europa
  filter (year == 2000 | year == 2022) |> ## rok
  filter (sex == 'BTSX') |> ## płeć
  filter (AGEGROUP == 'YEARS18-PLUS') ## grupa wiekowa

o.eur.agg <- o.eur |>
  group_by(year) |>
  summarise(mean = mean(p),
            median = median(p),
            q1 = quantile(p, 0.25),
            q3 = quantile(p, 0.75),
            variance = var(p),
            sd = sd(p),
            iqr = IQR(p),
            ##iqr2 = q3 - q1,
            skweness = moments::skewness(p)
            )

knitr::kable(o.eur.agg)

## Wykres pudełkowy

p1 <- ggplot(o.eur, aes(x = factor(year), y = p)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Otyłość w Europie w latach 2000, 2010, 2022",
       caption = "Źródło: gho tabela"
       x = "Rok",
       y = "p")
p1


## Przykład korelacja & regresja
## Obesity vs Diabets
## https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-diabetes-age-standardized
## Age standardized  prevalenc of diabets
## standaryzowana względem wieku częstość występowania cukrzycy (%)
c0 <- read.csv("./cukrzycy_who.csv",
              dec = ".", sep = ";", header=T, na.strings ="NA")

c.eur <- c0 |>
  filter (ParentLocationCode == 'EUR') |> ## Europa
  filter (year == 2000 | year == 2010 | year == 2022) |> ## rok
  filter (sex == 'BTSX') |> ## płeć
  filter (AGEGROUP == 'YEARS18-PLUS') ## grupa wiekowa

oc.eur <- left_join(o.eur, c.eur, by=c('year', 'COUNTRY')) |>
  filter (year == 2022) |>
  select (o=p.x, d=p.y)

## Odsetek osób z otyłością a odsetek osób z cukrzyć w Europie
## w roku 2022

p1 <- ggplot(oc.eur, aes(x = o, y = d)) +
  geom_point(color = "blue") +
  labs(title = "Wykres XY",
       x = "x",
       y = "y")

m1 <- lm (d ~ o, data=oc.eur)

summary(m1)


## Przykład szereg czasowy
m <- read.csv("WHOMortalityDatabase_Deaths_sex_age_a_country_area_year-Lymphomas.csv",
              ##row.names = NULL,
              dec = ".", sep = ",", header=T, na.strings ="NA")

m.pl <- m |>
  filter (Country.Code == 'POL') |>
  filter (Age.group.code == 'Age_all') |>
  filter (Sex != 'All') |>
  select (Year, Sex,
          value=Number,
          p=Percentage.of.cause.specific.deaths.out.of.total.deaths,
          dr=Death.rate.per.100.000.population) |>
  mutate (trend = row_number())

## liczba zgonów
p1 <- m.pl |> ggplot(aes(x=Year, y=value, color=Sex)) +
  geom_line() +
  geom_point()
p1

## wskaźnik / 100 tys
p2 <- m.pl |> ggplot(aes(x=Year, y=dr, color=Sex)) +
  geom_line()
p2

## trend liniowy
m1 <- lm(value ~ trend, data=m.pl)
summary(m1)

## Średnie tempo zmian
stw <- m.pl %>%
  arrange(Year) %>%
  summarise(
    cagr = (last(value) / first(value))^(1/(n() - 1)) - 1
  ) %>%
  mutate(cagr = cagr * 100) |>
  unlist() |> unname ()

stw

## koniec
