library('tidyverse')

## Przykład analiza struktury
## Obesity

## Przykład korelacja & regresja
## Obesity vs Diabets

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
