library('tidyverse')
m <- read.csv("WHOMortalityDatabase_Deaths_sex_age_a_country_area_year-Lymphomas.csv", 
              ##row.names = NULL,
              dec = ".", sep = ",", header=T, na.strings ="NA")

m.pl <- m |>
  filter (Country.Code == 'POL') |>
  filter (Age.group.code == 'Age_all') |>
  filter (Sex != 'All') |>
  select (Year, Sex, value=Number, p=Percentage.of.cause.specific.deaths.out.of.total.deaths,
          dr=Death.rate.per.100.000.population)

p1 <- m.pl |> ggplot(aes(x=Year, y=value, color=Sex)) +
  geom_line()
p1

p2 <- m.pl |> ggplot(aes(x=Year, y=dr, color=Sex)) +
  geom_line()
p2
