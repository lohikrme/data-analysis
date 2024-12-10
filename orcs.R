
# mission: year 2000 sauron had 100 orcs. year 2500 he had enough orcs for war.
# which year sauron had 10% of orcs needed to war. 
# Assume orc amount doubles every 50 years, continiously.

install.packages("ggplot2")
library(ggplot2)

orc_population = c(100, 200, 400, 800, 1600, 3200, 6400, 12800, 25600, 51200, 102400)
orc_population_years = c(2000, 2050, 2100, 2150, 2200, 2250, 2300, 2350, 2400, 2450, 2500)

orc_data = data.frame(year = orc_population_years, population = orc_population)

# Laske 10% maksimipopulaatiosta
max_population = max(orc_population)
ten_percent_intervals = seq(0.1, 1, by = 0.1) * max_population

ggplot(orc_data, aes(x = year, y = population)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_hline(yintercept = ten_percent_intervals, linetype = "solid", color = "gray") +
  labs(title = "Orc Population Growth Over Time",
       x = "Year",
       y = "Orc Population") +
  theme_minimal()


# calculate doubling time (just for test purpose)
print(paste("Doubling time: ", (2500-2000)/log2(102400/100)))

# calculate using logarithm the year when 10% of orcs are spawned
print(paste("The year when 10% of needed orcs are spawned:", 50*(log2(102.4))+2000))
       