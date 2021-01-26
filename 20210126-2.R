glimpse(data.frame(Titanic))

xtabs(Freq ~ Class + Sex+ Age + Survived, data.frame(Titanic))

mosaicplot(Titanic, man = "Survival on the Titanic")
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)

apply(Titanic, c(3,4), sum)
round(prop.table(apply(Titanic, c(3, 4), sum),margin = 1) ,3)

apply(Titanic, c(2,4), sum)
round(prop.table(apply(Titanic, c(2, 4), sum),margin = 1) ,3)

t2 = data.frame(Titanic)

t2 %>% group_by(Sex) %>%
  summarize(n = sum(Freq),
            survivors = sum(ifelse(Survived == "Yes", Freq, 0))) %>%
  mutate(rate_Survival=survivors/n)

gapminder %>% filter(year == 2007) %>%
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point() + scale_x_log10() +
  ggtitle("Gapminder data for 2007")

gapminder %>% filter(year == 2007) %>%
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(aes(size=pop, col=continent)) + scale_x_log10() +
  ggtitle("Gapminder data for 2007") +

gapminder %>%
  ggplot(aes(year, lifeExp, group=country, col=continent)) +
  geom_line()

gapminder %>%
  ggplot(aes(year, lifeExp, group = country,col=continent)) +
  geom_line() +
  facet_wrap(~ continent)

