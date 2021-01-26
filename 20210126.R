library(gapminder)
gapminder
head(gapminder)
tail(gapminder)

install.packages('dplyr')
library(dplyr)
library(ggplot2)

gapminder$lifeExp

gapminder$gdpPercap

gapminder[, c('lifeExp', 'gdpPercap')]
gapminder %>% select(gdpPercap, lifeExp)

summary(gapminder$lifeExp)
summary(gapminder$gdpPercap)
cor(gapminder$lifeExp, gapminder$gdpPercap)

opar = par(mfrow=c(2,2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, ncalss=50)
hist(sqrt(gapminder$gdpPercap),nclass=50)
hist(log10(gapminder$gdpPercap),nclass = 50)
plot(log(gapminder$gdpPercap), gapminder$lifeExp, cex=0.5)

cor(gapminder$lifeExp, log10(gapminder$gdpPercap))
library(ggplot2)

library(ggplot2)
library(dplyr)
library(gapminder)

opar = par(mfrow = c(2,2))
gapminder %>% ggplot(aes(x=lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() + 
  scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() +
  scale_x_log10() + geom_smooth()

example(ggplot)

df <- data.frame(gp = factor(rep(letters[1:3], each=10)), y=rnorm(30))
glimpse(df)

ds <- df %>% group_by(gp) %>% summarize(mean = mean(y), sd = sd(y))
ds

ggplot(df, aes(x=gp, y =y)) +
  geom_point() +
  geom_point(data = ds, aes(x=gp, y=mean),
             colour = 'red' , size =3)

ggplot() +
  geom_point(data = df, aes(x =gp, y=y)) +
  geom_point(data = ds, aes(x=gp,y=mean),
             colour = 'red', size = 3) +
  geom_errorbar(data = ds, aes(x=gp, y=mean,
                               ymin = mean - sd, ymax = mean + sd),
                colour = 'red', width = 0.4)

ggplot(gapminder, aes(lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(lifeExp)) + geom_histogram()

glimpse(diamonds)
summary(diamonds)

par(mfrow= c(2,2))
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() +
  scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_freqpoly() +
  scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_density() +
  scale_x_log10()

summary(gapminder)

diamonds %>% ggplot(aes(cut)) + geom_bar()

table(diamonds$cut)
prop.table(table(diamonds$cut))
round(prop.table(table(diamonds$cut))*100, 1)

diamonds %>%
  group_by(cut) %>%
  tally() %>%
  mutate(pct = round(n/sum(n)*100,1))

tally(diamonds)

diamonds %>% ggplot(aes(carat, price)) + geom_point()
diamonds %>% ggplot(aes(carat, price)) + geom_point(alpha=0.1)
mpg %>% ggplot(aes(cyl,hwy)) + geom_point()
mpg %>% ggplot(aes(cyl,hwy)) + geom_jitter()

pairs(diamonds %>% sample_n(1000))

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()

mpg %>% ggplot(aes(class, hwy)) + geom_jitter(col = 'gray') +
  geom_boxplot(alpha = .5)
mpg %>% mutate(class = reorder(class, hwy, median)) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col = 'gray') +
  geom_boxplot(alpha = 0.5)
mpg %>%
  mutate(class=factor(class, levels =
                        c("2seater", "subcompact", "compact",
                          "midsize", "minivan", "suv", "pickup"))) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col='gray') +
  geom_boxplot(alpha=.5)
mpg %>%
  mutate(class=factor(class, levels=
                        c("2seater", "subcompact", "compact",
                          "midsize", "minivan", "suv", "pickup"))) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col = 'gray') +
  geom_boxplot(alpha=.5) + coord_flip()

help(pairs)
