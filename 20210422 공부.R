library(gapminder)
library(dplyr)
library(ggplot2)

gapminder
glimpse(gapminder)

gapminder %>% ggplot(aes(x=lifeExp)) + geom_histogram()

diamonds %>% ggplot(aes(x=cut)) + geom_bar()

mpg

Titanic

mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)

gapminder %>%
  ggplot(aes(year, lifeExp, group = country,col=continent)) +
  geom_line() +
  facet_wrap(~ continent)

iris
i2 <- tbl_df(iris)

glimpse(i2)

gapminder %>%
  summarize(n_obs = n(),
            n_countries = n_distinct(country))

sample_n(gapminder, 10)
sample_frac(gapminder, 0.01)

# innder_XXX 공부하기

MD <- read.csv("C:/Users/Rprogram/Desktop/경득 통프/imdb.csv", header = T,
               stringsAsFactors = T)


glimpse(MD2)
MD$year2 <- as.numeric(as.character(MD$year))
bbs <- which(MD$year2>1800 & MD$year2<2020)
MD1 <- MD[bbs,]
glimpse(MD1)


WTF <- as.character(MD1$imdbRating)
MD1$imdbRating2 <- as.numeric(WTF)

glimpse(MD)

MD1$ratingCount2 <- as.numeric(as.character((MD1$ratingCount)))

MD %>% summarize(n_distinct(year))

MD1 %>% ggplot(aes(year2)) + geom_bar()

MD1 %>% ggplot(aes(x=year ,y=imdbRating2)) + geom_point()


x<- MD1 %>%
  group_by(year2) %>%
  summarise(median(imdbRating2,na.rm = TRUE))

#데이터프레임 이름넣는법
names(x) <- c("year","med")

glimpse(x)
summary(x)


labels(x)

x %>% ggplot(aes(x=year, y=med)) + geom_line()
x %>%
  ggplot(aes(x=year, y=med, fill=year)) + geom_bar(stat = 'identity')


MD1 %>% ggplot(aes(x=ratingCount2,y=imdbRating2, hue = "Type 1",split=TRUE)) + geom_jitter()

y <- MD1 %>%
  group_by(ratingCount2) %>%
  summarise(median(imdbRating2,na.rm = TRUE))

glimpse(y)
names(y) <- c("a","b")
y %>% ggplot(aes(x=a, y=b)) + geom_point()

glimpse(MD1)

MD1 %>% ggplot(aes(x=Adult,y=imdbRating2)) + geom_point()

               