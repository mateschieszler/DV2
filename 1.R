library(students)
?students
str(students)



ggplot(students, aes(x = math, y = shoe)) +
  geom_point(aes(color = z)) +

ggplot(students, aes(x = math, y = shoe)) +
  geom_point(aes(color = z)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) + 
  geom_smooth(aes(color = z))

fit <- lm(math ~ shoe, data = students)
summary(fit)

plot(students$shoe, students$math)
abline(fit, col = 'red')

library(ggplot2)
ggplot(students, aes(shoe, math)) + geom_point() + geom_smooth(method = 'lm')

fit <- lm(math ~ shoe + x, data = students)
summary(fit)

residuals(lm(math ~ x, data = students))
residuals(lm(shoe ~ x, data = students))

cor(residuals(lm(math ~ x, data = students)),
    residuals(lm(shoe ~ x, data = students)))

install.packages('psych')
library(psych)
partial.r(students, 1:2, 3)

plot(students)

library(GGally)
ggpairs(students)

install.packages('gtExtras')
install.packages('svglite')
library(svglite)
library(gtExtras)
gt_plt_summary(students)


.secret


download.file('https://bit.ly/de-cities-distance', 'cities.xls', mode = 'wb')
install.packages('readxl')
library('readxl')
cities <- read_excel('cities.xls')
str(cities)

#drop first column
cities <- cities[, -1]
str(cities)

#drop last 3 row
cities <- cities[1:15,]

plot(cities)

## MDS multi dimensional scalingű
?cmdscale
# what is a distance matrix??????
mds <- cmdscale(as.dist(cities))
plot(mds)
text(mds[, 1], mds[, 2], names(cities))
mds[, 2] <- mds[, 2] * -1

plot(mds)
text(mds[, 1], mds[, 2], names(cities))

library(data.table)

str(mds)

as.data.frame(mds)

mds <- as.data.frame(mds)
mds$city <- rownames(mds)

ggplot(mds, aes(x = V1, y = V2)) + geom_point() + geom_text(aes(label = city))

ggplot(mds, aes(x = V1, y = V2, label = city))  + geom_text() + theme_void()                                                         

## TODO 
?eurodist

str(eurodist)

eur <- cmdscale(as.dist(eurodist))
eur[, 2] <- eur[, 2] * -1
plot(eur)
text(eur[, 1], eur[, 2])
eur <- as.data.frame(eur)
eur$city <- rownames(eur)
ggplot(eur, aes(x = V1, y = V2)) + geom_point() + geom_text(aes(label = city))
ggplot(eur, aes(x = V1, y = V2, label = city))  + geom_text() + theme_void()

eurodist

install.packages('tidygeocoder')
library(tidygeocoder)

library(data.table)
eur <- data.table(geocode(eur, 'city'))
geocode(eur, 'city')
ggplot(eur,aes(x=long,y=lat))+geom_point()+geom_text(aes(label=city))



install.packages('maps')
?maps::world
world <- map_data('world')

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region))

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  coord_fixed(1.3)

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  geom_point(data = eur, aes(long, lat), color = 'orange') +
  coord_fixed(1.3)

world$a <- grepl('^A', world$region)

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region, fill = a)) +
  geom_point(data = eur, aes(long, lat), color = 'black') +
  coord_fixed(1.3)  

install.packages('ggmap')
library(ggmap)
register_stadiamaps('cb9c8776-fd87-4be8-a54d-cc4f6d1d5af4')

map <- get_stadiamap(
  c(
    left = min(eur$long) - 5,
    right = max(eur$long) + 5,
    top = max(eur$lat) + 5,
    bottom = min(eur$lat) - 5
  ),
  zoom = 4,
  maptype = 'stamen_toner'
)

str(map)

ggmap(map)

ggmap(map) + 
  geom_point(data = eur, aes(long, lat), color = 'orange')

install.packages('sf')
library(sf)
geoeur <- st_as_sf(x = eur, coords = c('long', 'lat'))
st_bbox(geoeur)

unname(st_bbox(geoeur))

map <- get_stadiamap(
  unname(st_bbox(geoeur)),
  zoom = 4,
  maptype = 'stamen_toner'
)

ggmap(map) + 
  geom_point(data = eur, aes(long, lat), color = 'orange')

download.file(
  'https://stacks.stanford.edu/file/druid:rc343vz5889/data.zip',
  'Austria_boundary.zip')
download.file(
  'https://stacks.stanford.edu/file/druid:yv617vc9132/data.zip',
  'Austria_divisions.zip')
unzip('Austria_boundary.zip')
unzip('Austria_divisions.zip')

library(sf)
st_layers('.')

adm0 <- st_read('.', layer = 'AUT_adm0')
plot(adm0)

adm2 <- st_read('.', layer = 'AUT_adm2')
plot(adm2)


cities <- fread('https://simplemaps.com/static/data/country-cities/at/at.csv')

ggplot() + 
  geom_sf(data = adm2, color = 'black', fill = 'white') +
  geom_sf(data = adm0, color = 'red', alpha = 0.0) +
  geom_point(data = cities, aes(lng, lat, size = population, color = admin_name)) +
  theme_void() + theme(legend.position = 'top')

ggplot() + 
  geom_sf(data = adm2, color = 'black', aes(fill = NAME_1)) +
  geom_sf(data = adm0, color = 'red', alpha = 0.0) +
  geom_point(data = cities, aes(lng, lat, size = population)) +
  theme_void() + theme(legend.position = 'top')

## geojson
download.file(
  'https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2021/simplified-95/bezirke_95_topo.json',
  'austria.geojson')

install.packages('leaflet')
library(leaflet)

map <- st_read('austria.geojson')
plot(map)

popup <- paste0('<strong>Name: </strong>', map$name)
leaflet(map) %>%
  addPolygons(
    weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5,
    popup = popup,
    label = ~name,
    layerId = ~name,
    labelOptions = labelOptions(noHide = TRUE),
    highlightOptions = highlightOptions(
      color = 'white', weight = 2,
      bringToFront = TRUE))

dist(mtcars)
mds <- as.data.frame(cmdscale(dist(mtcars)))
mds$car <- rownames(mtcars)
ggplot(mds, aes(V1, V2, label = car)) + geom_text()

install.packages('ggrepel')
library(ggrepel)

ggplot(mds, aes(V1, V2, label = car)) + geom_point() + geom_text_repel()

mds <- as.data.frame(cmdscale(dist(scale(mtcars))))
mds$car <- rownames(mtcars)
ggplot(mds, aes(V1, V2, label = car)) + geom_text()
ggplot(mds, aes(V1, V2, label = car)) + geom_point() + geom_text_repel()

UCBAdmissions
str(UCBAdmissions)
plot(UCBAdmissions)

UCB <- as.data.frame(UCBAdmissions)

ggplot(UCB, aes(x = Dept,y = Freq)) + geom_col(color = Gender)
ggplot(UCB, aes(x = Gender, y = Freq)) + geom_col()

ggplot(UCB, aes(x = Gender, y = Freq, fill = Admit)) + geom_col()
ggplot(UCB, aes(x = Gender, y = Freq, fill = Admit)) + geom_col(position = 'fill')

ggplot(UCB, aes(x = Gender, y = Freq, fill = Admit)) + geom_col(position = 'fill') +
  scale_fill_manual(values = c('Admitted' = 'darkgreen',
                               'Rejected' = 'darkred'))

ggplot(UCB, aes(x = Gender, y = Freq, fill = Admit)) + geom_col(position = 'fill') +
  scale_fill_manual(values = c('Admitted' = 'darkgreen',
                               'Rejected' = 'darkred')) + facet_wrap(~Dept)

?iris
## TODO scatterplot on Sepal.Length ~ Sepal.Width + lm

iris <- as.data.frame(iris)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
  geom_smooth(method = 'lm') +
  geom_smooth(aes(color = Species), method = 'lm')

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color=Species)) +
  geom_smooth(color='black', method = 'lm') +
  geom_smooth(aes(color = Species), se = FALSE, method = 'lm') +
  theme_grey()

?geom_smooth

mds <- as.data.frame(cmdscale(dist(iris)))
mds$Species <- iris$Species
ggplot(mds, aes(V1, V2)) + 
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(color = Species), method = 'lm', se = FALSE) +
  geom_smooth(method = 'lm', color = 'black', se = FALSE)

mds <- as.data.frame(cmdscale(dist(iris[,1:4])))
mds$Species <- iris$Species
ggplot(mds, aes(V1, V2)) + 
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(color = Species), method = 'lm', se = FALSE) +
  geom_smooth(method = 'lm', color = 'black', se = FALSE)

anscombe
ans <- as.data.frame(anscombe)
df <- data.frame(x = anscombe$x1, y = anscombe$y1)
ggplot(df,aes(x, y)) + geom_point()


lapply(1:8, function(i) anscombe[, i])
lapply(1:4, function(i) anscombe[,c(i, i + 4)])
lapply(1:4, function(i) data.frame(x =anscombe[,i], y = anscombe[,i+4]))
rbindlist(lapply(1:4, function(i) data.frame(x =anscombe[,i], y = anscombe[,i+4])))
       
library(data.table)
df <- rbindlist(lapply(1:4, function(i) data.frame(
  x = anscombe[, i], 
  y = anscombe[, i+4], 
  type = i)))
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~type)       
       
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~type) + geom_smooth(method = 'lm', se = FALSE)

install.packages('datasauRus')
library(datasauRus)
str(datasaurus_dozen_wide)

?lapply

datasaurus_dozen_wide
df <- rbindlist(lapply(seq(1, 25, by = 2), function(i) data.frame(
  x = datasaurus_dozen_wide[, i, drop = TRUE], 
  y = datasaurus_dozen_wide[, i+1, drop = TRUE], 
  type = sub('_x$', '', names(datasaurus_dozen_wide)[i]))))
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~type) 
     