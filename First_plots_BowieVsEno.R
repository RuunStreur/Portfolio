library(tidyverse)
library(spotifyr)

bowie <- get_playlist_audio_features("", "109zncLRngc5ofl1DsCHcE")
eno <- get_playlist_audio_features("", "6Rb67d7vVnq1FFAASwy5p5")
ziggy <- get_album("48D1hRORqJq52qsnUYZX56")
eno

ggplot(bowie, aes(x=energy, y=danceability, size=10, color=track.album.name))+geom_point()
ggplot(eno, aes(x=energy, y=danceability, size=tempo, color=track.album.name))+geom_point()

bowie2 <- bowie%>%
  group_by(track.album.name)%>%
  summarize(meanDance=mean(danceability))


ggplot(bowie2, aes(track.album.name,meanDance, color=track.album.name, fill=track.album.name)) + geom_col()

ggplot(bowie2, aes(x = meanDance, y = track.album.name)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 30, yend = track.album.name), size = 2) +
  geom_text(aes(label = round(meanDance,1)), color = "white", size = 1.5) +
  scale_x_continuous("", expand = c(0,0), limits = c(30,90), position = "top")
