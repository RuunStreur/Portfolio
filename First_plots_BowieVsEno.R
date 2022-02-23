library(tidyverse)
library(spotifyr)
library(ggplot2)

bowie <- get_playlist_audio_features("", "109zncLRngc5ofl1DsCHcE")
eno <- get_playlist_audio_features("", "6Rb67d7vVnq1FFAASwy5p5")
ziggy <- get_album("48D1hRORqJq52qsnUYZX56")
eno

ggplot(bowie, aes(x=energy, y=danceability, size=10, color=track.album.name))+geom_point()
ggplot(eno, aes(x=energy, y=danceability, size=tempo, color=track.album.name))+geom_point()

bowie2 <- bowie%>%
  group_by(track.album.name)%>%
  summarize(meanDance=mean(danceability), meanVal = mean(valence))

eno2 <- eno%>%
  group_by(track.album.name)%>%
  summarize(meanDance=mean(danceability), meanVal = mean(valence))

ggplot(eno2, aes(track.album.name,meanVal, color=track.album.name, fill=track.album.name)) + geom_col() + theme_light()
  labs(
  title = "The mean valence in Bowie's Berlin Trilogy",
  x='Album',
  y= 'Mean Valence',
  caption = 'Ruun Streur 2022'
  )

ggplot(bowie2, aes(track.album.name,meanVal, color=track.album.name, fill=track.album.name)) + geom_col()+
  labs(
    title = "The mean valence in five of Bowie's Albums",
    x='Album',
    y= 'Mean Valence',
    caption = 'Ruun Streur 2022'
  )




