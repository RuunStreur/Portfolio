library(tidyverse)
library(spotifyr)

bliepjes <- get_playlist_audio_features("", "4lWd8vOEKRp1oWPIUl7Ndw")
bliepjes
balcony <- get_playlist_audio_features("", "1lGJfgn1POcdpewsw3YSNP")
disco <- get_playlist_audio_features("", "37i9dQZF1DXbS8bPVXXR2B")
bowie <- get_playlist_audio_features("", "109zncLRngc5ofl1DsCHcE")
eno <- get_playlist_audio_features("", "6Rb67d7vVnq1FFAASwy5p5")
eno %>%
  filter(key==7)
ggplot(eno, aes(x=loudness, y=energy, size=danceability))+geom_point()
ggplot(disco, aes(x=loudness, y=energy, size=danceability, color=mode))+geom_point()
