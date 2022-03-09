library(tidyverse)
library(spotifyr)
library(ggplot2)
library(compmus)

### Loading the playlists
bowie <- get_playlist_audio_features("", "109zncLRngc5ofl1DsCHcE")
bowie_eno <- get_playlist_audio_features("", "6Rb67d7vVnq1FFAASwy5p5")
TH <- get_playlist_audio_features("", "1K27mtJcCFQw4IxJLZlmNQ")
TH_eno <- get_playlist_audio_features("", "4XrUACEdnQh8WfygAgSKxd")
#bowie_disc <- get_playlist_audio_features("", "0mkz40ZahTeC6y2klc527h")

elliott <- get_playlist_audio_features("","6lPirgggG93MAzPSocd7zE")
elliott <- elliott %>% summarize(meanDance=mean(danceability), meanVal = mean(valence), meanEn = mean(energy), meanTemp = mean(tempo))
Nbowie$track.artists[1][[1]][3]
b[[1]][3]


bowie_extracol <- bowie %>% add_column(is_eno = 0)
bowie_eno_extracol <- bowie_eno %>% add_column(is_eno = 1)
TH_extracol <- TH %>% add_column(is_eno = 0)
TH_eno_extracol <- TH_eno %>% add_column(is_eno = 1)
bowie_all <- bind_rows(bowie_extracol, bowie_eno_extracol)
TH_all <- bind_rows(TH_extracol, TH_eno_extracol)


ggplot(bowie, aes(x=energy, y=danceability, size=10, color=track.album.name))+geom_point()
ggplot(bowie_eno, aes(x=energy, y=danceability, size=tempo, color=track.album.name))+geom_point()

bowie2 <- bowie%>%
  group_by(track.album.name)%>%
  summarize(meanDance=mean(danceability), meanVal = mean(valence))

eno2 <- bowie_eno%>%
  group_by(track.album.name)%>%
  summarize(meanDance=mean(danceability), meanVal = mean(valence))

enoplot <- ggplot(eno2, aes(track.album.name,meanVal, color=track.album.name, fill=track.album.name)) + geom_col() + theme_light()
  labs(
  title = "The mean valence in Bowie's Berlin Trilogy",
  x='Album',
  y= 'Mean Valence',
  caption = 'Ruun Streur 2022'
  )

bowieplot <- ggplot(bowie2, aes(track.album.name,meanVal, color=track.album.name, fill=track.album.name)) + geom_col()+
  labs(
    title = "The mean valence in five of Bowie's Albums",
    x='Album',
    y= 'Mean Valence',
    caption = 'Ruun Streur 2022'
  )

bowie2 <- bowie%>%
  group_by(track.album.name)%>%
  summarize(meanDance=mean(danceability), meanVal = mean(valence))

eno2 <- bowie_eno%>%
  group_by(track.album.name)%>%
  summarize(meanDance=mean(danceability), meanVal = mean(valence))

enoplot <- ggplot(eno2, aes(track.album.name,meanVal, color=track.artists, fill=track.album.name)) + geom_col() + theme_light()
labs(
  title = "The mean valence in Bowie's Berlin Trilogy",
  x='Album',
  y= 'Mean Valence',
  caption = 'Ruun Streur 2022'
)

enoplot
bowieplot <- ggplot(bowie2, aes(track.album.name,meanVal, color=track.album.name, fill=track.album.name)) + geom_col()+
  labs(
    title = "The mean valence in five of Bowie's Albums",
    x='Album',
    y= 'Mean Valence',
    caption = 'Ruun Streur 2022'
  )

bowie_discSum <- bowie_disc%>%
  group_by(track.album.name)%>%
  summarize(meanDance=mean(danceability), meanVal = mean(valence), meanTempo = mean(tempo), meanAc = mean(acousticness), meanInst = mean(instrumentalness),
            meanPop = mean(track.popularity), meanDur = mean(track.duration_ms)
  )

bowie_discplot <- ggplot(bowie_discSum, aes(x=reorder(track.album.name, meanDance), meanDance, color=track.album.name, fill=track.album.name)) + geom_col() + 
  theme_light() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(
    title = "The mean danceability in all of Bowie's albums",
    x='Album',
    y= 'Mean danceability'
  )

#bowie_discSum2_grob <- tableGrob(bowie_discSum2, rows=NULL, theme=tt)
#bowie_discSum2_grob[1:10, ], as.table = TRUE
#tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
#bowie_discSum2 <- bowie_discSum[order(bowie_discSum$meanDance, decreasing = TRUE),] 


heroes <-
  get_tidy_audio_analysis("5Ci3b8pfpLA9Zk17qGXBCF") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

helden <-
  get_tidy_audio_analysis("7dCiqEHU97OX7mkRG1Kdjm") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

compmus_long_distance(
  heroes %>% mutate(pitches = map(pitches, compmus_normalise, "manhattan")),
  helden %>% mutate(pitches = map(pitches, compmus_normalise, "manhattan")),
  feature = pitches,
  method = "manhattan"
) %>%
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_equal() +
  labs(x = "Heroes", y = "Helden") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)

######################

bowie <- get_playlist_audio_features("", "109zncLRngc5ofl1DsCHcE")
bowie_eno <- get_playlist_audio_features("", "6Rb67d7vVnq1FFAASwy5p5")
TH <- get_playlist_audio_features("", "1K27mtJcCFQw4IxJLZlmNQ")
TH_eno <- get_playlist_audio_features("", "4XrUACEdnQh8WfygAgSKxd")

bowieSum <- bowie%>%
  summarize(meanVal = mean(danceability)) %>% add_column(is_eno = 0, artist = 'bowie')
bowie_enoSum <- bowie_eno%>%
  summarize(meanVal = mean(danceability)) %>% add_column(is_eno = 1, artist = 'bowie')
THSum <- TH%>%
  summarize(meanVal = mean(danceability)) %>% add_column(is_eno = 0, artist = 'TH')
TH_enoSum <- TH_eno%>%
  summarize(meanVal = mean(danceability)) %>% add_column(is_eno = 1, artist = 'TH')
all_sum1 <- bind_rows(bowieSum, bowie_enoSum, THSum, TH_enoSum)

bowie_val <- subset(all_sum1, artist=="bowie" & is_eno=="0")$meanVal
bowie_eno_val <- subset(all_sum1, artist=="bowie" & is_eno=="1")$meanVal
TH_val <- subset(all_sum1, artist=="TH" & is_eno=="0")$meanVal
TH_eno_val <- subset(all_sum1, artist=="TH" & is_eno=="1")$meanVal

bowieval <- data.frame(artist = 'David Bowie', without_eno = bowie_val, with_eno = bowie_eno_val)
THval <- data.frame(artist = 'Talking Heads', without_eno = TH_val, with_eno = TH_eno_val)
all_sum <- bind_rows(bowieval, THval)

colnames(all_sum) <- c("artist", "without_eno", "with_eno")
left_label <- paste(all_sum$artist, all_sum$`without_eno`,sep=", ")
right_label <- paste(all_sum$artist, all_sum$`with_eno`,sep=", ")
all_sum$class <- ifelse((all_sum$`with_eno` - all_sum$`without_eno`) < 0, "red", "green")

p <- ggplot(all_sum) + geom_segment(aes(x=1, xend=2, y=`without_eno`, yend=`with_eno`, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +
  labs(x="", y="Mean danceability") +  # Axis labels
  xlim(.5, 2.5) + ylim(0.4,(1.1*(max(all_sum$`without_eno`, all_sum$`with_eno`))))

p <- p + geom_text(label=left_label, y=all_sum$`without_eno`, x=rep(1, NROW(all_sum)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=all_sum$`with_eno`, x=rep(2, NROW(all_sum)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="Without Eno", x=1, y=1.1*(max(all_sum$`without_eno`, all_sum$`with_eno`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="With Eno", x=2, y=1.1*(max(all_sum$`without_eno`, all_sum$`with_eno`)), hjust=-0.1, size=5)  # title

p + theme(axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))
labs(
  title = 'The mean danceability of Eno vs. non-Eno produced albums'
)

########################

library(ggplot2)
library(plotly)

bowie_all$duration_z <- round((bowie_all$track.duration_ms - mean(bowie_all$track.duration_ms))/sd(bowie_all$track.duration_ms), 2) 
bowie_all$is_eno2 <- ifelse(bowie_all$is_eno < 1, "No", "Yes")  # above / below avg flag
bowie_all <- bowie_all[order(bowie_all$duration_z), ]
bowie_all$track.name <- factor(bowie_all$track.name, levels = bowie_all$track.name)  # convert to factor to retain sorted order in plot.

divbar1 <- ggplot(bowie_all, aes(x=track.name, y=duration_z)) + 
  geom_bar(stat='identity', aes(fill=is_eno2), width=.5)  +
  scale_fill_manual(name="Produced by Eno", 
                    labels = c("Produced by Eno", "Not Produced by Eno"), 
                    values = c("Yes"="#00ba38", "No"="#f8766d")) + 
  labs(subtitle="Normalised duration of Bowie's songs", 
       title= "Bowie's song durations") +
  theme( 
    axis.text.y = element_text(vjust=0.2, size=5),
    axis.ticks.x = element_blank()) +
  xlab('Song') + ylab('Normalized durations') +
  coord_flip()

TH_all$duration_z <- round((TH_all$track.duration_ms - mean(TH_all$track.duration_ms))/sd(TH_all$track.duration_ms), 2) 
TH_all$is_eno2 <- ifelse(TH_all$is_eno < 1, "No", "Yes")  # above / below avg flag
TH_all <- TH_all[order(TH_all$duration_z), ]
TH_all$track.name <- factor(TH_all$track.name, levels = TH_all$track.name)  # convert to factor to retain sorted order in plot.

divbar2 <- ggplot(TH_all, aes(x=track.name, y=duration_z)) + 
  geom_bar(stat='identity', aes(fill=is_eno2), width=.5)  +
  scale_fill_manual(name="Produced by Eno", 
                    labels = c("Produced by Eno", "Not Produced by Eno"), 
                    values = c("Yes"="#00ba38", "No"="#f8766d")) + 
  labs(subtitle="Normalised duration of TH's songs", 
       title= "Talking Heads's song durations") + 
  theme( 
    axis.text.y = element_text(vjust=0.2, size=5),
    axis.ticks.x = element_blank()) +
  xlab('Song') + ylab('Normalized durations') +
  coord_flip()

ggplotly(divbar2)
ggplotly(divbar1)
library(kableExtra)
tafel <- bowie[0:10, 6:16]
tafel2 <- tafel %>%
  kbl() %>%
  kable_material(c("striped", "hover"))

kbl(tafel) %>% kable_styling(bootstrap_options = c("striped", "hover"))

####################

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )

twenty_five <-
  get_tidy_audio_analysis("0yhwdmbgkKdE1plV8xWdrd") %>%
  compmus_align(bars, segments) %>%
  select(bars) %>%
  unnest(bars) %>%
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  )

twenty_five %>% 
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "")
