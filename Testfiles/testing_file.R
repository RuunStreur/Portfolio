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

bowie$track.artists[1][[1]][3]
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


