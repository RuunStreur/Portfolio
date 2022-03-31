library(tidyverse)
library(spotifyr)
library(ggplot2)
library(compmus)

### Loading the playlists
bowie <- get_playlist_audio_features("", "109zncLRngc5ofl1DsCHcE")
bowie_eno <- get_playlist_audio_features("", "6Rb67d7vVnq1FFAASwy5p5")
TH <- get_playlist_audio_features("", "1K27mtJcCFQw4IxJLZlmNQ")
TH_eno <- get_playlist_audio_features("", "4XrUACEdnQh8WfygAgSKxd")

bowie_clean <- cbind(bowie_all[6:7], bowie_all[9], bowie_all[10:16], bowie_all['is_eno'])
TH_clean <- cbind(TH_all[6:7], TH_all[9], TH_all[10:16], TH_all['is_eno'])




r_bowie <- cor(bowie_clean)
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}
p_bowie <- cor.test.p(bowie_clean)

bowiemap <- heatmaply_cor(
  r_bowie,
  node_type = "scatter",
  point_size_mat = -log10(p_bowie), 
  point_size_name = "-log10(p-value)",
  label_names = c("x", "y", "Correlation")
)

r_TH <- cor(TH_clean)

cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}
p_TH <- cor.test.p(bowie_clean)

THmap <- heatmaply_cor(
  r_TH, xlab = 'Features' , ylab = "Features", main = "The correlations of features, Talking Heads", 
  node_type = "scatter",
  point_size_mat = -log10(p_TH), 
  point_size_name = "-log10(p-value)",
  label_names = c("x", "y", "Correlation")
) 

bowiemap
THmap

#####

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
  labs(x = "Heroes", y = "Helden", title = "Dynamic time warping analysis of Helden vs. Heroes") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)


