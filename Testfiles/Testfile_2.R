library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(ggplot2)
library(compmus)

cap <- get_tidy_audio_analysis("0yhwdmbgkKdE1plV8xWdrd") 
heroes <- get_tidy_audio_analysis("5Ci3b8pfpLA9Zk17qGXBCF")
bop <- get_tidy_audio_analysis("4eBCTzBsSjYgrLH5clQf2x")
sans <- get_tidy_audio_analysis("58uyNQ2aC329uLMZpU9Ptz")
fela <- get_tidy_audio_analysis("0Q4O6v2akT3RfkNxCt522f")
place <- get_tidy_audio_analysis("6aBUnkXuCEQQHAlTokv9or")

psycho <- get_tidy_audio_analysis("5cE2nMYDgYj9gqG4KLafKy")
nf <- get_tidy_audio_analysis("2pmytOddxLVaASHQqTH9LO")
aypt <- get_tidy_audio_analysis("7Js4OF5MUb2bqJe09g4uQE")
hill <- get_tidy_audio_analysis("1raM9iX3HPhZvINoYCSWc9")
ata <- get_tidy_audio_analysis("37CRPk0L5ZpfPeePEPwE0t")

song <-
  heroes %>% # Change URI.
  compmus_align(bars, segments) %>%                     # Change `bars`
  select(bars) %>%                                      #   in all three
  unnest(bars) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "manhattan"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "manhattan"              # Change summary & norm.
      )
  )

#cepto

  
cepto1 <- song %>%
  compmus_gather_timbre() %>%
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()

cepto1
#ssm

ssm <- song %>%
  compmus_self_similarity(timbre, "aitchison") %>% 
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
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs( title = "Self Similarity Matrix of Bowie's 'Heroes'", 
       x = "",
       y = "")

ssm

###

library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit %>% 
    collect_predictions() %>% 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit %>% 
    conf_mat_resampled() %>% 
    group_by(Prediction) %>% mutate(precision = Freq / sum(Freq)) %>% 
    group_by(Truth) %>% mutate(recall = Freq / sum(Freq)) %>% 
    ungroup() %>% filter(Prediction == Truth) %>% 
    select(class = Prediction, precision, recall)
}  


halloween <-
  get_playlist_audio_features("", "6dOw9NxwvrQ7rCwatayV4A") %>%
  add_audio_analysis() %>%
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))

halloween_dist <- dist(halloween_juice, method = "euclidean")

halloween_juice <-
  recipe(
    track.name ~
      danceability +
      valence +
      energy,
    data = halloween
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(halloween %>% mutate(track.name = str_trunc(track.name, 20))) %>%
  juice() %>%
  column_to_rownames("track.name")

halloween_dist %>% 
  hclust(method = "complete") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram()


heatmaply(
  halloween_juice,
  hclustfun = hclust,
  hclust_method = "complete",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)



### Clustering unit

#```{r}

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit %>% 
    collect_predictions() %>% 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit %>% 
    conf_mat_resampled() %>% 
    group_by(Prediction) %>% mutate(precision = Freq / sum(Freq)) %>% 
    group_by(Truth) %>% mutate(recall = Freq / sum(Freq)) %>% 
    ungroup() %>% filter(Prediction == Truth) %>% 
    select(class = Prediction, precision, recall)
}  


bowie_dend <-
  get_playlist_audio_features("", "2bpiIW4K1BeSRXMNtCmI0M") %>%
  add_audio_analysis() %>%
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))

th_dend <-
  get_playlist_audio_features("", "6dOw9NxwvrQ7rCwatayV4A") %>%
  add_audio_analysis() %>%
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))

bowie_dend_juice <-
  recipe(
    track.name ~
      danceability +
      valence +
      energy,
    data = bowie_dend
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(th_dend %>% mutate(track.name = str_trunc(track.name, 20))) %>%
  juice() %>%
  column_to_rownames("track.name")

th_dend_juice <-
  recipe(
    track.name ~
      danceability +
      valence +
      energy,
    data = th_dend
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(th_dend %>% mutate(track.name = str_trunc(track.name, 20))) %>%
  juice() %>%
  column_to_rownames("track.name")

bowie_dend_dist <- dist(bowie_dend_juice, method = "euclidean")


th_dend_dist <- dist(th_dend_juice, method = "euclidean")

dend0 <- bowie_dend_dist %>% 
  hclust(method = "complete") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram() + labs(title = "Dendogram of Bowie's corpus")

dend1 <- th_dend_dist %>% 
  hclust(method = "complete") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram() + labs(title = "Dendogram of the Talking Heads corpus")

#```

#```{r}
grid.arrange(dend0, dend1, ncol = 2)
#```

***
  
  A clustering analysis of all Bowies's and Talking Heads's songs, produced and not pruduced by Eno, clustered by energy, valence and danceability.
