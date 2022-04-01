library(flexdashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)
library(gridExtra)
library(patchwork)
library(knitr)
library(kableExtra)
library(scales)

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
  bowie_all %>%
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
      instrumentalness + 
      loudness +
      energy,
    data = bowie_dend
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(bowie_dend %>% mutate(track.name = str_trunc(track.name, 20))) %>%
  juice() %>%
  column_to_rownames("track.name")

bowie_dend_dist <- dist(bowie_dend_juice, method = "euclidean")

dend0 <- bowie_dend_dist %>% 
  hclust(method = "complete") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram() + labs(title = "Dendogram of Bowie's corpus")
  
th_dend <-
  TH_all %>%
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

th_dend_juice <-
  recipe(
    track.name ~
      instrumentalness +
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

th_dend_dist <- dist(th_dend_juice, method = "euclidean")

TH_dend <- th_dend_dist %>% 
  hclust(method = "complete") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram() + labs(title = "Dendogram of the Talking Heads corpus")

bowie_heat <- heatmaply(
  bowie_dend_juice,
  hclustfun = hclust,
  hclust_method = "complete",  # Change for single, average, or complete linkage.
  dist_method = "euclidean",   showticklabels = c(TRUE, FALSE)
) 

TH_heat <- heatmaply(
  th_dend_juice,
  hclustfun = hclust,
  hclust_method = "complete",  # Change for single, average, or complete linkage.
  dist_method = "euclidean",   showticklabels = c(TRUE, FALSE)
)
TH_heat


subplot(bowie_heat, TH_heat)
