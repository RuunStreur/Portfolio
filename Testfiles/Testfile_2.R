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




