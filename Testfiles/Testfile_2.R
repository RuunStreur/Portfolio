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


