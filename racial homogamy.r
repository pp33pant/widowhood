# racial homogamy 
female_race_homo <- female %>%
filter(racial_homo == 1)
main_line_race(female_race_homo)
ggsave("female_white_homo_line.png",width = 8, height = 6, units = "in")

female_race_hetero <- female %>%
filter(racial_hetero != 1)
main_line_race(female_race_hetero)
ggsave("female_white_hetero_line.png",width = 8, height = 6, units = "in")
