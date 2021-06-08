library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(extrafont)
library(colorspace)
library(showtext)
#  data
castaways <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')

personalities_winners <- castaways %>%
  filter(result == "Sole Survivor") %>%
  group_by(personality_type) %>%
  summarise(types = n()) %>%
  mutate(percent_winners = types/sum(types))

personalities_all <- castaways %>%
  group_by(personality_type) %>%
  summarise(types = n()) %>%
  drop_na() %>%
  mutate(level_1 = ifelse(str_sub(personality_type,2,2) == "N", "Introspective",
                          "Observant"),
         level_2 = ifelse(str_sub(personality_type,2,3) == "NT", "Rational",
                          ifelse(str_sub(personality_type,2,3) == "NF", "Idealist",
                                 ifelse(str_sub(personality_type,2,2) == "S" & str_sub(personality_type,4,4) == "P", "Artisan",
                                        "Guardian"))),
         level_3 = ifelse(str_sub(personality_type,2,4) == "NTJ", "Coordinator",
                          ifelse(str_sub(personality_type,2,4) == "NTP", "Engineer",
                                 ifelse(str_sub(personality_type,2,4) == "NFJ", "Mentor",
                                        ifelse(str_sub(personality_type,2,4) == "NFP", "Advocate",
                                               ifelse(str_sub(personality_type,2,4) == "STJ", "Administrator",
                                                      ifelse(str_sub(personality_type,2,4) == "STP", "Operator",
                                                             ifelse(str_sub(personality_type,2,4) == "SFJ", "Conservator",
                                                                    "Entertainer"))))))),
         level_4 = ifelse(str_sub(personality_type,1,4) == "INTJ", "Mastermind",
                          ifelse(str_sub(personality_type,1,4) == "INTP", "Architect",
                                 ifelse(str_sub(personality_type,1,4) == "INFJ", "Counselor",
                                        ifelse(str_sub(personality_type,1,4) == "INFP", "Healer",
                                               ifelse(str_sub(personality_type,1,4) == "ISTJ", "Inspector",
                                                      ifelse(str_sub(personality_type,1,4) == "ISTP", "Crafter",
                                                             ifelse(str_sub(personality_type,1,4) == "ISFJ", "Protector",
                                                                    ifelse(str_sub(personality_type,1,4) == "ISFP", "Composer",
                                                                           ifelse(str_sub(personality_type,1,4) == "ENTJ", "Fieldmarshal",
                                                                                  ifelse(str_sub(personality_type,1,4) == "ENTP", "Inventor",
                                                                                         ifelse(str_sub(personality_type,1,4) == "ENFJ", "Teacher",
                                                                                                ifelse(str_sub(personality_type,1,4) == "ENFP", "Champion",
                                                                                                       ifelse(str_sub(personality_type,1,4) == "ESTJ", "Supervisor",
                                                                                                              ifelse(str_sub(personality_type,1,4) == "ESTP", "Promoter",
                                                                                                                     ifelse(str_sub(personality_type,1,4) == "ESFJ", "Provider",
                                                                                                                            "Performer"))))))))))))))),
         percent_contestant = types/sum(types),
         ring1_min = ifelse(level_1 == 'Introspective', 0, 8),
         ring1_max = ifelse(level_1 == 'Introspective', 8, 16),
         ring2_min = ifelse(level_2 == 'Idealist', 0,
                            ifelse(level_2 == 'Rational', 4,
                                   ifelse(level_2 == 'Guardian', 8, 12 ))),
         ring2_max = ifelse(level_2 == 'Idealist', 4,
                            ifelse(level_2 == 'Rational', 8,
                                   ifelse(level_2 == 'Guardian', 12, 16 ))),
         ring3_min = ifelse(level_3 == 'Mentor', 0,
                            ifelse(level_3 == 'Advocate', 2,
                                   ifelse(level_3 == 'Coordinator', 4,
                                          ifelse(level_3 == 'Engineer', 6,
                                                 ifelse(level_3 == 'Conservator', 8,
                                                        ifelse(level_3 == 'Administrator', 10,
                                                               ifelse(level_3 == 'Entertainer', 12,
                                                                      14 ))))))),
         ring3_max = ifelse(level_3 == 'Mentor', 2,
                            ifelse(level_3 == 'Advocate', 4,
                                   ifelse(level_3 == 'Coordinator', 6,
                                          ifelse(level_3 == 'Engineer', 8,
                                                 ifelse(level_3 == 'Conservator', 10,
                                                        ifelse(level_3 == 'Administrator', 12,
                                                               ifelse(level_3 == 'Entertainer', 14,
                                                                      16 )))))))) %>%
  rowid_to_column(var = 'rowid') %>%
  mutate(ring4_min = ifelse(level_4 == 'Counselor', 0,
                           ifelse(level_4 == 'Teacher', 1,
                                  ifelse(level_4 == 'Healer', 2,
                                         ifelse(level_4 == 'Champion', 3,
                                                ifelse(level_4 == 'Mastermind', 4,
                                                       ifelse(level_4 == 'Fieldmarshal', 5,
                                                              ifelse(level_4 == 'Architect', 6,
                                                                     ifelse(level_4 == 'Inventor', 7,
                                                                            ifelse(level_4 == 'Provider', 8,
                                                                                   ifelse(level_4 == 'Protector', 9,
                                                                                          ifelse(level_4 == 'Supervisor', 10,
                                                                                                 ifelse(level_4 == 'Inspector',11,
                                                                                                        ifelse(level_4 == 'Performer', 12,
                                                                                                               ifelse(level_4 == 'Composer', 13,
                                                                                                                      ifelse(level_4 == 'Promoter', 14,
                                                                                                                             15 )))))))))))))))) %>%
  mutate(ring4_max = ifelse(level_4 == 'Counselor', 1,
                            ifelse(level_4 == 'Teacher', 2,
                                   ifelse(level_4 == 'Healer', 3,
                                          ifelse(level_4 == 'Champion', 4,
                                                 ifelse(level_4 == 'Mastermind', 5,
                                                        ifelse(level_4 == 'Fieldmarshal', 6,
                                                               ifelse(level_4 == 'Architect', 7,
                                                                      ifelse(level_4 == 'Inventor', 8,
                                                                             ifelse(level_4 == 'Provider', 9,
                                                                                    ifelse(level_4 == 'Protector', 10,
                                                                                           ifelse(level_4 == 'Supervisor', 11,
                                                                                                  ifelse(level_4 == 'Inspector',12,
                                                                                                         ifelse(level_4 == 'Performer', 13,
                                                                                                                ifelse(level_4 == 'Composer', 14,
                                                                                                                       ifelse(level_4 == 'Promoter', 15,
                                                                                                                              16 ))))))))))))))))
personalities_all$percent_winners <- personalities_winners$percent_winners

ggplot(personalities_all ) +
  #inner circle
  geom_rect(aes( xmin = -5, xmax = 0, ymin = 0, ymax = 16), fill = "#ff7b00",color = "#ff7b00") +
  #innermost ring
  geom_rect(aes( xmin = 0, xmax = 4, ymin = ring1_min, ymax = ring1_max, fill = level_1), fill = "#ff8800",color = "grey") +
  geom_text(aes( x = 2, y = (ring1_min + ring1_max)/2, label = level_1, size = 1,
                 angle = c(270,270,270,270,90,90,90,90,270,270,270,270,90,90,90,90)), family = "Roboto Medium") +
  # inner ring 2
  geom_rect(aes( xmin = 4, xmax = 7, ymin = ring2_min, ymax = ring2_max, fill = level_2), fill =  "#ff9500", color = "grey") +
  geom_text(aes( x = 5.5, y = (ring2_min + ring2_max)/2, label = level_2, size = 1,
                 angle = c(-45,-45,-135,-135,135,45,135,45, -45,-45,-135,-135,135,45,135,45)), family = "Roboto Medium") +
  #inner ring 3
  geom_rect(aes( xmin = 7, xmax = 9, ymin = ring3_min, ymax = ring3_max, fill = level_3), fill =  "#ffa200", color = "grey") +
  geom_text(aes( x = 8, y = (ring3_min + ring3_max)/2, label = level_3, size = 1,
                 angle = c(-22.5,-67.5,-112.5,-157.5,157.5,67.5,112.5,22.5, -22.5,-67.5,-112.5,-157.5,157.5,67.5,112.5,22.5)), family = "Roboto Medium",) +
  # inner ring 4
  geom_rect(aes( xmin = 9, xmax = 10, ymin = ring4_min, ymax = ring4_max, fill = level_4), fill =  "#ffb700", color = "grey") +
  geom_text(aes( x = 9.5, y = (ring4_min + ring4_max)/2, label = level_4, size = 1,
                 angle = c(-33.75,-78.75,-123.75,-168.75,168.75,78.75,123.75,33.75,-11.5,-56.25,-101.25,-146.25,146.25,56.25,101.25,11.25)), family = "Roboto Medium") +
  # winner sunbeam
  geom_rect(aes( xmin = 10, xmax = 10+100*percent_winners, ymin = ring4_min, ymax = ring4_max - 0.5, fill = level_4), fill = "#ffd000", color = "grey") +
  geom_text(aes( x = 11, y = (ring4_min + ring4_max)/2-0.25, label = paste(round(100*percent_winners, digits = 1), "%"), size =1,
                 angle = c(-33.75,-78.75,-123.75,-168.75,168.75,78.75,123.75,33.75,-11.5,-56.25,-101.25,-146.25,146.25,56.25,101.25,11.25)), family = "Roboto Medium") +
  # all contestants sunbeam
   geom_rect(aes( xmin = 10, xmax = 10+100*percent_contestant, ymin = ring4_min + 0.5, ymax = ring4_max, fill = level_4), fill = "#ffaa00", color = "grey") +
   geom_text(aes( x = 11, y = (ring4_min + ring4_max)/2+0.25, label = paste(round(100*percent_contestant,digits = 1), "%"), size =1,
                  angle = c(-33.75,-78.75,-123.75,-168.75,168.75,78.75,123.75,33.75,-11.5,-56.25,-101.25,-146.25,146.25,56.25,101.25,11.25)), family = "Roboto Medium") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(-5, NA)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#87ceeb"),
        legend.position = "none",
        plot.margin     = margin(-300, -300, -350, -350)) +
  annotate("text",
           x      = 24,
           y      = 5,
           label  = "Graphic: Todd Warczak | Source: survivoR Package",
           family = "Roboto Medium",
           size   = 7,
           color  = "#274c77",
           vjust  = 28,
           hjust  = 0.85) +
  # title & description in center of plot
  annotate("text",
           x      = 0,
           y      = 0,
           label  = "Survivor",
           family = "Survivor Font",
           size   = 20,
           color  = "Black",
           vjust  = 3,
           hjust  = 0.5) +
  annotate("text",
           x      = 0,
           y      = 0,
           label  = "Personalities",
           family = "Survivor Font",
           size   = 14,
           color  = "Black",
           vjust  = 6,
           hjust  = 0.5) +
  geom_textbox(aes(x     = x,
                   y     = y,
                   label = label),
               inherit.aes = FALSE,
               width       = unit(4.3, "inch"),
               height      = unit(1, "inch"),
               hjust       = -0.9,
               vjust       = 8.5,
               fill        = "black",
               box.colour  = "black",
               box.padding = unit(c(15, 5, 1, 18), "pt"),
               data        = tibble(x = 1,
                                    y = 1,
                                    label = c("<p style='font-family:Roboto;font-size:20pt;color:#ffaa00'
                                              >Sunbeam</span><span style='color:#ffffff'>: % of all contestants</span></p><br>
                                              <p style='font-family:Roboto;font-size:20pt;color:#ffd000'
                                              >Sunbeam</span><span style='color:#ffffff'>: % of all winners</span></p>")))

ggsave("survivor.png", width = 20, height = 20, dpi = 600)
ggsave("survivor_low_res.png", width = 20, height = 20, dpi = 200)

