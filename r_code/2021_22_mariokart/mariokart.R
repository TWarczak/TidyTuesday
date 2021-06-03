library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(extrafont)
library(lubridate)
library(colorspace)
library(ggimage)
library(ggforce)

#  data
tuesdata <- tidytuesdayR::tt_load(2021, 22)
readme(tuesdata)

records <- tuesdata$records
drivers <- tuesdata$drivers

lane_labs <- record_tbl %>%
             group_by(track) %>%
             filter(id == min(id))

record_tbl <- records %>%
              group_by(player, track, type, shortcut) %>%
              summarise(record_days = sum(record_duration),
                        first_rec_day = min(date)) %>%
              mutate(image = ifelse(type == "Three Lap" & shortcut == "Yes",
                                    "images/2021_22_mariokart/red_triple.png",
                             ifelse(type == "Single Lap" & shortcut == "Yes",
                                    "images/2021_22_mariokart/red_single.png",
                             ifelse(type == "Three Lap" & shortcut == "No",
                                    "images/2021_22_mariokart/green_triple.png",
                                    "images/2021_22_mariokart/green_single.png")))) %>%
              ungroup() %>%
              mutate(track     = as_factor(track),
                     track_num = recode(track,
                                        "Banshee Boardwalk"     = 1,
                                        "Bowser's Castle"       = 2,
                                        "Choco Mountain"        = 3,
                                        "D.K.'s Jungle Parkway" = 4,
                                        "Frappe Snowland"       = 5,
                                        "Kalimari Desert"       = 6,
                                        "Koopa Troopa Beach"    = 7,
                                        "Luigi Raceway"         = 8,
                                        "Mario Raceway"         = 9,
                                        "Moo Moo Farm"          = 10,
                                        "Rainbow Road"          = 11,
                                        "Royal Raceway"         = 12,
                                        "Sherbet Land"          = 13,
                                        "Toad's Turnpike"       = 14,
                                        "Wario Stadium"         = 15,
                                        "Yoshi Valley"          = 16 )) %>%
              arrange(track_num) %>%
              mutate(id      = row_number(),
                     starman = ifelse(id == 115, "images/2021_22_mariokart/starman3.png", NA ),
                     banana  = ifelse(player == 'Myles' & record_days > 50 & record_days < 2000,
                                      "images/2021_22_mariokart/banana.png", NA),
                     mystery = ifelse(player == 'David B',
                                      "images/2021_22_mariokart/mystery_box.png", NA),
                     coin    = ifelse(player == "JWhalls",
                                      "images/2021_22_mariokart/coin.png", NA))

# 8+ years holding a record get shell
shells_tbl <- record_tbl %>%
  filter(record_days > 2920)

# want 4 bananas
banana_tbl <- record_tbl %>%
  filter(!is.na(banana))

# want 1 starman
starman_tbl <- record_tbl %>%
  filter(!is.na(starman))

# want 4 mystery boxes
mystery_tbl <- record_tbl %>%
  filter(!is.na(mystery))

# want 13 coins
coin_tbl <- record_tbl %>%
  filter(!is.na(coin))

# ggplot
(mario_rainbow <- record_tbl %>%
  ggplot(aes(x     = id,
             y     = record_days,
             color = track_num,
             fill  = track_num )) +
  geom_image(data = starman_tbl,
             aes(image = starman,
                 x     = 770,
                 y     = 3200,
                 color = NULL,
                 fill  = NULL,
                 size  = 250)) +
  ggforce::geom_link(aes(x     = id,
                         xend  = id,
                         y     = 0,
                         yend  = record_days,
                         color = track_num,
                         alpha = record_days ),
                     n    = 200,
                     size = .45 ) +
  geom_point(aes(y     = record_days,
                 size  = record_days )) +
  geom_image(data = shells_tbl, aes(image = image,
                                    color = NULL,
                                    size  = 1)) +
  geom_image(data = banana_tbl, aes(image = banana,
                                    x     = c(150,400,450,600),
                                    y     = c(400,2750,700,2000),
                                    color = NULL,
                                    fill  = NULL,
                                    size  = 2)) +
  geom_image(data = mystery_tbl, aes(image = mystery,
                                     x     = c(150,300,450,600),
                                     y     = c(1500,1500,1500,1500),
                                     color = NULL,
                                     fill  = NULL,
                                     size  = 2)) +
  geom_image(data = coin_tbl, aes(image = coin,
                                  x     = c(150,150,150,150,150,150,450,490,530,570,610,650,690),
                                  y     = c(1700,1800,1900,2000,2100,2200,
                                            1650,1690,1730,1770,1810,1850,1890),
                                  color = NULL,
                                  fill  = NULL,
                                  size  = 0.4)) +
  coord_polar(theta = "y",
              start = 4.00,
              clip = "off") +
  scale_x_continuous(limits = c(-450, NA),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 6500),
                     expand = c(0, 0)) +
  scale_color_gradientn(colors = c("#75029C","#0036EB","#0099F3","#00CCF4","#1DBE28","#20E030",
                                   "#8DFB38", "#FFFF38","#FFD82E","#FF6B27","#E40E15"),
                        guide  = FALSE) +
  scale_size(range = c(.0015, 1), guide = FALSE) +
  scale_alpha(range = c(.4, 1), guide = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        plot.margin     = margin(-170, -181, -330, -181)) +
  ## track names
  geom_text(aes(y     = 0,
                label = track),
            data     = lane_labs,
            family   = "Roboto",
            fontface = "bold",
            size     = 7,
            vjust    = 1.5,
            hjust    = -0.1,
            color    = "white",
            angle    = -50.5 ) +
  # caption
  annotate("text",
           x      = 700,
           y      = 4000,
           label  = "Graphic: Todd Warczak \nSource: Mario Kart World Records",
           family = "Roboto Thin",
           size   = 8,
           color  = "white",
           vjust  = 14,
           hjust  = 0.65) +
  # title & description in center of plot
  geom_textbox(aes(x     = id,
                   y     = record_days,
                   label = label),
               inherit.aes = FALSE,
               width       = unit(9.1, "inch"),
               hjust       = -0.02,
               vjust       = 0.26,
               fill        = NA,
               box.colour  = NA,
               data        = tibble(id          = 1,
                                    record_days = 500,
    label = "<p style='font-family:SuperMario;font-size:110pt;color:#ff0000'>
                Mario Kart 64</p><br>
             <p style='font-family:SuperMario;font-size:90pt;color:black'> . <
                span style='color:#4db000'>World Records</span></p><br>
             <p style='font-family:Roboto;font-size:18pt;color:white'> Line =
                world record held by player, sorted by track, purple: Banshee
                Boardwalk to red: Yoshi Valley. Line length = cumulative days
                record held by that player. Records held for 8+ total years are
                given shells. Single-shell = single-lap record, triple-shell =
                3-lap record. Red shells = shortcut used, green shells = no shortcut.</p>"
               )) +
  # Info about longest line (Lacey)
  geom_textbox(aes(x     = id,
                   y     = record_days,
                   label = label),
               inherit.aes = FALSE,
               width      = unit(4.6, 'inch'),
               hjust       = 0.1,
               vjust       = 1.3,
               fill        = NA,
               box.colour  = NA,
               data        = tibble(id          = 100,
                                    record_days = 5700,
    label       = "<p style='font-family:Roboto;font-size:16pt;color:white'>
                      - 'Lacey' held the 3-lap, no shortcut D.K.'s Jungle Parkway race
                        record for 5,531 cumulative days or 15.15 years.<br><br>
                      -  First claimed record 2001-08-21 with a 2min 16.78sec finish.
                        Last reclaimed record 2018-02-10 with a 2min 12.35sec finish,
                        but lost record to 'Dan' 35 days later.</p>"
               )))
ggsave("rainbow_road.png", width = 25, height = 22.3, dpi = 600)
