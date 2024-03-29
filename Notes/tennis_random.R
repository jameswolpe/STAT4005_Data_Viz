library(tidyverse)
atp_df <- read_csv("data/atp_matches_2019.csv") %>%
  mutate(tour = "ATP")
#> Rows: 2781 Columns: 49
#> ── Column specification ────────────────────────────────────
#> Delimiter: ","
#> chr (16): tourney_id, tourney_name, surface, tourney_lev...
#> dbl (33): draw_size, tourney_date, match_num, winner_id,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
wta_df <- read_csv("data/wta_matches_2019.csv") %>%
  mutate(tour = "WTA")
#> Rows: 3114 Columns: 49
#> ── Column specification ────────────────────────────────────
#> Delimiter: ","
#> chr (16): tourney_id, tourney_name, surface, tourney_lev...
#> dbl (33): draw_size, tourney_date, match_num, winner_id,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
both_df <- bind_rows(atp_df, wta_df)

both_long <- both_df %>% pivot_longer(c(winner_name, loser_name))

## only keep players who have player over 50 matches
both_n50 <- both_long %>% group_by(value) %>% count() %>%
  filter(n > 50)

## construct various statistics
major_tennis <- semi_join(both_long, both_n50, by = c("value"))
major_tennis <- major_tennis %>% mutate(w_svperc = 100 * w_1stIn / w_svpt,
                                        l_svperc = 100 * l_1stIn / l_svpt,
                                        w_firstwon = 100 * w_1stWon / w_1stIn,
                                        l_firstwon = 100 * l_1stWon / l_1stIn,
                                        w_secondwon = 100 * w_2ndWon / (w_svpt - w_1stIn),
                                        l_secondwon = 100 * l_2ndWon / (l_svpt - l_1stIn))

major_tennis_w <- major_tennis %>% filter(name == "winner_name")
major_tennis_l <- major_tennis %>% filter(name == "loser_name")

w_small <- major_tennis_w %>% select(tourney_name, surface, value, winner_seed, w_ace, w_df, w_svperc,
                                     w_firstwon, w_secondwon, tour) %>%
  rename(seed = winner_seed, ace = w_ace, df = w_df, svperc = w_svperc,
         firstwon = w_firstwon, secondwon = w_secondwon) %>%
  mutate(result = "win")

l_small <- major_tennis_l %>% select(tourney_name, surface, value, loser_seed, l_ace, l_df, l_svperc, l_firstwon, l_secondwon, tour)  %>%
  rename(seed = loser_seed, ace = l_ace, df = l_df, svperc = l_svperc,
         firstwon = l_firstwon, secondwon = l_secondwon) %>%
  mutate(result = "loss")

df <- bind_rows(w_small, l_small) %>%
  rename(player = "value")

df_wta <- df %>% filter(tour == "WTA")

