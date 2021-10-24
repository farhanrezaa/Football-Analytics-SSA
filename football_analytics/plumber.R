library(plumber)
library(tidyverse)
library(worldfootballR)

# Function to Get Player Statistics
player_stats = function(player_url, season){
  passing_stats = fb_player_season_stats(player_url, stat_type = 'passing') %>% 
    select(player_name, Season, Comp, Att_Total, Cmp_Total, Att_Long, Cmp_Long,
           Final_Third, KP, Prog)
  
  standard_stats = fb_player_season_stats(player_url, stat_type = 'standard') %>% 
    mutate(goals_plus_assist = G_minus_PK + Ast,
           xG_plus_xA = xG_Expected + xA_Expected) %>% 
    select(player_name, Season, Comp, Min_Time, G_minus_PK, Ast, goals_plus_assist, xG_plus_xA,
           xG_Expected, xA_Expected, npxG_Expected)
  
  shooting_stats = fb_player_season_stats(player_url, stat_type = 'shooting') %>% 
    select(player_name, Season, Comp, Sh_Standard, SoT_Standard)
  
  defense_stats = fb_player_season_stats(player_url, stat_type = 'defense') 
  names(defense_stats)[13] = 'att_3rd_tackle'
  names(defense_stats)[22] = 'mid_3rd_pressure'
  names(defense_stats)[23] = 'att_3rd_pressure'
  defense_stats = defense_stats %>%  
    select(player_name, Season, Comp, Tkl_Tackles, TklW_Tackles, Press_Pressures, Succ_Pressures,
           att_3rd_tackle, mid_3rd_pressure, att_3rd_pressure, Int, Clr)
  
  possession_stats = fb_player_season_stats(player_url, stat_type = 'possession')
  names(possession_stats)[13] = 'att_3rd_touches'
  names(possession_stats)[14] = 'att_pen_touches'
  possession_stats = possession_stats %>% 
    select(player_name, Season, Comp, att_3rd_touches, att_pen_touches, Att_Dribbles,
           Succ_Dribbles)
  
  misc_stats = fb_player_season_stats(player_url, stat_type = 'misc') %>%  
    select(player_name, Season, Comp, Recov, Won_Aerial_Duels, Lost_Aerial_Duels)
  
  
  player_summary_stats = full_join(passing_stats, standard_stats, 
                                   by = c('player_name', 'Season', 'Comp')) %>% 
    full_join(shooting_stats, by = c('player_name', 'Season', 'Comp')) %>% 
    full_join(defense_stats, by = c('player_name', 'Season', 'Comp')) %>% 
    full_join(possession_stats, by = c('player_name', 'Season', 'Comp')) %>% 
    full_join(misc_stats, by = c('player_name', 'Season', 'Comp'))
  
  # Column Order
  col_order = c('player_name', 'Season', 'Comp', 'Min_Time', 'Att_Total', 'Cmp_Total',
                'Att_Long', 'Cmp_Long', 'Att_Dribbles', 'Succ_Dribbles', 'xA_Expected', 'Ast',
                'xG_Expected', 'npxG_Expected', 'G_minus_PK', 'xG_plus_xA', 'goals_plus_assist', 
                'KP', 'Prog', 'Sh_Standard', 'SoT_Standard', 'Tkl_Tackles', 'TklW_Tackles', 
                'Press_Pressures', 'Succ_Pressures', 'att_3rd_tackle', 'mid_3rd_pressure',
                'att_3rd_pressure', 'Int', 'Clr', 'att_3rd_touches', 'att_pen_touches', 
                'Recov', 'Won_Aerial_Duels', 'Lost_Aerial_Duels')
  player_summary_stats = player_summary_stats[, col_order] %>% arrange(Season, Comp)
  
  player_summary_stats = player_summary_stats %>% 
    filter(Season == season) %>% 
    select(player_name, Comp, Min_Time, 
           xA_Expected, Att_Dribbles, npxG_Expected, G_minus_PK, Ast,
           KP, Sh_Standard, att_pen_touches, xG_plus_xA) %>% 
    group_by(player_name) %>% 
    summarise(total_minute = sum(Min_Time, na.rm = TRUE),
              npGoal = sum(G_minus_PK, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
              xA = sum(xA_Expected, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
              Dribble = sum(Att_Dribbles, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
              npxG = sum(npxG_Expected, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
              key_pass = sum(KP, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
              shoot = sum(Sh_Standard, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
              pen_touch = sum(att_pen_touches, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
              xG_xA = sum(xG_plus_xA, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE))
}


#* @apiTitle Football Analytics API
#* @apiDescription API for getting Player Statistics like xG, xA, Key Pass, etc

#* Return Player Statistics
#* @param player_link
#* @param season
#* @get /stats
function(player_link, season){
  player_stats(player_link, season)
}

#* Return Player Statistics
#* @param player_link
#* @param season
#* @post /stats
function(player_link, season){
  player_stats(player_link, season)
}
