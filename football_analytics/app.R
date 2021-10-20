# Next Step :
#1. Change Image Size
#2. Give more explanation on UI
#3. Make code shorter!
#4. Make sure there is no copyright violated
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(worldfootballR)
library(dplyr)
library(ggradar)
library(fmsb)

options(shiny.maxRequestSize = 200*1024^2)
options("digits" = 2) #Max 2 Decimal Digits

# Function to Get Player Statistics
# You can adjust this if you need another metrics
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


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                navbarPage(
                    title = "Self Service Football Analytics",
                    h5("Source : fbref.com"),
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                width = 4,
                tags$head(
                    tags$style(
                        type = "text/css",
                        "
        #loadmessage {
        position: bottom;
        top: 0px;
        left: 0px;
        width: 100%;
        padding: 5px 0px 5px 0px;
        text-align: center;
        font-weight: bold;
        font-size: 100%;
        color: #ffffff;
        background-color: #d9230f;
        z-index: 105;
        }
        "
                    )
                ),
                            
                textInput("player_link_1", "Player 1 Link (Get from fbref.com/en/players)", "https://fbref.com/en/players/f25c8e3a/Naby-Keita"),
                textInput("player_link_2", "Player 2 Link (Get from fbref.com/en/players)", "https://fbref.com/en/players/77e84962/Thiago-Alcantara"),
                textInput("season_1", "Season of Player 1", "2021-2022"),
                textInput("season_2", "Season of Player 2", "2021-2022"),
                p("Click Run to Process"),
                p("The Process will take ~20 seconds"),
                actionButton("do", "Run"),
                br(),
                br(),
                
                h6("Powered by:"),
                tags$img(
                    src = 'worldfootballr.png',
                    height = 125,
                    width = 123
                )
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tab",
                        tabPanel("Table Summary", h4('All Metrics per 90 Minutes'), dataTableOutput(outputId = "player_stats_summary")),
                        tabPanel("Chart Summary", plotOutput("radar_chart", click = "plot_click")),
                        tabPanel("Metrics Definition", h4('All Metrics per 90 Minutes'), dataTableOutput(outputId = "definition"))
                )
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    summary_stats_1 <- eventReactive(input$do,{
        player_url = input$player_link_1
        season = input$season_1
        player_stats(
            player_url, season
        )
    })
    
    summary_stats_2 <- eventReactive(input$do,{
        player_url = input$player_link_2
        season = input$season_2
        player_stats(
            player_url, season
        )
    })
    
    player_comparison <- reactive({
        rbind(summary_stats_1(), summary_stats_2()) %>% 
            select(player_name, npGoal, xA, Dribble, npxG, key_pass, shoot, 
                   pen_touch, xG_xA
                   ) %>% 
            rename(
                `Player Name` = player_name,
                `Non-Penalty Goal` = npGoal,
                xA = xA,
                Dribble = Dribble,
                `Non-Penalty xG` = npxG,
                `Key Pass` = key_pass,
                Shoot = shoot,
                `Touch in Pen Box` = pen_touch,
                `xG + xA` = xG_xA
            )
    })
    
    player_comparison_chart <- reactive({
        rbind(summary_stats_1(), summary_stats_2()) %>% 
            select(npGoal, xA, Dribble, npxG, key_pass, shoot, 
                   pen_touch, xG_xA
                   ) %>% 
            rename(
                `Non-Penalty Goal` = npGoal,
                xA = xA,
                Dribble = Dribble,
                `Non-Penalty xG` = npxG,
                `Key Pass` = key_pass,
                Shoot = shoot,
                `Touch in Pen Box` = pen_touch,
                `xG + xA` = xG_xA
                )
    })
    
    areas <- c(rgb(1, 0, 0, 0.25),
               rgb(0, 1, 0, 0.25),
               rgb(0, 0, 1, 0.25),
               rgb(1, 1, 0, 0.35))
    
    definition_table <- data.frame(Metrics = c('npGoal', 'xA', 'npxG', 'Key Pass', 'Shoot', 'Pen Touch', 'xG + xA'),
                                   Definition = c(
                                       'Non-Penalty Goal that Player Scored',
                                       'Measure the likelihood that a given pass will become an assist. So if xA = 0.5, that means on average the player will have 1 assist every 2 matches',
                                       'Measures the quality of a shot (exclude Penalty) based on several variables such as assist type, shot angle and distance from goal, whether it was a headed shot and whether it was defined as a big chance. So if npxG = 0.5, that means on average the player will have 1 goal (exclude penalty) every 2 matches',
                                       'The final pass or pass-cum-shot leading to the recipient of the ball having an attempt at goal without scoring.',
                                       '#Shoot the player produce',
                                       '#Touches the player have in opposition penalty box',
                                       'Expected Goals (exclude Pen) + Expected Assists'
                                   )
    )

    output$player_stats_summary <- renderDataTable({player_comparison()})
    output$radar_chart <- renderPlot({
        # Create Radar Chart for Comparing Players
        radarchart(player_comparison_chart(),
                   title = 'Compare Player Using Radar Chart',
                   maxmin = FALSE,
                   cglty = 1,       # Grid line type
                   cglcol = "gray", # Grid line color
                   pcol = 2:4,      # Color for each line
                   plwd = 2,        # Width for each line
                   plty = 1,        # Line type for each line
                   pfcol = areas)
        legend("topright",
               legend = c(summary_stats_1()$player_name, summary_stats_2()$player_name),
               bty = "n", pch = 20, col = areas,
               text.col = "grey25", pt.cex = 2)
    })
    
    output$definition <- renderDataTable({definition_table})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
