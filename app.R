library(ggplot2)
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(tidyverse)
library(tigris)
theme_set(theme_classic())
options(tigris_use_cache = TRUE)


# Functions
get_sentiment_info <- function(company) {
  if(is.na({{company}}))
    return()
  
  aggregated_sentiment_company <- read_csv(paste0('data/aggregated/aggregated_sentiment_',{{company}},'.csv')) %>% 
    select(-c(...1))
  
  n_reviews <- nrow(aggregated_sentiment_company)
  p_pos <- round((sum(aggregated_sentiment_company$final_sentiment == 'pos') / n_reviews)*100, 2)
  p_neg <- round(100-p_pos, 2)
  return(list(n_reviews=n_reviews,p_pos=paste0(p_pos,'%'),p_neg=paste0(p_neg,'%')))
}
get_ratings_info <- function(company) {
  filtered_data <- read_csv('data/aggregated/aggregated_mean_ratings.csv') %>% 
    select(-c(...1))  %>% 
    filter(company==tolower({{company}}))
  
  n_ratings <- filtered_data$n_ratings
  
  company_name <- switch (
    {{company}},
    'airbnb' = 'Airbnb',
    'booking' = 'Booking.com',
    'tripadvisor' = 'Tripadvisor'
  )
  
  plot_data <- filtered_data %>% 
    pivot_longer(cols = -c(company,n_ratings), names_to = "category", values_to = "rating")
  
  ratings_plot <- plot_data %>% 
    ggplot(aes(x = category, y = rating-2.5, fill = rating-2.5>=0)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0), width=0.6) +
    geom_text(
      aes(label = rating), 
      position = position_dodge(width = 0.6), 
      hjust = ifelse(plot_data$rating-2.5 < 0, 1, -0.1)
      ) +
    labs(title = paste('Categorical ratings of', company_name), x = "", y = "") +
    scale_x_discrete(
      labels = NULL,
      limits = c("career_opportunities_rating", "compensation_and_benefits_rating", 
                 "senior_management_rating", "work_life_balance_rating", 
                 "culture_and_values_rating", "rating")
    ) +    
    scale_y_continuous(labels = NULL) +
    scale_fill_manual(values = c('FALSE'='darkslategrey', 'TRUE'='slateblue')) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    coord_flip() +
    annotate("text", x = 0.2, y = -2.5, label = "0") +
    annotate("text", x = 0.2, y = 2.5, label = "5") +
    annotate("text", x = -0.1, y = 2.5, label = "") +
    theme(
      plot.title = element_text(hjust=0.5),
      axis.ticks = element_blank(), axis.line.y = element_blank(), 
      # plot.margin = unit(c(1, 1, 1.5, 1.5), "cm"),
      legend.position = 'none'
      )
    expand_limits(y = c(-3, 4)) +  # Expand the y-axis limits
  return(list(n_ratings=n_ratings, ratings_plot=ratings_plot))
}
get_wordcloud_summary <- function(company) {
  summary <- read_csv(paste0('data/aggregated/wordcloud_summaries.csv')) %>%
    filter(company=={{company}}) %>% 
    pull(summary)
  return(summary)
}

# Dashboard UI Components
dashboard_body <- function() {
  fluidPage(
    fluidRow(
      selectizeInput(
        'companies', 
        'Choose up to two companies from the dropdown:', 
        c('Airbnb'='airbnb','Booking.com'='booking','Tripadvisor'='tripadvisor'), 
        options=list(placeholder = 'Choose here', maxItems = 2), 
        multiple=TRUE, 
        width = '30%'
      )
    ), 
    tags$br(),
    tags$br(),
    fluidRow(
      column(6,
          fluidRow(h3(class='centered', textOutput('name_companyA'))), 
          fluidRow( class='raised-rectangle slategrey-bg',
            column(3, class='raised-rectangle slateblue-bg',
                   tags$div(class='centered', tags$h4('No. of ratings')),
                   tags$div(class='centered', tags$h4(textOutput('n_ratings_A')))
            ),
            column(3, class='raised-rectangle slateblue-bg',
                   tags$div(class='centered', tags$h4('Text reviews')),
                   tags$div(class='centered', tags$h4(textOutput('n_reviews_A')))
            ),
            column(3, class='raised-rectangle slateblue-bg',
                   tags$div(class='centered', tags$h4('Positive reviews')),
                   tags$div(class='centered', tags$h4(textOutput('p_pos_A')))
            ),
            column(3, class='raised-rectangle slateblue-bg',
                   tags$div(class='centered', tags$h4('Negative reviews')),
                   tags$div(class='centered', tags$h4(textOutput('p_neg_A')))
            )
          )
      ), 
      column(6, 
          fluidRow(h3(class='centered', textOutput('name_companyB'))), 
          fluidRow( class='raised-rectangle slategrey-bg',
            column(3, class='raised-rectangle slateblue-bg',
                   tags$div(class='centered', tags$h4('No. of ratings')),
                   tags$div(class='centered', tags$h4(textOutput('n_ratings_B')))
            ),
            column(3, class='raised-rectangle slateblue-bg',
                   tags$div(class='centered', tags$h4('Text reviews')),
                   tags$div(class='centered', tags$h4(textOutput('n_reviews_B')))
            ),
            column(3, class='raised-rectangle slateblue-bg',
                   tags$div(class='centered', tags$h4('Positive reviews')),
                   tags$div(class='centered', tags$h4(textOutput('p_pos_B')))
            ),
            column(3, class='raised-rectangle slateblue-bg',
                   tags$div(class='centered', tags$h4('Negative reviews')),
                   tags$div(class='centered', tags$h4(textOutput('p_neg_B')))
            )
          )
      ), 
    ), 
    tags$hr(),
    fluidRow(
      tags$div(
        style = "text-align: center;",
        tags$h1('How do these companies compare in various categories?')
      ),
      tags$br()
    ), 
    fluidRow(
      column(5, 
             # tags$p(class='center-padded-p',textOutput('companyA')),
             plotOutput('companyA_category_ratings_plot')
      ), 
      column(2, 
             tags$p(class='centered', strong('Company Categories')),
             tags$p(class='center-padded-p','Overall Rating'),
             tags$p(class='center-padded-p','Job Culture'),
             tags$p(class='center-padded-p','Work-Life Balance'),
             tags$p(class='center-padded-p','Senior Management'),
             tags$p(class='center-padded-p','Compensation and Benefits'),
             tags$p(class='center-padded-p','Career Opportunities'),
      ),
      column(3, 
             # tags$p(class='center-padded-p',textOutput('companyB')),
             plotOutput('companyB_category_ratings_plot'))
    ), 
    fluidRow(
      tags$div(
        style = "text-align: center;",
        tags$h1('What do employees talk about mostly in their reviews?')
      ),
      tags$br()
    ), 
    fluidRow( 
      column(5, 
             div(class='justified-wide large', textOutput('summary_companyA')),
             tags$br(),
             tags$div(style="border-top: 1px solid black; width: 250%;"),
             tags$br(),
             imageOutput('companyA_titles_wordcloud'),
             tags$div(style="border-top: 1px solid black; width: 250%;"),
             tags$br(),
             tags$br(),
             imageOutput('companyA_text_wordcloud')
             ),
      column(1,tags$div(style="border-right: 1px solid black; height: 950px;")),
      column(5, 
             div(class='justified-wide large', textOutput('summary_companyB')),
             tags$br(),
             tags$br(),
             imageOutput('companyB_titles_wordcloud'),
             tags$br(), tags$br(),
             imageOutput('companyB_text_wordcloud')
             )
    )
  )
}

ui <- dashboardPage(
  dashboardHeader(title = 'Company Competitor Analysis', titleWidth = 'calc(100%)'),
  dashboardSidebar(
    width = 'calc(0%)',
    sidebarMenu(
      id = 'tabs',
      menuItem('Dashboard' , tabName = 'dashboard')
    )
  ),
  dashboardBody(
    includeCSS('styles/app.css'),
    tabItems(tabItem(tabName = 'dashboard',  dashboard_body()))
  ),
  skin = 'purple'
)

server <- function(input, output, session) {
  companies <- reactive({input$companies})
  
  observeEvent(companies(), {
    companyA <- companies()[1]
    companyB <- companies()[2]
    
    ### Company-A results
    ## Fetch Company-A output information from functions
    ratings_info_companyA <- get_ratings_info(companyA)
    sentiment_info_companyA <- get_sentiment_info(companyA)
    summary_companyA <- get_wordcloud_summary(companyA)
    
    ## Generate UI components for Company-A
    # Review Statistics
    output$name_companyA <- renderText(paste(str_to_title(companyA),"Stats"))
    output$n_ratings_A <- renderText(ratings_info_companyA$n_ratings)
    output$n_reviews_A <- renderText(sentiment_info_companyA$n_reviews)
    output$p_pos_A <- renderText(sentiment_info_companyA$p_pos)
    output$p_neg_A <- renderText(sentiment_info_companyA$p_neg)
    # Ratings plot
    output$companyA_category_ratings_plot <- renderPlot(ratings_info_companyA$ratings_plot, width=525, height=300)
    # Word clouds Summary
    output$summary_companyA <- renderText(summary_companyA)
    # Word cloud - Review titles
    output$companyA_titles_wordcloud <- renderImage({
      list(src = paste0('www/wordcloud_titles_',companyA,'.png'),
           width = '120%',
           contentType = 'image/png')
    }, deleteFile = FALSE)
    # Word cloud - Review text
    output$companyA_text_wordcloud <- renderImage({
      list(src = paste0('www/wordcloud_text_',companyA,'.png'),
           width = '120%',
           contentType = 'image/png')
    }, deleteFile = FALSE)
    
    ### Company-B results
    ## Fetch Company-B output information from functions
    ratings_info_companyB <- get_ratings_info(companyB)
    sentiment_info_companyB <- get_sentiment_info(companyB)
    summary_companyB <- get_wordcloud_summary(companyB)
    
    ## Generate UI components for Company-B
    # Review Statistics
    output$name_companyB <- renderText(paste(str_to_title(companyB),"Stats"))
    output$n_ratings_B <- renderText(ratings_info_companyB$n_ratings)
    output$n_reviews_B <- renderText(sentiment_info_companyB$n_reviews)
    output$p_pos_B <- renderText(sentiment_info_companyB$p_pos)
    output$p_neg_B <- renderText(sentiment_info_companyB$p_neg)
    # Ratings plot
    output$companyB_category_ratings_plot <- renderPlot(ratings_info_companyB$ratings_plot, width=525, height=300)
    # Word clouds Summary
    output$summary_companyB <- renderText(summary_companyB)
    # Word cloud - Review titles
    output$companyB_titles_wordcloud <- renderImage({
      list(src = paste0('www/wordcloud_titles_',companyB,'.png'),
           width = '120%',
           contentType = 'image/png')
    }, deleteFile = FALSE)
    # Word cloud - Review text
    output$companyB_text_wordcloud <- renderImage({
      list(src = paste0('www/wordcloud_text_',companyB,'.png'),
           width = '120%',
           contentType = 'image/png')
    }, deleteFile = FALSE)
  })
}

shinyApp(ui, server)