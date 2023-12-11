library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr) 
library(stringr)
library(plotly)
library(rsconnect)


df <- read.csv("merged_anime_data.csv")

df <- df %>%
  mutate(genre_list = lapply(strsplit(gsub("\\[|\\]", "", genre.x), ",\\s*"), as.character)) %>%
  select(-genre.x) %>%  # 
  unnest(genre_list) %>%
  rename(genre = genre_list) 


ui <- navbarPage(
  "Anime Data Storytelling",
  tabPanel("Introduction",
           fluidPage(
             titlePanel("Welcome to the Anime Data Storytelling App!"),
             h1("Project Introduction"),
             div(
               p("Enter our project - Exploring Anime Preferences and Trends - a journey deep into anime data. Our goal is to reveal how anime influences viewer preferences, trends, and patterns, as well as what elements make certain works phenomenal presences across diverse storytelling genres. From genre analysis to rating trends, we will explore the multidimensional impact of anime."),
               p("Anime, a cultural phenomenon originating in Japan, has permeated the global entertainment scene. Many phenomenal anime have captivated audiences around the world with their vibrant colors, intricate storylines and unique characters.  They affect audiences in different ways-mapping political science, exploring absolute and relative freedom, and visualizing youth literature.An anime that discusses camping can make young people love going out and making friends and camping in the wilderness. They would enjoy the winding rivers, steep cliffs and charming flowers like lilies and daisies. In the rivers, they could go swimming, fishing and boating. Then, they would hike through the deciduous, coniferous, and the tundra regions; an anime about a band plants a musical dream in people's minds; an anime about relationship might teach people how to get along well with others, how to understand and respect each other, how to focus on the things that we have in common and tolerate the differences, how to think in others’ perspectives; an anime set in reality can cause tourism to flourish and promote the commercialization of an entire region, etc.But what determines the success of some anime works? Is it the genre, the narrative of the unique art style? What exactly is the kernel of phenomenal anime that influences society and people? With platforms like MyAnimeList, we now have a treasure trove of data to delve into these questions."),
               p("Art comes from life and is above life. Imagine the world we live in, whether there are things that have happened to you that make you feel a lot of emotions, whether there are some things that make you feel the same way when you watch them, influencing your behavior, hobbies and even the trajectory of your life. Then anime may be the way to visualize and universalize these stories of love, adventure, despair and hope, sublimating them and touching every viewer. This is the world of anime, a Japanese cultural marvel that has touched millions of people around the globe. But what makes an anime stand out from the crowd in a sea of stories? Our project, Exploring Anime Preferences and Trends, seeks to answer this question. By delving into MyAnimeList's data, we aim to uncover patterns, preferences, and trends in the world of anime. It's not just a story about anime, it's a narrative about culture, art, and the language of global storytelling, a way to demystify the magic behind the global appeal of phenomenal anime."),
               p("Based on the above, our data analysis will focus on the analysis of manga titles and ratings. Specifically, for all the comics we introduced, we categorized them by topic. Then, we define those with an average user rating of 6 or more as high rating titles, and vice versa for low rating titles. We will explore how users' preferred titles differ under the two broad categories of highly rated titles and lowly rated titles. So we will get two types of analysis:"),
               p("The first is a genre analysis: using the dataset we will be able to determine if there is a most popular genre. Whether the user's genre preferences have changed or whether they have stayed within the general trend. Users are able to use this to confirm whether their preferences are popular or niche, and to find like-minded fellow anime fans."),
               p("Then there are also popularity indicators: using the database we will also determine if the surge in popularity of some of the anime titles on the market is related to genre factors?"),
               p("In short, the project aims to provide a comprehensive, data-driven narrative about trends, preferences and patterns in the world of anime. By utilizing a rich dataset, we hope to reveal the factors that drive anime's popularity around the world, providing insight into the connections between different factors of specific anime. In this way, we can identify users' own preference profiles and help them to better choose the right anime for them and find fellow users."),
               h2("Data introduction"),
               p("By categorizing and rating thousands of anime titles in MyAnimeList, we'll show which genres appeal to viewers the most and which to relatively few. Our analysis will not only focus on which genres are favored, but will also explore how viewer preferences change over time."),
               h2("Shiny app Introduction"),
               p("Our Shiny app is a multi-page interactive platform where each page is a chapter in the data narrative. We will increase user engagement and storytelling through dynamic charts and graphs, a sea of user cross-cloud flow art, and exploratory data visualization tools. Users will be able to experience first-hand the data and insights behind anime works through our visual storytelling:"),
               p("1. Page 1: Anime Genre Distribution ----
We'll show the distribution of high and low rated anime genres and provide interactivity through a sea of cross-cloud flowing art to give users insight into their favorite anime genres."),
               p("2. Page 2: Analysis of High-Rated and Low-Rated Anime ----
Analyzes the differences between highly rated and lowly rated anime and explains what these differences mean for viewer choice and market trends."),
               p("3. Page 3: Anime Production Trends ----
                Tracks historical trends in anime production, highlights the impact of some potential disasters on the industry, and predicts future developments.")
             ),
             imageOutput("introImage")
           )
  ),
  tabPanel("Story 1: Genre Analysis",
           fluidPage(
             titlePanel("Anime Genre Distribution in High and Low Score Categories"),
             mainPanel(
               uiOutput("statsText"),
               p("This plot provides insight into the distribution of anime genres across high and low scoring animes. Explore how different genres perform in terms of viewer scores and see if certain genres are more likely to be rated higher or lower."),
               p(HTML("<strong> Interact with the score category in plot below to see detailed graph for filtering</strong>."), align = "center"),
               plotlyOutput("genrePlotly"),  # Interactive Plotly charts
               htmlOutput("genreAnalysisText")  # Display statistics for selected types
             )
           )
  ),
  tabPanel("Story 2: High vs Low Analysis",
           fluidPage(
             titlePanel("High vs Low Score Category Distribution"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("scoreCategorySelect", "Select Score Category:", choices = c("High", "Low"))
               ),
               mainPanel(
                 plotOutput("scoreCategoryPlot"),
                 p("Select a score category to see how genres are distributed within that category."),
                 tableOutput("genreTable"),
                 htmlOutput("analysisText")
               )
             )
           )
  ),
  tabPanel("Story 3: Industry Trends",
           fluidPage(
             titlePanel("Anime Production Trends Over the Years"),
             mainPanel(
               plotOutput("trendPlot"),
               htmlOutput("trendNarrative"), 
               textOutput("trendStatsText")  # Placeholder for the statistical summary text
             )
           )
  ),
  tabPanel("About us",
           fluidPage(
             h1("About Us"),
             p("This project was brought to you by:"),
             p(strong("Gefei Shen")),
             p("AMCS-DSS major"),
             p(a("gefeis3@uw.edu", href = "mailto:gefeis3@uw.edu")),
             p(strong("Jiahao Xu")),
             p("Communication major"),
             p(a("xjh2003@uw.edu", href = "mailto:xjh2003@uw.edu")),
             p("Feel free to reach out to us with your questions or feedback regarding this app.")
           )
  )
)


server <- function(input, output) {
  # intro picture
  output$introImage <- renderImage({
    # Return a list containing the image path and other attributes like height and width
    list(
      src = "intro_picture.png",
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Introduction Image"
    )
  }, deleteFile = FALSE)
  
  # Story 1: Genre Analysis
  output$genrePlot <- renderPlot({
    df_genre <- df %>%
      count(genre, new_category) %>%
      group_by(genre) %>%
      mutate(total = sum(n)) %>%
      ungroup() 
    
    ggplot(df_genre, aes(x = reorder(genre, total), y = n, fill = new_category)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Genre", y = "Count", title = "Number of Animes by Genre and Score Category") +
      scale_fill_discrete(name = "Score Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$statsText <- renderText({
    stats <- df %>%
      group_by(new_category, genre) %>%
      summarise(
        Count = n(),
        Mean = mean(Count),
        Median = median(Count),
        SD = sd(Count),
        .groups = 'drop'
      )
    
    stats_high <- stats %>%
      filter(new_category == "High") %>%
      summarise(
        Max = max(Count), 
        Max_Genre = genre[which.max(Count)],
        Mean = mean(Count),
        Median = median(Count),
        SD = sd(Count)
      )
    
    stats_low <- stats %>%
      filter(new_category == "Low") %>%
      summarise(
        Min = min(Count), 
        Min_Genre = genre[which.min(Count)],
        Mean = mean(Count),
        Median = median(Count),
        SD = sd(Count)
      )
    
    HTML(paste(
      "Some statistics:<br>",
      "For the High category:<br>",
      "The maximum count is ", stats_high$Max, 
      " for the genre ", stats_high$Max_Genre, ".<br>",
      "The mean count is ", round(stats_high$Mean, 2), ".<br>",
      "The median count is ", stats_high$Median, ".<br>",
      "The standard deviation is ", round(stats_high$SD, 2), ".<br><br>",
      
      "For the Low category:<br>",
      "The minimum count is ", stats_low$Min, 
      " for the genre ", stats_low$Min_Genre, ".<br>",
      "The mean count is ", round(stats_low$Mean, 2), ".<br>",
      "The median count is ", stats_low$Median, ".<br>",
      "The standard deviation is ", round(stats_low$SD, 2), "."
    ))
  })
  

  
  # Create bar charts with Plotly
  output$genrePlotly <- renderPlotly({
    df_genre <- df %>%
      count(genre, new_category) %>%
      group_by(genre) %>%
      mutate(total = sum(n)) %>%
      ungroup()
    
    p <- ggplot(df_genre, aes(x = reorder(genre, total), y = n, fill = new_category)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Genre", y = "Count", title = "Number of Animes by Genre and Score Category") +
      scale_fill_discrete(name = "Score Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")  # Add text alerts for mouse hover
  })
  
  # Dynamically display statistics for selected types
  output$selectedGenreStats <- renderUI({
    genre_data <- event_data("plotly_click", source = "genrePlotly")
    
    if (!is.null(genre_data)) {
      genre_name <- genre_data$x
      genre_stats <- df %>%
        filter(genre == genre_name) %>%
        group_by(new_category) %>%
        summarise(
          Count = n(),
          Mean = mean(Count),
          Median = median(Count),
          SD = sd(Count),
          .groups = 'drop'
        )
      
      renderText({
        paste("Statistics for genre:", genre_name, "<br>",
              "High category - Count:", genre_stats$Count[genre_stats$new_category == "High"], "<br>",
              "Low category - Count:", genre_stats$Count[genre_stats$new_category == "Low"], "<br>")
      })
    } else {
      renderText({"Click on a bar to see detailed statistics for a genre."})
    }
  })
  
  output$genreAnalysisText <- renderUI({
    narrative <- HTML(paste(
      "<h3>Genre Analysis Narrative</h3>",
      "<p>On this page, we focus on the distribution of anime titles in comparison between high and low ratings. Our view visualizes the significant dominance of comedy titles among highly rated anime, with a high number of 4,065, revealing a strong audience preference for this type of light-hearted entertainment content. The popularity of such comedies reflects viewers' general quest for a joyful atmosphere.</p>",
      "<p>In contrast, however, the girl love genre appears very infrequently in low-rated anime, with only seven titles included, hinting at either its relatively narrow audience base or a particular pickiness for the genre. In addition, the unrated NA category provides further room for exploration, potentially revealing emerging anime works or those potential masterpieces that are not yet widely recognized.</p>",
      "<p>By analyzing in detail the average number of highly rated anime, 797.47, and the average number of lowly rated anime, 267.05, we can not only observe how certain titles win over viewers, but also get a sense of the fluctuations in the market's perception of the quality of different titles. These statistics - a median of 522 for high ratings and 143 for low ratings, along with the corresponding standard deviations - further reinforce the diversity of viewers' evaluations of anime from different titles.</p>",
      "<p>Taken together, our data views are more than just a stack of numbers; they tell stories about audience preferences, market trends, and content quality. Our interactive visualization tool invites users to explore these differences in depth, leading to deeper insights and understanding of the anime space.</p>"
    ))
    narrative
  })
  
  
  
  
  # Story 2: High vs Low Analysis
  output$scoreCategoryPlot <- renderPlot({
    req(input$scoreCategorySelect)  
    
    
    df_filtered <- df %>% 
      filter(new_category == input$scoreCategorySelect)
    
   
    df_genre_filtered <- df_filtered %>%
      count(genre) %>%
      arrange(desc(n))
    
   
    ggplot(df_genre_filtered, aes(x = reorder(genre, n), y = n)) +
      geom_bar(stat = "identity", fill = ifelse(input$scoreCategorySelect == "High", "red", "blue")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Genre", y = "Count", title = paste("Number of Animes in", input$scoreCategorySelect, "Score Category"))
  })
  
  output$genreTable <- renderTable({
    df_filtered <- df %>%    
      filter(new_category == input$scoreCategorySelect)
    
    df_genre_filtered <- df_filtered %>%
      count(genre) %>%
      arrange(desc(n))
    
    df_genre_filtered
  })
  
   output$analysisText <- renderUI({
    narrative <- HTML(paste(
      "<h3>Score Analysis Narrative</h3>",
      "<p>On this page, we take a closer look at high and low rated anime titles. Using MyAnimeList's extensive database, we reveal how popular titles like comedy and action take the lead in highly rated anime, while at the same time, music and children's titles appear more frequently in low-rated anime. This contrast not only highlights the popularity of different titles, but also hints at the diversity of audience expectations and different responses to story quality.</p>",
      "<p>In particular, comedy, a universally popular subject, far outnumbers low-rated anime, suggesting that when comedy is handled properly, it can greatly satisfy viewers' entertainment needs. For low-rated anime, on the other hand, the abundance of comedic themes may suggest an inconsistency in the quality of work in this category. The presence of action and fantasy themes in the low-scoring section, though they perform well in the high-scoring anime, suggests that even the most popular themes can fail to live up to viewers' expectations at times.</p>",
      "<p>Overall, the analysis on this page emphasizes the multifactorial nature of anime success and how viewers give ratings based on personal preferences and the quality of execution of the subject matter. Our interactive exploration tool further allows users to gain insight into their favorite titles based on the distribution of high and low scores.</p>"
    ))
    narrative
  })
  
  
  # Story 3: Industry Trends 
  anime_count_by_year <- reactive({
    req(df) 
    
    df %>%
      filter(year <= 2021) %>%  # Exclude years after 2021
      group_by(year) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(year)
  })
  
  # Plot using the reactive expression
  output$trendPlot <- renderPlot({
    df_trend <- anime_count_by_year() # Execute the reactive expression
    
    # Define the range for the y-axis label positioning
    y_label_position <- max(df_trend$count)
    y_label_position_low <- min(df_trend$count[df_trend$year >= 2020])
    
    ggplot(df_trend, aes(x = year, y = count)) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = 2020, linetype = "dashed", color = "red") + # Add a vertical line for 2020
      geom_text(aes(x = 2020, y = y_label_position, label = "2020"), vjust = -1) +
      geom_text(aes(x = 2022, y = y_label_position_low, label = "Decline due to incomplete data or COVID-19 impact"), angle = 90, vjust = 1) +
      labs(title = "Anime Production Trends Up to 2021",
           x = "Year",
           y = "Number of Animes Produced",
           caption = "Over the past century, the anime industry has seen a steady increase in production. The recent downturn in the last two years is likely due to incomplete data or the economic impact of the COVID-19 pandemic. As the situation improves, we might expect a resurgence in production.") +
      theme_minimal()
  })
  
  output$trendNarrative <- renderText({
    HTML(paste(
      "<h3>Trend Analysis Narrative</h3>",
      "<p>Over the past century, the anime industry has experienced a significant rise in production, showcasing the growing popularity and global reach of this form of entertainment..</p>",
      "<p>Also we can find out this graph ends at 2021, and while it shows a significant drop, it's important to note that the industry is already showing signs of recovery as of 2022, which could predict a positive trend in the coming years.</p>",
      "<p>But with the emergence of Covid-19, economic downturn. many boutique and niche anime companies have gone out of business or reduced funding for anime production due to lack of financial backing, resulting in the reduction of some titles or the delayed appearance of some anime sequels. The industry can also be hit by potential man-made or natural disasters that strike production companies, such as the famous Kyoto arson case of '19 (which led to the burning of KyoAni's work and had to stop production of many anime), as well as the deaths of some manga artists will also slow down the growth of the industry.</p>",
      "<p>With the pandemic receding and the recovery of the companies, there is potential for a rebound, as production companies adapt to new ways of working and audiences continue to crave new content.</p>",
      "<p>Although the drop in numbers might suggest a period of stagnation, it is likely to be temporary. As the world recovers, we may see the industry not only return to its previous trajectory but perhaps even surpass it, driven by innovation and an ever-increasing fanbase.</p>"
    ))
  })
}


shinyApp(ui = ui, server = server)
