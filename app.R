#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(shinyWidgets)
source("support/source.R")


# Define UI for application that draws a histogram

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "NET. DASHBOARD",
    tags$li(a(href = 'https://www.linkedin.com/in/hendra-respati-94105036/',
              icon("linkedin"),
              title = "About the Author"),
            class = "dropdown"),
    
    tags$li(a(href = 'http://www.netmedia.co.id/',
              img(src = "logo1.png",
                  title = "Official Website", height = "20px"),
              style = "padding-top:14px; padding-bottom:10px;"),
            class="dropdown"),
   
    dropdownMenu(type = "message",
              messageItem(
              from = "NET. Dashboard",
              message = "Your subscription updated",
              icon = icon("life-ring"),
              time = substr(Sys.time(),1,10)
     )        
    )                         
  ),
  
  dashboardSidebar(
    hr(),
    sidebarMenu(
      id="menu",
      menuItem("Overview", tabName = "overview", icon=icon("line-chart")),
      menuItem("Revenue Analysis", tabName = "revenue", icon=icon("youtube")),
      menuItem("Content Analysis", tabName = "content", icon=icon("youtube"),
               badgeLabel = "new", badgeColor = "red"),
      menuItem("Github Links", 
               icon=icon("github"), href="https://github.com/hendrarespati")
    ),
    conditionalPanel(condition = 'input.menu == "overview"',
                     setSliderColor("#FFD700",1), #change slider color
                     sliderInput("date_slider_overview",
                                 "Analysis Period",
                                 min= as.Date('2018-01-01'),
                                 max= as.Date('2018-03-01'),
                                 value=c(as.Date('2018-01-25'),
                                         as.Date('2018-02-15'))
                                 
                      
                     ) #sliderInput end
                     
    ) #conditionalPanel end 
                    
  ),   #dashboardsidebar end
  
  dashboardBody(
    tags$head(
      tags$link(rel= "stylesheet", type = "text/css", href = "custom.css") #change layout dashboard with css
    ),
    tabItems(
      # Overview page
      tabItem("overview",
              fluidRow(
                infoBoxOutput("YoutubeRev"),
                infoBoxOutput("YoutubeView"),
                infoBoxOutput("RedYoutubeRev"),
                infoBoxOutput("RedYoutubeView")
              ),
              fluidRow(
                tabBox(title = "Revenue/View",
                       #selected = 'Total Revenue',
                       #collapsible = T,
                       width=12,
                       tabPanel("Total Revenue",
                                plotlyOutput("total_plot",height=500)
                                #tabPanel end
                       ),
                       tabPanel("Youtube Red Revenue",
                                plotlyOutput("red_plot",height=500)
                               
                       ))  #tabPanel end
                
                #fluidRow end
                ),
              fluidRow(
                box(title="Youtube", 
                    collapsible = T,
                    width=4, solidHeader = T,
                    dataTableOutput("totaltable")
                ),
                box(title="Youtube Red", 
                    collapsible = T,
                    width=4, solidHeader = T,
                    dataTableOutput("redtable")
                )
                
              ) #fluidRow end
             
      ), #tabitem end
      tabItem("revenue",
              fluidRow(
                tabBox(title = "Youtube Revenue",
                       width=12,
                       tabPanel("Top 10 NET. Revenue by Country",
                                title="Summary", solidHeader=T,
                                collapsible = T,
                                width=1,
                                
                                plotlyOutput("map_rev", height = 700),
                                fluidRow(
                                  box(
                                    width = 2,
                                    dataTableOutput("regiontable")
                                  ),
                                  box(
                                    title = "Top 30 Channel by Revenue", solidHeader = T,
                                    collapsible = T,
                                    width = 5,
                                    plotlyOutput("top30_channel")
                                  ),
                                  box(
                                    title = "Top 30 Video Asset by Revenue", solidheader = T,
                                    collapsible = T,
                                    width = 5,
                                    plotlyOutput("top30_asset")
                                  )
                                )),

                       
                       
                       tabPanel("Revenue vs CPM",
                       
                                title = " Views vs Estimated Monetized Playback", solidHeader =T,
                                collapsible = T,
                                plotlyOutput("comptable"),
                                fluidRow(
                                  box(
                                    title="Analysis",
                                    solidHeader = T,
                                    width=8,
                                    p("The estimated monetized playbacks refers to the number how frequently the monetized videos have been played. If we compare it to number of views, the gap betweeen them relatively far. One way to increase monetized playbacks is to tell the audience at the very beginning of video about an offer or discount that they can get somewhere later in the video. The video content must be great so we can hold attention of viewers and make them watch video until the end.")
                                  )
                                )
                                
                                )
                                
                )
              )
       ),
                  
       
       tabItem("content",
               fluidRow(
                 tabBox(title = "Content Analysis",
                        width = 12,
                        tabPanel("Likes vs Dislikes",
                                 title = "Likes vs Dislikes", solidHeader =T,
                                 collapsible = T,
                                 width=1,
                                 
                                 plotlyOutput("likedis", height = 700),
                                 fluidRow(
                                   box(
                                     title = "Favorability Index", solidHeader = T,
                                     width = 7,
                                     plotlyOutput("favindexoutput")
                                   ),
                                   box(
                                     title="Analysis",
                                     solidHeader = T,
                                     width=4,
                                     p("Information and hard news genre's are the type of content that generate more engagemnet in our channel. they have better likeratio (like/views) and many viewers want to share the video content.")
                                   )
                                 )),
                        tabPanel("Sentiment vs Watching time (min)",
                                 title = "Sentiment vs Watching time (min)", solidHeader = T,
                                 collapsible = T,
                                 width=1,
                                 plotlyOutput("sentvswatch", height = 1000)),
                                 fluidRow(
                                   box(
                                     title="Analysis",
                                     solidHeader = T,
                                     width=8,
                                     p("Zulu and Asian Games Channel are the highest at postive sentiment for their video contents. Channel with entertainment genre, like Ini Talkshow, Tonigh show gained more watching time views spent than other genres")
                                   )
                                 )
                                
                        
                  )#tab box
           
              
                ) #fluid row end
              
      )#tbItems end
    )#tabitems end
      
    )#dashboardBody end
    
  )#dashboardPage end
    
    
  




    
  
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  yrev <- bydate_NET %>% summarise(sum(`YouTube ad revenue (USD)`))
  yview <- bydate_NET %>% summarise(sum(Views))
  
  #set reactive fuction for data(first Page)
  output$YoutubeRev <- renderInfoBox({
    infoBox(
      title = "Revenue", 
      value = yrev, icon=icon("youtube"), 
      subtitle = "Total revenue (USD) in 2018.",
      color = "black"
    )
  })
  
  output$YoutubeView <- renderInfoBox({
    infoBox(
      title = "Views", 
      value = yview, icon=icon("youtube"), 
      subtitle = "Total views in 2018.",
      color = "black"
    )
  })
  
  rev_viz <- reactive({
    rev_plot(start = input$date_slider_overview[1], 
               end = input$date_slider_overview[2]) 
 })
  
  view_viz <- reactive({
    views_plot(start = input$date_slider_overview[1], 
               end = input$date_slider_overview[2]) 
})
  
  red_viz <- reactive({
    red_plot(start = input$date_slider_overview[1], 
             end = input$date_slider_overview[2]) 
})
  
  redview_viz <- reactive({
    redviews_plot(start = input$date_slider_overview[1], 
             end = input$date_slider_overview[2])
})

  #set reactive plot
  output$total_plot <- renderPlotly({
    rev_data <- rev_viz()
    
    rev_chart <- rev_data %>%
      plot_ly(x = ~Date, y = ~Revenue, type = "scatter",mode= "line", colors="dodgerblue4" ) %>%
      layout(yaxis= list(title="Youtube Revenue"),
          xaxis = list(rangeslider = list(visible = F)))
    
    view_data <- view_viz()

    bar_chart <- view_data %>%
      plot_ly( x=~Date, y=~Views, type='bar', colors = "goldenrod3") %>% 
       layout(yaxis = list(title = "Views"), xaxis = list(title ="Date"))
  
    subplot(rev_chart, bar_chart, heights = c(0.7,0.2), nrows=2,shareX = TRUE, titleY = TRUE)

  })

  output$red_plot <- renderPlotly({
    red_data  <- red_viz()
      
    red_chart <- red_data %>%
      plot_ly(x=~Date, y = ~Revenue, type = "scatter",mode= "line") %>% 
      layout(yaxis= list(title="Youtube Red Revenue"),
             xaxis = list(rangeslider = list(visible = F))) 
  
    redview_data <- redview_viz()
    
   redview_chart <- redview_data %>%
     plot_ly(x=~Date, y=~red_views, type = 'bar') %>%
    layout(yaxis = list(title = "Red Views"), xaxis = list(title ="Date"))
  
  subplot(red_chart, redview_chart, heights = c(0.7,0.2), nrows=2,shareX = TRUE, titleY = TRUE)
  
  })
  
  #set reactive table
  output$totaltable <- renderDataTable({
    rev_viz() %>%
      as.tibble()
  })
  output$redtable <- renderDataTable({
    red_viz() %>%
      as.tibble()
 })
  output$map_rev <- renderPlotly({
    ggplotly(map_plot, originalData = F)
 })
  output$regiontable <- renderDataTable({
    region_NET_filt
  })
  output$top30_channel <- renderPlotly({
    ggplotly(channel_colp)
  })
  output$top30_asset <- renderPlotly({
    ggplotly(asset_colp)
  })
  output$comptable <- renderPlotly({
    ggplotly(comp)
  })
  output$likedis <- renderPlotly({
    ggplotly(pexp)
  })
  output$favindexoutput <- renderPlotly({
    ggplotly(colp2)
  })
  output$sentvswatch <- renderPlotly({
    ggplotly(foxp)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

