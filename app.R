#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mortality  Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tableOutput("table")
    ),
    # Show a plot of the generated distribution
    mainPanel(h3("Draw boxes around points to see the raw data."),
              plotOutput("scatterPlot", brush = "plot_brush")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(WHO)
  library(tidyverse)
  library(data.table)
  
  # codes <- get_codes()
  # hw_codes <- codes[grep('hrh', codes$label, ignore.case=T),]
  # glimpse(codes)
  child_codes <- c('imr')#, 'nmr', 'u5mr')
  for (i in child_codes){
    #gets the data from WHO
    assign(paste0(i, '_data'), as_tibble(data.table(get_data(i))[datasource == 'DHS',]))
    x <- paste0(i, '_data')
    setnames(get(eval(x)), paste(i,colnames(get(eval(x))), sep="_"))
    assign(eval(x), get(eval(x)) %>%
             separate(get(eval(paste0(i, '_value'))), into = c(paste0(i, '_mean'), paste0(i, '_stdev')), sep = " ", fill = "right", convert = T))
  }
  #gets healthcare worker data
  hw <- data.table(get_data("HRH_29"))
  names(hw) <- paste0( "hw_", names(hw))
  #joins the data down to a dense matrix
  comp_data <- left_join(hw, imr_data, by = c("hw_country"="imr_country", "hw_year"="imr_year"))
  final_data <- comp_data[,c("hw_country", "hw_value", "imr_mean")]
  final_data <- filter(final_data, !is.na(imr_mean)) %>%
    filter(imr_mean != 'No') %>%
    as.tibble() %>%
    group_by(hw_country) %>%
    arrange(hw_country)
  final_data$imr_mean <- as.numeric(final_data$imr_mean)
  final_data <- final_data %>% summarise(imr_mean = mean(imr_mean), hw_mean = mean(hw_value))
  
  log.model <-lm(log(imr_mean) ~ hw_mean, final_data)
  log.model.df <- data.frame(x = final_data$hw_mean,
                             y = exp(fitted(log.model)))
  
  
  output$scatterPlot <- renderPlot({
    # draw the scatter plot
    ggplot(final_data, mapping = aes(x=hw_mean, y=imr_mean, color=hw_country)) +
      labs(x="Health Workers per 1000", y="infant deaths per 1000 live births") +
      geom_point() +
      geom_smooth(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 1)
  })
  
  output$table <- renderTable({brushedPoints(final_data, input$plot_brush, xvar = "hw_mean", yvar = "imr_mean")})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

