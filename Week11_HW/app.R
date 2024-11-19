# MBIO 612: Week 11 Homework
# Make a shiny app

library(shiny) #to make shiny app
library(tidyverse) #for data manipulation
library(kableExtra) #for creating tables

groundhogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv') #read in groundhog info
predictions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv') #read in annual prediction info
choice_list <- unique(groundhogs$name) #create a list of the groundhogs so people can select a specific one and look at their predictions
years <- unique(predictions$year) #create a list of years so people can select a year and see a summary of all predictions for that year

ui <- fluidPage(
  titlePanel("Groundhog Day Information!"), #add title
  
  mainPanel(p("Select a year to see a summary of the groundhog predictions for that specific year!")), #add caption/description for the first dynamic bar graph
  
  selectInput(inputId = "year", #ID name for the input
              label = "Choose a Year", #label above the input
              choices= years,
              selected= max(years)), #drop down selection for prediction years (years with no predictions were removed to avoid blank ggplot being created)
  
  
  plotOutput("bar"), #output 1 is a bar graph
  
  mainPanel(p("Select a specific groundhog to see all of their past predictions! Does your favorite groundhog constantly see their shadow or are they're predictions variable?")), #add caption/description for the second dynamic kable table
  
  selectInput(inputId= "name", #ID name for the input
              label= "Select a Groundhog", #label above the input
              choices= choice_list), #drop down selection for groundhogs so we can make a table to visualize all of their predictions
  
  htmlOutput("ghog_pred") #output 2 is a kable table
  
)
#NOTE: I am very redundant with cleaning of the reactive functions "annual_preds" and "groundhog_data". I attempted to make a separate function that joins the "prediction" and "groundhogs" data frames and cleans them, however I could not successfully create my outputs using that method. So pls forgive me :'(
server <- function(input, output){
  annual_preds <- reactive({ 
    groundhogs %>% #pull in groundhog data
    left_join(predictions) %>% #join groundhog data with prediction data
    select(name, year, shadow) %>% #select columns useful for analysis
    na.omit() %>% #remove NAs
    mutate(shadow= replace(shadow, shadow== TRUE, "Six More Weeks of Winter")) %>% #change value for plot (becomes x-axis value)
    mutate(shadow= replace(shadow, shadow== FALSE, "Early Spring")) %>% #change value for plot (becomes x-axis value)
    filter(year== input$year) %>% #connect selected year to the year we plot
    group_by(shadow) %>% #group data by prediction
    summarise(total= n()) #calculate the total for each prediction that year
  })
  
  output$bar <- renderPlot({
    ggplot(data= annual_preds(), #create plot
          aes(x= shadow, #x= prediction type
              y= total, #y= number of predictions for each type that year
              fill= shadow)) + #color the prediction types differently
      geom_col() + #create bar graph
      labs(title= "Groundhog Predictions", #add title
           x= "Weather Prediction", #add x-axis title
           y= "Total Number of Predictions", #add y-axis title
           caption= "Source: Data Science Learning Community. 2024. Tidy Tuesday: A weekly social data project.") + #add where we got the data from
      scale_fill_manual(values= c("tomato3", "steelblue2")) + #select colors for the plot
      theme_bw() + #add nice theme
      theme(legend.position = "none", #remove legend
            plot.title= element_text(face= "bold", size= 15), #change text size and bold title
            plot.caption= element_text(size= 6)) #change text size of data source
  })
  
  groundhog_data <- reactive({ 
    groundhogs %>% #pull in groundhog data
    left_join(predictions) %>% #join groundhog data with prediction data
    select(name, year, shadow) %>%  #select columns useful for analysis
    na.omit() %>%  #remove NAs
    mutate(shadow= replace(shadow, shadow== TRUE, "Six More Weeks of Winter")) %>% #change value for plot (becomes table value)
    mutate(shadow= replace(shadow, shadow== FALSE, "Early Spring")) %>% #change value for plot (becomes table value)
    rename(Year= year, Prediction= shadow) %>% #rename columns so they look good in the table
    filter(name== input$name) %>% #connect selected groundhog to the groundhog predictions that we display
    select(Year, Prediction)  #select columns we want displayed in the table
  })
  
  output$ghog_pred <- renderText({
    kbl(groundhog_data()) %>% #create kable
    kable_classic_2(lightable_options = "striped") %>% #make it striped
    kable_styling(full_width = FALSE, row_label_position = "c") #make it thin
    })
  
}

shinyApp(ui = ui, server = server)