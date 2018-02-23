library(shiny)
library(tidyverse)
library(forcats)

# Data cleaning

# import data
data<-read.csv("data/ucr_crime_1975_2015.csv")

# exclude data I do not want
# I need ORI, year, department_name, and normalized crime data 
data<-data[c(-4,-5,-6,-7,-8,-9,-16,-17)]

# remove Nas
data<-na.omit(data)

# To keep consistency, I remove incomplete records that did not report crime for all 12 months
# in a given year 
data<-filter(data,months_reported==12)

# Since ORI (the first two letters are the abbreviation of states) records the states cities belong to,
# so I use information in the ORI column to group cities.

library(stringi)

data$ORI<-as.character(data$ORI)
data$state<-sapply(data$ORI,function(x) stri_sub(x,from = 1,to=2))
data$state<-factor(data$state)

# Again, exclude features that I do not want: months_reported
data<-data[-4]

# nlevels(data$state) # Among 50 states, we have records from 32 states and Washington, D.C.
# those states are "AZ" "CA" "CO" "DC" "FL" "GA" "HI" "IL" "IN" "KS" "KY" "LA"
# "MA" "MD" "MI" "MN" "MO" "NB" "NC" "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA"
# "TN" "TX" "UT" "VA" "WA" "WI"

# check what is "NB"
# filter(data, state=="NB") # By checking with website, I find that the "NB" is actually "NE"
data$state <- as.character(data$state)
data$state[data$state == "NB"] <- "NE"
data$state <- factor(data$state)

crime<-data

# get data to make the map
library(maps)
map_usa<-map_data("state")
usa_states<-map_data("state")
usa_states<-usa_states[-6]

# need to reset wd here
sabbr<-read.csv("https://raw.githubusercontent.com/TinaQian2017/Milestone_532_new/master/data/states_abbr.csv",header = T)
sabbr$region<-as.character(sabbr$region)
mapindex<-left_join(usa_states,sabbr)
mapdata<-full_join(mapindex,data)

# change headers
names(mapdata)[11:14]<-c('Homicides', 'Rape', 'Robbery', 'Aggravated_Assault')
names(crime)[5:8]<-c('Homicides', 'Rape', 'Robbery', 'Aggravated_Assault')



# Define UI ----
ui <- navbarPage("Crime Rate in the U.S.A", 
                 
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(width=4,
                                         h3("Choice of Inputs"),
                                         br(),
                                         # checkbox input - pick crime type
                                         checkboxGroupInput("checkbox",h4("Crime Type"),
                                                            choices=c('Homicides', 'Rape', 'Robbery', 'Aggravated_Assault'),
                                                            selected = 'Homicides'),
                                         p("The app will sum up the crime rate of different crime types that you choose."),
                                         # slider input
                                         sliderInput("slider1", h4("Year"),
                                                     min = 1975, max = 2014, value = c(1975,2014),sep="")
                            ),
                            # Showing the plot
                            mainPanel(h3("Crime Rate in The U.S.A"),
                                      plotOutput("themap"),
                                      p("Note: The crime rate is calculated by dividing
                         the number of crimes by population, recording the number of crime per 100k people.
                                        And the states are colored by their average crime rate over time chosen. If you
                                        choose multiple crime types, their rates will be summed up to show you the density of crimes in the state."),
                                      br(),
                                      strong("Acknowledgement"),
                                      p("The app is built on the Marshall Violent Crime dataset. The data was collected from
                                        city level, and some states did not have any data recorded by the dataset. Therefore,
                                      you can see some grey in the map, where no crime data is available. Based on the available data, I group records from the same state together as a
                                        representative as the crime rate in the state. "))
                            
                 )
                 ),       
                 
                 tabPanel("Data Explore",
                  sidebarLayout(
                   sidebarPanel(width=4,
                                h3("Choice of Inputs"),
                                br(),
                                h4("Weight for Crimes"),
                                
                                p("Assign weight to different type of crimes. Feel free to assign any 
                                  values to different crime types by typing numbers in, since the weights will be standardized. However,
                                 it is more institutive to sum the weights up to 1 (the default setting)."),
                                
                                
                                # numeric input
                                numericInput("weightho", "Weight for Homicides",
                                             
                                             min = 0, max = 1, value = 0.7, step = 0.05),
                                numericInput("weightra", "Weight for Rape",
                                             min = 0, max = 1, value = 0.1, step = 0.05),
                                numericInput("weightro", "Weight for Robbery",
                                             min = 0, max = 1, value = 0.1, step = 0.05),
                                numericInput("weightag", "Weight for Aggravated Assault",
                                             min = 0, max = 1, value = 0.1, step = 0.05),
                                br(),
                                
                                # slider input
                                sliderInput("slider2", h4("Year"),
                                            min = 1975, max = 2014, value = c(1975,2014),sep=""),
                                # checkbox input - pick crime type
                                checkboxGroupInput("checkbox2",h4("States"),
                                                   choices=unique(crime$state),
                                                   selected = unique(crime$state))
                   ),
                   # Showing the plot
                   mainPanel(
                       h3("Overall Rating of States According to Risk Scores"),
                       plotOutput("theplot1"),
                       strong("The larger the risk score is, the more dangerous the state is.
                         The score is the mean of the product of real crime rate and the standardized weight you assign.
                              The states are ordered by their risk scores, with the safest states on the left and most dangerous
                              states on the right."),
                       h3("Distribution of Real Crime Records"),
                       plotOutput("theplot2"),
                       p("The plot shows the real record of crime rates. The states are in the same order as the plot above."),
                       p("Note: The crime rate is calculated by dividing
                         the number of crimes by population, recording the number of crime per 100k people.")
                 )
                  )
                 )
)
                          
                          
                  

# Define server logic ----
server <- function(input, output) {
  output$themap<-renderPlot({

    
    data<-mapdata %>% 
      filter(year %in% input$slider1)
  
    # prepare data to add labels in map
    label<-mapdata %>% group_by(state) %>% summarise(y=mean(lat),x=mean(long))
    hh<-label %>%filter(is.na(state)) %>% mutate(state="ND", x=-98.36) 
    label_c<-rbind(label,hh)

    
    if (is.null(input$checkbox)){
      ggplot() +
        # background map
        geom_polygon(data=map_usa, aes(x = long, y = lat, group = group),alpha=0.7) +
        geom_text(data=label_c, aes(label=state,x=x,y=y),check_overlap=TRUE)+
        labs(x="Longitude",y="Latitude")+
        theme_classic()
      
    } else {
      
      newdf=data%>%select(input$checkbox)
      data$Crime_Rate=apply(newdf,1,function(x) sum(x))
      
    ggplot() +
    # background map
      geom_polygon(data=map_usa, aes(x = long, y = lat, group = group),alpha=0.7) +
      labs(x="Longitude",y="Latitude")+
      # crime data layer
      geom_polygon(data=data, aes(x = long, y = lat, group = group, fill = Crime_Rate )) +
                     labs(x="Longitude",y="Latitude")+
                    scale_fill_distiller(palette="Blues",direction=1)+
      theme_classic()+
      theme(legend.position="bottom")+
      geom_text(data=label_c, aes(label=state,x=x,y=y),check_overlap=TRUE)
    }
    
})
    
    output$theplot1<-renderPlot({
      data2<-crime %>%
        filter(year %in% input$slider2)%>%
        filter(state %in% input$checkbox2)
      
      total_weight<-input$weightho+input$weightra+input$weightro+input$weightag
      
      data2$Homicides<-data2$Homicides*input$weightho/total_weight
      data2$Rape<-data2$Rape*input$weightra/total_weight
      data2$Robbery<-data2$Robbery*input$weightro/total_weight
      data2$Aggravated_Assault<-data2$Aggravated_Assault*input$weightag/total_weight
      
      # calculate scores
      scores<-data2 %>% group_by(state)%>%summarise(mean=mean(Homicides)+mean(Rape)+mean(Robbery)+mean(Aggravated_Assault))
      scores$state<-factor(scores$state)
      
      ggplot(scores,aes(x=fct_reorder(state,mean),y=mean))+geom_point()+
        labs(x="State",y="Risk Scores")
    })
    
    output$theplot2<-renderPlot({
      data2<-crime %>%
        filter(year %in% input$slider2) %>%
        filter(state %in% input$checkbox2)
      
      data2$Homicides<-data2$Homicides*input$weightho
      data2$Rape<-data2$Rape*input$weightra
      data2$Robbery<-data2$Robbery*input$weightro
      data2$Aggravated_Assault<-data2$Aggravated_Assault*input$weightag
      
      # calculate scores
      scores<-data2 %>% group_by(state)%>%summarise(mean=mean(Homicides)+mean(Rape)+mean(Robbery)+mean(Aggravated_Assault))
      scores$state<-factor(scores$state)
      
      # real data
      data3<-crime %>%
        filter(year %in% input$slider2) %>%
        filter(state %in% input$checkbox2)
      
      index=c()
      if (input$weightho!=0) index<-append(index,"Homicides")
      if (input$weightra!=0) index<-append(index,"Rape")
      if (input$weightro!=0) index<-append(index,"Robbery")
      if (input$weightag!=0) index<-append(index,"Aggravated_Assault")

      newdf2<-data3%>%select(index)
      data3$new<-apply(newdf2,1,function(x) sum(x))
      data4<-left_join(data3,scores,by="state")
      
      ggplot(data4,aes(x=fct_reorder(state,mean)))+geom_boxplot(aes(y=new))+
        labs(x="State",y="Real Crime Rate According to Records")
        
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)