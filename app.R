## TO DO
# rename factors in Adult survey - AND relevel
# Add Retention Data - new page
# Check Data for errors
# Set up GitHub Page
# Add Tables for text responses where appropriate.- see Skill Acq App - Priorities Table for an example.



#####################################################################################################
# TA JUNIOR COMPS PROJECT
####################################################################################################


library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(condformat)
library(googlesheets)
library(shiny)
library(zoo)
library(kableExtra)
library(forcats)
library(stringr)
library(readr)




#####################################################################################################
# Import Data
####################################################################################################


SurveyData <- read_csv("Final Survey Data.csv", na = c("", "NA")) %>%
  mutate(Q1 = fct_relevel(Q1, c("YES", "NO"))) %>%
  mutate(Q4 = ifelse(Q4 == "DM", "DOES NOT MATTER", ifelse(Q4 == "TL", "TOO LONG", Q4)))

SurveyData$`AGE-BASED` <-  as.factor(SurveyData$`AGE-BASED`)
SurveyData$`GENDER-BASED` <-  as.factor(SurveyData$`GENDER-BASED`)
SurveyData$Gender <-  as.factor(SurveyData$Gender)


SurveyData.X2Long <- gather(SurveyData, Q2.Sport, Q2.Response, "2.AUSTRALIAN_FOOTBALL":"2.ATHLETICS", factor_key=TRUE) %>%
  mutate(Q2.Response = replace_na(Q2.Response))
SurveyData.X3Long <- gather(SurveyData, Q3.Time, Q3.Response, "3.MWAS":"3.AnyTime", factor_key=TRUE) %>%
  mutate(Q3TIME = ifelse(Q3.Time == "3.MWAS", "Mid-Week After School", ifelse(Q3.Time == "3.FAS", "Friday After School", ifelse(Q3.Time == "3.SatAM", "Saturday AM", ifelse(Q3.Time == "3.SatPM", "Saturday PM", ifelse(Q3.Time == "3.SunAM", "Sunday AM", ifelse(Q3.Time == "3.SunPM", "Sunday PM", ifelse(Q3.Time == "AnyTime", "Any Time", "NA"))))))))%>%
  mutate(Q3TIME = as.factor(Q3TIME))



###################################################################################################################################
# BUILD SHINY APP - ui
###################################################################################################################################

# User interface for Shiny

ui <- navbarPage("TA Junior Comps Project",
                 # Create radio buttons to select time period for break down
                 navbarMenu("Children Survey",
                            tabPanel("Playing Tennis",
                                     fluidRow(column(3, 
                                                     wellPanel(
                                                       p("VARIABLES"),
                                                       selectInput(inputId = "state",
                                                                   label = "State",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$State)))),
                                                       sliderInput(inputId = "age",
                                                                   label = "Age",
                                                                   min=min(SurveyData$Age, na.rm = TRUE), max=max(SurveyData$Age, na.rm = TRUE), value=c(min(SurveyData$Age, na.rm = TRUE), max(SurveyData$Age, na.rm = TRUE)), 
                                                                   step=1, width = "90%", animate = F, sep = "", ticks = F),
                                                       selectInput(inputId = "sex",
                                                                   label = "Gender",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$Gender)))),
                                                       selectInput(inputId = "agebased",
                                                                   label = "Age-Based or Open",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'AGE-BASED')))),
                                                       selectInput(inputId = "genderbased",
                                                                   label = "Gender-Split or MIxed",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'GENDER-BASED')))))),
                                              column(4, plotOutput("Q1C"),
                                                        plotOutput("Q3C"),
                                                        plotOutput("Q5C"),
                                                        plotOutput("Q7C"),
                                                        plotOutput("Q9C"),
                                                        plotOutput("Q11C"),
                                                        plotOutput("Q13C")),
                                              column(4, plotOutput("Q2C"),
                                                        plotOutput("Q4C"),
                                                        plotOutput("Q6C"),
                                                        plotOutput("Q8C"),
                                                        plotOutput("Q10C"),
                                                        plotOutput("Q12C"),
                                                        plotOutput("Q14C")))),
                            tabPanel("Compared to other sports",
                                     fluidRow(column(3, 
                                                     wellPanel(
                                                       p("VARIABLES"),
                                                       selectInput(inputId = "state",
                                                                   label = "State",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$State)))),
                                                       sliderInput(inputId = "age",
                                                                   label = "Age",
                                                                   min=min(SurveyData$Age, na.rm = TRUE), max=max(SurveyData$Age, na.rm = TRUE), value=c(min(SurveyData$Age, na.rm = TRUE), max(SurveyData$Age, na.rm = TRUE)), 
                                                                   step=1, width = "90%", animate = F, sep = "", ticks = F),
                                                       selectInput(inputId = "sex",
                                                                   label = "Gender",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$Gender)))),
                                                       selectInput(inputId = "agebased",
                                                                   label = "Age-Based or Open",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$AGE.BASED)))),
                                                       selectInput(inputId = "genderbased",
                                                                   label = "Gender-Split or MIxed",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$GENDER.BASED)))))),
                                              column(4,
                                                     plotOutput("Q15C"),
                                                     plotOutput("Q17C"),
                                                     plotOutput("Q19C")),
                                              column(4, 
                                                     plotOutput("Q16C"),
                                                     plotOutput("Q18C"))))),
                            navbarMenu("Adult Survey",
                                       tabPanel("Tennis",
                                                fluidRow(column(3, 
                                                                wellPanel(
                                                                  p("VARIABLES"),
                                                                  selectInput(inputId = "state3",
                                                                              label = "State",
                                                                              c("All",
                                                                                unique(as.character(SurveyData$State)))),
                                                                  sliderInput(inputId = "age3",
                                                                              label = "Age",
                                                                              min=min(SurveyData$Age, na.rm = TRUE), max=max(SurveyData$Age, na.rm = TRUE), value=c(min(SurveyData$Age, na.rm = TRUE), max(SurveyData$Age, na.rm = TRUE)), 
                                                                              step=1, width = "90%", animate = F, sep = "", ticks = F),
                                                                  selectInput(inputId = "sex3",
                                                                              label = "Gender",
                                                                              c("All",
                                                                                unique(as.character(SurveyData$Gender)))),
                                                                  selectInput(inputId = "agebased3",
                                                                              label = "Age-Based or Open",
                                                                              c("All",
                                                                                unique(as.character(SurveyData$AGE.BASED)))),
                                                                  selectInput(inputId = "genderbased3",
                                                                              label = "Gender-Split or MIxed",
                                                                              c("All",
                                                                                unique(as.character(SurveyData$GENDER.BASED)))))),
                                                         column(4, plotOutput("Q1"),
                                                                plotOutput("Q3.plot"),
                                                                plotOutput("Q5a"),
                                                                plotOutput("Q5c"),
                                                                plotOutput("Q7"),
                                                                plotOutput("Q9"),
                                                                plotOutput("Q11")),
                                                         column(4, plotOutput("Q2.plot"),
                                                                plotOutput("Q4"),
                                                                plotOutput("Q5b"),
                                                                plotOutput("Q6"),
                                                                plotOutput("Q8"),
                                                                plotOutput("Q10")))))
                            
)




###################################################################################################################################
# BUILD SHINY APP - server
###################################################################################################################################


server = function(input, output) {
  
  
  ##############################################################################################################################
  # Children's Survey: TENNIS
  ##############################################################################################################################
  
  plot.data.function <- function(x, question) {
    
    
    answers <- c("UNSURE", "NO", "MN", "MY", "YES")
    
    data.x <- SurveyData %>%
      rename(variable = x) %>%
      subset(variable %in% answers) %>%
      mutate(variable = fct_relevel(variable, answers))
    
    
    if (input$state != "All") {
      data.x <- data.x[data.x$State == input$state,]
    }
    
    if (input$agebased != "All") {
      data.x <- data.x[data.x$AGE.BASED == input$agebased,]
    }
    
    if (input$genderbased != "All") {
      data.x <- data.x[data.x$GENDER.BASED == input$genderbased,]
    }
    
    if (input$sex != "All") {
      data.x <- data.x[data.x$Gender == input$sex,]
    }
    
    
    
    data.x <- data.x[data.x$Age >= input$age[1] & data.x$Age <= input$age[2],]
    
    
    
    x.plot <- ggplot(data.x, aes(x = factor(variable)))+
      theme_gray()+
      geom_text(stat = 'count', aes(label =..count.., y = (..count..)/sum(..count..) + 0.025))+
      geom_bar(aes(y = (..count..)/sum(..count..))) + 
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_x_discrete(limits = answers,
                       labels=c("UNSURE", "NO", "MAYBE NO", "MAYBE YES", "YES"))+
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 14, angle = 45, vjust = .5),
            plot.title = element_text(size = 16, hjust = .5, face = "bold"))+
      labs(title = question,
           subtitle = x,
           x = "",
           y = "Frequency")
    
    return(x.plot)
  }  
  
  question.data <- tribble(
    ~x, ~question,
    "Q1C", "I enjoy playing competition",
    "Q2C", "I feel happy after I have \nplayed competition tennis",
    "Q3C", "I am confident with \nscoring a tennis match",
    "Q4C", "I find calling lines \n(balls in and out) difficult",
    "Q5C", "I find competition tennis \ntoo difficult",
    "Q6C", "I have fun playing singles",
    "Q7C", "I have fun playing doubles",
    "Q8C", "I get bored when I play \n competition tennis",
    "Q9C", "I think the time I spend \noff court (waiting) is too long",
    "Q10C", "I think the time I spend \non court is too long",
    "Q11C", "I enjoy playing with the \norange/green balls",
    "Q12C", "I would prefer to play \nwith adult balls",
    "Q13C", "I find rallying the ball difficult",
    "Q14C", "I find serving the ball difficult",
    "Q15C", "Tennis is much easier to play \n(compared to other sports)",
    "Q16C", "Tennis is much more fun to \nplay (compared to other sports)",
    "Q17C", "I wait around to play tennis longer \n(compared to other sports)",
    "Q18C", "I travel further to play tennis \n(compared to other sports)",
    "Q19C", "The team takes longer to play \ntennis (compared to other sports)"
  )
  

  
  
  # Map = create a list by applying a function to a vector of arguments
  
  function_for_map <- function(i){
    name <- question.data$x[[i]]
    
    output[[name]] <- renderPlot({
      plot.data.function(x = question.data$x[i],
                         question = question.data$question[i])
    })
  }
  
  Map(function_for_map, 1:nrow(question.data))
  



    ##############################################################################################################################
    # Children's Survey: COMPARED TO OTHER SPORTS
    ##############################################################################################################################
  
  question.data.2 <- tribble(
    ~x, ~question,
    "Q15C", "Tennis much easier to play \n(compared to other sports)",
    "Q16C", "Tennis is much more fun to \nplay (compared to other sports)",
    "Q17C", "I wait around to play tennis longer \n(compared to other sports)",
    "Q18C", "I travel further to play tennis \n(compared to other sports)",
    "Q19C", "The team takes longer to play \ntennis (compared to other sports)"
  )
  
  
  # Map = create a list by applying a function to a vector of arguments
  
  function_for_map.2 <- function(i){
    name <- question.data.2$x[[i]]
    
    output[[name]] <- renderPlot({
      plot.data.function(x = question.data.2$x[i],
                         question = question.data.2$question[i])
    })
  }
  
  Map(function_for_map.2, 1:nrow(question.data.2))
  


  
  ##############################################################################################################################
  # Adult's Survey: TENNIS
  ##############################################################################################################################
  
  plot.data.function.3 <- function(x, question) {
    

    data.y <- SurveyData %>%
      rename(variable = x) 
    
    if (input$state3 != "All") {
      data.y <- data.y[data.y$State == input$state3,]
    }
    
    if (input$agebased3 != "All") {
      data.y <- data.y[data.y$AGE.BASED == input$agebased3,]
    }
    
    if (input$genderbased3 != "All") {
      data.y <- data.y[data.y$GENDER.BASED == input$genderbased3,]
    }
    
    if (input$sex3 != "All") {
      data.y <- data.y[data.y$Gender == input$sex3,]
    }
    
    
    
    data.y <- data.y[data.y$Age >= input$age3[1] & data.y$Age <= input$age3[2],]

    
    x.plot <- ggplot(data.y[!is.na(data.y$variable), ], aes(x = factor(variable)))+
      theme_gray()+
      geom_text(stat = 'count', aes(label =..count.., y = (..count..)/sum(..count..) + 0.025))+
      geom_bar(aes(y = (..count..)/sum(..count..)), drop = TRUE) + 
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 14, angle = 45, vjust = .5),
            plot.title = element_text(size = 16, hjust = .5, face = "bold"))+
      labs(title = question,
           subtitle = x,
           x = "",
           y = "Frequency")
    
    return(x.plot)
  }  
  
  question.data.3 <- tribble(
    ~x, ~question,
    "Q1", "Does your child (or children) \nparticipate in another organised sport \noutside of school",
    "Q3", "What is your preferred day and \ntime for competition tennis?",
    "Q4", "Are you happy with the duration \nof the current competition that your \nchild plays? (i.e., the duration of \ntime at the club each day)",
    "Q5a", "What is the ideal duration for the \nentire team to finish playing \ncompetition tennis?",
    "Q5b", "What is the ideal duration for \nyour child to play in competition tennis?",
    "Q5c", "What is the ideal number of \nmatches for your child to play?",
    "Q6", "How long did it take you to \ntravel to today's competition venue?",
    "Q6a", "Please rate how you found \nthis travel time",
    "Q7", "Do you think SINGLES tennis \n(in its current competition format) \nis easy or difficult for your child \n(or children) to play and execute the core \nskills (i.e., rallying & serving)",
    "Q8", "Do you think DOUBLES tennis \n(in its current competition format) \nis easy or difficult for your child \n(or children) \nto play and execute the core \nskills (i.e., rallying & serving)",
    "Q9", "Does your child's competition \npromote a sense of team?",
    "Q10", "Would you prefer your child to play \nin a competition that was gender-mixed \n(males & females) OR only within \ntheir gender?",
    "Q11", "Would you prefer your child to play \ntennis against older players \n(Presuming skill level is similar) or \nagainst children of similar age"
  )
  
  # output$X1C <- renderPlot(plot.data.function(x = question.data$x[1],
  #                                                 question = question.data$question[1]))
  # output$X2C <- renderPlot(plot.data.function(x = question.data$x[2],
  #                                                 question = question.data$question[2]))
  # output$X3C <- renderPlot(plot.data.function(x = question.data$x[3],
  #                                                 question = question.data$question[3]))
  # 
  
  
  # Map = create a list by applying a function to a vector of arguments
  
  function_for_map.3 <- function(i){
    name <- question.data.3$x[[i]]
    
    output[[name]] <- renderPlot({
      plot.data.function.3(x = question.data.3$x[i],
                         question = question.data.3$question[i])
    })
  }
  
  Map(function_for_map.3, 1:nrow(question.data.3))
  
  
  ################
  #Q2 - ADULT
  ################
  
  
  output$Q2.plot <-  renderPlot({
    
    data.Q2 <- SurveyData.X2Long
    
    if (input$state3 != "All") {
      data.Q2 <- data.Q2[data.Q2$State == input$state3,]
    }
    
    if (input$agebased3 != "All") {
      data.Q2 <- data.Q2[data.Q2$AGE.BASED == input$agebased3,]
    }
    
    if (input$genderbased3 != "All") {
      data.Q2 <- data.Q2[data.Q2$GENDER.BASED == input$genderbased3,]
    }
    
    if (input$sex3 != "All") {
      data.Q2 <- data.Q2[data.Q2$Gender == input$sex3,]
    }
    
    
    
    data.Q2 <- data.Q2[data.Q2$Age >= input$age3[1] & data.Q2$Age <= input$age3[2],]
    

    data.Q2 <- data.Q2 %>%
        group_by(Q2.Sport) %>%
        summarise(Q2Freq = sum(Q2.Response, na.rm = T))
    
    Q2.plot <- ggplot(data.Q2[!is.na(data.Q2$Q2Freq), ], aes(reorder(Q2.Sport, Q2Freq, sum), Q2Freq, label = Q2Freq)) +
      theme_grey()+
      geom_col(fill = "black") +
      geom_text(hjust = 0, colour = "black") +
      coord_flip() +
      theme(axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 16, hjust = .5, face = "bold"))+
      labs(title = "Which Sport(s)?",
           subtitle = "Q2",
           x = "",
           y = "Frequency")

    return(Q2.plot)

  })
  


  ################
  #Q3 - ADULT
  ################
  
  
  output$Q3.plot <-  renderPlot({
    
    data.Q3 <- SurveyData.X3Long
    
    if (input$state3 != "All") {
      data.Q3 <- data.Q3[data.Q3$State == input$state3,]
    }
    
    if (input$agebased3 != "All") {
      data.Q3 <- data.Q3[data.Q3$AGE.BASED == input$agebased3,]
    }
    
    if (input$genderbased3 != "All") {
      data.Q3 <- data.Q3[data.Q3$GENDER.BASED == input$genderbased3,]
    }
    
    if (input$sex3 != "All") {
      data.Q3 <- data.Q3[data.Q3$Gender == input$sex3,]
    }
    
    
    
    data.Q3 <- data.Q3[data.Q3$Age >= input$age3[1] & data.Q3$Age <= input$age3[2],]
    
    
    data.Q3 <- data.Q3 %>%
      group_by(Q3TIME) %>%
      summarise(Q3Freq = sum(Q3.Response, na.rm = T))
    
    Q3.plot <- ggplot(data.Q3[!is.na(data.Q3$Q3Freq), ], aes(x = Q3TIME, y = (Q3Freq/length(SurveyData$Q3)), label = Q3Freq)) +
      theme_gray()+
      geom_text(vjust = 0, colour = "black") +
      coord_flip() +
      geom_col() +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      theme(axis.text.y = element_text(size = 14),
            axis.text.x = element_text(size = 10, angle = 45, vjust = .5),
            plot.title = element_text(size = 16, hjust = .5, face = "bold")) +
      theme(axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 16)) +
      labs(title = "What is your preferred day & \ntime for competition tennis?",
           subtitle = "Q3",
           x = "",
           y = "Frequency")
    
    return(Q3.plot)
    
  })
  
  
  
  
}

shinyApp(ui, server)

