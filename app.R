## TO DO
# rename factors in Adult survey - AND relevel
# Add Retention Data - new page
# Check Data for errors
# Set up GitHub Page
# Add Tables for text responses where appropriate.- see Skill Acq App - Priorities Table for an example.



#####################################################################################################
# TA JUNIOR COMPS PROJECT
####################################################################################################


library(tidyverse)
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
library(DT)
library("httpuv")




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



# Create lists for input variables





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
                                                       selectInput(inputId = "state2",
                                                                   label = "State",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$State)))),
                                                       sliderInput(inputId = "age2",
                                                                   label = "Age",
                                                                   min=min(SurveyData$Age, na.rm = TRUE), max=max(SurveyData$Age, na.rm = TRUE), value=c(min(SurveyData$Age, na.rm = TRUE), max(SurveyData$Age, na.rm = TRUE)), 
                                                                   step=1, width = "90%", animate = F, sep = "", ticks = F),
                                                       selectInput(inputId = "sex2",
                                                                   label = "Gender",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$Gender)))),
                                                       selectInput(inputId = "agebased2",
                                                                   label = "Age-Based or Open",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'AGE-BASED')))),
                                                       selectInput(inputId = "genderbased2",
                                                                   label = "Gender-Split or MIxed",
                                                                   c("All",
                                                                     unique(!is.na(as.factor(SurveyData$'GENDER-BASED'))))))),
                                              column(4,
                                                     plotOutput("Q15C"),
                                                     plotOutput("Q17C"),
                                                     plotOutput("Q19C")),
                                              column(4, 
                                                     plotOutput("Q16C"),
                                                     plotOutput("Q18C")))),
                            tabPanel("Statements",
                                     fluidRow(column(3, 
                                                     wellPanel(
                                                       p("VARIABLES"),
                                                       selectInput(inputId = "state2a",
                                                                   label = "State",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$State)))),
                                                       sliderInput(inputId = "age2a",
                                                                   label = "Age",
                                                                   min=min(SurveyData$Age, na.rm = TRUE), max=max(SurveyData$Age, na.rm = TRUE), value=c(min(SurveyData$Age, na.rm = TRUE), max(SurveyData$Age, na.rm = TRUE)), 
                                                                   step=1, width = "90%", animate = F, sep = "", ticks = F),
                                                       selectInput(inputId = "sex2a",
                                                                   label = "Gender",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$Gender)))),
                                                       selectInput(inputId = "agebased2a",
                                                                   label = "Age-Based or Open",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'AGE-BASED')))),
                                                       selectInput(inputId = "genderbased2a",
                                                                   label = "Gender-Split or MIxed",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'GENDER-BASED')))))),
                                              column(9, dataTableOutput("ChildrenStatements.Q1"),
                                                        dataTableOutput("ChildrenStatements.Q5"),
                                                        dataTableOutput("ChildrenStatements.Q8"))))),
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
                                                                     unique(as.character(SurveyData$'AGE-BASED')))),
                                                       selectInput(inputId = "genderbased3",
                                                                   label = "Gender-Split or MIxed",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'GENDER-BASED')))))),
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
                                                     plotOutput("Q6.plot"),
                                                     plotOutput("Q8"),
                                                     plotOutput("Q10")))),
                            tabPanel("Retention",
                                     fluidRow(column(3, 
                                                     wellPanel(
                                                       p("VARIABLES"),
                                                       selectInput(inputId = "state4",
                                                                   label = "State",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$State)))),
                                                       sliderInput(inputId = "age4",
                                                                   label = "Age",
                                                                   min=min(SurveyData$Age, na.rm = TRUE), max=max(SurveyData$Age, na.rm = TRUE), value=c(min(SurveyData$Age, na.rm = TRUE), max(SurveyData$Age, na.rm = TRUE)), 
                                                                   step=1, width = "90%", animate = F, sep = "", ticks = F),
                                                       selectInput(inputId = "sex4",
                                                                   label = "Gender",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$Gender)))),
                                                       selectInput(inputId = "agebased4",
                                                                   label = "Age-Based or Open",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'AGE-BASED')))),
                                                       selectInput(inputId = "genderbased4",
                                                                   label = "Gender-Split or MIxed",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'GENDER-BASED')))))),
                                              column(4, plotOutput("Q12"),
                                                     plotOutput("Q15")),
                                              column(4, plotOutput("Q14"),
                                                            plotOutput("Q16"))),
                                     fluidRow(column(12, dataTableOutput("ParentRetention.Q13"),
                                                     dataTableOutput("ParentRetention.Q17")))),
                            tabPanel("Statements",
                                     fluidRow(column(3, 
                                                     wellPanel(
                                                       p("VARIABLES"),
                                                       selectInput(inputId = "state3a",
                                                                   label = "State",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$State)))),
                                                       sliderInput(inputId = "age3a",
                                                                   label = "Age",
                                                                   min=min(SurveyData$Age, na.rm = TRUE), max=max(SurveyData$Age, na.rm = TRUE), value=c(min(SurveyData$Age, na.rm = TRUE), max(SurveyData$Age, na.rm = TRUE)), 
                                                                   step=1, width = "90%", animate = F, sep = "", ticks = F),
                                                       selectInput(inputId = "sex3a",
                                                                   label = "Gender",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$Gender)))),
                                                       selectInput(inputId = "agebased3a",
                                                                   label = "Age-Based or Open",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'AGE-BASED')))),
                                                       selectInput(inputId = "genderbased3a",
                                                                   label = "Gender-Split or MIxed",
                                                                   c("All",
                                                                     unique(as.character(SurveyData$'GENDER-BASED')))))),
                                              column(9, dataTableOutput("ParentStatements.Q7"),
                                                     dataTableOutput("ParentStatements.Q8"),
                                                     dataTableOutput("ParentStatements.Q18.LIKE"),
                                                     dataTableOutput("ParentStatements.Q18.DISLIKE"),
                                                     dataTableOutput("ParentStatements.Q18.IMPROVEMENTS")))))
                 
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
      data.x <- data.x[data.x$'AGE-BASED' == input$agebased,]
    }
    
    if (input$genderbased != "All") {
      data.x <- data.x[data.x$'GENDER-BASED' == input$genderbased,]
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
    "Q1C", "I enjoy playing competition \ntennis",
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
  
  
  plot.data.function2 <- function(x, question) {
    
    
    answers <- c("UNSURE", "NO", "MN", "MY", "YES")
    
    data.z <- SurveyData %>%
      rename(variable = x) %>%
      subset(variable %in% answers) %>%
      mutate(variable = fct_relevel(variable, answers))
    
    
    if (input$state2 != "All") {
      data.z <- data.z[data.z$State == input$state2,]
    }
    
    if (input$agebased2 != "All") {
      data.z <- data.z[data.z$'AGE-BASED' == input$agebased2,]
    }
    
    if (input$genderbased2 != "All") {
      data.z <- data.z[data.z$'GENDER-BASED' == input$genderbased2,]
    }
    
    if (input$sex2 != "All") {
      data.z <- data.z[data.z$Gender == input$sex2,]
    }
    
    
    data.z <- data.z[data.z$Age >= input$age2[1] & data.z$Age <= input$age2[2],]
    
    
    
    z.plot <- ggplot(data.z, aes(x = factor(variable)))+
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
    
    return(z.plot)
  }  
  
  
  
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
      plot.data.function2(x = question.data.2$x[i],
                          question = question.data.2$question[i])
    })
  }
  
  Map(function_for_map.2, 1:nrow(question.data.2))
  
  
  
  
  
  ##############################################################################################################################
  # Children Statements
  ##############################################################################################################################
  
  
  output$ChildrenStatements.Q1 <- DT::renderDataTable(DT::datatable({
    
    data.Q1CA <- SurveyData
    
    if (input$state2a != "All") {
      data.Q1CA <- data.Q1CA[data.Q1CA$State == input$state2a,]
    }
    
    if (input$agebased2a != "All") {
      data.Q1CA <- data.Q1CA[data.Q1CA$'AGE-BASED' == input$agebased2a,]
    }
    
    if (input$genderbased2a != "All") {
      data.Q1CA <- data.Q1CA[data.Q1CA$'GENDER-BASED' == input$genderbased2a,]
    }
    
    if (input$sex2a != "All") {
      data.Q1CA <- data.Q1CA[data.Q1CA$Gender == input$sex2a,]
    }
    
    data.Q1CA <- data.Q1CA[data.Q1CA$Age >= input$age2a[1] & data.Q1CA$Age <= input$age2a[2],]
    
    data.Q1CA %>%
      select(Q1C, Q1CA) %>%
      drop_na(Q1CA) %>%
      rename('Why?' = Q1CA,
             'I enjoy playing competition tennis' = Q1C) 
    
    
  }, options = list(pageLength = 10)))
  
  
  
  output$ChildrenStatements.Q5 <- DT::renderDataTable(DT::datatable({
    
    data.Q5CA <- SurveyData
    
    if (input$state2a != "All") {
      data.Q5CA <- data.Q5CA[data.Q5CA$State == input$state2a,]
    }
    
    if (input$agebased2a != "All") {
      data.Q5CA <- data.Q5CA[data.Q5CA$'AGE-BASED' == input$agebased2a,]
    }
    
    if (input$genderbased2a != "All") {
      data.Q5CA <- data.Q5CA[data.Q5CA$'GENDER-BASED' == input$genderbased2a,]
    }
    
    if (input$sex2a != "All") {
      data.Q5CA <- data.Q5CA[data.Q5CA$Gender == input$sex2a,]
    }
    
    data.Q5CA <- data.Q5CA[data.Q5CA$Age >= input$age2a[1] & data.Q5CA$Age <= input$age2a[2],]
    
    data.Q5CA %>%
      select(Q5C, Q5Ca) %>%
      drop_na(Q5Ca) %>%
      rename('I find competition tennis too difficult' = Q5C,
             'Why?' = Q5Ca) 
    
    
  }, options = list(pageLength = 10)))
  
  
  
  
  
  output$ChildrenStatements.Q8 <- DT::renderDataTable(DT::datatable({
    
    data.Q8CA <- SurveyData
    
    if (input$state2a != "All") {
      data.Q8CA <- data.Q8CA[data.Q8CA$State == input$state2a,]
    }
    
    if (input$agebased2a != "All") {
      data.Q8CA <- data.Q8CA[data.Q8CA$'AGE-BASED' == input$agebased2a,]
    }
    
    if (input$genderbased2a != "All") {
      data.Q8CA <- data.Q8CA[data.Q8CA$'GENDER-BASED' == input$genderbased2a,]
    }
    
    if (input$sex2a != "All") {
      data.Q8CA <- data.Q8CA[data.Q8CA$Gender == input$sex2a,]
    }
    
    data.Q8CA <- data.Q8CA[data.Q8CA$Age >= input$age2a[1] & data.Q8CA$Age <= input$age2a[2],]
    
    data.Q8CA %>%
      select(Q8C, Q8Ca) %>%
      drop_na(Q8Ca) %>%
      rename('Why?' = Q8Ca,
             'I get bored playing competition tennis' = Q8C) 
    
    
  }, options = list(pageLength = 10)))
  
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
      data.y <- data.y[data.y$'AGE-BASED' == input$agebased3,]
    }
    
    if (input$genderbased3 != "All") {
      data.y <- data.y[data.y$'GENDER-BASED' == input$genderbased3,]
    }
    
    if (input$sex3 != "All") {
      data.y <- data.y[data.y$Gender == input$sex3,]
    }
    
    
    
    data.y <- data.y[data.y$Age >= input$age3[1] & data.y$Age <= input$age3[2],]
    
    
    y.plot <- ggplot(data.y[!is.na(data.y$variable), ], aes(x = factor(variable)))+
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
    
    return(y.plot)
  }  
  
  question.data.3 <- tribble(
    ~x, ~question,
    "Q1", "Does your child (or children) \nparticipate in another organised sport \noutside of school",
    "Q3", "What is your preferred day and \ntime for competition tennis?",
    "Q4", "Are you happy with the duration \nof the current competition that your \nchild plays? (i.e., the duration of \ntime at the club each day)",
    "Q5a", "What is the ideal duration for the \nentire team to finish playing \ncompetition tennis?",
    "Q5b", "What is the ideal duration for \nyour child to play in competition tennis?",
    "Q5c", "What is the ideal number of \nmatches for your child to play?",
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
      data.Q2 <- data.Q2[data.Q2$'AGE-BASED' == input$agebased3,]
    }
    
    if (input$genderbased3 != "All") {
      data.Q2 <- data.Q2[data.Q2$'GENDER-BASED' == input$genderbased3,]
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
      data.Q3 <- data.Q3[data.Q3$'AGE-BASED' == input$agebased3,]
    }
    
    if (input$genderbased3 != "All") {
      data.Q3 <- data.Q3[data.Q3$'GENDER-BASED' == input$genderbased3,]
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
      geom_text(hjust = -.5, colour = "black") +
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
  

  
  ################
  #Q6 - ADULT
  ################
  
  
  output$Q6.plot <-  renderPlot({
    
    data.Q6 <- SurveyData
    
    if (input$state3 != "All") {
      data.Q6 <- data.Q6[data.Q6$State == input$state3,]
    }
    
    if (input$agebased3 != "All") {
      data.Q6 <- data.Q6[data.Q6$'AGE-BASED' == input$agebased3,]
    }
    
    if (input$genderbased3 != "All") {
      data.Q6 <- data.Q6[data.Q6$'GENDER-BASED' == input$genderbased3,]
    }
    
    if (input$sex3 != "All") {
      data.Q6 <- data.Q6[data.Q6$Gender == input$sex3,]
    }
    
    
    
    data.Q6 <- data.Q6[data.Q6$Age >= input$age3[1] & data.Q6$Age <= input$age3[2],]
    
    
    Q6.plot <- ggplot(data.Q6[!is.na(data.Q6$Q6), ], aes(x = factor(Q6)))+
      theme_gray()+
      geom_text(stat = 'count', aes(label =..count.., y = (..count..)/sum(..count..) + 0.025))+
      geom_bar(aes(y = (..count..)/sum(..count..)), drop = TRUE) + 
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_x_discrete(limits = c("VC", "SC", "N", "SI", "VI"),
                       labels=c("VERY CONVENIENT", "SOMEWHAT CONVENIENT", "NEITHER CONVENIENT \nNOR INCONVENENT", "SOMEHWAT INCONVENIENT", "VERY INCONVENIENT")) +
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 14, angle = 45, vjust = .5),
            plot.title = element_text(size = 16, hjust = .5, face = "bold"))+
      labs(title = "How long did it take you to \ntravel to today's competition venue?",
           subtitle = "Q6",
           x = "",
           y = "Frequency")
    
    return(Q6.plot)
    
  })
  
  
  
  
  ################
  #Q7 & 8 - ADULT
  ################
  
  
  plot.data.function.A78 <- function(x, question) {
    
    
    data.y <- SurveyData %>%
      rename(variable = x) 
    
    if (input$state3 != "All") {
      data.y <- data.y[data.y$State == input$state3,]
    }
    
    if (input$agebased3 != "All") {
      data.y <- data.y[data.y$'AGE-BASED' == input$agebased3,]
    }
    
    if (input$genderbased3 != "All") {
      data.y <- data.y[data.y$'GENDER-BASED' == input$genderbased3,]
    }
    
    if (input$sex3 != "All") {
      data.y <- data.y[data.y$Gender == input$sex3,]
    }
    
    
    
    data.y <- data.y[data.y$Age >= input$age3[1] & data.y$Age <= input$age3[2],]
    
    
    y.plot <- ggplot(data.y[!is.na(data.y$variable), ], aes(x = factor(variable)))+
      theme_gray()+
      geom_text(stat = 'count', aes(label =..count.., y = (..count..)/sum(..count..) + 0.025))+
      geom_bar(aes(y = (..count..)/sum(..count..)), drop = TRUE) + 
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_x_discrete(limits = c("VE", "SE", "N", "SD", "VD"),
                       labels=c("VERY EASY", "SOMEWHAT EASY", "NEITHER EASY \nNOR DIFFICULT", "SOMEHWAT DIFFICULT", "VERY DIFFICULT")) +
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 14, angle = 45, vjust = .5),
            plot.title = element_text(size = 16, hjust = .5, face = "bold"))+
      labs(title = question,
           subtitle = x,
           x = "",
           y = "Frequency")
    
    return(y.plot)
  }  
  
  question.data.A78 <- tribble(
    ~x, ~question,
    "Q7", "Do you think SINGLES tennis \n(in its current competition format) \nis easy or difficult for your child \n(or children) to play and execute the core \nskills (i.e., rallying & serving)",
    "Q8", "Do you think DOUBLES tennis \n(in its current competition format) \nis easy or difficult for your child \n(or children) to play and execute the core \nskills (i.e., rallying & serving)"
  )
  

  
  # Map = create a list by applying a function to a vector of arguments
  
  function_for_map.A78 <- function(i){
    name <- question.data.A78$x[[i]]
    
    output[[name]] <- renderPlot({
      plot.data.function.A78(x = question.data.A78$x[i],
                           question = question.data.A78$question[i])
    })
  }
  
  Map(function_for_map.A78, 1:nrow(question.data.A78))
  
  
  ##############################################################################################################################
  # Adult's Survey: RETENTION
  ##############################################################################################################################
  
  plot.data.function.4 <- function(x, question) {
    
    
    data.q <- SurveyData %>%
      rename(variable = x) 
    
    if (input$state4 != "All") {
      data.q <- data.q[data.q$State == input$state4,]
    }
    
    if (input$agebased4 != "All") {
      data.q <- data.q[data.q$'AGE-BASED' == input$agebased4,]
    }
    
    if (input$genderbased4 != "All") {
      data.q <- data.q[data.q$'GENDER-BASED' == input$genderbased4,]
    }
    
    if (input$sex4 != "All") {
      data.q <- data.q[data.q$Gender == input$sex4,]
    }
    
    
    
    data.q <- data.q[data.q$Age >= input$age4[1] & data.q$Age <= input$age4[2],]
    
    
    q.plot <- ggplot(data.q[!is.na(data.q$variable), ], aes(x = factor(variable)))+
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
    
    return(q.plot)
  }  
  
  question.data.4 <- tribble(
    ~x, ~question,
    "Q12", "Do you think your child \nwill continue to play tennis \nafter this season?",
    "Q14", "Do you think your child \nwill play in both summer \nand winter this year?",
    "Q15", "Do you have any other \nchildren who used to play \ntennis but now do not?",
    "Q16", "What age did they stop \nplaying tennis?"
  )
  
  
  function_for_map.4 <- function(i){
    name <- question.data.4$x[[i]]
    
    output[[name]] <- renderPlot({
      plot.data.function.4(x = question.data.4$x[i],
                           question = question.data.4$question[i])
    })
  }
  
  Map(function_for_map.4, 1:nrow(question.data.4))
  
  
  ## Retention Qual Statements
  
  output$ParentRetention.Q13 <- DT::renderDataTable(DT::datatable({
    
    data.Q13 <- SurveyData
    
    if (input$state4 != "All") {
      data.Q13 <- data.Q13[data.Q13$State == input$state4,]
    }
    
    if (input$agebased4 != "All") {
      data.Q13 <- data.Q13[data.Q13$'AGE-BASED' == input$agebased4,]
    }
    
    if (input$genderbased4 != "All") {
      data.Q13 <- data.Q13[data.Q13$'GENDER-BASED' == input$genderbased4,]
    }
    
    if (input$sex4 != "All") {
      data.Q13 <- data.Q13[data.Q13$Gender == input$sex4,]
    }
    
    data.Q13 <- data.Q13[data.Q13$Age >= input$age4[1] & data.Q13$Age <= input$age4[2],]
    
    data.Q13 %>%
      select(Q13) %>%
      drop_na(Q13) %>%
      rename('Why DONT you think your child will continue to play tennis after this season?' = Q13)
    
    
  }, options = list(pageLength = 10)))
  
  
  
  output$ParentRetention.Q17 <- DT::renderDataTable(DT::datatable({
    
    data.Q17 <- SurveyData
    
    if (input$state4 != "All") {
      data.Q17 <- data.Q17[data.Q17$State == input$state4,]
    }
    
    if (input$agebased4 != "All") {
      data.Q17 <- data.Q17[data.Q17$'AGE-BASED' == input$agebased4,]
    }
    
    if (input$genderbased4 != "All") {
      data.Q17 <- data.Q17[data.Q17$'GENDER-BASED' == input$genderbased4,]
    }
    
    if (input$sex4 != "All") {
      data.Q17 <- data.Q17[data.Q17$Gender == input$sex4,]
    }
    
    data.Q17 <- data.Q17[data.Q17$Age >= input$age4[1] & data.Q17$Age <= input$age4[2],]
    
    data.Q17 %>%
      select(Q17) %>%
      drop_na(Q17) %>%
      rename('In your opinion, why did your child(ren) stop playing tennis?' = Q17)
    
    
  }, options = list(pageLength = 10)))
  
  
  
  ##############################################################################################################################
  # Parent Statements
  ##############################################################################################################################
  
  
  output$ParentStatements.Q7 <- DT::renderDataTable(DT::datatable({
    
    data.Q7A <- SurveyData
    
    if (input$state3a != "All") {
      data.Q7A <- data.Q7A[data.Q7A$State == input$state3a,]
    }
    
    if (input$agebased3a != "All") {
      data.Q7A <- data.Q7A[data.Q7A$'AGE-BASED' == input$agebased3a,]
    }
    
    if (input$genderbased3a != "All") {
      data.Q7A <- data.Q7A[data.Q7A$'GENDER-BASED' == input$genderbased3a,]
    }
    
    if (input$sex3a != "All") {
      data.Q7A <- data.Q7A[data.Q7A$Gender == input$sex3a,]
    }
    
    data.Q7A <- data.Q7A[data.Q7A$Age >= input$age3a[1] & data.Q7A$Age <= input$age3a[2],]
    
    data.Q7A %>%
      select(Q7, Q7A) %>%
      drop_na(Q7A) %>%
      rename('Please clarify why you think SINGLES is very easy or very difficult?' = Q7A,
             'Rating of SINGLES difficulty (in current format)' = Q7) 
    
  }, options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '150px', targets = c(1))),
    pageLength = 10)
  ))
  
  
  
  output$ParentStatements.Q8 <- DT::renderDataTable(DT::datatable({
    
    data.Q8A <- SurveyData
    
    if (input$state2a != "All") {
      data.Q8A <- data.Q8A[data.Q8A$State == input$state2a,]
    }
    
    if (input$agebased2a != "All") {
      data.Q8A <- data.Q8A[data.Q8A$'AGE-BASED' == input$agebased2a,]
    }
    
    if (input$genderbased2a != "All") {
      data.Q8A <- data.Q8A[data.Q8A$'GENDER-BASED' == input$genderbased2a,]
    }
    
    if (input$sex2a != "All") {
      data.Q8A <- data.Q8A[data.Q8A$Gender == input$sex2a,]
    }
    
    data.Q8A <- data.Q8A[data.Q8A$Age >= input$age2a[1] & data.Q8A$Age <= input$age2a[2],]
    
    data.Q8A %>%
      select(Q8, Q8a) %>%
      drop_na(Q8a) %>%
      rename('Please clarify why you think DOUBLES is very easy or very difficult?' = Q8a,
             'Rating of DOUBLES difficulty (in current format)' = Q8) 

  }, options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '150px', targets = c(1))),
    pageLength = 10)
    ))
  
  
  
  
  output$ParentStatements.Q18.LIKE <- DT::renderDataTable(DT::datatable({
    
    data.Q18.LIKE <- SurveyData
    
    if (input$state2a != "All") {
      data.Q18.LIKE <- data.Q18.LIKE[data.Q18.LIKE$State == input$state2a,]
    }
    
    if (input$agebased2a != "All") {
      data.Q18.LIKE <- data.Q18.LIKE[data.Q18.LIKE$'AGE-BASED' == input$agebased2a,]
    }
    
    if (input$genderbased2a != "All") {
      data.Q18.LIKE <- data.Q18.LIKE[data.Q18.LIKE$'GENDER-BASED' == input$genderbased2a,]
    }
    
    if (input$sex2a != "All") {
      data.Q18.LIKE <- data.Q18.LIKE[data.Q18.LIKE$Gender == input$sex2a,]
    }
    
    data.Q18.LIKE <- data.Q18.LIKE[data.Q18.LIKE$Age >= input$age2a[1] & data.Q18.LIKE$Age <= input$age2a[2],]
    
    data.Q18.LIKE %>%
      select(Q18.LIKE) %>%
      drop_na(Q18.LIKE) %>%
      rename('Any other comments about junior tennis? LIKES' = Q18.LIKE)
    
  }, options = list(pageLength = 10)))
  
  
  
  
  output$ParentStatements.Q18.DISLIKE <- DT::renderDataTable(DT::datatable({
    
    data.Q18.DISLIKE <- SurveyData
    
    if (input$state2a != "All") {
      data.Q18.DISLIKE <- data.Q18.DISLIKE[data.Q18.DISLIKE$State == input$state2a,]
    }
    
    if (input$agebased2a != "All") {
      data.Q18.DISLIKE <- data.Q18.DISLIKE[data.Q18.DISLIKE$'AGE-BASED' == input$agebased2a,]
    }
    
    if (input$genderbased2a != "All") {
      data.Q18.DISLIKE <- data.Q18.DISLIKE[data.Q18.DISLIKE$'GENDER-BASED' == input$genderbased2a,]
    }
    
    if (input$sex2a != "All") {
      data.Q18.DISLIKE <- data.Q18.DISLIKE[data.Q18.DISLIKE$Gender == input$sex2a,]
    }
    
    data.Q18.DISLIKE <- data.Q18.DISLIKE[data.Q18.DISLIKE$Age >= input$age2a[1] & data.Q18.DISLIKE$Age <= input$age2a[2],]
    
    data.Q18.DISLIKE %>%
      select(Q18.DISLIKE) %>%
      drop_na(Q18.DISLIKE) %>%
      rename('Any other comments about junior tennis? DISLIKES' = Q18.DISLIKE)
    
  }, options = list(pageLength = 10)))
  
  
  
  
  output$ParentStatements.Q18.IMPROVEMENTS <- DT::renderDataTable(DT::datatable({
    
    data.Q18.IMPROVEMENTS <- SurveyData
    
    if (input$state2a != "All") {
      data.Q18.IMPROVEMENTS <- data.Q18.IMPROVEMENTS[data.Q18.IMPROVEMENTS$State == input$state2a,]
    }
    
    if (input$agebased2a != "All") {
      data.Q18.IMPROVEMENTS <- data.Q18.IMPROVEMENTS[data.Q18.IMPROVEMENTS$'AGE-BASED' == input$agebased2a,]
    }
    
    if (input$genderbased2a != "All") {
      data.Q18.IMPROVEMENTS <- data.Q18.IMPROVEMENTS[data.Q18.IMPROVEMENTS$'GENDER-BASED' == input$genderbased2a,]
    }
    
    if (input$sex2a != "All") {
      data.Q18.IMPROVEMENTS <- data.Q18.IMPROVEMENTS[data.Q18.IMPROVEMENTS$Gender == input$sex2a,]
    }
    
    data.Q18.IMPROVEMENTS <- data.Q18.IMPROVEMENTS[data.Q18.IMPROVEMENTS$Age >= input$age2a[1] & data.Q18.IMPROVEMENTS$Age <= input$age2a[2],]
    
    data.Q18.IMPROVEMENTS %>%
      select(Q18.IMPROVEMENTS) %>%
      drop_na(Q18.IMPROVEMENTS) %>%
      rename('Any other comments about junior tennis? IMPROVEMENTS' = Q18.IMPROVEMENTS)
    
  }, options = list(pageLength = 10)))
  
}

shinyApp(ui, server)

