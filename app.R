pt<-read.csv("Data Scientist Traits_October 11, 2020_07.57.csv")
head(pt)
colnames(pt)
#Create vector containing all personality questions
questions<- c("Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6","Q3_7","Q3_8","Q3_9","Q3_10",
              "Q5_1","Q5_2","Q5_3","Q5_4","Q5_5","Q5_6","Q5_7","Q5_8","Q5_9","Q5_10",
              "Q9_1","Q9_2","Q9_3","Q9_4","Q9_5","Q9_6","Q9_7","Q9_8","Q9_9","Q9_10",
              "Q11_1","Q11_2","Q11_3","Q11_4","Q11_5","Q11_6","Q11_7","Q11_8","Q11_9","Q11_10",
              "Q12_1","Q12_2","Q12_3","Q12_4","Q12_5","Q12_6","Q12_7","Q12_8","Q12_9","Q12_10")



questions
question_levels = c("1", "2", "3", "4", "5")

# remane row values
for (i in 1:length(questions)){
  pt[which(pt[,questions[i]]=='Disagree'), questions[i]] <- 1 
  pt[which(pt[,questions[i]]=='Somewhat Disagree'), questions[i]] <- 2
  pt[which(pt[,questions[i]]=='Neutral'), questions[i]] <- 3
  pt[which(pt[,questions[i]]=='Somewhat Agree'), questions[i]] <- 4
  pt[which(pt[,questions[i]]=='Agree'), questions[i]] <- 5
}

head(pt)

# works (redundant)
for (i in 1:length(questions)){
  pt[,questions[i]] <- as.numeric(pt[,questions[i]])
}
head(pt)

# loop 


library(dplyr)

pt <- pt %>% 
  mutate(Extraversion = 20 + Q3_1 - Q3_6 + Q5_1 - Q5_6 + Q9_1 - Q9_6 + Q11_1 - Q11_6 + Q12_1 - Q12_6, 
         Agreeableness= 14 - Q3_2 + Q3_7 - Q5_2 + Q5_7 - Q9_2 + Q9_7 - Q11_2 + Q11_7 + Q12_2 + Q12_7,
         Conscientiousness=  14 + Q3_3 - Q3_8 + Q5_3 - Q5_8 + Q9_3 - Q9_8 + Q11_3 - Q11_8 + Q12_3 + Q12_8,
         Neuroticism= 38 - Q3_4 + Q3_9 - Q5_4 + Q5_9 - Q9_4 - Q9_9 - Q11_4 - Q11_9 - Q12_4 - Q12_9,
         Openness= 8 + Q3_5 - Q3_10 + Q5_5 - Q5_10 + Q9_5 - Q9_10 + Q11_5 + Q11_10 + Q12_5 + Q12_10)

pt

# Taiwan showed up several times because there were some empty spaces in the string
pt$Q18 = trimws(pt$Q18)

names(pt)[15] <- "Profession" 
names(pt)[22] <- "origin_country"
names(pt)[20] <- "Gender"
names(pt)[17] <- "Sector"
names(pt)[21] <- "Bachelors"
head(pt)

country <- c("origin_country")
# remane row values
for (i in 1:length(country)){
  pt[which(pt[,country[i]]==""), country[i]] <- "Not answered" 
}


# shortening profession sectors:
profession <- c("Sector")
# remane row values
for (i in 1:length(profession)){
  pt[which(pt[,profession[i]]=="Communications"), profession[i]] <- "Comm"
  pt[which(pt[,profession[i]]=="Manufacturing"), profession[i]] <- "Mfg"
  pt[which(pt[,profession[i]]=="Government"), profession[i]] <- "Gov't"
  pt[which(pt[,profession[i]]=="Legal Services"), profession[i]] <- "Legal"
  pt[which(pt[,profession[i]]=="Supply Chain"), profession[i]] <- "SCM"
  pt[which(pt[,profession[i]]=="eCommerce"), profession[i]] <- "eComm"
  pt[which(pt[,profession[i]]=="Identity and Access Management"), profession[i]] <- "IAM"
  pt[which(pt[,profession[i]]=="Construction"), profession[i]] <- "Constrn"
  pt[which(pt[,profession[i]]=="Transportation"), profession[i]] <- "Transp"
}


# countries, but the barplot would not work for it for some reason 
library(ggplot2)

# Country of origin
ggplot(pt, aes(origin_country)) +
  geom_bar(aes(color = origin_country, fill = origin_country)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gender
mytable <- table(pt$Gender)
mytable
lbls <- paste(names(mytable), "\n", mytable, sep="")
colors = c("red", "blue", "green")
pie11 <- pie(mytable, labels = lbls, col = colors,
             main="Pie Chart of Gender \n of BAIM Graduates")

library(dplyr)
library(tidyr)
by_edu <- pt[order(pt$Bachelors), ]
by_edu
pt

# Bachelors
ggplot(by_edu, aes(Bachelors)) +
  geom_bar(aes(color = origin_country, fill = origin_country)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "")+
  ggtitle("Undergraduate Background of BAIM Students")



# Extraversion
Extravesion_col <- pt %>%
  group_by(Profession, Sector)%>%
  mutate(Extraversion_avg = mean(Extraversion),
         Agreeableness_avg = mean(Agreeableness),
         Conscientiousness_avg = mean(Conscientiousness),
         Neuroticism_avg = mean(Neuroticism),
         Openness_avg = mean(Openness)) %>%
  distinct(Extraversion_avg) %>%
  arrange(desc(Extraversion_avg))

Extravesion_col
Extravesion_col<-as.data.frame(Extravesion_col)
Extravesion_col

extraversion <- ggplot(Extravesion_col, aes(x = as.factor(Profession), y = Extraversion_avg, group = Sector)) +
  geom_col(aes(fill = Sector), color="black") +
  geom_text(aes(label = paste(round(Extraversion_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
  labs(y = "Average Extraversion by Profession", x="")
extraversion


# Agreeableness
Agreeableness_col <- pt %>%
  group_by(Profession, Sector)%>%
  mutate(Extraversion_avg = mean(Extraversion),
         Agreeableness_avg = mean(Agreeableness),
         Conscientiousness_avg = mean(Conscientiousness),
         Neuroticism_avg = mean(Neuroticism),
         Openness_avg = mean(Openness)) %>%
  distinct(Agreeableness_avg) %>%
  arrange(desc(Agreeableness_avg))

Agreeableness_col
Agreeableness_col<-as.data.frame(Agreeableness_col)
Agreeableness_col

agreeableness <- ggplot(Agreeableness_col, aes(x = as.factor(Profession), y = Agreeableness_avg, group = Sector)) +
  geom_col(aes(fill = Sector), color="black") +
  geom_text(aes(label = paste(round(Agreeableness_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
  labs(y = "Average Agreeableness by Profession", x="")
agreeableness

# Conscientiousness
Conscientiousness_col <- pt %>%
  group_by(Profession, Sector)%>%
  mutate(Extraversion_avg = mean(Extraversion),
         Agreeableness_avg = mean(Agreeableness),
         Conscientiousness_avg = mean(Conscientiousness),
         Neuroticism_avg = mean(Neuroticism),
         Openness_avg = mean(Openness)) %>%
  distinct(Conscientiousness_avg) %>%
  arrange(desc(Conscientiousness_avg))

Conscientiousness_col
Conscientiousness_col<-as.data.frame(Conscientiousness_col)
Conscientiousness_col

conscientiousness <- ggplot(Conscientiousness_col, aes(x = as.factor(Profession), y = Conscientiousness_avg, group = Sector)) +
  geom_col(aes(fill = Sector), color="black") +
  geom_text(aes(label = paste(round(Conscientiousness_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
  labs(y = "Average Conscientiousness by Profession", x="")
conscientiousness

#Neuroticism
Neuroticism_col <- pt %>%
  group_by(Profession, Sector)%>%
  mutate(Extraversion_avg = mean(Extraversion),
         Agreeableness_avg = mean(Agreeableness),
         Conscientiousness_avg = mean(Conscientiousness),
         Neuroticism_avg = mean(Neuroticism),
         Openness_avg = mean(Openness)) %>%
  distinct(Neuroticism_avg) %>%
  arrange(desc(Neuroticism_avg))

Neuroticism_col
Neuroticism_col<-as.data.frame(Neuroticism_col)
Neuroticism_col

neuroticism <- ggplot(Neuroticism_col, aes(x = as.factor(Profession), y = Neuroticism_avg, group = Sector)) +
  geom_col(aes(fill = Sector), color="black") +
  geom_text(aes(label = paste(round(Neuroticism_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
  labs(y = "Average Neuroticism by Profession", x="")
neuroticism

#Openness
Openness_col <- pt %>%
  group_by(Profession, Sector)%>%
  mutate(Extraversion_avg = mean(Extraversion),
         Agreeableness_avg = mean(Agreeableness),
         Conscientiousness_avg = mean(Conscientiousness),
         Neuroticism_avg = mean(Neuroticism),
         Openness_avg = mean(Openness)) %>%
  distinct(Openness_avg) %>%
  arrange(desc(Openness_avg))

Openness_col
Openness_col<-as.data.frame(Openness_col)
Openness_col

openness <- ggplot(Openness_col, aes(x = as.factor(Profession), y = Openness_avg, group = Sector)) +
  geom_col(aes(fill = Sector), color="black") +
  geom_text(aes(label = paste(round(Openness_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
  labs(y = "Average Openness by Profession", x="")
openness


##### APP PART ######

ui <- fluidPage(
  
  headerPanel('Personality Test Findings, Purdue University BAIM Program'),
  
  sidebarPanel(
    
    selectInput('Personality', 'Personality Traits', c('Extraversion', 'Openness', 'Agreeablesness', 'Conscientiousness', 'Neuroticism' ),selected="Extraversion",multiple=FALSE),
    
  ),
  
  mainPanel(
    plotOutput('selected_graph'),
    plotOutput('plot8'),
    plotOutput('plot6'),
    plotOutput('plot7'),
    h3("Statistical Summary of Extraversion"),
    verbatimTextOutput("stats1"),
    h3("Statistical Summary of Agreeableness"),
    verbatimTextOutput("stats2"),
    h3("Statistical Summary of Conscientiousness"),
    verbatimTextOutput("stats3"),  
    h3("Statistical Summary of Neuroticism"),
    verbatimTextOutput("stats4"), 
    h3("Statistical Summary of Openness"),
    verbatimTextOutput("stats5"),     
    
    dataTableOutput('table1'),

    
    
  )
  
)


server <- function(input, output) {
  
  
  
  tableSelectedData <- reactive({
    pt
  })
  
  plotSelectedData <- reactive({
    
    pt
    
  })
  
  plot1<-reactive({Extravesion_col <- pt %>%
    group_by(Profession, Sector)%>%
    mutate(Extraversion_avg = mean(Extraversion),
           Agreeableness_avg = mean(Agreeableness),
           Conscientiousness_avg = mean(Conscientiousness),
           Neuroticism_avg = mean(Neuroticism),
           Openness_avg = mean(Openness)) %>%
    distinct(Extraversion_avg) %>%
    arrange(desc(Extraversion_avg))
  
  Extravesion_col
  Extravesion_col<-as.data.frame(Extravesion_col)
  Extravesion_col
  
  extraversion <- ggplot(Extravesion_col, aes(x = as.factor(Profession), y = Extraversion_avg, group = Sector)) +
    geom_col(aes(fill = Sector), color="black") +
    geom_text(aes(label = paste(round(Extraversion_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
    labs(y = "Average Extraversion by Profession", x="")
  extraversion})
  
  plot2<-reactive({Agreeableness_col <- pt %>%
    group_by(Profession, Sector)%>%
    mutate(Extraversion_avg = mean(Extraversion),
           Agreeableness_avg = mean(Agreeableness),
           Conscientiousness_avg = mean(Conscientiousness),
           Neuroticism_avg = mean(Neuroticism),
           Openness_avg = mean(Openness)) %>%
    distinct(Agreeableness_avg) %>%
    arrange(desc(Agreeableness_avg))
  
  Agreeableness_col
  Agreeableness_col<-as.data.frame(Agreeableness_col)
  Agreeableness_col
  
  agreeableness <- ggplot(Agreeableness_col, aes(x = as.factor(Profession), y = Agreeableness_avg, group = Sector)) +
    geom_col(aes(fill = Sector), color="black") +
    geom_text(aes(label = paste(round(Agreeableness_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
    labs(y = "Average Agreeableness by Profession", x="")
  agreeableness})
  
  
  plot3<-reactive({Conscientiousness_col <- pt %>%
    group_by(Profession, Sector)%>%
    mutate(Extraversion_avg = mean(Extraversion),
           Agreeableness_avg = mean(Agreeableness),
           Conscientiousness_avg = mean(Conscientiousness),
           Neuroticism_avg = mean(Neuroticism),
           Openness_avg = mean(Openness)) %>%
    distinct(Conscientiousness_avg) %>%
    arrange(desc(Conscientiousness_avg))
  
  Conscientiousness_col
  Conscientiousness_col<-as.data.frame(Conscientiousness_col)
  Conscientiousness_col
  
  conscientiousness <- ggplot(Conscientiousness_col, aes(x = as.factor(Profession), y = Conscientiousness_avg, group = Sector)) +
    geom_col(aes(fill = Sector), color="black") +
    geom_text(aes(label = paste(round(Conscientiousness_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
    labs(y = "Average Conscientiousness by Profession", x="")
  conscientiousness})
  
  plot4<-reactive({Neuroticism_col <- pt %>%
    group_by(Profession, Sector)%>%
    mutate(Extraversion_avg = mean(Extraversion),
           Agreeableness_avg = mean(Agreeableness),
           Conscientiousness_avg = mean(Conscientiousness),
           Neuroticism_avg = mean(Neuroticism),
           Openness_avg = mean(Openness)) %>%
    distinct(Neuroticism_avg) %>%
    arrange(desc(Neuroticism_avg))
  
  Neuroticism_col
  Neuroticism_col<-as.data.frame(Neuroticism_col)
  Neuroticism_col
  
  neuroticism <- ggplot(Neuroticism_col, aes(x = as.factor(Profession), y = Neuroticism_avg, group = Sector)) +
    geom_col(aes(fill = Sector), color="black") +
    geom_text(aes(label = paste(round(Neuroticism_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
    labs(y = "Average Neuroticism by Profession", x="")
  neuroticism})
  
  plot5<-reactive({Openness_col <- pt %>%
    group_by(Profession, Sector)%>%
    mutate(Extraversion_avg = mean(Extraversion),
           Agreeableness_avg = mean(Agreeableness),
           Conscientiousness_avg = mean(Conscientiousness),
           Neuroticism_avg = mean(Neuroticism),
           Openness_avg = mean(Openness)) %>%
    distinct(Openness_avg) %>%
    arrange(desc(Openness_avg))
  
  Openness_col
  Openness_col<-as.data.frame(Openness_col)
  Openness_col
  
  openness <- ggplot(Openness_col, aes(x = as.factor(Profession), y = Openness_avg, group = Sector)) +
    geom_col(aes(fill = Sector), color="black") +
    geom_text(aes(label = paste(round(Openness_avg,0),"=",Sector)), position = position_stack(vjust = 0.5), colour = "black") +
    labs(y = "Average Openness by Profession", x="")
  openness})
  
  graphInput<-reactive({
    switch(input$Personality,
           "Extraversion" = plot1(),
           "Agreeablesness" = plot2(),
           "Conscientiousness"=plot3(),
           "Neuroticism"=plot4(),
           "Openness"=plot5()
    )
  })
  
  output$selected_graph <- renderPlot({
    graphInput()
  })
  
  output$stats1 <-renderPrint({
    summary(pt$Extraversion)
  })
  
  output$stats2 <-renderPrint({
    summary(pt$Agreeableness)
  })
  
  output$stats3 <-renderPrint({
    summary(pt$Conscientiousness)
  })
  
  output$stats4 <-renderPrint({
    summary(pt$Neuroticism)
  })
  
  output$stats5 <-renderPrint({
    summary(pt$Extraversion)
  })
  
  # third way
  #if(input$Personality == "Extraversion") {
  #  plot1()
  #}
  #else if(input$Personality == "Agreeableness") {
  #  plot2()
  #}
  
  #switch(input$Personality,
  #       "Extraversion" = plot1(),
  #       "Openness" = plot2(),
  #       "" = c(plot1(), plot2())
  #)
  
  # second way
  #ifelse(input$Personality == "Extraversion", plot1(), 0)
  #ifelse(input$Personality == "Agreeableness", plot2(), 0)
  
  #first way
  #switch(input$Personality, 
  #       "Extraversion" = plot1(),
  #       "Agreeableness" =plot2())
  #})
  
  # Gender
  output$plot6 <- renderPlot({
    mytable <- table(pt$Gender)
    lbls <- paste(names(mytable), "\n", mytable, sep="")
    colors = c("red", "blue", "green")
    pie11 <- pie(mytable, labels = lbls, col = colors,
                 main="Pie Chart of Gender \n of BAIM Graduates")
  })
  
  # Country of Origin
  output$plot7 <- renderPlot({
    ggplot(pt, aes(origin_country)) +
      geom_bar(aes(color = origin_country, fill = origin_country)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Bachelor's
  output$plot8 <- renderPlot({# Bachelors
    ggplot(by_edu, aes(Bachelors)) +
      geom_bar(aes(color = origin_country, fill = origin_country)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "")+
      ggtitle("Undergraduate Background of BAIM Students")})
  
  
  output$table1 <- renderDataTable({
    data.frame(tableSelectedData())
  })
  
}


shinyApp(ui = ui, server = server)
