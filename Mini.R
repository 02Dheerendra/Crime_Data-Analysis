library(shiny)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(treemap)
library(dplyr)
library(reshape2)
library(plyr)
library(devtools)

data <- read.csv("E:/dheerendra/crime-data-analysis-in-India-main/data.csv")
w <- read.csv("E:/dheerendra/crime-data-analysis-in-India-main/crime against women.csv", row.names=NULL)
tn <- read.csv("E:/dheerendra/crime-data-analysis-in-India-main/data/taminadu.csv")
tn_new <- read.csv("E:/dheerendra/crime-data-analysis-in-India-main/data/taminadu1.csv")

con <- select(data,State,Year,Murder,Dowry.Deaths,Hurt,Assault.on.Women,Sexual.Harassment,Stalking,Kidnapping.and.Abduction,Rape,Theft,Circulate.False.Fake.News.Rumours,Insult.to.the.Modesty.of.Women)
contn <- select(tn,District,Year,Hurt,Offences.Affecting.the.Human.Body,Theft,Rash.Driving.on.Public.way,Miscellaneous.IPC.Crimes,Total.Cognizable.IPC.crimes,Robbery,Obstruction.on.Public.way,Assault.on.Women,Cruelty.by.Husband.or.his.Relatives,Rape,Circulate.False.Fake.News.Rumours,Theft)

tn_total <- select(tn_new,District,Year,Hurt,Offences.Affecting.the.Human.Body,Theft,Rash.Driving.on.Public.way,Miscellaneous.IPC.Crimes,Total.Cognizable.IPC.crimes,Robbery,Obstruction.on.Public.way,Assault.on.Women,Cruelty.by.Husband.or.his.Relatives,Rape,Circulate.False.Fake.News.Rumours,Theft)

years_total_df <- con[,2:13] %>% group_by(Year) %>% summarize_all(funs(sum))
colnames(years_total_df) <- c("Year", "Murder","Dowry","Hurt", "Assaultw", "SexHar","Stalking", "KidnapAbd","Rape","Theft","Fakenews","Insultw")

years_total_df$Crime <- rowSums(years_total_df[ , c(2:12)],na.rm=TRUE)

yearly_crime_df <- data.frame(Year=integer(), Crime=integer(), Number=integer())
for (row in 1:nrow(years_total_df)){
  year <- years_total_df[row, "Year"]
  murder <- years_total_df[row, "Murder"]
  dowry <- years_total_df[row, "Dowry"]
  hurt <- years_total_df[row, "Hurt"]
  assaultw <- years_total_df[row, "Assaultw"]
  sexhar <- years_total_df[row, "SexHar"]
  stalking <- years_total_df[row, "Stalking"]
  kidnapabd <- years_total_df[row, "KidnapAbd"]
  rape <- years_total_df[row, "Rape"]
  theft <- years_total_df[row, "Theft"]
  fakenews <- years_total_df[row, "Fakenews"]
  insultw <- years_total_df[row, "Insultw"]
  
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Murder", murder)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Dowry", dowry)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Hurt", hurt)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Assaultw", assaultw)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "SexHar", sexhar)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Stalking", stalking)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "KidnapAbd", kidnapabd)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Rape", rape)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Theft", theft)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Fakenews", fakenews)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Insultw", insultw)
  
}

bar_chart_total_crimes <- ggplot(data=years_total_df, aes(x=Year, y=Crime, fill=factor(Year))) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=Crime), vjust=2.6, color="white", size=10) 
labs(fill = "Year")

# ------------ TAMIL NADU ------------#

#Hurt cases
tn1 <- contn %>%
  plot_ly(
    x = ~Hurt, 
    y = ~District, 
    size = ~Hurt, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Hurt Cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Theft cases
tn3 <- contn %>%
  plot_ly(
    x = ~Theft, 
    y = ~District, 
    size = ~Theft, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Cases of Theft: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Obstruction on Public way cases
tn6 <- contn %>%
  plot_ly(
    x = ~Obstruction.on.Public.way, 
    y = ~District, 
    size = ~Obstruction.on.Public.way, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>  #Obstruction on Public way cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )
#Total IPC cases
tn8 <- contn %>%
  plot_ly(
    x = ~Total.Cognizable.IPC.crimes, 
    y = ~District, 
    size = ~Total.Cognizable.IPC.crimes, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Cases of Offences Affecting the Human Body: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )
#-------------------------------------# 
#Murder Cases
f1 <- data %>%
  plot_ly(
    x = ~Murder, 
    y = ~State, 
    size = ~Murder, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Murder Cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )
#Dowry Deaths
x1 <- data$Dowry.Deaths
f2 <- data %>%
  plot_ly(
    x = ~x1, 
    y = ~State, 
    size = ~x1, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Dowry Deaths: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )
#Hurt
f3 <- data %>%
  plot_ly(
    x = ~Hurt, 
    y = ~State, 
    size = ~Hurt, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Hurt: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Assault on Women
x2 <- data$Assault.on.Women
f4 <- data %>%
  plot_ly(
    x = ~x2, 
    y = ~State, 
    size = ~x2, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Assault on Women: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Sexual Harassment
x4 <- data$Sexual.Harassment
f5 <- data %>%
  plot_ly(
    x = ~x4, 
    y = ~State, 
    size = ~x4, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Sexual Harassment: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )


#Kidnapping and Abduction
x5 <- data$Kidnapping.and.Abduction
f7 <- data %>%
  plot_ly(
    x = ~x5, 
    y = ~State, 
    size = ~x5, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Kidnapping and Abduction: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )
#Rape
x6 <- data$Rape
f8 <- data %>%
  plot_ly(
    x = ~x6, 
    y = ~State, 
    size = ~x6, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Rape: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Theft
x7 <- data$Theft
f9 <- data %>%
  plot_ly(
    x = ~x7, 
    y = ~State, 
    size = ~x7, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Theft: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )
ui <- dashboardPage(
  dashboardHeader(title = "Crime Data Analysis"),
  dashboardSidebar( sidebarMenu(
    menuItem("Description", tabName = "home", icon = icon("home")),
    menuItem("Tamil Nadu", tabName = "tn", icon = icon("bar-chart-o")),
    menuItem("Data Analysis", tabName = "data", icon = icon("play")),
    menuItem("Conclusion",tabName = "inference",icon=icon("table"))
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName ="home",tags$h1("CRIME DATA ANALYSIS"),
        tags$h4("Crime is a very old concept and it is transmitted to the society from generation to generation. Crime produces law and order situation. It is a social evil. It is generated by the society and the society also suffers a lot because of crime committed by its members. The rising wave of crime to-day has caused alarm in the public.
                With the rapid improvement of lifestyle and urbanization, the graph of crimes is also on the increase.
                As the report says that India has high illiteracy rate, high population density, low job opportunities, these have become one of the reasons for high crime rate in India. Due to different problems in every state, different states have different crime rate. In this project, we analyze the patterns of crime performed on a dataset with demographic information of crime in India(State-Wise & District-Wise)."),
        tags$br(),
        
        tags$h2("DATASET"),
        tags$h4("First we obtained the dataset from the",tags$strong("NCRB Website"),"We performed data cleaning and data wrangling."),
        tags$h4("Then we performed the exploratory data analysis on the datasets that we cleaned."),
        tags$br(),
        tags$h2("About Project"),
        tags$h4("Citizen in today's modern world wants to live in a safe environment and neighborhood.
However it is a known fact that crime in some form, exists in our society. Although
we cannot control what goes on around us, we can definitely try to take a few steps to
aid the government and police authorities in trying to control it.Hence, taking inspiration from the facts stated above, we decided to
process this data provided and analyze it to identify the trends in crime over the years")
        
      ),
      tabItem(
        tabName ="tn",tags$h1("Tamil Nadu: District-wise Analysis"),
        fluidRow(column(9,selectInput("cr1","Choose the Crime:",choices=c("Hurt","Obstruction on public way","Theft","Total IPC Crimes"
        )))),
        fluidRow(plotlyOutput("Plot3"),
                 
                 tags$br(),
        ),
        fluidRow(
          box(
            title = paste("Individual Crime Type over the Years 2017-2020") , status ="success", background="black", width = 15,
            plotlyOutput(("linegraph1"), height="500px")
          ) 
        )
      ),
      tabItem(tabName = "data",tags$h1("Exploratory Data Analysis : Plots"),
              fluidRow(
                
                box(
                  width = 6, height = "428px",
                  
                  valueBox(29, "States", icon = icon("landmark"), color="blue"),
                  
                  valueBox(8, "Union Territories", icon = icon("bell"), color = "olive"),  
                  
                  valueBox(11, "Crime Types", icon = icon("gopuram"), color = "yellow"),  
                  
                  
                  tags$br(),
                  
                  tags$h4("We have analyzed over 11 crime types across all the States and Union Territories of India. We have also analyzed the crime types over these years individually."),tags$br(),
                  
                ),
                
                box(
                  title = paste("Individual Crime Type over the Years 2017-2020") , status ="success", background="black", width = 6,
                  plotlyOutput(("linegraph"), height="365px")
                )
              ),
              fluidRow(
                box(
                  title = "Crime over the Years", status ="success", width= 6,
                  plotOutput(outputId = "barchart", height = "350px", hover="plot_hover"), tags$br(),
                  tags$h4("This bar plot shows the total number of cases over the 4 years. We can see that the cases were increased steadily from 2017 to 2019 and decreased in 2020 due to the pandemic year.")
                ),
                box(
                  title = paste("Crime Breakdown") , status ="success", width =6,
                  plotOutput(outputId = "treemap", height = "350px"), tags$br(),
                  tags$h4("The treemap plot gives us an idea about the majorly occuring crime, we can see that theft cases is the highest, second highest is hurt, third highest is kidnapping and abduction and so on.")
                )
              ),
              fluidRow(column(9,selectInput("cr","Choose the Crime:",choices=c("Murder","Dowry","Hurt","Assault on Women",
                                                                               "Sexual Harassment","Kidnapping and Abduction",
                                                                               "Rape","Theft")))),
              fluidRow(plotlyOutput("Plot1")),tags$br(),
      ),
      tabItem(
        tabName = "inference",tags$h1("CONCLUSION"),
        tags$h4("We conclude by saying that with all the visualizations it can be noted that the pandemic year i.e",tags$strong("2020"),"has reduced the number of crimes occuring.",tags$br(),"But it should be noted that some of the crimes like murder and hurt has increased and cases due to dowry deaths, assault on women, sexual harassment, stalking, kidnapping and abduction, rape, theft decreased in the pandemic period (2020) due to the lockdown imposed in the country."),
      )
    ))
  ,skin=c("purple"))

server <- function(input, output,session) { 
  output$linegraph <- renderPlotly({
    years_total_df%>%plot_ly(x=~Year)%>%
      add_lines(y=years_total_df$Crime,name="Total Crimes",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Murder,name="Murder",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Hurt,name="Hurt",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Assaultw,name="Assault on women",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$SexHar,name="Sexual Harassment",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$KidnapAbd,name="Kidnapping and Abduction",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Rape,name="Rape",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Theft,name="Theft",line= list(shape = "line"))})
  output$linegraph1 <- renderPlotly({
    tn_total%>%plot_ly(x=~Year)%>%
      add_lines(y=tn_total$Miscellaneous.IPC.Crimes,name="Total Crimes",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Theft,name="Theft",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Hurt,name="Hurt",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Robbery,name="Robbery",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Assault.on.Women,name="Assault on women",line= list(shape = "line")) %>%
      add_lines(y=tn_total$Cruelty.by.Husband.or.his.Relatives,name="Cruelty by husbands",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Rape,name="Rape",line= list(shape = "line"))
      })
  output$barchart <- renderPlot(bar_chart_total_crimes)
  output$treemap <- renderPlot({
    total_year <- yearly_crime_df %>% group_by(`Crime`) %>% summarize_all(funs(sum), na.rm=TRUE)
    #print(crime_total_df)
    print(total_year)
    ggplot(total_year, aes(fill= `Crime`, area=`Number`)) + 
      geom_treemap() +
      geom_treemap_text(colour = "white", place="centre", label=paste(total_year$Crime,": ",total_year$Number)) +
      labs(title=paste("Aggregated Crime Distribution for 2017-2020")) +
      theme(legend.position="right")  +
      scale_fill_brewer(palette="Paired")
  })
  output$Plot1<- renderPlotly({
    if(input$cr=="Murder"){
      subplot(f1,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Dowry"){
      subplot(f2,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Hurt"){
      subplot(f3,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Assault on Women"){
      subplot(f4,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Sexual Harassment"){
      subplot(f5,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Kidnapping and Abduction"){
      subplot(f7,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Rape"){
      subplot(f8,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Theft"){
      subplot(f9,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
  }
  )
  output$Plot3<- renderPlotly({
    if(input$cr1=="Hurt"){
      subplot(tn1,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Obstruction on public way"){
      subplot(tn6,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Theft"){
      subplot(tn3,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Total IPC Crimes"){
      subplot(tn8,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
  }
  )
  output$Plot2<- renderPlotly({
    subplot(ww,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
  }
  )
  
}
shinyApp(ui, server)
