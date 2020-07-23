library(ggthemes)
library(ggrepel)
library(shiny)
library(shinyWidgets)
library(shinydashboard) 
library(shinydashboardPlus) 
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(leaflet)
library(DT)
library(plotly)
library(countrycode)
library(tidyverse)
library(scales) 
library(shinythemes)
library(maps)

#load data
covid_ny_url = "https://health.data.ny.gov/api/views/xdss-u53e/rows.csv"
data <- read_csv(covid_ny_url)
colnames(data) <- c("Test.Date", "County","New.Positives","Cumulative.Number.of.Positives", 
                    "Total.Number.of.Tests.Performed","Cumulative.Number.of.Tests.Performed")
counties <-map_data("county")
counties[counties == "st lawrence"] <- "st. lawrence"
ny_county <- subset(counties, region=="new york")

#convert data type
data$Test.Date <- as.Date(data$Test.Date, format="%m/%d/%Y")

#load data Malaria
covid_death_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
covid_cases_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
malaria_incidence_dir = './data/number-of-malaria-cases-per-100000-individuals.csv'
malaria_death_dir = './data/death-rates-malaria.csv'
pop_dir = './data/total_pop.csv'
pop_den_dir = './data/pop_den.csv'

read_data <- function(dir, covid){
  if (covid) {
    df = read_csv(dir) %>%
      group_by(`Country/Region`) %>%
      summarise_if(is.numeric, list(sum=sum)) %>%
      # select(-c('Lat_sum', 'Long_sum')) %>%
      # mutate(sum = rowSums(.[-1])) %>%
      select_at(c(1,ncol(.))) %>%
      rename_with(~gsub("_sum", "", .x, fixed = FALSE))
  }
  else {
    df = read_csv(dir, col_names = TRUE) 
    names(df) = c('Entity', 'Code', 'Year', 'Rate')
    if('World' %in% df$Entity) {
      df = drop_na(df, Code)
    }
  }
  return(df)
}

parse_data <- function(death_dir, case_dir, covid){
  if (covid){
    covid_deaths = read_data(death_dir, TRUE)
    covid_cases = read_data(case_dir, TRUE)
    covid_full = inner_join(covid_deaths, covid_cases, 'Country/Region') %>%
      rename_with(~gsub("\\.x", "_deaths", .x, fixed = FALSE)) %>%
      rename_with(~gsub("\\.y", "_cases", .x, fixed = FALSE))
    
    for (i in c(pop_dir, pop_den_dir)) {
      dt = read_csv(i, skip = 4) %>%
        select(c('Country Code', 'Country Name','2018'))
      name = gsub('./data/', '', gsub('.csv', '', i))
      assign(name, dt)
    }
    
    covid_full = covid_full %>%
      # generate country codes based on country names to match `pop` 
      mutate(country_code = countrycode(covid_full$`Country/Region`, 'country.name', 'iso3c')) %>%
      inner_join(total_pop, c('country_code'='Country Code')) %>%
      select(-c('Country Name')) %>%
      rename('population' = '2018') %>%
      # 3rd column is the death total
      # 4th column is the case total
      # 5th column is the country's population
      mutate(incidence_rate = .[[3]]/.[[5]]*100000) %>%
      mutate(death_rate = .[[2]]/.[[5]]*100000) %>%
      inner_join(pop_den, c('country_code'='Country Code')) %>%
      rename('population_density' = '2018')
    return(covid_full)
  }
  
  else {
    malaria_death = read_data(death_dir, FALSE) %>%
      group_by(Code) %>% 
      filter(Year == max(Year))
    
    malaria_incidence = read_data(case_dir, FALSE) %>%
      group_by(Code) %>%
      filter(Year == max(Year))
    
    malaria_full = full_join(malaria_incidence, malaria_death, c('Code')) %>%
      group_by(Code) %>% 
      mutate(Entity.y=replace(Entity.y, Entity.x=='Korea, South', 'South Korea')) %>%
      select(-'Entity.x') %>%
      rename('incidence_rate' = 'Rate.x') %>%
      rename('death_rate' = 'Rate.y') %>%
      rename('Entity' = 'Entity.y') %>%
      ungroup()
    
    return(malaria_full)
  }
}
malaria_full = parse_data(malaria_incidence_dir, malaria_death_dir, FALSE)
covid_full = parse_data(covid_death_url, covid_cases_url, TRUE)
full_df = full_join(malaria_full, covid_full, by = c('Code'='country_code'), suffix = c("_malaria", "_covid")) %>%
  select(-c('Entity')) %>%
  select(-ends_with(c('_cases', '_deaths')))


##############################################################
######################Shiny Dashboard#########################
##############################################################

ui <- dashboardPagePlus(
  skin = "blue",
  header = dashboardHeaderPlus(title = "COVID-19", 
                               enable_rightsidebar = TRUE, rightSidebarIcon = "gears"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "page1", icon = icon("info")),
      menuItem("Maps of New York", tabName = "page2", icon = icon("map-o")),
      menuItem("Comparisons to Malaria ", tabName = "page3", icon = icon("map-o")),
      menuItem("Data", tabName = "page4", icon = icon("database"))
    )
  ),
  body = dashboardBody(
    tags$head(tags$style("section.content {overflow-y: hidden;}")),
    tabItems(
      tabItem(tabName = "page1",
              img(src = 'im.png', height = '260px', width = '1000px'),
              column(width = 6,
                     box(
                       title = "Project Description", width = NULL, solidHeader = TRUE, 
                       status = "primary",
                       "The COVID-19 pandemic is the defining global health crisis of our 
                       time and the greatest challenge we have faced since World War Two. 
                       The geographic distribution of coronavirus is thus brought into 
                       sharper focus. Among the worst hit (up until when we did the project) 
                       was New York State. Thus, it is meaningful to see how each county 
                       within the state was impacted across time. Furthermore, COVID-19’s 
                       correlations (in terms of mortality rates and prevalence) with infectious 
                       diseases such as Malaria come into question. As a result, we have 
                       developed some visualizations to directly illustrate the varying 
                       mortality rates and incidence rates of COVID-19 and Malaria across 
                       different countries."
                     ),
                     box(
                       title = "Description of Datasets", width = NULL, solidHeader = TRUE, 
                       status = "primary",
                       "When constructing the first dashboard, we use the New York State Statewide 
                       COVID-19 Testing dataset, which is provided by the New York State Department 
                       of Health. It includes information on the number of tests of individuals for 
                       COVID-19 infections performed in New York State beginning March 1, 2020, when 
                       the first case of COVID-19 was identified in the state.",br(),br(),
                       "When constructing the second dashboard, we use several datasets. The COVID-19 
                       datasets are provided by the Center for Systems Science and Engineering (CSSE) 
                       at Johns Hopkins University, which contain the time series data of confirmed 
                       cases and deaths of coronavirus by country/region. The Malaria datasets are 
                       provided by the research titled Malaria of Max Roser and Hannah Ritchie (2013), 
                       which contain the number of Malaria cases per 100,000 individuals and death rates 
                       of countries around the world. The total population and population density datasets 
                       are provided by the United Nations Department of Economic and Social Affairs. We
                       used the most recent available data for each country. 2017 is the most recent year 
                       for all countries in terms of incidence rates, while each country’s most recent year 
                       is different in terms of death rates."
                     ),
              ),
              column(width = 6,
                     box(
                       title = "Research Question 1: How are the COVID-19 cases distributed in the New York State?",status = "primary", width = NULL,
                       "Since the New York State has been one of the states that suffered the most from the outbreak, 
                       we decide to plot choropleth maps to visualize a series of coronavirus data. The first dashboard 
                       shows the cumulative positive cases, new positive cases, total number of tests performed, and 
                       cumulative number of tests performed in the state."
                     ),
                     box(
                       title = "Research Question 2. How does the COVID-19 distribution correlate 
                       with that of Malaria?", width = NULL, status = "primary",
                       "As the COVID-19 pandemic spreads rapidly around the globe, there is an urgent need 
                       to aggressively tackle the novel coronavirus while ensuring that other killer diseases, 
                       such as Malaria, are not neglected. For the benefit of jointly addressing those two types 
                       of diseases, the second dashboard displays the death rates and incidence per 1,000,000 people
                       of COVID-19 and Malaria on a world map, and a scatter plot shows their correlation directly."
                     )
              ),
              tabBox(
                side = "left", height = "130px",
                selected = "Methodology",
                tabPanel("Methodology", "Data Cleaning: convert data type, merge data into a single data frame. 
                Data Exploration: Geom_mapping, Geom_scatterplot"),
                tabPanel("References", 
                         tags$a(href="https://health.data.ny.gov/Health/New-York-State-Statewide-COVID-19-Testing/xdss-u53e/data",
                                target="_blank",
                                "The official website of New York state"),
                         br(),
                         tags$a(href="https://github.com/CSSEGISandData/COVID-19",
                                target="_blank",
                                "Github"),
                         br(),
                         tags$a(href="https://ourworldindata.org/malaria",
                                target="_blank",
                                "Our World in Data"),
                         br(),
                         tags$a(href="https://population.un.org/wpp/Download/Standard/CSV/",
                                target="_blank",
                                "United Nations")
                         ),
                tabPanel("Contact Information", 
                         "Li Jiang",
                         tags$a(href="mailto:ljiang38@jhu.edu",
                                target="_blank",
                                "ljiang38@jhu.edu"),
                        br(),
                         "Yilin Liu",
                         tags$a(href="mailto:yliu321@jhu.edu",
                                target="_blank",
                                "yliu321@jhu.edu"),
                         br(),
                        "Zhijun Lu",
                        tags$a(href="mailto:zlu28@jhu.edu",
                               target="_blank",
                               "zlu28@jhu.edu"),
                        br(),
                        "Mingyu Yan",
                        tags$a(href="mailto:myan8@jhu.edu",
                               target="_blank",
                               "myan8@jhu.edu"),
                        br()
                         )
              ),
              img(src = "logo.png", height = '162px', width = '422px'),
              ),
      tabItem(tabName = "page2",
              img(src = 'nyc660.jpg', height = '220px', width = '330px'),
              img(src = 'download.jpeg', height = '220px', width = '330px'),
              img(src = 'image.jpg', height = '220px', width = '330px'),
              br(),br(),
              radioGroupButtons(inputId = "Selection1",label = "Covid-19 New York Maps",
              choices = c("Cumulative Positive Cases",
                          "New Positive Cases", 
                          "Cumulative Number of Tests Performed", 
                          "New Total Number of Tests Performed"),
                individual = TRUE,
                direction = "vertical"
              ),
              dateInput("Test.Date", "Date:", format = "yyyy-mm-dd"),
              h4("Hover your mouse cursor over to see number of each region"),
              plotlyOutput("plot2", height = 500)
              ),
      tabItem(tabName = "page3",
              radioGroupButtons(inputId = "Selection2",label = "Comparison Maps",
                                choices = c("Malaria Incidence Rate", 
                                            "Malaria Death Rate", 
                                            "Covid-19 Incidence Rate",
                                            "Covid-19 Death Rate"),
                                individual = TRUE,
                                direction = "vertical"
              ),
              plotlyOutput("plot3", height = 300),
              radioGroupButtons(inputId = "Selection3",label = "Comparison Scatter Plots",
                                choices = c("Malaria Incidence Rate vs. Covid-19 Incidence Rate", 
                                            "Malaria Death Rate vs. Covid-19 Death Rate"),
                                individual = TRUE,
                                direction = "vertical"
              ),
              plotlyOutput("plot4", height = 400),
      ),
      tabItem(tabName = "page4",
              h4("Maps of New York-Data Source"),
              dataTableOutput("myTable1", height = 510),
              h4("Comparisons to Malaria-Data Source"),
              dataTableOutput("myTable2", height = 510)
      )
    )
  ),
  title = "DashboardPage"
)

server <- function(input, output, session) {

  output$plot2 = renderPlotly({
    #merge data into single df
    test <- subset(data, Test.Date == input$Test.Date)
    test <- test %>% mutate(subregion=tolower(County))
    ny_test <- merge(ny_county, test, by="subregion", all.x = TRUE)
    ny_test = ny_test[order(ny_test$order),]
    
    #create map
    if (input$Selection1=="Cumulative Positive Cases"){
      p <- ny_test %>% 
        ggplot(mapping = aes(x = long, y = lat, group = group, text=paste(County, ": ",Cumulative.Number.of.Positives))) +
        coord_equal() +
        theme_map() +
        geom_polygon(color = "black") +
        annotate(geom = "text", x = -78, y = 44.5, size = 5, colour = "grey80", label = input$Test.Date)
      
      p = p + 
        aes(fill = Cumulative.Number.of.Positives) + 
        scale_fill_gradient(
          low = "white", 
          high = "#CB454A",
          limit=c(0,67000),
          labels = comma) + 
        labs(title = "Cumulative Positive Cases")
    }
    
    else if (input$Selection1=="New Positive Cases"){
      p <- ny_test %>% 
        ggplot(mapping = aes(x = long, y = lat, group = group, text=paste(County, ": ",New.Positives))) +
        coord_equal() +
        theme_map() +
        geom_polygon(color = "black") +
        annotate(geom = "text", x = -78, y = 44.5, size = 5, colour = "grey80", label = input$Test.Date)
      
      p = p + 
        aes(fill = New.Positives) + 
        scale_fill_gradient(
          low = "white", 
          high = "#E69F00",
          limit=c(0,2700),
          labels = comma) + 
        labs(title = "New Positive Cases")
    }
    
    else if (input$Selection1=="Cumulative Number of Tests Performed"){
      p <- ny_test %>% 
        ggplot(mapping = aes(x = long, y = lat, group = group, text=paste(County, ": ", Cumulative.Number.of.Tests.Performed))) +
        coord_equal() +
        theme_map() +
        geom_polygon(color = "black") +
        annotate(geom = "text", x = -78, y = 44.5, size = 5, colour = "grey80", label = input$Test.Date)
      
      p = p + 
        aes(fill = Cumulative.Number.of.Tests.Performed) + 
        scale_fill_gradient(
          low = "white", 
          high = "blue",
          limit=c(0,600000),
          labels = comma) + 
        labs(title = "Cumulative Number of Tests Performed")
    }
    
    else if (input$Selection1=="New Total Number of Tests Performed"){
      p <- ny_test %>% 
        ggplot(mapping = aes(x = long, y = lat, group = group, text=paste(County, ": ",Total.Number.of.Tests.Performed))) +
        coord_equal() +
        theme_map() +
        geom_polygon(color = "black") +
        annotate(geom = "text", x = -78, y = 44.5, size = 5, colour = "grey80", label = input$Test.Date)
      
      p = p + 
        aes(fill = Total.Number.of.Tests.Performed) + 
        scale_fill_gradient(
          low = "white", 
          high = "#56B4E9",
          limit=c(0,11000),
          labels = comma) + 
        labs(title = "New Total Number of Tests Performed")
    }

    ggplotly(p, tooltip = "text")
  })
  
  output$plot3 = renderPlotly({
    #Plotting
    
    if (input$Selection2=="Malaria Incidence Rate"){
      fig <- plot_ly(full_df, type='choropleth', locations=~Code, text=~`Country/Region`,
                     z = ~incidence_rate_malaria, color = ~incidence_rate_malaria, colors = 'Oranges',
                     zauto=T, showscale = F, colorscale="Reds")
      fig <- fig %>% layout(title = 'Malaria Incidence Rate (per 100,000 people)')
      fig
    }
    
    else if (input$Selection2=="Malaria Death Rate"){
      fig <- plot_ly(full_df, type='choropleth', locations=~Code, text=~`Country/Region`,
                     z = ~death_rate_malaria, color = ~death_rate_malaria, colors = 'Reds',
                     zauto=T, showscale = F, colorscale="Reds")
      fig <- fig %>% layout(title = 'Malaria Death Rate (per 100,000 people)')
      fig
    }
    
    else if (input$Selection2=="Covid-19 Incidence Rate"){
      fig <- plot_ly(full_df, type='choropleth', locations=~Code, text=~`Country/Region`,
                     z = ~incidence_rate_covid, color = ~incidence_rate_covid, colors = 'Oranges',
                     zauto=T, showscale = F, colorscale="Reds")
      fig <- fig %>% layout(title = 'COVID-19 Incidence Rate (per 100,000 people)')
      fig
    }
    else if (input$Selection2=="Covid-19 Death Rate"){
      fig <- plot_ly(full_df, type='choropleth', locations=~Code, text=~`Country/Region`,
                     z = ~death_rate_covid, color = ~death_rate_covid, colors = 'Reds',
                     zauto=T, showscale = F, colorscale="Reds")
      fig <- fig %>% layout(title = 'COVID-19 Death Rate (per 100,000 people)')
      fig
    }
  })
  
  output$plot4 = renderPlotly({
    malaria_full = parse_data(malaria_incidence_dir, malaria_death_dir, FALSE)
    covid_full = parse_data(covid_death_url, covid_cases_url, TRUE)
    full_df = full_join(malaria_full, covid_full, by = c('Code'='country_code'), suffix = c("_malaria", "_covid")) %>%
      select(-c('Entity')) %>%
      select(-ends_with(c('_cases', '_deaths')))
    
    if (input$Selection3=="Malaria Incidence Rate vs. Covid-19 Incidence Rate"){
      fig <- plot_ly(full_df, text = full_df$`Country/Region`)
      fig <- fig %>% 
        add_trace(x = ~incidence_rate_malaria, y = ~incidence_rate_covid, type = 'scatter', mode='markers') %>%
        layout(title = "Correlation between Malaria and COVID-19") %>%
        layout(xaxis = list(title = 'Malaria Incidence Rate'),
               yaxis = list(title = 'COVID-19 Incidence Rate'))
      
      fig <- fig %>% layout(title = 'Malaria vs. COVID-19 Incidence Rate (per 100,000 people)')
      fig
    }
    
    else if (input$Selection3=="Malaria Death Rate vs. Covid-19 Death Rate"){
      fig <- plot_ly(full_df, text = full_df$`Country/Region`)
      
      fig <- fig %>% 
        add_trace(x = ~death_rate_malaria, y = ~death_rate_covid, 
                  name = 'Covid-19', type = 'scatter', mode = 'markers')%>% 
        layout(xaxis = list(title = 'Malaria Death Rate'),
                            yaxis = list(title = 'COVID-19 Death Rate'))
      fig <- fig %>% layout(title = 'Malaria vs. COVID-19 Death Rate (per 100,000 people)')
      fig
    }
  })
  output$myTable1 = renderDataTable({
    return(datatable(ny_county, rownames= FALSE,fillContainer=TRUE))
  })
  output$myTable2 = renderDataTable({
    return(datatable(full_df, rownames= FALSE,fillContainer=TRUE))
  })
}

shinyApp(ui = ui, server = server)


