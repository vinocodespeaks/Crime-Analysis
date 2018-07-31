## Crime Analytics Dashboard - Server
## Author: SOLAI MURUGAN V 2018
#setwd("/media/bdalab/docs/part_time/student_pjt/Crime_2pjt/crime_analytics-master_SFCDataset/crime_shiny/")
## Usage:

## This file performs all the data wrangling required for building the charts and tables.

library(shiny)
library(scales)
library(dplyr)
library(ggplot2)
library(magrittr)
library(readr)
library(leaflet)
library(dplyr)
library(tidyr)
library(stringr)
library(htmltools)
library(forcats)
#install.packages("forcats")
##Import dataset and additional table with cities information

crime_data <- read_csv('./data/crime_dataset.csv')
city_info <- read_tsv('./data/city_data.tsv')
city_info$lat <- as.numeric(city_info$lat)
city_info$long <- as.numeric(city_info$long)
crime_types <- data_frame(crime_type= c("homs_sum","rape_sum","rob_sum","agg_ass_sum","violent_crime"),
                          type = c("Homicide","Rape","Robbery","Aggravated Assault","All"))

#Some filtering of cities with NA or suburban counties.
exclude_list <- c("MD00301","CA01900","KY05680","FL01300")

crime_data <- crime_data %>% filter(!(ORI %in% exclude_list))

##Data wrangling

crime_df <- crime_data %>% 
  gather(crime_type,quantity,5:9) %>% 
  dplyr::select(ORI,year,total_pop,crime_type,quantity) %>% 
  inner_join(crime_types) %>% 
  dplyr::select(-crime_type) %>% 
  inner_join(city_info,by=c("ORI"="code")) %>% 
  dplyr::select(-department_name,-search_name) %>% 
  mutate(quantity_rel = quantity / total_pop * 100000) %>% 
  filter(real_name != "National")

##Create forecast

forecast_base <- crime_df %>% 
  filter(year == 2015) %>% 
  group_by(year,real_name,type) %>% 
  summarize(quantity_rel = sum(quantity_rel,na.rm=TRUE)) %>% 
  dplyr::select(real_name,year,type,quantity_rel)

forecast <- crime_df %>% 
  filter(year %in% c(2013,2014,2015)) %>% 
  group_by(real_name,type) %>% 
  summarize(f_qty_2016 = mean(quantity,na.rm=TRUE),
            f_pop_2016 = mean(total_pop,na.rm = TRUE),
            quantity_rel = f_qty_2016/f_pop_2016*100000,
            year = 2016) %>% 
  dplyr::select(real_name,year,type,quantity_rel)

forecast_combined <- bind_rows(forecast_base,forecast)



#Added foe Age compariison 11/03/2018
women_data <- read_csv('./data/paCAW.csv')  
age_crime <- women_data[women_data$`Male Below 18 Years`, c(2,3)]

unique(na.omit(age_crime$`Crime head`))
unique(na.omit(age_crime$`Male Below 18 Years`) )
age_crime_plot <- na.omit ( count(age_crime, `Crime head`) )


  
shinyServer(function(input, output) {
  
  # Define server logic required to make the map.
  output$mymap <- renderLeaflet({
    test <- crime_df %>% 
      filter(year == input$yearInput,type == input$crimeInput)
    if(input$stateInput != "ALL"){
      test <- test %>% filter(state==input$stateInput)
    }
    if(input$relCheckbox == TRUE){
      test <- test %>% mutate(quantity = quantity_rel)
    }
    rule <- 25/max(test$quantity,na.rm=TRUE)
    leaflet(data=test) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>% 
      addTiles() %>%
      addCircleMarkers(~long, 
                       ~lat,
                       popup = ~paste("<b>",real_name,"</b>",
                                      "</br>",year,
                                      "</br><b>Type:</b>",type,
                                      "</br><b>Quantity:</b>",round(quantity)),
                       label = ~as.character(real_name),
                       radius = ~(quantity * rule),
                       stroke = FALSE, 
                       fillOpacity = 0.5)
  })
  
  #Code for bar chart 1 next to the map
  output$bar_overview_1 <- renderPlot({
    data_bar <- crime_df %>% 
      filter(year == input$yearInput,type == input$crimeInput)
    if(input$relCheckbox == TRUE){
      data_bar <- data_bar %>% mutate(quantity = quantity_rel)
    }
    
    top_10 <- data_bar %>% 
      group_by(real_name) %>% 
      summarize(quantity = sum(quantity,na.rm=TRUE)) %>% 
      arrange(desc(quantity)) %>% 
      top_n(10) %>% 
      mutate(type = "Top 10")
    
    
    ggplot(top_10) +
      geom_col(aes(x=fct_reorder(real_name,desc(quantity)),y=quantity),color="#A85042",fill="#A85042",alpha=0.7)+
      ggtitle(paste("Top 10 most violent cities - by ",input$crimeInput))+
      scale_y_continuous("# of crimes")+
      scale_x_discrete("")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    })
  
  #Code for bar chart 2 next to the map
  output$bar_overview_2 <- renderPlot({
    data_bar <- crime_df %>% 
      filter(year == input$yearInput,type == input$crimeInput)
    if(input$relCheckbox == TRUE){
      data_bar <- data_bar %>% mutate(quantity = quantity_rel)
    }
    
    bot_10 <- data_bar %>% 
      group_by(real_name) %>% 
      summarize(quantity = sum(quantity,na.rm=TRUE)) %>% 
      arrange(desc(quantity)) %>% 
      top_n(-10) %>% 
      mutate(type = "Bottom 10")
    
    
    ggplot(bot_10) +
      geom_col(aes(x=fct_reorder(real_name,desc(quantity)),y=quantity),color="blue",fill="blue",alpha=0.7)+
      ggtitle(paste("Top 10 safest cities - by ",input$crimeInput))+
      scale_y_continuous("# of crimes")+
      scale_x_discrete("")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
      
  
  #Code for creating the phrase with kpi.
  output$kpi1 <- renderUI({
    test <- crime_df %>% 
      filter(year == max(crime_df$year),
             real_name %in% c(input$cityInput1,input$cityInput2),
             type == "All") %>% 
      arrange(quantity_rel)
    tagList(
      h3(paste(test$real_name[1],
               " is safer than ",
               test$real_name[2],
               ". During the last year, ",
               test$real_name[1],
               " had ",
               round(test$quantity_rel[1]),
               " crimes per 100,000 people, while ",
               test$real_name[2],
               " had ",
               round(test$quantity_rel[2]),
               ". Scroll the following plots to find more details."
      ))
    )
  })
  
  #Code for creating the plots.
  
  comparison <- crime_df %>% 
    group_by(real_name,year,type) %>% 
    summarize(total=sum(quantity_rel))
  
  output$distPlot1 <- renderPlot({
    comparison <- comparison %>% 
      filter(year >= 1985,
             year <= 2015,
             real_name == input$cityInput1)
    
    sub_forecast <- forecast %>% 
      filter(real_name == input$cityInput1)
    
    p_1 <-  ggplot(comparison %>% filter(real_name == input$cityInput1))+
              geom_line(aes(x=year,y=total, color = type),size=2,alpha=0.7)+
              scale_x_continuous("Year")+
              scale_y_continuous("# of crimes per 100k people",limits = c(0,3000))+
              ggtitle(paste("Relative Crime Statistics for ",input$cityInput1))+
              theme_minimal()+
              theme(legend.position = "bottom")
    
    if(input$forCheckbox == TRUE){
        p_1 <- p_1 + geom_point(data=sub_forecast,aes(x=year,y=quantity_rel,color=type),shape=18,size=3)
    }
    p_1
  })
  
  output$distPlot2 <- renderPlot({
    comparison <- comparison %>% 
      filter(year >= 1985,
             year <= 2015,
             real_name == input$cityInput2)
    
    sub_forecast <- forecast_combined %>% 
      filter(real_name == input$cityInput2)
    
    p_2 <-  ggplot(comparison %>% filter(real_name == input$cityInput2))+
              geom_line(aes(x=year,y=total, color = type),size=2,alpha=0.7)+
              scale_x_continuous("Year")+
              scale_y_continuous("# of crimes per 100k people",limits = c(0,3000))+
              ggtitle(paste("Relative Crime Statistics for ",input$cityInput2))+
              theme_minimal()+
              theme(legend.position = "bottom")
    
    if(input$forCheckbox == TRUE){
        p_2 <- p_2 + 
          geom_point(data=sub_forecast,aes(x=year,y=quantity_rel,color=type),shape = 18,size=3)
    }
    
    p_2
  })
  

  
  # Define server logic required to make the tables.
  output$table1 <- renderTable({
    if (input$forCheckbox == TRUE){
      sub_forecast <- forecast %>% 
        filter(real_name == input$cityInput1)
      
      crime_table_1 <- crime_df %>% 
        filter(real_name == input$cityInput1,
               year %in% c(1985,2015)) %>% 
        dplyr::select(real_name,year,type,quantity_rel) %>% 
        bind_rows(sub_forecast) %>% 
        spread(year,quantity_rel)
      
      colnames(crime_table_1)[5] <- "Forecast 2016"
    }else{
      crime_table_1 <- crime_df %>% 
        filter(real_name == input$cityInput1,
               year %in% c(input$yearInput2[1],input$yearInput2[2])) %>% 
        dplyr::select(real_name,year,type,quantity_rel) %>% 
        spread(year,quantity_rel)
    }
    
    crime_table_1<- crime_table_1 %>% 
      dplyr::select(-real_name)
    
    crime_table_1})
  
  output$table2 <- renderTable({
    if (input$forCheckbox == TRUE){
      sub_forecast <- forecast %>% 
        filter(real_name == input$cityInput2)
      
      crime_table_2 <- crime_df %>% 
        filter(real_name == input$cityInput2,
               year %in% c(1985,2015)) %>% 
        dplyr::select(real_name,year,type,quantity_rel) %>% 
        bind_rows(sub_forecast) %>% 
        spread(year,quantity_rel)
      
      colnames(crime_table_2)[5] <- "Forecast_2016"

    }else{
      crime_table_2 <- crime_df %>% 
        filter(real_name == input$cityInput2,
               year %in% c(1985,2015)) %>% 
        dplyr::select(real_name,year,type,quantity_rel) %>% 
        spread(year,quantity_rel)
    }
    
    crime_table_2<- crime_table_2 %>% 
      dplyr::select(-real_name)

    crime_table_2})
  
  
  #Added foe Age compariison 11/03/2018     
   output$distPlotage1 <- renderPlot({
     age_crime <- women_data[, ]
     print(input$maleInput1)
     unique(na.omit(age_crime$`Crime head`))
     unique(na.omit(age_crime$`Male Below 18 Years`) )
     
     #get index postion based n the input choice
     index <- which(colnames(age_crime) %in% input$maleInput1)
     print(index)
     #age_crime_plot <- na.omit ( count(age_crime, `Crime head`,`Male Below 18 Years`) )
     age_crime_plot <- aggregate(age_crime[,c(index)], by=list(Category=age_crime$`Crime head`), FUN=sum)
     colnames(age_crime_plot) <- c('Category','x')
    #age_crime_plot[,-c(grep("\\TOTAL\\b", age_crime_plot$Category))]
     #remove TOTAL from result
     #age_crime_plot <-subset(age_crime_plot, !(age_crime_plot$Category %in% 'TOTAL CRIMES AGAINST WOMEN') )
     
     
     print("solai")
    print(age_crime_plot)
    ggplot(age_crime_plot) +
     geom_col(aes(x=fct_reorder(Category,desc(x)),y=x),color="#A85042",fill="#A85042",alpha=0.7)+
     ggtitle(paste(" Crime Against Women - by ",input$maleInput1))+
     scale_y_continuous("#No. of crimes")+
     scale_x_discrete("")+
     theme_minimal()+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   })
   
   
   output$crimetable1 <- renderTable({
     age_crime <- women_data[, ]
     print(input$maleInput1)
     unique(na.omit(age_crime$`Crime head`))
     unique(na.omit(age_crime$`Male Below 18 Years`) )
     
     #get index postion based n the input choice
     index <- which(colnames(age_crime) %in% input$maleInput1)
     print(index)
     #age_crime_plot <- na.omit ( count(age_crime, `Crime head`,`Male Below 18 Years`) )
     age_crime_plot <- aggregate(age_crime[,c(index)], by=list(Category=age_crime$`Crime head`), FUN=sum)
     colnames(age_crime_plot) <- c('Category','x')
     
     age_crime_plot <- age_crime_plot[order(-age_crime_plot$x),]   
     age_crime_plot
   })
   
})
