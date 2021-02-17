
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
# set colour set 
key_col = "#cc4c02"


# import data
# this file contains all number and date information 
overall_info = read.csv("covid19info.csv")

#this file contains all country location information, lat, lon, continent, etc. 
countries_info = read.csv("countries_location.csv")

#map information 
worldcountry = geojson_read("50m.geojson", what = "sp")


#Data Preprocessing Part
#Cumulative population: sum up by different dates then give names for the two columns
cv_aggregated = aggregate(overall_info$cases, by=list(Category=overall_info$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

#New cases 
# add variable for new cases
# loop from all rows, then the second one minus the first one to get the newly added number
# now we have three columns: date/cases/new
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add global column then change the "date" format to be Date  
# now we have four columns: date/cases/new/region(with all "global") 
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# extract time stamp from overall_info
update = tail(overall_info$last_update,1) 


# convert into date format for overall_info
if (any(grepl("/", overall_info$date))) { 
  overall_info$date = format(as.Date(overall_info$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { overall_info$date = as.Date(overall_info$date, format="%Y-%m-%d") }

overall_info$date = as.Date(overall_info$date)

# get the time range
min_date = as.Date(min(overall_info$date),"%Y-%m-%d")
max_date = as.Date(max(overall_info$date),"%Y-%m-%d")

# merge cv data with country data with all information we need now 
overall_info = merge(overall_info, countries_info, by = "country")

# reorder the date with correct time series
overall_info = overall_info[order(overall_info$date),]

#filter out population more than one million
# we ignore small countries otherwise we would have too much information to deal with
overall_info$million_pop = as.numeric(overall_info$population>1e6)

# creat variable for the most recent data
recent_info = subset(overall_info, date==max_date) 
current_case_count = sum(recent_info$cases)
current_case_count_China = sum(recent_info$cases[recent_info$country=="Mainland China"])
current_case_count_other = sum(recent_info$cases[recent_info$country!="Mainland China"])
current_death_count = sum(recent_info$deaths)

# create subset for countries with at least 1000 cases
recent_filtered = subset(recent_info, cases>=1000)

# write current day's data
write.csv(recent_info %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
)), "recent_info.csv")

# aggregate at continent level
continent_level = subset(overall_info, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# aggregate at global level
global_level = overall_info %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# select large countries for mapping polygons
# get the location information filtered via alpha3 column using country abbreviation
large_countries = recent_info %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
#order the countries with alhabetic order
large_countries = large_countries[order(large_countries$alpha3),]

# assign colours to ensure consistency between plots 
#get good and similar set of colors from RColorBrewer.package
colors_all = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(overall_info$country)), as.character(unique(continent_level$continent)),"Global")
country_cols = colors_all[1:length(cls_names)]
names(country_cols) = cls_names

### Functions for two figures on map page ###
# function to plot cumulative COVID cases by date
cumulative_region_plot = function(cv_aggregated, chosen_date) {
  plot_df = subset(cv_aggregated, date<=chosen_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=c(key_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_region_plot = function(cv_aggregated, chosen_date) {
  plot_df_new = subset(cv_aggregated, date<=chosen_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(key_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}



# second page function needed 
# function:plot new cases by region on second page 
country_cases_plot = function(overall_info, date) {
    g = ggplot(overall_info, aes(x = date, y = new_outcome, fill = region, 
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      #xlim(c(plot_start_date,max_date)) +
      xlab("Date")

  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function: plot cumulative cases by region on second page 
country_cases_cumulative = function(overall_info, date) {
    g = ggplot(overall_info, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      #xlim(c(plot_start_date,max_date)) + 
      xlab("Date")
 
  
  g1 = g + geom_line(alpha=1.0) + geom_point(size = 1, alpha = 1.0) +
    ylab("cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) 
}

# function: plot cumulative cases by region on log10 scale on second page
country_log_function = function(overall_info, date)  {
    g = ggplot(overall_info, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      #xlim(date) +
      xlab("Date")

  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative (log10)") + theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


# create plotting parameters for map
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "topright",
    overlayGroups = c("COVID-19 (cumulative)", "COVID-19 (active)", "COVID-19 (new)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("COVID-19 (active)", "COVID-19 (new)"))  #%>%



### SHINY UI ###
ui <- bootstrapPage(
  # give header a theme/design
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, position = "fixed-bottom",
             "COVID-19 tracker",
             
             tabPanel("COVID-19 mapper",
                      div(class="mapping",
                          
                          tags$head(includeCSS("styles.css")),
                          
                          leafletOutput("mapinfo", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 10, left = 55, width = 210, fixed=TRUE,
                                        draggable = TRUE, height = NULL,
                                        
                                        #link with reactive function in server part
                                        span(tags$i(h6("Brief Summary of The Total Population")), style="color:#045a8d"),
                                        h4(textOutput("reactive_cumulative_count"), align = "right"),
                                        h4(textOutput("reactive_deathscount"), align = "right"),
                                        span(h4(textOutput("reactive_recoveredcount"), align = "right"), style="color:#006d2c"),
                                        span(h4(textOutput("reactive_activecount"), align = "right"), style="color:#cc4c02"),
                                        h6(textOutput("date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_num"), align = "right"),
                                        
                                        # link with reactive from plot function 
                                        plotOutput("curve", height="130px", width="100%"),
                                        plotOutput("cumulative_region_plot", height="130px", width="100%"),
                                        
                                        #prepare date input slider
                                        sliderInput("chosen_date",
                                                    label = h5("Select mapping date"),
                                                    min = as.Date(min_date,"%Y-%m-%d"),
                                                    max = as.Date(max_date,"%Y-%m-%d"),
                                                    value = as.Date(max_date),
                                                    timeFormat = "%d %b", 
                                                    animate=animationOptions(interval = 3000, loop = FALSE))
                          )
                      )
                          
                 
             ),
             # add UI interface for th second page
             tabPanel("Region plots",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          #add 3 drop-down menus 
                          pickerInput("level_select", "Level:",   
                                      choices = c("Global", "Continent", "Country"), 
                                      selected = c("Country"),
                                      multiple = FALSE),
                          
                          pickerInput("region_select", "Country/Region:",   
                                      choices = as.character(recent_filtered[order(-recent_filtered$cases),]$country), 
                                      options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                      selected = recent_filtered$country,
                                      multiple = TRUE), 
                          
                          pickerInput("outcome_select", "Outcome:",   
                                      choices = c( "Cases (total)", "Deaths (total)"), 
                                      selected = c("Cases (total)"),
                                      multiple = FALSE),
                          
                          "Select regions, and categories from drop-down menues to see plots. Countries with more 1000 confirmed cases are contained here."
                        ),
                        # add panel as well as the figure to be put on the panel 
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                            tabPanel("New", plotlyOutput("country_plot")),
                            tabPanel("Cumulative (log10)", plotlyOutput("country_cumulative_log"))
                          )
                        
                      )
             )
             
                     ) )
)
            

             
             
             
             
### SHINY SERVER ###

server = function(input, output, session) {
  
  # Map Page 
  # output with a reactive result by summing up all cases as cumulative from "reactive general"
  output$reactive_cumulative_count <- renderText({
    paste0("Total:",prettyNum(sum(reactive_db()$cases), big.mark=","))
  })
  
  # for the date format 
  output$date_reactive <- renderText({
    paste0("on ",format(as.POSIXct(input$chosen_date),"%d %B %Y"))
  })
  # output with a reactive result by summing up all recovered 
  output$reactive_recoveredcount <- renderText({
    paste0("Recovered:",prettyNum(sum(reactive_db()$recovered), big.mark=","))
  })
  
  # output with a reactive result by summing up all for deaths cases 
  output$reactive_deathscount <- renderText({
    paste0("Deaths:",prettyNum(sum(reactive_db()$deaths), big.mark=","))
  })
  
  
  # output with a reactive result by summing up all for active cases 
  output$reactive_activecount <- renderText({
    paste0("Active:",prettyNum(sum(reactive_db()$active_cases), big.mark=","))
  })
  # output with a reactive result by summing up the number of countries 
  output$reactive_country_num <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  # for date chosen "reactive general" cases
  reactive_db = reactive({
    overall_info %>% filter(date == input$chosen_date)
  })
  # output with a reactive result by summing up all for new cases 
  reactive_db_recent = reactive({
    overall_info %>% filter(date == input$chosen_date & new_cases>0)
  })
  # output with a reactive result for polygon shapes
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
    large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
    large_countries
  })
  # output with a reactive result for polygon shapes filtered from exisiting locations/countries
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
  })

  
  # put map onto the page
  output$mapinfo <- renderLeaflet({ 
    basemap
  })
  # link with date input slider, map info and also reactive polygon visualizations 
  observeEvent(input$chosen_date, {
    leafletProxy("mapinfo") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15,fillColor = 'Orange') %>%
      
      #add markers with new cases from recent reactive 
      addCircleMarkers(data = reactive_db_recent(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
                       fillOpacity = 0.1, color = key_col, group = "COVID-19 (new)",
                       label = sprintf("<strong>%s</strong>(new)<br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d", reactive_db_recent()$country, reactive_db_recent()$new_cases, reactive_db_recent()$new_deaths, reactive_db_recent()$new_recovered) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = key_col),
                         textsize = "15px", direction = "auto")) %>%
      #add markers with new cases from general case reactive 
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                       fillOpacity = 0.1, color = key_col, group = "COVID-19 (cumulative)",
                       label = sprintf("<strong>%s</strong>(cumulative)<br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = key_col),
                         textsize = "15px", direction = "auto")) %>%
      #add markers with active cases from general case reactive 
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                       fillOpacity = 0.1, color = key_col, group = "COVID-19 (active)",
                       label = sprintf("<strong>%s</strong>(active)<br/>Confirmed cases: %g<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = key_col),
                         textsize = "15px", direction = "auto"))
  })
  
  
  #second page with regional plot
  #link with the cumulative country plot we defined earlier 
  output$cumulative_region_plot <- renderPlot({
    cumulative_region_plot(cv_aggregated, input$chosen_date)
  })
  #link with the new cases country plot we defined earlier 
  output$curve <- renderPlot({
    new_cases_region_plot(cv_aggregated, input$chosen_date)
  })
  
 
  
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(recent_filtered[order(-recent_filtered$cases),]$country), 
                        selected = recent_filtered$country)
    }
  }, ignoreInit = TRUE)
  
  # create dataframe with selected countries
  country_reactive = reactive({
    if (input$level_select=="Global") { 
      db = global_level
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = continent_level 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = overall_info
      db$region = db$country
    }
    # create dataframe with selected categories
    if (input$outcome_select=="Cases (total)") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select=="Deaths (total)") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    db %>% filter(region %in% input$region_select)
  })
  
  # which country plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive(), date)
  })
  
  # which country plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive(), date)
  })
  
  # which country plots
  output$country_cumulative_log <- renderPlotly({
    country_log_function(country_reactive(), date)
  })
  
  
}


shinyApp(ui, server)

