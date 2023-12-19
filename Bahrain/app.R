#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(maps)
library(leaflet)
library(shinyjs)



ui <- fluidPage(
  titlePanel("Bahrain Analysis"),
  


  selectInput("section", "Choose a Section:", 
              choices = c("General Description", "Key Demographics", 
                          "Comparison with Other Island Nations", "SWOT Analysis", "Bahrain Views")),
  
  

  uiOutput("content_selector"),
  

  mainPanel(
    uiOutput("selected_section")
    ,
    uiOutput("plot_explanation") 
    ,
    plotOutput("island_comparison_plot", width = "100%", height = "1000px") # Adjust size here
    #,plotOutput("world_map_plot", width = "100%", height = "1000px")
    #,plotOutput("bahrain_map", width = "100%", height = "1000px")
     # Plot for the comparison section
   ,uiOutput("comparison_explanation") # Text explanation for the comparison plot
   
   ,uiOutput("swot_analysis")
   
   ,uiOutput("image_display") # UI output for the images
   ,actionButton("prev_img", "Previous", style = "margin-right: 10px;")  # Button to go to the previous image
   ,actionButton("next_img", "Next")     # Button to go to the next image
   ,hidden(div(id = "image_holder"))       # Hidden div to store the current image
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Conditional UI for content selection based on section
  output$content_selector <- renderUI({
    if(input$section == "General Description") {
      radioButtons("content", "Select the content you want to display:",
                   choices = list(
                     "Map showing the location of the island state in the world" = "world_map",
                     "Map of the island state" = "island_map",
                     "Key facts about the island state" = "key_facts",
                     "A brief narrative description of the island state" = "narrative_description"
                   ),
                   selected = "world_map")
    } else if(input$section == "Key Demographics") {
      radioButtons("demographics_content", "Select the content you want to display:",
                   choices = list(
                     "Population, Urbanization, and GDP" = "pop_urb_gdp",
                     "Unemployment, Inflation, and Trade Balances" = "unemp_infl_trade",
                     "Education" = "education"
                   ))
    }
    else if(input$section == "SWOT Analysis") {
      radioButtons("swot_content", "Select the content you want to display:",
                   choices = list(
                     "Strengths" = "strengths",
                     "Weaknesses" = "weaknesses",
                     "Opportunities" = "opportunities",
                     "Threats" = "threats"
                   ))
    }
  })
  
  # Reactive value to store the current image index, start with the first image
  current_image_index <- reactiveVal(1)
  
  # Observe clicking on "Previous" button
  observeEvent(input$prev_img, {
    current_image_index(max(1, current_image_index() - 1))  # Decrease index but not below 1
  })
  
  # Observe clicking on "Next" button
  observeEvent(input$next_img, {
    current_image_index(min(2, current_image_index() + 1))  # Increase index but not above the number of images
  })
  
  # Output for displaying images
  output$image_display <- renderUI({
    if(input$section == "Bahrain Views") {
      if(current_image_index() == 1) {
        img(src = "1.jpg", height = "1500px")  
      } else {
        img(src = "2.jpg", height = "1500px")  
      }
    }
  })
  
  # Show/Hide navigation buttons based on the selected section
  observe({
    if(input$section == "Bahrain Views") {
      shinyjs::show("prev_img")
      shinyjs::show("next_img")
    } else {
      shinyjs::hide("prev_img")
      shinyjs::hide("next_img")
    }
  })
  
  # Render content based on section and selection
  output$selected_section <- renderUI({
    if(input$section == "General Description") {
      switch(input$content,
             world_map = plotOutput("world_map_plot"),
             island_map = leafletOutput("bahrain_map"),
             key_facts = tableOutput("key_facts_output"),
             narrative_description = verbatimTextOutput("narrative_description_output"))
    } else if(input$section == "Key Demographics") {
      switch(input$demographics_content,
             pop_urb_gdp = plotOutput("pop_urb_gdp_plot"),
             unemp_infl_trade = plotOutput("unemp_infl_trade_plot"),
             education = plotOutput("education_plot"))
    }
    else if(input$section == "SWOT Analysis") {
      switch(input$swot_content,
             strengths = HTML("
             <h5>Strengths:</h5>
        <ul>
          <li><strong>Economic Growth:</strong> Bahrain shows a consistent upward trend in GDP and GDP per capita, indicating robust economic growth and improvement in the standard of living.</li>
          <li><strong>Urbanization:</strong> A high and increasing urban population percentage suggests a potential for economic diversification and the development of urban infrastructure.</li>
          <li><strong>Education Commitment:</strong> Despite fluctuations, high enrollment rates in primary and secondary education indicate a strong commitment to education.</li>
          <li><strong>Healthcare Spending:</strong> Moderate health expenditure per capita shows investment in the healthcare sector, with a significant role for private spending, suggesting a strong private healthcare industry.</li>
        </ul>"),
             weaknesses = HTML("
             <h5>Weaknesses:</h5>
        <ul>
          <li><strong>Economic Volatility:</strong> The GDP growth rate shows volatility, reflecting susceptibility to external economic shocks, likely due to reliance on oil exports.</li>
          <li><strong>Educational Scores:</strong> Lower educational scores compared to similar nations may indicate room for improvement in the quality of education.</li>
          <li><strong>Environmental Impact:</strong> High emissions per capita demonstrate environmental challenges associated with industrial and energy sectors.</li>
        </ul>"),
             opportunities = HTML("
             <h5>Opportunities:</h5>
        <ul>
          <li><strong>Diversification:</strong> With a growing urban population, there is an opportunity to diversify the economy beyond oil into sectors such as finance, tourism, and information technology.</li>
          <li><strong>Educational Improvement:</strong> By investing in education quality and infrastructure, Bahrain can enhance its human capital to compete on a global scale.</li>
          <li><strong>Healthcare Development:</strong> The existing private healthcare expenditure indicates a potential market for healthcare services and medical tourism.</li>
        </ul>"),
             threats = HTML("
             <h5>Threats:</h5>
        <ul>
          <li><strong>Economic Dependency:</strong> Bahrain’s economy is vulnerable to fluctuations in the global oil market, which poses a risk to economic stability.</li>
          <li><strong>Environmental Sustainability:</strong> The high rate of emissions per capita suggests sustainability issues that could lead to long-term environmental and health problems.</li>
          <li><strong>Regional Instability:</strong> Being in a geopolitically sensitive region, Bahrain could be affected by regional tensions and conflicts that might impact its economic and social stability.</li>
        </ul>")
      )
    }
  })
  
  # Load data
  data_df <- reactive({
    read_csv("API_BHR_DS2_en_csv_v2_6262972.csv")
  })
  
  # General Description plots
  output$world_map_plot <- renderPlot({ 
    world_map <- map_data("world")
    ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
      geom_point(data = data.frame(long = 50.5833, lat = 26.2167), aes(x = long, y = lat), color = "red")+
      annotate("text", x = 50.5833, y = 26.3167, label = "Bahrain", hjust = 0, vjust = 0, size = 4,color = "red") +
      theme_minimal()
    })
  
  
  output$bahrain_map <- renderLeaflet({   leaflet() %>%
      addTiles() %>%
      addMarkers(lng = 50.5833, lat = 26.2167, popup = "Manama, Bahrain") })
  
  # Structure the data into a dataframe
  key_facts_data <- data.frame(
    Category = c("Geography and History", "Land and Climate", "Natural Resources and Economy", "Flora and Fauna", "Demographics and Culture"),
    Description = c(
      "Bahrain is a small Arab state in the Persian Gulf, consisting of Bahrain Island and about 30 smaller islands. Historically, Bahrain has been an important trading center, believed to be the site of the ancient Dilmun kingdom, and has been ruled by the Khalīfah family since the late 18th century.",
      "The total land area of Bahrain is slightly greater than Singapore, extending about 30 miles from north to south and 10 miles from east to west. The climate features hot and humid summers, with cooler, more pleasant winters. Rainfall is limited, mostly confined to the winter months.",
      "Despite being in a prime oil-producing region, Bahrain has only small petroleum reserves and primarily processes crude oil from neighboring countries. The financial, commercial services, communications sectors, and tourism have grown significantly.",
      "The country supports a variety of desert plants, fruit trees, and vegetable gardens, with limited animal life adapted to desert conditions.",
      "Roughly half of Bahrain's population is Arab, with a significant proportion of foreign-born residents from Iran, India, Pakistan, Britain, and the United States. Arabic is the official language, with English widely used as a compulsory second language in schools."
    )
  )
  
  # Adjust the server function
  output$key_facts_output <- renderTable({
    # Return the key_facts_data dataframe
    key_facts_data
  }, align = 'l', sanitize.text.function = function(x) x)  # Use 'l' for left alignment of text
  
  
  
  output$narrative_description_output <- renderText({ 
    HTML("Bahrain, an island nation in the Persian Gulf, 
         is known for its rich cultural history and modern economic prowess. 
         Once famous for its pearl fisheries, 
         Bahrain has now transformed into a banking and financial hub of the Middle East. 
         The country boasts a blend of ancient and modern architecture, 
         exemplified by the Bahrain World Trade Center and the Bahrain Fort. 
         Its capital, Manama, is a bustling cosmopolitan city with a diverse population, 
         reflecting a blend of local and expatriate communities. 
         Bahrain's economy is also bolstered by oil and petroleum production, 
         though in recent years, there's been a push towards diversification. 
         The nation is also known for the Bahrain Grand Prix, 
         a major Formula One race, attracting visitors from around the globe. 
         Despite its small size, 
         Bahrain maintains a significant role in regional politics and is a member of the Gulf Cooperation Council. 
         The country's history, dating back to ancient Dilmun civilization, 
         adds to its rich cultural landscape, 
         making it a unique blend of tradition and modernity.")
    })

  
  # Render the image in the 'Key facts about the island state' section
  output$key_facts_image <- renderUI({
    if(input$section == "General Description" && input$content == "key_facts") {
      tags$img(src = "1.jpg", height = "1000px", width = "auto")
    }
  })
  
  # Render the image in the 'A brief narrative description of the island state' section
  output$narrative_description_image <- renderUI({
    if(input$section == "General Description" && input$content == "narrative_description") {
      tags$img(src = "1.jpeg", height = "1000px", width = "auto")
    }
  })
  
  
  # Key Demographics plots
  output$pop_urb_gdp_plot <- renderPlot({
    # Filtering data for Population, Urbanization, and GDP
    key_indicators <- c('Population, total', 'Urban population (% of total population)', 
                        'GDP (current US$)', 'GDP per capita (current US$)', 'GDP growth (annual %)')
    
    key_data_df <- data_df() %>%
      filter(Indicator_Name %in% key_indicators) %>%
      select(-c("Name", "Code", "Indicator_Code")) %>%
      gather(key = "Year", value = "Value", -"Indicator_Name")
    # Melting data for ggplot
    

    plot_indicator <- function(data, indicator_name) {
      # Filter data for the specified indicator
      indicator_data <- filter(data, Indicator_Name== indicator_name)
      
      # Plotting
      p <- ggplot(indicator_data, aes(x = as.numeric(Year), y = Value)) +
        geom_line(size = 1) +
        theme_light(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              plot.title = element_text(size = 16, face = "bold")) +
        scale_x_continuous(breaks = seq(min(as.numeric(indicator_data$Year)), max(as.numeric(indicator_data$Year)), by = 5)) +
        labs(title = paste(indicator_name, "in Bahrain"), x = "Year", y = "Value")
      
      return(p)
    }
    
    ggplot(key_data_df, aes(x = as.numeric(Year), y = Value)) +
      geom_line(size = 1) +
      facet_wrap(~ Indicator_Name, scales = "free_y") +
      theme_light(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      labs(x = "Year", y = "Value")# The R code for plotting Population, Urbanization, and GDP goes here
  })
  output$unemp_infl_trade_plot <- renderPlot({
    # Filtering for Unemployment, Inflation, and Trade Balances
    additional_indicators <- c('Unemployment, total (% of total labor force) (national estimate)',
                               'Inflation, consumer prices (annual %)', 
                               'Current account balance (BoP, current US$)', 
                               'Exports of goods and services (% of GDP)', 
                               'Imports of goods and services (% of GDP)')
    
    additional_data_df <- data_df() %>%
      filter(Indicator_Name %in% additional_indicators)%>%
      select(-c(Name, Code, Indicator_Code)) %>%
      gather(key = "Year", value = "Value", -Indicator_Name)
    
    # Plotting 
    ggplot(additional_data_df, aes(x = as.numeric(Year), y = Value)) +
      geom_line(size = 1) +
      facet_wrap(~ Indicator_Name, scales = "free_y") +
      theme_light(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      labs(x = "Year", y = "Value")
  })
  
  
  
  output$education_plot <- renderPlot({
    # Filtering for Education 
    further_indicators <- c('School enrollment, secondary (% gross)', 
                            'School enrollment, primary (% gross)')
    
    
    # Melting data for ggplot
    further_data_df <- data_df() %>%
      filter(Indicator_Name %in% further_indicators) %>%
      select(-Name, -Code, -Indicator_Code) %>%
      gather(key = "Year", value = "Value", -Indicator_Name)
    
    # Plotting each indicator separately
    ggplot(further_data_df, aes(x = as.numeric(Year), y = Value)) +
      geom_line(size = 1) +
      facet_wrap(~ Indicator_Name, scales = "free_y") +
      theme_light(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      labs(x = "Year", y = "Value")
  })
  
  
  # Text explanation for each plot
  output$plot_explanation <- renderUI({
    if(input$section == "Key Demographics") {
      if(input$demographics_content == "pop_urb_gdp") {
        HTML("
        <ul>
          <li><strong>Total Population Growth:</strong> The first graph illustrates Bahrain's total population over time, which shows a consistent upward trend. Starting from 1980, the population has increased steadily, suggesting a stable rise in residents. This could be attributed to various factors such as natural population growth and immigration.</li>
          
          <li><strong>GDP and GDP Growth Rate:</strong> The top middle graph represents Bahrain's Gross Domestic Product (GDP) in current US dollars, alongside the GDP growth rate depicted as a percentage. The GDP graph indicates substantial economic growth over the years, with some fluctuations in the growth rate. The GDP growth rate appears quite volatile, with periods of significant expansion followed by occasional contractions, reflecting the dynamic nature of Bahrain's economy.</li>
          
          <li><strong>Urban Population Percentage:</strong> The third graph shows the percentage of Bahrain's population that is urban. It indicates a steady increase in urbanization from the 1980s to the present, with the urban population now constituting a dominant portion of the total population. This trend reflects global urbanization patterns and suggests a concentration of the population in urban areas, likely due to the availability of jobs, services, and amenities.</li>
          
          <li><strong>GDP Per Capita:</strong> The bottom right graph displays the GDP per capita in current US dollars. There has been a general upward trajectory, particularly noticeable from the 2000s onward, indicating an increase in the average economic output per person. This suggests that the standard of living and the overall wealth of the average citizen in Bahrain might have improved over time.</li>
          
          <li><strong>In summary:</strong> these graphs collectively depict a nation that has experienced significant economic development, a growing and increasingly urban population, and an improvement in wealth per capita. Bahrain's economic indicators suggest a robust and developing economy with a strong urban sector. However, the volatility in the GDP growth rate highlights the need for economic diversification and stability to manage and mitigate the effects of economic downturns.</li>
        </ul>"
             )
      } 
      else if(input$demographics_content == "unemp_infl_trade") {
        HTML("
        <ul>
          <li><strong>Current Account Balance:</strong> This graph illustrates Bahrain's current account balance in US dollars. It shows significant fluctuations over time with a peak around 2008 followed by a sharp decline, which may indicate a period of substantial trade surplus that then shifted towards a deficit or a reduced surplus. This could be reflective of changes in global oil prices, given Bahrain's economic reliance on petroleum exports.</li>
          
          <li><strong>Inflation Rate:</strong> The second graph depicts the inflation rate based on consumer prices. There are notable spikes in inflation, particularly one around the early 1990s and another significant peak around 2008. Following 2008, the inflation rate seems to have stabilized to a lower level. These peaks could correlate with global economic events or shifts in domestic economic policy.</li>
          
          <li><strong>Exports of Goods and Services:</strong> The top right graph presents exports of goods and services as a percentage of GDP. There's a marked increase in the mid-2000s, indicating a period of heightened export activity relative to the size of the economy. This could suggest a boom in the demand for Bahrain's exports or an increase in global commodity prices.</li>
          
          <li><strong>Unemployment Rate:</strong> The bottom left graph shows Bahrain's unemployment rate as a percentage of the total labor force. The unemployment rate seems relatively low with a few upward ticks. Notably, there's a sharp decline in the reported rate after 2000. This could be due to a variety of factors including changes in labor market policies, economic diversification, or shifts in the demographic composition of the workforce.</li>
          
          <li><strong>Imports of Goods and Services:</strong> The last graph illustrates imports of goods and services as a percentage of GDP. There is a clear decline in imports from the 1980s through the 2000s, followed by a more variable pattern in recent years. The decline might reflect an improvement in domestic production or a change in economic structure, while the variability in recent years could be indicative of fluctuating market conditions or changing trade policies.</li>
          
          <li><strong>In summary:</strong> the economic indicators from this dashboard suggest Bahrain has gone through various economic phases characterized by shifts in trade balances, inflation rates, export and import activities, and unemployment rates. The data points to an economy that has dealt with external and internal challenges, with the effects of global market changes being quite evident. Importantly, the trends suggest periods of economic prosperity as well as times of adjustment and stabilization, which are common in economies with a strong focus on international trade and oil exports.</li>
        </ul>
      ")
      } 
      else if(input$demographics_content == "education") {
        HTML("<ul>
          <li><strong>Primary School Enrollment: </strong> The left graph shows the school enrollment rate for primary education as a percentage of the gross enrollment ratio. There are significant fluctuations, with peaks suggesting periods when enrollment rates were above the total age-relevant population. This could be due to repeat students or late enrollments. The dips, especially the sharp one around the late 2000s, could indicate political or economic events that temporarily reduced enrollment. Overall, the rate remains above 90%, indicating a high level of primary education participation.</li>
          
          <li><strong>Secondary School Enrollment:</strong> The right graph depicts secondary school enrollment as a percentage of the gross enrollment ratio. The trend shows growth from the 1980s to the early 2000s, indicating an increasing capacity and emphasis on secondary education. The graph also shows variability, with some sharp declines and subsequent recoveries, possibly reflecting changes in educational policy, economic conditions, or demographic shifts. The general upward trend, however, suggests progress in the educational sector's development.</li>
          
          <li><strong>In summary:</strong> These education graphs reflect Bahrain's commitment to education with generally high enrollment rates. The volatility seen in the graphs may reflect the impact of various external factors on education participation. Despite these fluctuations, the overall trend towards increased enrollment in primary and secondary education suggests a positive outlook for the educational attainment of the population in Bahrain. This investment in education aligns with global trends emphasizing the importance of human capital development as a pillar for economic growth and social development.</li>
        </ul>")
      }
    }
  })
  
  # Render the plot for "Comparison with Other Island Nations"
  output$island_comparison_plot <- renderPlot({
    if(input$section == "Comparison with Other Island Nations") {

      
      # Prepare the data
      data <- data.frame(
        Country = rep(c("Bahrain", "Cyprus", "Maldives"), each = 6),
        Indicator = rep(c("GDP Per Capita", "GDP PPP", "Education Bachelors %",
                          "Gov Expenditure Per Student %", "Private Health Expenditure %",
                          "Health Expenditure Per Capita"), times = 3),
        Value = c(9058.92, 76342224646, 23.33, 11.16, 37.30, 1109.99,  # Bahrain
                  26653.36, 39653965589, 29.79, 31.88, 21.49, 2245.36,  # Cyprus
                  156110.02, 10988789343, 6.36, 16.08, 18.31, 825.57)   # Maldives
      )
      
      # Plotting
      ggplot(data, aes(x = Indicator, y = Value, fill = Country)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_wrap(~Indicator, scales = "free", ncol = 2) + # Adjust number of columns as needed
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"), # X axis labels
          axis.text.y = element_text(size = 12, face = "bold"), # Y axis labels
          axis.title.x = element_text(size = 14, face = "bold"), # X axis title
          axis.title.y = element_text(size = 14, face = "bold"), # Y axis title
          plot.title = element_text(size = 16, face = "bold"), # Plot title
          plot.margin = unit(c(1, 1, 1, 1), "cm"), # Adjust margins as needed
          legend.position = "bottom", # Position the legend at the bottom
          legend.text = element_text(size = 12), # Legend text
          legend.title = element_text(size = 14, face = "bold") # Legend title
        ) +
        guides(fill = guide_legend(title = "Country")) + # Set legend title
        labs(
          title = "Country Comparison across Various Indicators", 
          x = "Indicator", 
          y = "Value"
        )
      
     
      
     
    }
  })
  
  # # Text explanation for the comparison plot
  # output$comparison_explanation <- renderUI({
  #   if(input$section == "Comparison with Other Island Nations") {
  #     HTML("
  #     <ul>
  #       <li><strong>Education Scores:</strong> The first graph compares education scores among the three countries. Bahrain appears to have a lower score than Cyprus but higher than the Maldives, suggesting Bahrain falls in the middle in terms of this particular education quality or achievement metric.</li>
  #       
  #       <li><strong>Emissions per Capita:</strong> In the second graph, Bahrain's emissions per capita are the highest among the three, with Cyprus and the Maldives having significantly lower figures. This could be indicative of Bahrain's industrial activities, energy consumption, and reliance on fossil fuels.</li>
  #       
  #       <li><strong>GDP PPP (Purchasing Power Parity):</strong> The third graph shows the GDP at purchasing power parity, where Bahrain has a lower value than Cyprus but is ahead of the Maldives. GDP PPP is a measure that accounts for the relative cost of living and inflation rates between countries, suggesting that Bahrain's economy, when adjusted for these factors, is smaller than Cyprus’s but larger than the Maldives’s.</li>
  #       
  #       <li><strong>Gov Expenditure per Student:</strong> The fourth graph indicates government expenditure per student, with Bahrain spending less than Cyprus but more than the Maldives. This reflects the financial resources allocated by the government towards each student's education.</li>
  #       
  #       <li><strong>Health Expenditure Per Capita:</strong> The fifth graph compares health expenditure per capita. Here, Bahrain is again in the middle, spending more than the Maldives but less than Cyprus. This measure reflects the amount of financial resources provided for health services per person.</li>
  #       
  #       <li><strong>Private Health Expenditure %:</strong> The last graph shows private health expenditure as a percentage of the total health expenditure. Bahrain has the highest percentage among the three, indicating a larger role for private spending in healthcare compared to Cyprus and the Maldives.</li>
  #       
  #       <li><strong>In summary:</strong> the charts suggest that Bahrain has relatively high emissions per capita and a significant private sector role in health expenditure compared to Cyprus and the Maldives. It occupies a middle position in terms of education scores, GDP PPP, government expenditure on education, and health expenditure per capita.</li>
  #     </ul>
  #   ")
  #   }
  # })
  
  output$comparison_explanation <- renderUI({
    if(input$section == "Comparison with Other Island Nations") {
      tabsetPanel(
        tabPanel("Education Scores", HTML("<li><strong>Education Scores:</strong> The first graph compares education scores among the three countries. Bahrain appears to have a lower score than Cyprus but higher than the Maldives, suggesting Bahrain falls in the middle in terms of this particular education quality or achievement metric.</li>")),
        tabPanel("Emissions per Capita", HTML("<li><strong>Emissions per Capita:</strong> In the second graph, Bahrain's emissions per capita are the highest among the three, with Cyprus and the Maldives having significantly lower figures. This could be indicative of Bahrain's industrial activities, energy consumption, and reliance on fossil fuels.</li>")),
        tabPanel("GDP PPP", HTML("<li><strong>GDP PPP (Purchasing Power Parity):</strong> The third graph shows the GDP at purchasing power parity, where Bahrain has a lower value than Cyprus but is ahead of the Maldives. GDP PPP is a measure that accounts for the relative cost of living and inflation rates between countries, suggesting that Bahrain's economy, when adjusted for these factors, is smaller than Cyprus’s but larger than the Maldives’s.</li>")),
        tabPanel("Gov Expenditure per Student", HTML("<li><strong>Gov Expenditure per Student:</strong> The fourth graph indicates government expenditure per student, with Bahrain spending less than Cyprus but more than the Maldives. This reflects the financial resources allocated by the government towards each student's education.</li>")),
        tabPanel("Health Expenditure Per Capita", HTML("<li><strong>Health Expenditure Per Capita:</strong> The fifth graph compares health expenditure per capita. Here, Bahrain is again in the middle, spending more than the Maldives but less than Cyprus. This measure reflects the amount of financial resources provided for health services per person.</li>")),
        tabPanel("Private Health Expenditure %", HTML("<li><strong>Private Health Expenditure %:</strong> The last graph shows private health expenditure as a percentage of the total health expenditure. Bahrain has the highest percentage among the three, indicating a larger role for private spending in healthcare compared to Cyprus and the Maldives.</li>")),
        tabPanel("Summary", HTML("<li><strong>In summary:</strong> the charts suggest that Bahrain has relatively high emissions per capita and a significant private sector role in health expenditure compared to Cyprus and the Maldives. It occupies a middle position in terms of education scores, GDP PPP, government expenditure on education, and health expenditure per capita.</li>"))
      )
    }
  })
  # Render the SWOT Analysis content
  output$swot_analysis <- renderUI({
    if(input$section == "SWOT Analysis") {
      tabsetPanel(
        tabPanel("Strengths", HTML(paste0("<ol>",
                                          "<li><strong>Economic Growth:</strong> Bahrain shows a consistent upward trend in GDP and GDP per capita, indicating robust economic growth and improvement in the standard of living.</li>",
                                          "<li><strong>Urbanization:</strong> A high and increasing urban population percentage suggests a potential for economic diversification and the development of urban infrastructure.</li>",
                                          "<li><strong>Education Commitment:</strong> Despite fluctuations, high enrollment rates in primary and secondary education indicate a strong commitment to education.</li>",
                                          "<li><strong>Healthcare Spending:</strong> Moderate health expenditure per capita shows investment in the healthcare sector, with a significant role for private spending, suggesting a strong private healthcare industry.</li>",
                                          "</ol>"
        ))),
        tabPanel("Weaknesses", HTML(paste0("<ol>",
                                           "<li><strong>Economic Volatility:</strong> The GDP growth rate shows volatility, reflecting susceptibility to external economic shocks, likely due to reliance on oil exports.</li>",
                                           "<li><strong>Educational Scores:</strong> Lower educational scores compared to similar nations may indicate room for improvement in the quality of education.</li>",
                                           "<li><strong>Environmental Impact:</strong> High emissions per capita demonstrate environmental challenges associated with industrial and energy sectors.</li>",
                                           "</ol>"
        ))),
        tabPanel("Opportunities", HTML(paste0("<ol>",
                                              "<li><strong>Diversification:</strong> With a growing urban population, there is an opportunity to diversify the economy beyond oil into sectors such as finance, tourism, and information technology.</li>",
                                              "<li><strong>Educational Improvement:</strong> By investing in education quality and infrastructure, Bahrain can enhance its human capital to compete on a global scale.</li>",
                                              "<li><strong>Healthcare Development:</strong> The existing private healthcare expenditure indicates a potential market for healthcare services and medical tourism.</li>",
                                              "</ol>"
        ))),
        tabPanel("Threats", HTML(paste0("<ol>",
                                        "<li><strong>Economic Dependency:</strong> Bahrain's economy is vulnerable to fluctuations in the global oil market, which poses a risk to economic stability.</li>",
                                        "<li><strong>Environmental Sustainability:</strong> The high rate of emissions per capita suggests sustainability issues that could lead to long-term environmental and health problems.</li>",
                                        "<li><strong>Regional Instability:</strong> Being in a geopolitically sensitive region, Bahrain could be affected by regional tensions and conflicts that might impact its economic and social stability.</li>",
                                        "</ol>"
        ))),
        tabPanel("Summary", HTML(paste0("<p>In conclusion, Bahrain possesses a strong economic foundation and is making substantial investments in urbanization and healthcare. However, it faces challenges in terms of economic diversification, educational improvements, and environmental impact. The country has the opportunity to leverage its strengths to mitigate these challenges and capitalize on new avenues for growth, particularly in the non-oil sectors. The regional political climate remains a significant external factor that Bahrain will need to navigate carefully.</p>"
        )))
      )
    }
  })
  
  # # Render SWOT Analysis text
  # output$swot_analysis <- renderUI({
  #   if(input$section == "SWOT Analysis") {
  #     HTML("
  #       <h4>SWOT Analysis of Bahrain</h4>
  #       <h5>Strengths:</h5>
  #       <ul>
  #         <li><strong>Economic Growth:</strong> Bahrain shows a consistent upward trend in GDP and GDP per capita, indicating robust economic growth and improvement in the standard of living.</li>
  #         <li><strong>Urbanization:</strong> A high and increasing urban population percentage suggests a potential for economic diversification and the development of urban infrastructure.</li>
  #         <li><strong>Education Commitment:</strong> Despite fluctuations, high enrollment rates in primary and secondary education indicate a strong commitment to education.</li>
  #         <li><strong>Healthcare Spending:</strong> Moderate health expenditure per capita shows investment in the healthcare sector, with a significant role for private spending, suggesting a strong private healthcare industry.</li>
  #       </ul>
  #       <h5>Weaknesses:</h5>
  #       <ul>
  #         <li><strong>Economic Volatility:</strong> The GDP growth rate shows volatility, reflecting susceptibility to external economic shocks, likely due to reliance on oil exports.</li>
  #         <li><strong>Educational Scores:</strong> Lower educational scores compared to similar nations may indicate room for improvement in the quality of education.</li>
  #         <li><strong>Environmental Impact:</strong> High emissions per capita demonstrate environmental challenges associated with industrial and energy sectors.</li>
  #       </ul>
  #       <h5>Opportunities:</h5>
  #       <ul>
  #         <li><strong>Diversification:</strong> With a growing urban population, there is an opportunity to diversify the economy beyond oil into sectors such as finance, tourism, and information technology.</li>
  #         <li><strong>Educational Improvement:</strong> By investing in education quality and infrastructure, Bahrain can enhance its human capital to compete on a global scale.</li>
  #         <li><strong>Healthcare Development:</strong> The existing private healthcare expenditure indicates a potential market for healthcare services and medical tourism.</li>
  #       </ul>
  #       <h5>Threats:</h5>
  #       <ul>
  #         <li><strong>Economic Dependency:</strong> Bahrain’s economy is vulnerable to fluctuations in the global oil market, which poses a risk to economic stability.</li>
  #         <li><strong>Environmental Sustainability:</strong> The high rate of emissions per capita suggests sustainability issues that could lead to long-term environmental and health problems.</li>
  #         <li><strong>Regional Instability:</strong> Being in a geopolitically sensitive region, Bahrain could be affected by regional tensions and conflicts that might impact its economic and social stability.</li>
  #       </ul>
  #       <p>In conclusion, Bahrain possesses a strong economic foundation and is making substantial investments in urbanization and healthcare. However, it faces challenges in terms of economic diversification, educational improvements, and environmental impact. The country has the opportunity to leverage its strengths to mitigate these challenges and capitalize on new avenues for growth, particularly in the non-oil sectors. The regional political climate remains a significant external factor that Bahrain will need to navigate carefully.</p>
  #     ")
  #   }
  # })
  
}

# Run the application
shinyApp(ui = ui, server = server)