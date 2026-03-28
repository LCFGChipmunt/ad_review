## Eczema Review Interactive tool ##
library(shinyjs)
library(shiny)
library(plotly)
library(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(readr)
library(DT)

## already added links, cleaned up the data and added in Author_date
## copy over to rshiny app file

# Load datasets
studies_415_cleaned_LINKS = read.csv("studies_415_cleaned_LINKS.csv")
immunome_TABLE = read.csv("immunome_rshinyapp.csv")
genome_TABLE = read.csv("genome_rshinyapp.csv")
microbiome_TABLE = read.csv("microbiome_rshinyapp.csv")
metabolome_TABLE = read.csv("metabolome_rshinyapp.csv")

immunome_ref <- paste(readLines("immunome_ref.txt", warn = FALSE), collapse = "\n")
microbiome_ref <- paste(readLines("microbiome_ref.txt", warn = FALSE), collapse = "\n")
metabolome_ref <- paste(readLines("metabolome_ref.txt", warn = FALSE), collapse = "\n")

## install DT package, and display the biomarker frequency and filtered tables as DT

## Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs for JavaScript operations
  tags$head(tags$style(HTML("
        #mainContent, #infoPage, #aboutPage, #genomePage, #immunomePage, #microbiomePage, #metabolomePage {\
        display: none; /* Hide other pages initially */\
        }\
        #landingPage {
        text-align: center;
        padding-top: 10%;
        background-image: url('eczema_wallpaper.png');
        background-size: cover
        background-position: center;
        opacity: 0.95; /* Adjust background opacity */
        height: 100vh;  # Makes the div full viewport height
        }
        #scrollableSidebar {
        max-height: 90vh; /* Limit height to 90% of the viewport */
        overflow-y: auto; /* Enable vertical scrolling */
        padding-right: 10px; /* Add space for scrollbar */
        }
        #getStartedBtn {
        background-color: #FFDBBB !important; /* Light orange */
        border-color: #FFDBBB !important;
        color: black !important;
        font-weight: bold;
        display: block; /* Stack buttons vertically */\
        margin: 10px auto; /* Center align */\
        }
        #findOutMoreBtn {
        background-color: #FFDBBB !important; /* Light orange */
        border-color: #FFDBBB !important;
        color: black !important;
        font-weight: bold;
        display: block; /* Stack buttons vertically */\
        margin: 10px auto; /* Center align */\
        }
        #diveBtn {
        background-color: #FFDBBB !important; /* Light orange */
        border-color: #FFDBBB !important;
        color: black !important;
        font-weight: bold;
        display: block; /* Stack buttons vertically */\
        margin: 10px auto; /* Center align */\
        }
        #footerText {\
        position: absolute;\
        bottom: 10px;\
        right: 20px;\
        font-size: 12px;\
        color: black;\
        opacity: 0.7;\
        }\
        #aboutBtn {\
        position: absolute;\
        bottom: 10px;\
        left: 20px;\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        }\
        #backToHomeBtn {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_home_main {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_home_info {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_home_about {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_home_immunome {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_home_genome {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_home_microbiome {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_home_metabolome {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_picker_immunome {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_picker_genome {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_picker_microbiome {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
        #back_picker_metabolome {\
        background-color: #FFDBBB !important; /* Light orange */\
        border-color: #FFDBBB !important;\
        color: black !important;\
        font-weight: bold;\
        margin-top: 20px;\
        }\
    "))),

  # Landing page content
  div(id = "landingPage",
      div(style = "
    background-color: rgba(255, 255, 255, 0.8); 
    padding: 20px; 
    border-radius: 15px; 
    display: inline-block; 
    margin-bottom: 20px;
    max-width: 80%;
  ",
      h1("Cross-Omics Biomarker Insights into Childhood Atopic Dermatitis: An AI-Augmented Comprehensive Review",
             style = "font-weight: bold; font-size: 28px;")
      ),
    br(),
    actionButton("findOutMoreBtn", "Find out about this tool!", class = "btn btn-success btn-lg"),
    actionButton("getStartedBtn", "Explore an overview of AD biomarkers!", class = "btn btn-success btn-lg"),
    actionButton("diveBtn", "Dive deeper into AD biomarkers!", class = "btn btn-success btn-lg"),
    actionButton("aboutBtn", "About Us", class = "btn btn-success btn-lg"),
    div(id = "footerText", "Background image generated by AI")
  ),

  # Info page, which is initially hidden
  div(id = "infoPage",
    actionButton("back_home_info", "Back to Home", class = "btn btn-lg"),
    h1("About this tool"),
    h4("This study reviews biomarkers associated with childhood atopic dermatitis from various omics. This web app summarises the 415 studies we have identified in an interactive and easy way for researchers to query relevant biomarkers and their associated studies for further exploration."),
    h1("How were the studies identified?"),
    img(src = "prisma.png", width = "80%"),
    br()
  ),

  # About Us page, which is initially hidden
  div(id = "aboutPage",
    actionButton("back_home_about", "Back to Home", class = "btn btn-lg"),
    h2("About Us"),
    h3("This work is led by A/Prof Caroline Lee from the Department of Biochemistry, NUS Medicine."),
    h3(a("Jia Wei Lee", href = "https://orcid.org/0009-0009-6125-4753", target = "_blank")),
    h3(a("Dr Evelyn Xiu Ling Loo", href = "https://orcid.org/0000-0001-7690-3191", target = "_blank")),
    h3(a("A/Prof Samuel Siong Chuan Chong", href = "https://orcid.org/0000-0002-1872-5937", target = "_blank")),
    h3(a("A/Prof Kenneth Hon Kim Ban", href = "https://orcid.org/0000-0001-7165-8713", target = "_blank")),
    h3(a("A/Prof Caroline Guat Lay Lee", href = "https://orcid.org/0000-0002-4323-3635", target = "_blank")),
    h3("For more information, please feel free to email bchleec@nus.edu.sg"),
    br()
  ),
  
  # Immunome Page
  div(id = "immunomePage",
      actionButton("back_home_immunome", "Back to Home", class = "btn btn-lg"),
      actionButton("back_picker_immunome", "Pick another Biomarker Type", class = "btn btn-lg"),
      h2("Immunome Biomarkers"),
      DTOutput("immunome_TABLE"),
      h3("References:"),
      div( style = "white-space: pre-wrap;",immunome_ref),
      br()
  ),
  
  # Genome Page
  div(id = "genomePage",
      actionButton("back_home_genome", "Back to Home", class = "btn btn-lg"),
      actionButton("back_picker_genome", "Pick another Biomarker Type", class = "btn btn-lg"),
      h2("Genome Biomarkers"),
      DTOutput("genome_TABLE"),
      h3("The variant annotation information is based on the Ensembl Variant Effect Predictor (VEP) database, while information on transcription binding (transcription factor ChIP peaks and chromatin accessibility) is extracted from RegulomeDB. For those without information in VEP (indicated in table), the functionalities were inferred."),
      br()
  ),
  
  # Microbiome Page
  div(id = "microbiomePage",
      actionButton("back_home_microbiome", "Back to Home", class = "btn btn-lg"),
      actionButton("back_picker_microbiome", "Pick another Biomarker Type", class = "btn btn-lg"),
      h2("Microbiome Biomarkers"),
      DTOutput("microbiome_TABLE"),
      h3("References:"),
      div( style = "white-space: pre-wrap;",microbiome_ref),
      br()
  ),
  
  # Metabolome Page
  div(id = "metabolomePage",
      actionButton("back_home_metabolome", "Back to Home", class = "btn btn-lg"),
      actionButton("back_picker_metabolome", "Pick another Biomarker Type", class = "btn btn-lg"),
      h2("Metabolome Biomarkers"),
      DTOutput("metabolome_TABLE"),
      h3("References:"),
      div( style = "white-space: pre-wrap;",metabolome_ref),
      br()
  ),
  
  # Main content, which is initially hidden
  div(id = "mainContent",
    actionButton("back_home_main", "Back to Home", class = "btn btn-lg"),
    h1("Overview of Childhood AD Biomarkers"),
    h4("Apply filters on the left or use the search bars on the right to locate specific biomarkers/ studies."),
    actionButton("helpBtn", "Help", icon = icon("info-circle"), class = "btn btn-lg"),
    sidebarLayout(
      sidebarPanel(
        div(
          id = "scrollableSidebar",
          uiOutput("year_ui"),
          uiOutput("country_ui"),
          uiOutput("study_design_ui"),
          uiOutput("comparison_type_ui"),
          uiOutput("biomarker_type_ui"),
          uiOutput("biomarker_site_ui")
        )
      ),
      mainPanel(
        h4("Word cloud of biomarkers with frequency > 1"),
        plotOutput(outputId = "biomarker_freq_wordcloud"),
        hr(),

        h4("Summary of biomarkers"),
        DTOutput(outputId = "biomarker_freq_table"),
        hr(),
        
        h4("Summary of studies"),
        # Output: Display the filtered table
        DTOutput(outputId = "filtered_table"),
        hr()
      )
    )
  )
)

## Define server logic
server <- function(input, output, session) {
  
  ## Define modal helper function
  openDeepDiveModal <- function() {
    shiny::showModal(
      shiny::modalDialog(
        title = "Select a biomarker space",
        radioButtons("deepChoice", NULL,
                     choices = c("Immunome","Genome","Microbiome","Metabolome"),
                     selected = "Immunome"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmDeepDive", "Go", class = "btn btn-primary")
        ),
        easyClose = TRUE
      )
    )
  }
  
  # Register all page IDs here (add your new pages too)
  all_pages <- c("landingPage", "mainContent", "infoPage", "aboutPage",
                 "genomePage", "immunomePage", "microbiomePage", "metabolomePage")
  
  ## function to transition between pages
  showPage <- function(id) {
    lapply(all_pages, function(x) shinyjs::hide(x))
    shinyjs::show(id)
  }
  
  ## transition between pages
  # Explore overview
  observeEvent(input$getStartedBtn,  { showPage("mainContent") })
  
  # Info
  observeEvent(input$findOutMoreBtn, { showPage("infoPage") })
  
  # About
  observeEvent(input$aboutBtn,       { showPage("aboutPage") })
  
  ## Back buttons
  observeEvent(input$back_home_info,       { showPage("landingPage") })
  observeEvent(input$back_home_main,       { showPage("landingPage") })
  observeEvent(input$back_home_about,      { showPage("landingPage") })
  observeEvent(input$back_home_immunome,   { showPage("landingPage") })
  observeEvent(input$back_home_genome,     { showPage("landingPage") })
  observeEvent(input$back_home_microbiome, { showPage("landingPage") })
  observeEvent(input$back_home_metabolome, { showPage("landingPage") })
  
  # Open the modal selector
  observeEvent(input$diveBtn, {
    openDeepDiveModal()
  })
  
  # Open the modal selector from the deep dive pages
  observeEvent(input$back_picker_immunome,   { openDeepDiveModal() })
  observeEvent(input$back_picker_genome,     { openDeepDiveModal() })
  observeEvent(input$back_picker_microbiome, { openDeepDiveModal() })
  observeEvent(input$back_picker_metabolome, { openDeepDiveModal() })

  # Route to the selected page
  observeEvent(input$confirmDeepDive, {
    req(input$deepChoice)
    removeModal()
    dest <- switch(tolower(input$deepChoice),
      "genome"     = "genomePage",
      "immunome"   = "immunomePage",
      "microbiome" = "microbiomePage",
      "metabolome" = "metabolomePage",
      "landingPage"
    )
    showPage(dest)
  })

  ## Help button
  observeEvent(input$helpBtn, {
    showModal(modalDialog(
      title = "Review of Childhood AD Biomarkers",
      "Use this interactive tool to explore key biomarkers identified in studies of childhood AD. Apply filters to tailor the results to your research interests. For targeted queries, use the search bar on the right to locate specific biomarkers/ studies.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Dynamically generate UI inputs for filtering
  output$year_ui <- renderUI({
    req(studies_415_cleaned_LINKS)  # Ensure the dataset is available
    choices = c("ALL", sort(unique(studies_415_cleaned_LINKS$Publication_year)))
    selectInput("year", "Select Publication Year", choices = choices, selected = "ALL", multiple = T)
  })

  output$country_ui <- renderUI({
    req(studies_415_cleaned_LINKS)  # Ensure the dataset is available
    choices = c("ALL", sort(unique(studies_415_cleaned_LINKS$Country)))
    selectInput("country", "Select Country (Country/ countries of the corresponding author's affiliations)", choices = choices, selected = "ALL", multiple = T)
  })
  
  output$study_design_ui <- renderUI({
    req(studies_415_cleaned_LINKS)  # Ensure the dataset is available
    choices = c("ALL", sort(unique(studies_415_cleaned_LINKS$Study_design)))
    selectInput("study_design", "Select Study Design", choices = choices, selected = "ALL", multiple = T)
  })
  
  output$comparison_type_ui <- renderUI({
    req(studies_415_cleaned_LINKS)  # Ensure the dataset is available
    choices = c("ALL", sort(unique(studies_415_cleaned_LINKS$Comparison_type)))
    selectInput("comparison_type", "Select Comparison Type", choices = choices, selected = "ALL", multiple = T)
  })
    
  output$biomarker_type_ui <- renderUI({
    req(studies_415_cleaned_LINKS)  # Ensure the dataset is available
    choices = c("ALL", sort(unique(studies_415_cleaned_LINKS$Biomarker_type)))
    selectInput("biomarker_type", "Select Biomarker Type", choices = choices, selected = "ALL", multiple = T)
  })

  output$biomarker_site_ui <- renderUI({
    req(studies_415_cleaned_LINKS)  # Ensure the dataset is available
    choices = c("ALL", sort(unique(studies_415_cleaned_LINKS$Biomarker_site)))
    selectInput("biomarker_site", "Select Biomarker Site", choices = choices, selected = "ALL", multiple = T)
  })  
  
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    req(studies_415_cleaned_LINKS)

    df <- studies_415_cleaned_LINKS  # Assign the loaded dataset

    # Apply filters based on user input
    if (!is.null(input$year) && !"ALL" %in% input$year) {
      df <- df %>% filter(Publication_year %in% input$year)
    }

    if (!is.null(input$country) && !"ALL" %in% input$country) {
      df <- df %>% filter(Country %in% input$country)
    }
    
    if (!is.null(input$study_design) && !"ALL" %in% input$study_design) {
      df <- df %>% filter(Study_design %in% input$study_design)
    }
    
    if (!is.null(input$comparison_type) && !"ALL" %in% input$comparison_type) {
      df <- df %>% filter(Comparison_type %in% input$comparison_type)
    }
    
    if (!is.null(input$biomarker_type) && !"ALL" %in% input$biomarker_type) {
      df <- df %>% filter(Biomarker_type %in% input$biomarker_type)
    }
    
    if (!is.null(input$biomarker_site) && !"ALL" %in% input$biomarker_site) {
      df <- df %>% filter(Biomarker_site %in% input$biomarker_site)
    }

    return(df)
  })
  
  # # Output: Display the filtered table
  # output$filtered_table <- renderTable({
  #   req(filtered_data())  # Ensure there is data to display
  #   filtered_data()  # Show the filtered dataframe
  # })
  
  ## OUTPUT: IMMUNOME_TABLE
  output$immunome_TABLE <- renderDT({
    datatable(immunome_TABLE,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  ## OUTPUT: GENOME_TABLE
  output$genome_TABLE <- renderDT({
    datatable(genome_TABLE,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  ## OUTPUT: MICROBIOME_TABLE
  output$microbiome_TABLE <- renderDT({
    datatable(microbiome_TABLE,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  ## OUTPUT: METABOLOME_TABLE
  output$metabolome_TABLE <- renderDT({
    datatable(metabolome_TABLE,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Output: Display the filtered table with links
  output$filtered_table <- renderDT({
    df <- filtered_data()
    
    # Create HTML link
    df$URL <- paste0("<a href='", df$Link, "' target='_blank'>View Study</a>")
    df_display <- df[, c("Author_date", "Study_title", "Publication_year", "URL",
                         "Country", "Study_design", "Sample_size", "Ages",
                         "Sequencing_methods", "Sequencing_platforms", "Statistical_methods", "Comparison_type",
                         "Biomarker_name", "Biomarker_type", "Biomarker_site", "Relationship", "Effect_size"
                         )]
    datatable(df_display, escape = FALSE)
  })
  
  # Output: Display the frequencies of biomarkers
  output$biomarker_freq_table <- renderDT({
    req(filtered_data())  # Ensure there is data to display
    
    # Calculate frequency of Biomarker_name column
    biomarker_freq <- table(filtered_data()$Biomarker_name)
    biomarker_freq_df <- data.frame(Biomarker_name = names(biomarker_freq), Frequency = as.numeric(biomarker_freq))
    
    # Filter out rows with frequency 0
    biomarker_freq_df <- biomarker_freq_df[biomarker_freq_df$Frequency > 0, ]
    
    # Sort the data frame by Publication_year in descending order
    biomarker_freq_df <- biomarker_freq_df[order(biomarker_freq_df$Frequency, decreasing = TRUE), ]
    
    datatable(biomarker_freq_df, escape = FALSE, rownames = FALSE)
  })

  # Reactive expression to generate word cloud based on filtered data
  output$biomarker_freq_wordcloud <- renderPlot({
    req(filtered_data())  # Ensure filtered data is available

    # Calculate frequency of Biomarker_name column
    biomarker_freq <- table(filtered_data()$Biomarker_name)
    biomarker_freq_df <- data.frame(Biomarker_name = names(biomarker_freq), Frequency = as.numeric(biomarker_freq))

    # Filter out rows with frequency 0
    biomarker_freq_df <- biomarker_freq_df[biomarker_freq_df$Frequency > 0, ]
    biomarker_freq_df <- biomarker_freq_df[order(biomarker_freq_df$Frequency, decreasing = TRUE), ]

    # Plot the word cloud
    wordcloud(words = biomarker_freq_df$Biomarker_name, 
              freq = biomarker_freq_df$Frequency, 
              min.freq = 2, 
              scale = c(2, 0.5), 
              colors = brewer.pal(8, "Dark2"))
  })

}

# Run the application
shinyApp(ui = ui, server = server)


