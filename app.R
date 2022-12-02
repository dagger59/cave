library(dplyr)
library(ggplot2)
library(waffle)
library(shiny)
library(DT)
library(shinycssloaders)
library(shinythemes)
library(shinybusy)

# Wordclouds
# SEE: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
library(wordcloud)
library(RColorBrewer)
library(tm)

# Waffle chart
the.free.dict <- "https://encyclopedia.thefreedictionary.com/"

#Import Data
vaers.sub.df <- read.csv("cave-sub-12-31-2021.csv")
vaers.sub.df$VAX_DATE_R <- as.Date(vaers.sub.df$VAX_DATE, "%m/%d/%Y")
vaers.grp.df <- read.csv("cave-grp-12-31-2021.csv")
vaers.died.df <- read.csv("cave-died-12-31-2021.csv")
vaers.died.df$VAX_DATE_R <- as.Date(vaers.died.df$VAX_DATE, "%m/%d/%Y")
vaers.died.df$DATEDIED_R <- as.Date(vaers.died.df$DATEDIED, "%m/%d/%Y")
vaers.died.df$DiedDays <- difftime(vaers.died.df$DATEDIED_R,vaers.died.df$VAX_DATE_R,units=c("days"))

# CSS for the UI
button_color_css <- "
#SearchBtnBlue {
  background: DodgerBlue;
  font-size: 15px;
}
.rightAlign{float:right;}

.centreDiv {
  text-align: center;
  vertical-align: middle;
}
.centreBox {
  margin: 10px 0px 10px 0px;
  height: 130px;
  text-align: center;
  vertical-align: middle;
  padding-top: 15px;
  color: antiquewhite;
}
.centreBox a {
  color: #8df1ec;
}
.ageBackground { background: #f3f3f3; 
                 color: #ffffff; 
                 border: 2px solid #b3adad;
}
.plumBackground { background: #d75e85; }
.plumBackground h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }
#.plumBackground:hover { background: #74243e; cursor: pointer; }
.plumBackground2 { background: #bf3b65; }
.plumBackground2 h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }
#.plumBackground2:hover { background: #74243e; cursor: pointer;}
.plumBackground3 { background: #95284a; }
.plumBackground3 h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }
#.plumBackground3:hover { background: #74243e; cursor: pointer; }


"

# Define UI
ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("CAVE", theme = shinytheme("spacelab"),
             tabPanel("Search", fluid = TRUE, icon = icon("search"),
                      tags$style(button_color_css),
                      tags$head(tags$link(rel="shortcut icon", href="favicon/database-32-171970.png", sizes="16x16")),
                      tags$head(tags$link(rel="shortcut icon", href="favicon/database-32-171970.png", sizes="32x32")),
                      tags$head(tags$link(rel="canonical", href="https://vaccines.shinyapps.io/cave/", sizes="32x32")),

                      # Open Graph
                      tags$head(tags$meta(property="og:title", content="Coronavirus Adverse Vaccine Events (CAVE)")),
                      tags$head(tags$meta(property="og:site_name", content="Coronavirus Adverse Vaccine Events (CAVE)")),
                      tags$head(tags$meta(property="og:url", content="https://vaccines.shinyapps.io/cave/")),
                      tags$head(tags$meta(property="og:description", content="Search 500,000 records and get personalised results based on age, gender, allergies and medical history.")),
                      tags$head(tags$meta(property="og:type", content="website")),
                      tags$head(tags$meta(property="og:image", content="https://images.unsplash.com/photo-1618961734760-466979ce35b0?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1032&q=80")),
                      #tags$head(tags$meta(property="", content="")),

                      # Other pre-load setup                      
                      add_busy_bar(color = "DodgerBlue", height = "5px"),

                      # Use manual spinners wih event
                      sidebarLayout(
                        sidebarPanel(
                          h3("Please report an adverse reaction ", a("here", href = "https://docs.google.com/forms/d/e/1FAIpQLSeg4yRtnzoFVmiFAmEu9cVRUj9pWzacCyUEaXV50o6gmApcuQ/viewform?usp=sf_link", target="_blank")),
                          helpText("Or fill out the form below and search up to 500,000 records to get a personalised view of Coronavirus Adverse Vaccine Events (CAVE)"),
                          titlePanel("Search"),
                          fluidRow(column(5,
                                          # Select which Gender(s) to plot
                                          checkboxGroupInput(inputId = "genderFinder",
                                                             label = "Select Gender(s):",
                                                             choices = c("Male" = "M", "Female" = "F"),
                                                             selected = c("M","F")
                                          ),
                                          
                                          # Select Age
                                          selectInput(inputId = "ageGroup",
                                                      label = "Select Age Group",
                                                      choices = c("0-9 Yrs"   = "A. (0-9)", 
                                                                  "10-19 Yrs" = "B (10-19)",
                                                                  "20-29 Yrs" = "C (20-29)",
                                                                  "30-39 Yrs" = "D (30-39)",
                                                                  "40-49 Yrs" = "E (40-49)",
                                                                  "50-59 Yrs" = "F (50-59)",
                                                                  "60-69 Yrs" = "G (60-69)",
                                                                  "70-79 Yrs" = "H (70-79)",
                                                                  "80-89 Yrs" = "I (80-89)",
                                                                  "90-99 Yrs" = "J (90-99)",
                                                                  "100-109 Yrs" = "K (100-109)",
                                                                  "110-119 Yrs" = "L (110-119)",
                                                                  "Unknown" = "U (U) Unknown"
                                                      ),
                                                      selected = "C (20-29)",
                                                      width = "100%"
                                          ),
                                    ),
                                    column(5, offset = 2,
                                           # Select which Vaccines(s) to plot
                                           checkboxGroupInput(inputId = "vaccineFinder",
                                                              label = "Select Vaccine(s):",
                                                              choices = c("Pfizer\\Biontech", "Janssen", "Moderna"),
                                                              selected = c("Pfizer\\Biontech", "Janssen", "Moderna"))
                                    )
                          ),
                          # Set vaccine Date Range
                          # See: https://shiny.rstudio.com/gallery/date-and-date-range.html
                          #dateRangeInput('vaxDateRange',
                          #               label = 'Vaccination Date Range: mm/dd/yyyy',
                          #               min = min(vaers.sub.df$VAX_DATE_R),
                          #               max = max(vaers.sub.df$VAX_DATE_R),
                          #               start = min(vaers.sub.df$VAX_DATE_R), 
                          #               end = max(vaers.sub.df$VAX_DATE_R),
                          #               format = "mm/dd/yyyy"
                          #),
                          #helpText("Date range (min:max) is set by the database"),
                          textInput(inputId = "medFinder", 
                                    label = 'Include Medications (or leave blank)',
                                    placeholder = 'e.g. Metformin'
                          ),
                          textInput(inputId = "histFinder", 
                                    label = 'Include History (or leave blank)',
                                    placeholder = 'e.g. Diabetes'
                          ),
                          textInput(inputId = "allergiesFinder", 
                                    label = 'Include Allergies (or leave blank)',
                                    placeholder = 'e.g. Penicillin'
                          ),
                          textInput(inputId = "excludeWordsFinder", 
                                    label = 'Exclude words/phrases (or leave blank)',
                                    placeholder = 'e.g. Product storage,Site,Error'
                          ),
                          helpText("Tip: Seperate words/phrases with commas"),
                          br(),
                          actionButton(inputId = "SearchBtn", label = " Search", icon = icon("search"), style = "width: 45%"),
                          br(),
                          br(),
                          helpText("This website is free to use and provided without advertising"),
                          helpText("Tip: See 'Dashboard' on the menu for more"),
                        ),
                        mainPanel(
                          plotOutput("wordCloud"),
                          h4(textOutput("vaers_numRows", inline = TRUE)),
                          dataTableOutput(outputId = "resultsTable"),
                          hr(),
                          br()
                        )
                      )
             ),

             tabPanel("Dashboard", fluid = TRUE, icon = icon("glasses"),
                      fluidRow(
                        div(class="centreDiv", 
                            p("Data from VAERS is limited to the most recent 500,000 records."),
                            p("This data is then filtered and 'unknown' records removed, i.e. unknown gender, age and vaccine manufacturer."),
                            h2("Adverse Events and Deaths")
                        )
                      ),
                      fluidRow(style="margin: 0px 30px;",
                        column(4, 
                               div(class="centreBox plumBackground",
                                   h2(style="color: #ffffff;", textOutput("cases", inline = TRUE)),
                                   p("ADVERSE EVENTS (Total*)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground",
                                   h2(style="color: #ffffff;", textOutput("casesF", inline = TRUE)),
                                   p("ADVERSE EVENTS (F)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground",
                                   h2(style="color: #ffffff;", textOutput("casesM", inline = TRUE)),
                                   p("ADVERSE EVENTS (M)")
                               )
                        ),
                      ),
                      fluidRow(style="margin: 0px 30px;",
                        column(4, 
                               div(class="centreBox plumBackground",
                                   h2(style="color: #ffffff; ", textOutput("deaths", inline = TRUE)),
                                   p("DEATHS (Total)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground",
                                   h2(style="color: #ffffff;", textOutput("deathsF", inline = TRUE)),
                                   p("DEATHS (F)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground",
                                   h2(style="color: #ffffff;", textOutput("deathsM", inline = TRUE)),
                                   p("DEATHS (M)")
                               )
                        ),
                      ),
                      fluidRow(style="margin: 20px 30px;",
                               column(6, 
                                      div(class="centreDiv",
                                          plotOutput("adverseEventsGraph"),
                                      )
                               ),
                               column(6,
                                      div(class="centreDiv",
                                          plotOutput("deathsGraph"),
                                      )
                               ),
                      ),
                      fluidRow(
                        div(class="centreDiv", 
                            h2("Hospitalisations")
                        )
                      ),
                      fluidRow(style="margin: 0px 30px;",
                        column(4, 
                               div(class="centreBox plumBackground2",
                                   h2(style="color: #ffffff; ", textOutput("hosp", inline = TRUE)),
                                   p("HOSPITALISATIONS (Total)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground2",
                                   h2(style="color: #ffffff;", textOutput("hospF", inline = TRUE)),
                                   p("HOSPITALISATIONS (F)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground2",
                                   h2(style="color: #ffffff;", textOutput("hospM", inline = TRUE)),
                                   p("HOSPITALISATIONS (M)")
                               )
                        ),
                      ),
                      fluidRow(style="margin: 20px 30px;",
                               column(12, 
                                      div(class="centreDiv",
                                          plotOutput("hospGraph"),
                                      )
                               ),
                      ),
                      fluidRow(
                        div(class="centreDiv", 
                            h2("Life Threatening and Emergency")
                        )
                      ),
                      fluidRow(style="margin: 0px 30px;",
                        column(4,
                               div(class="centreBox plumBackground3",
                                   h2(style="color: #ffffff; ", textOutput("lthreat", inline = TRUE)),
                                   p("LIFE THREATENING (Total)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground3",
                                   h2(style="color: #ffffff;", textOutput("lthreatF", inline = TRUE)),
                                   p("LIFE THREATENING (F)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground3",
                                   h2(style="color: #ffffff;", textOutput("lthreatM", inline = TRUE)),
                                   p("LIFE THREATENING (M)")
                               )
                        ),
                      ),
                      fluidRow(style="margin: 0px 30px;",
                        column(4, 
                               div(class="centreBox plumBackground3",
                                   h2(style="color: #ffffff; ", textOutput("er", inline = TRUE)),
                                   p("EMERGENCY CASES (Total)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground3",
                                   h2(style="color: #ffffff;", textOutput("erF", inline = TRUE)),
                                   p("EMERGENCY CASES (F)")
                               )
                        ),
                        column(4,
                               div(class="centreBox plumBackground3",
                                   h2(style="color: #ffffff;", textOutput("erM", inline = TRUE)),
                                   p("EMERGENCY CASES (M)")
                               )
                        ),
                      ),
                      fluidRow(style="margin: 20px 30px;",
                               column(6,
                                      div(class="centreDiv",
                                          plotOutput("lifeThreatGraph"),
                                      )
                               ),
                               column(6,
                                      div(class="centreDiv",
                                          plotOutput("emergencyGraph"),
                                      )
                               ),
                      ),
                      fluidRow(class="centreDiv", 
                       br(),
                       p("*NOTE: Unknown reports for gender, age & manufacturer have been omitted"),
                       p("If you have any questions, feedback or problems with the site, please contact me at uea15577@gmail.com"),
                      ),
                      br(),
             ),

             navbarMenu("More", icon = icon("info-circle"),
                        tabPanel("Deaths", fluid = TRUE,
                                 fluidRow(
                                   column(12,
                                          #h3(p("Website Disclaimer")),
                                          h5(p("The Vaccines database has records where people have died.  These are the records associated with those who have a VAERS report filed following the death of an indivudual. Their inclusion is not meant to alarm or scare. Moreover, they are the final account made by either physician, friend, relative or medical practitioner and relay some very personal accounts of what is becomming an increasingly very impersonnel environment."),
                                          ),
                                          h3(p("Deaths: ", textOutput("deaths2", inline = TRUE))),
                                          helpText("TIP: Use the filter to reduce the results"),
                                          br(),
                                          dataTableOutput(outputId = "diedTable"),
                                          hr(),
                                          br(),
                                   ))
                        ),
                        
                        tabPanel("Sources", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h3(p("VAERS")),
                                          h5(p("CAVE uses data from the Vaccine Adverse Events Reporting System (", a("VAERS", href = "https://vaers.hhs.gov/"), "). VAERS is a database used to record and detect possible safety issues with vaccines. Data for adverse events from vaccines date back to 1990 and data is made publicly available to medical practitioners and the general public alike."),
                                             p("VAERS data can be entered by medical personnel and individuals and contains reports of their experiences."),
                                             p("The Center for Disease Control (", a("CDC", href = "https://www.cdc.gov/"), ") and Food and Drug Administration (", a("FDA", href = "https://www.fda.gov/"), ") jointly administer VAERS and respond to reports."),
                                             p("The data is comprehensive and provided in a way that can be analysed by the public, though skills in data handling are required to process it.  However, the CDC does provide a GUI 'like' tool called ", a("CDC WONDER", href = "https://wonder.cdc.gov/vaers.html"), " which is available to the public."),
                                             p("CAVE is kept up to date by periodically collecting the latest 500,000 adverse event records from VAERS and removes some fields to fit within the limits imposed by the website hosting company. A description of the fields can be made available by contacting me at uea15577@gmail.com."),
                                             br(),
                                             img(src = "vaers-logo.png", height = "40px"),
                                             br(),
                                             br()
                                          )
                                   ),
                                   column(6,
                                          h3(p("Other Sources")),
                                          h5(p("The following sources are used for additional data"),
                                             HTML("<ul><li>Population statistics - <a href='https://www.census.gov/topics/population.html'>US Census Department</a></li><li>COVID cases (US) - <a href='https://covid.cdc.gov/covid-data-tracker/#datatracker-home'>CDC Covid Data Tracker</a></li><li>Medical definitions - <a href='https://encyclopedia.thefreedictionary.com/'>The Free Dictionary</a></li></ul>")
                                          ),
                                          br(),
                                          h3(p("Adverse Event Reporting")),
                                          h5(p("Use following links to report any adverse events"),
                                             HTML("<ul><li>In the US - <a href='https://vaers.hhs.gov/reportevent.html'>VAERS</a></li><li>In the UK - <a href='https://coronavirus-yellowcard.mhra.gov.uk/'>Yellow Card</a></li></ul>")
                                          ),
                                          br()
                                   ))
                                 
                        ),

                        tabPanel("Disclaimer", fluid = TRUE,
                                 fluidRow(
                                   column(12,
                                          #h3(p("Website Disclaimer")),
                                          h5(p("The Coronavirus Adverse Event (CAVE) website uses the most recent 500,000 data records on the Coronavirus vaccine from the Vaccine Adverse Event Reporting System(", a("VAERS", href = "https://vaers.hhs.gov/data.html"), ") database.  A person skilled in data analytics would be able to reproduce the same results as produced here.  However, what follows is the required medical disclaimer.")),
                                          h3(p("Medical Disclaimer")),
                                          h5(p("This website does not provide medical advice It is intended for informational purposes only. It is not a substitute for professional medical advice, diagnosis or treatment. Never ignore professional medical advice in seeking treatment because of something you have read on this website."),
                                             p("All of the content provided on this website, such as text, treatments, dosages, outcomes, charts, patient profiles, graphics, photographs, images, advice, messages, forum postings, and any other material provided on the website are for informational purposes only and are not a substitute for professional medical advice or treatment."),
                                             p("Always seek the advice of your physician or other qualified health provider regarding your health. Never disregard professional medical advice or delay in seeking it because of something you have read on this website")
                                          ),
                                          br(),
                                          h4("If you think you may have a medical emergency, immediately call your doctor or dial the emergency code in your country."),
                                          br()
                                   ))
                        ),
                        
                        tabPanel("About", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h3(p("About the Project")),
                                          h5(p("The Coronavirus Adverse Vaccine Event (CAVE) project is intended to provide you with personalised information about adverse events from the coronavirus vaccine so that you can make an informed decision about if a vaccination is right for you.")),
                                          h5(p("The project began after work I had done to analyse data on the Coronavirus vaccines from sources in the US and UK to isolate side effects specific to my gender, age group, medical history and circumstance. As an analyst, I had access to the tools and data to do this, but realised that many people may not.  Therefore, I began to put the work I had done for myself into the CAVE website project and make it available to everyone for free.")),
                                          h5(p("I hope you find it interesting, helpful and/or useful.  Any comments or questions are welcome.")),
                                          br(),
                                          h4(p("Spread The Word")),
                                          h5(p("Please help spread the word by sharing it on your social networks and/or sending it to a friend"),
                                             br()
                                          ),
                                   ),
                                   column(6,
                                          h3(p("About the Author")),
                                          h5(p("Add some information"),
                                             
                                          ),
                                          hr(),
                                          HTML('<img src="pen-y-fan.jpg", height="300px"'),
                                          br()
                                   )
                                 ),
                                 br(),
                                 hr(),
                                 h5("Credits:"),
                                 h6(
                                   p("Photo by Brano on ",
                                     a("Unsplash",
                                       href = "https://unsplash.com/photos/QSuou3VAtf4"))),
                                 h6(
                                   p("Photo by Nathan Dumlao on ",
                                     a("Unsplash",
                                       href = "https://unsplash.com/photos/Y3AqmbmtLQI"))),
                                 h5("Built with",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                    ".")
                        )
             )
  )
)

# Define server
server <- function(input, output, session) {

  # -------------------------
  # Function to get search options
  getDbFromSearchOptions <- function() {
    isolate({
      # Get panel inputs
      genders <- input$genderFinder
      ageGroups <- input$ageGroup
      vaccines <- toupper(input$vaccineFinder)
      meds <- tolower(input$medFinder)
      hist <- tolower(input$histFinder)
      allergies <- tolower(input$allergiesFinder)
      excludeWords <- tolower(input$excludeWordsFinder)
      #dateRange <- input$vaxDateRange
      
      #print(paste(genders))
      #print(paste(ageGroups))
      #print(paste(vaccines))
      #print(paste("meds=", meds, " length=", nchar(meds)))
      #print(paste("hist=", hist, " length=", nchar(hist)))
      #print(paste("allergies=", allergies, " length=", nchar(allergies)))
      #print(paste("exclude=", excludeWords, " length=", length(excludeWords)))
      #print(paste("from", dateRange[1]))
      #print(paste("to", dateRange[2]))
      
      # Update reactive values
      tmp.db <- vaers.sub.df  %>% 
        filter(SEX %in% genders) %>%
        filter(AgeGroup %in% ageGroups) %>%
        filter(VAX_MANU %in% vaccines) 
        #filter(VAX_DATE_R >= dateRange[1] & VAX_DATE_R <= dateRange[2])
      
      # Previous Medications
      # TODO: handle comma separated options
      if(nchar(meds) > 1) {
        #print("processing meds")
        tmp.db <- tmp.db  %>%
          filter(grepl(meds, OTHER_MEDS))
      }
      
      # Previous History
      # TODO: handle comma separated options
      if(nchar(hist) > 1) {
        #print("processing hist")
        tmp.db <- tmp.db  %>%
          filter(grepl(hist, HISTORY))
      }
      
      # Previous Allergies
      # TODO: handle comma separated options
      if(nchar(allergies) > 1) {
        #print("processing allergies")
        tmp.db <- tmp.db  %>%
          filter(grepl(allergies, ALLERGIES))
      }
      
      # Filter out specific words/phrases that we want to exclude
      if(nchar(excludeWords) > 1) {
        # Remove leading and trailing whitespace and split into vector
        excludeWords <- trimws(excludeWords)
        excludeWords <- unlist(strsplit(excludeWords, ","))
        #print(paste("processing excluded words:", excludeWords))
        for(word in excludeWords) {
          word <- trimws(word)
          tmp.db <- tmp.db  %>%
            filter(!grepl(word, tolower(SYMPTOMS)))
            #print(paste("rows:", nrow(tmp.db)))
        }
      }
      
    })
    
    return(tmp.db)  
  }
  
  # -------------------------
  # See: https://riptutorial.com/shiny/example/32342/reactivevalues
  rVals <- reactiveValues(db=getDbFromSearchOptions())

  # -------------------------
  # Function to render a wordcloud
  renderWordcloud <- function(vaers, excludeTheseWords=NULL) {
    # --------------
    df <- vaers
    
    # Show notification 
    showNotification("Please wait", closeButton=FALSE, duration = 5)
    
    # Seed for reproducibility
    set.seed(1234)  
    
    # Setup the (filtered) data frame
    df$OTHER_MEDS <- tolower(df$OTHER_MEDS)
    df$ALLERGIES <- tolower(df$ALLERGIES)
    df$HISTORY <- tolower(df$HISTORY)
    
    showNotification(paste("Found", format(nrow(df), big.mark = ",", scientific = FALSE), "matches"), closeButton=FALSE, duration = 5)
    
    # Sample the data
    if(nrow(df) > 25000) 
    {
      df <- df[sample(nrow(df), 25000), ]
      showNotification(paste("Sampling", format(nrow(df), big.mark = ",", scientific = FALSE), "events"), closeButton=FALSE, duration = 5)
    } 
    
    # Create a vector containing only the text (remove symptom delimiter)
    #showNotification(paste("Preprocessing"), closeButton=FALSE, duration = 5)
    text <- gsub("~", " ", df$SYMPTOMS)

    # Create a corpus  
    #showNotification(paste("Creating corpus"), closeButton=FALSE, duration = 5)
    docs <- Corpus(VectorSource(text))
    
    # Clean the corpus, e.g. stop words etc.
    #showNotification(paste("Removing stop words"), closeButton=FALSE, duration = 5)
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    # Remove from docs
    #docs <- tm_map(docs, removeWords, c("foo","bar")) 

    # Create a document/term matrix
    showNotification(paste("Nearly there"), closeButton=FALSE, duration = 5)
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)

    # Remove the notification
    showNotification("Preparing wordcloud", closeButton=FALSE, duration = 5)
    
    # Generate word cloud
    par(mar = rep(0, 4))
    
    if (nrow(df) == 0) {
      ggplot() +
        theme_void() +
        ggtitle("No results fit the current selection. \nPlease modify the inputs and try again.") +
        theme(plot.title = element_text(color = "#0075ff", size = 20))
    } else {    
      wordcloud(words = df$word, freq = df$freq, min.freq = 1,
                max.words=70, random.order=FALSE, rot.per=0.0, fixed.asp = FALSE,
                colors=brewer.pal(8, "Dark2"), scale=c(10,0.3))
    }
  }
  
  # -------------------------
  output$vaers_numRows <- renderText({
    events <- nrow(rVals$db)
    died <- sum(rVals$db$DIED == "Y")
    died.pc <- format(round((died/events)*100, 1), nsmall = 1)
    
    paste0("Results: ", format(events, big.mark = ",", scientific = FALSE), " events (", died, " died ", died.pc, "%)")
  })

  # -------------------------
  output$searchTip <- renderText({
    paste0("Tip: Results appear here once you have selected inputs and pressed 'Search'")
  })
  
  # -------------------------
  getNumCases <- function() {
    cases <- sum(vaers.grp.df$n)
    return(paste0(format(cases, big.mark = ",", scientific = FALSE)))
  }
  
  getNumDeaths <- function() {
    deaths <- sum(vaers.grp.df[which(vaers.grp.df$DIED=='Y'), "n"])
    return(paste0(format(deaths, big.mark = ",", scientific = FALSE)))
  }
  
  output$cases <- renderText({
    getNumCases()
  })

  output$casesM <- renderText({
    casesM <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='M'), "n"])
    paste0(format(casesM, big.mark = ",", scientific = FALSE))
  })

  output$casesF <- renderText({
    casesF <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='F'), "n"])
    paste0(format(casesF, big.mark = ",", scientific = FALSE))
  })

  # -------------------------
  output$deaths <- renderText({
    getNumDeaths()
  })
  
  output$deaths2 <- renderText({
    getNumDeaths()
  })
  
  output$deathsM <- renderText({
    deathsM <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='M' & vaers.grp.df$DIED=='Y'), "n"])
    paste0(format(deathsM, big.mark = ",", scientific = FALSE))
  })
  
  output$deathsF <- renderText({
    deathsF <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='F' & vaers.grp.df$DIED=='Y'), "n"])
    paste0(format(deathsF, big.mark = ",", scientific = FALSE))
  })
  
  # -------------------------
  output$hosp <- renderText({
    hosp <- sum(vaers.grp.df[which(vaers.grp.df$HOSPITAL=='Y'), "n"])
    paste0(format(hosp, big.mark = ",", scientific = FALSE))
  })
  
  output$hospM <- renderText({
    hospM <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='M' & vaers.grp.df$HOSPITAL=='Y'), "n"])
    paste0(format(hospM, big.mark = ",", scientific = FALSE))
  })
  
  output$hospF <- renderText({
    hospF <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='F' & vaers.grp.df$HOSPITAL=='Y'), "n"])
    paste0(format(hospF, big.mark = ",", scientific = FALSE))
  })

  # -------------------------
  output$lthreat <- renderText({
    lthreat <- sum(vaers.grp.df[which(vaers.grp.df$L_THREAT=='Y'), "n"])
    paste0(format(lthreat, big.mark = ",", scientific = FALSE))
  })
  
  output$lthreatM <- renderText({
    lthreatM <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='M' & vaers.grp.df$L_THREAT=='Y'), "n"])
    paste0(format(lthreatM, big.mark = ",", scientific = FALSE))
  })
  
  output$lthreatF <- renderText({
    lthreatF <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='F' & vaers.grp.df$L_THREAT=='Y'), "n"])
    paste0(format(lthreatF, big.mark = ",", scientific = FALSE))
  })
  
  # -------------------------
  output$er <- renderText({
    er <- sum(vaers.grp.df[which(vaers.grp.df$ER_ED_VISIT=='Y'), "n"])
    paste0(format(er, big.mark = ",", scientific = FALSE))
  })
  
  output$erM <- renderText({
    erM <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='M' & vaers.grp.df$ER_ED_VISIT=='Y'), "n"])
    paste0(format(erM, big.mark = ",", scientific = FALSE))
  })
  
  output$erF <- renderText({
    erF <- sum(vaers.grp.df[which(vaers.grp.df$SEX=='F' & vaers.grp.df$ER_ED_VISIT=='Y'), "n"])
    paste0(format(erF, big.mark = ",", scientific = FALSE))
  })

  # -------------------------
  # observe event for updating the reactiveValues
  observeEvent(input$SearchBtn,
  {
    # Set reactive value to force refresh
    rVals$db <- getDbFromSearchOptions()
  })
  
  # -------------------------
  output$wordCloud <- renderPlot({
    renderWordcloud(rVals$db)
  })
  
  output$adverseEventsGraph <- renderPlot({
    isolate({
      df <- vaers.grp.df
      ggplot(data=df, aes(x=n, y=AgeGroup, fill=VAX_MANU)) + 
        geom_bar(stat='identity') +
        labs(title = paste0("Adverse Events (n=", sum(df$n),")"), caption="Source: VAERS", x="Instances", y="Age Group", fill='') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") +
        facet_wrap(~ SEX)
    })
  })

  output$deathsGraph <- renderPlot({
    isolate({
      df <- filter(vaers.grp.df, DIED=="Y")
      ggplot(data=df, aes(x=n, y=AgeGroup, fill=VAX_MANU)) + 
        geom_bar(stat='identity') +
        labs(title = paste0("Deaths (n=", sum(df$n),")"), caption="Source: VAERS", x="Instances", y="Age Group", fill='') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") +
        facet_wrap(~ SEX)
    })
  })
  
  output$hospGraph <- renderPlot({
    isolate({
      df <- filter(vaers.grp.df, HOSPITAL=="Y")
      ggplot(data=df, aes(x=n, y=AgeGroup, fill=VAX_MANU)) + 
        geom_bar(stat='identity') +
        labs(title = paste0("Hospitalisations (n=", sum(df$n),")"), caption="Source: VAERS", x="Instances", y="Age Group", fill='') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") +
        facet_wrap(~ SEX)
    })
  })
  
  output$lifeThreatGraph <- renderPlot({
    isolate({
      df <- filter(vaers.grp.df, L_THREAT=="Y")
      ggplot(data=df, aes(x=n, y=AgeGroup, fill=VAX_MANU)) + 
        geom_bar(stat='identity') +
        labs(title = paste0("Life Threatening (n=", sum(df$n),")"), caption="Source: VAERS", x="Instances", y="Age Group", fill='') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") +
        facet_wrap(~ SEX)
    })
  })
  
  output$emergencyGraph <- renderPlot({
    isolate({
      df <- filter(vaers.grp.df, ER_ED_VISIT=="Y")
      ggplot(data=df, aes(x=n, y=AgeGroup, fill=VAX_MANU)) + 
        geom_bar(stat='identity') +
        labs(title = paste0("Emergency (n=", sum(df$n),")"), caption="Source: VAERS", x="Instances", y="Age Group", fill='') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") +
        facet_wrap(~ SEX)
    })
  })
  
  # -------------------------
  # See: https://rstudio.github.io/DT/
  # And: https://getbootstrap.com/docs/3.4/css/#tables-example
  output$resultsTable <- DT::renderDataTable({ 
    # Select columns
    df <- rVals$db %>% select("AGE_YRS",
                              "SEX",
                              "OTHER_MEDS",
                              "HISTORY",
                              "ALLERGIES",
                              "VAX_MANU",
                              "SYMPTOMS")
    
    # Sample the data
    if(nrow(df) > 500) 
    {
     df <- df[sample(nrow(df), 500), ]
    } 
    
    # Linking symptoms to The Free Dictionary
    #df$SYMPTOMS <- paste0("<a href='",the.free.dict, df$SYMPTOMS, "' target='blank'>", df$SYMPTOMS, "</a>" )
    for(i in 1:nrow(df)) {
      syms.orig <- trimws(df[i,"SYMPTOMS"]) 
      syms.orig <- unlist(strsplit(syms.orig, "~"))
      syms.new <- ''
      for(sym in syms.orig) {
        sym <- trimws(sym)
        if(nchar(sym) > 0) {
          sym <- paste0("<a href='",the.free.dict, sym, "' target='blank'>", sym, "</a>" )
          syms.new <- paste(syms.new, sym)
        }
        df[i,"SYMPTOMS"] <- syms.new
      }
    }
    
    showNotification("All done", closeButton=FALSE, duration = 5)
    
    # Show the datatable
    # See: https://datatables.net/reference/option/
    # And: https://rstudio.github.io/DT/
    DT::datatable(df,
                  class='table compact', 
                  style="jqueryui", 
                  rownames = FALSE, 
                  escape = FALSE, 
                  width="120%",
                  options = list(ordering=F, 
                                 language = list(search = 'Filter:'),
                                 initComplete = JS(
                                     "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#428ab1', 'color': '#fff'});",
                                     "}"
                                   ),
                                 searchHighlight = TRUE
                  ),
                  colnames = c("Age" = "AGE_YRS", "Sex" = "SEX", "Medication" = "OTHER_MEDS", "History" = "HISTORY", "Allergies" = "ALLERGIES", "Vaccine" = "VAX_MANU", "Symptoms" = "SYMPTOMS"))
  })
  
  
  output$diedTable <- DT::renderDataTable({ 
    isolate({
      # Select columns
      df <- vaers.died.df %>% select("AGE_YRS",
                                "SEX",
                                "DiedDays",
                                "VAX_LOT",
                                "SYMPTOM_TEXT")
      

      # Show the datatable
      # See: https://datatables.net/reference/option/
      # And: https://rstudio.github.io/DT/
      DT::datatable(df,
                    #class='table compact', 
                    #style="jqueryui", 
                    rownames = FALSE, 
                    escape = FALSE, 
                    options = list(order = list(list(0, 'asc')),
                                   ordering=T, 
                                   language = list(search = 'Filter:'),
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     #"$(this.api().table().header()).css({'background-color': '#428ab1', 'color': '#fff'});",
                                     "}"
                                   ),
                                   searchHighlight = TRUE
                    ),
                    filter = 'top',
                    colnames = c("Age" = "AGE_YRS", "Sex" = "SEX", "Days" = "DiedDays", "Lot" = "VAX_LOT", "Comments" = "SYMPTOM_TEXT"))
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
