#   0. Preparation for the App ----

## 0.1 Load necessary packages ----

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(thematic)
library(dashboardthemes)
library(readxl)
library(proxy)
library(reshape2)
library(reactlog)
library(tm)
library(SnowballC)
library(wordcloud)

reactlog_enable()

##  0.2 Create function(s) ----


Feedback_wordcloud <- function(FB_column, custom = c("riebedebie")){
    
    text <- FB_column
    docs <- Corpus(VectorSource(text))
    docs <- docs %>%
        tm_map(removeNumbers) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("dutch"))
    docs <- tm_map(docs, stemDocument, language = "dutch")
    docs <- tm_map(docs, removeWords, custom)
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    
    set.seed(1234) # for reproducibility 
    wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
              max.words=200, random.order=FALSE, rot.per=0.35,           
              colors=brewer.pal(8, "Dark2"))
}

#   1.  User Interface definition ----

ui <- dashboardPage( 
    
    dashboardHeader(title = "Comproved Feedback Analyzer"),
    
    ##  1.1 Sidebar ----
    
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = "Info",
                tabName = "Info",
                icon = icon("info")),
            menuItem(
                text = "Upload Files",
                tabName = "Upload",
                icon = icon("upload")),
            menuItem(
                "General stats",
                tabName = "general_stats_tab",
                icon = icon("dashboard")
            ),
            menuItem(
                "Explore assessors",
                tabName = "explore_assessors_tab",
                icon = icon("portrait")),
            menuItem(
                "Download summary",
                tabName = "create_csv_tab",
                icon = icon("download"))
        )
    ),
    
    ##  1.2 Body ----
    
    dashboardBody(
        ### changing theme
        shinyDashboardThemes(
            theme = "poor_mans_flatly"
        ),
        
        ###  1.2.1 Info tab ----
        
        tabItems(
            tabItem(tabName = "Info",
                    
                    fluidRow(
                        box(
                            h3("Info"),
                            p("This App is developed to explore Comproved feedback fields."),
                            br(),
                            p("When using this App some guidelines have to be taken into account: "),
                            tags$ul(
                                tags$li("To start, consider the number of ", strong("criteria in your assessment"), 
                                        " as you really need to put that number to the correct number in the tab", tags$i("Upload Files")
                                ),
                                tags$li("You need to input that number in the App before uploading a file, otherwise it will throw you with a number of errors!"
                                ),
                                tags$li("Then, upload the correct files in the", tags$i("Upload Files"), 
                                        " tab. If no compatible files are uploaded you will get a number of errors."
                                )
                            )
                            ,
                            width = 12
                        ),
                    ),
                    fluidRow(
                        box(title = "Background",
                            p("In Comproved assessors are sometimes asked to give feedback on both products assessed after they have decided on which of both is best. 
                              This results in a number of text fields that are sometimes hard to analyse. In this App some functions are provided so that whoever 
                              runs a CJ can get a faster idea of the feedback given and the effort put in the feedback by the assessors. Also it is possible to 
                              analyse the feedback process and content of a single assessor."),
                            br(),
                            p("If you are not really informed on Comparative Judgement and ", 
                              strong("Comproved"), " more information can be found on the ", 
                              tags$a(href = "https://comproved.com/", "Comproved Website"), "."),
                            p("Or you can get into contact with Comproved using the email ", 
                              tags$a(href = "mailto:info@comproved.com", "info@comproved.com", target = "_blank"),
                              "."),
                            status = "info",
                            solidHeader = TRUE,
                            width = 12)
                    )
            ),
            
            ###  1.2.2 Upload tab ----
            
            tabItem(tabName = "Upload",
                    
                    fluidRow(
                        box(
                            h3("File upload"),
                            p("Here you find two upload boxes to upload the necessary files."),
                            br(),
                            p("You should follow the following steps in the correct order:"),
                            tags$ol(
                                tags$li("First put the number of criteria on the correct number concerning your App"),
                                tags$li("Then upload the necessary files"),
                                tags$li("If you want to restart, click the Refresh button in you browser and start at the first step again")
                            ),
                            width = 12
                        )
                    ), 
                    fluidRow(
                        box(title = "STEP 1: ",
                            numericInput("ncriteria", "Number of criteria in the assessment", value = 3, min = 1, step = 0.5),
                            solidHeader = TRUE,
                            status = "danger",
                            width = 4
                        ),
                        box(title = "STEP 2: Feedback File",
                            p("A first file is the file that contains all the feedback items.", strong("Typically this file ends with ", em("...-feedback.xlsx"))),
                            fileInput("FB_File", accept = c(".xls", ".xlsx"), label = "Feedback file upload:"),
                            status = "warning",
                            solidHeader = TRUE,
                            width = 4
                        ), 
                        box(title = "STEP 3: Process File",
                            p("A second file is the file that contains all the data on the comparisons.", strong("Typically this file ends with ", em("...-comparisons.xlsx"))),
                            fileInput("Proces_File", accept = c(".xls", ".xlsx"), label = "Proces file upload:"),
                            status = "success",
                            solidHeader = TRUE,
                            width = 4
                        )
                    )
            ),
            
            ###  1.2.3 General stats tab ----
            
            tabItem(tabName = "general_stats_tab",
                    fluidRow(
                        infoBoxOutput("n_assessors_box", width = 4),
                        infoBoxOutput("median_FB_time_box", width = 4),
                        infoBoxOutput("mean_n_char_box", width = 4)
                    ),
                    fluidRow(
                        box(
                            title = "Number of characters in FB fields by criterium",
                            p("Here you find a graph that summarises how big the difference is in number of characters per FB field. 
                          It takes the data of all FB fields together."),
                            plotOutput("Hist_mean_n_char"),
                            tableOutput("summary_n_char_criterium"),
                            width = 12,
                            collapsible = T
                        ),
                        box(selectizeInput("Filter_words1", 
                                           label = "If you want to remove words from the wordcloud, select them in the field below:", 
                                           multiple = T, 
                                           choices = NULL),
                            plotOutput("FB_Wordcloud_overall"),
                            title = "Most frequent words in FB Fields",
                            width = 12,
                            collapsible = T
                        ),
                        box(
                            title = "Rank of assessors by number of characters used",
                            p("This graph is interactive. You can identify assessors by moving your cursor over the dots in the graph."),
                            plotlyOutput("RankAssessorsPlot"),
                            collapsible = T,
                            width = 12
                        )
                    )
            ),
            
            ###  1.2.4 Explore assessors tab ----
            
            tabItem(tabName = "explore_assessors_tab",
                    fluidRow(
                        box(selectInput(
                            inputId = "Id_assessor_i",
                            label = "Choose the assessor",
                            choices = NULL
                        ),
                        
                        p("Select an assessor to update the information on the different fields and graphs in the tabs on this screen."),
                        br(),
                        p("The similarity scores need to be interpreted as follows: values close to 1 indicate strong overlap between FB fields and scores close to 0 indicate lack of overlap between FB fields."),
                        width = 12
                        )
                    ),
                    fluidRow(
                        tabBox(id = "Assessor_tabs",
                               tabPanel("Summary for Assessor", 
                                        infoBoxOutput("total_FB_time_box_a", width = 6),
                                        infoBoxOutput("total_n_char_box_a", width = 6),
                                        infoBoxOutput("Similarity_mean_Infobox_a", width = 6),
                                        infoBoxOutput("Similarity_perc_Infobox_a", width = 6)),
                               tabPanel("Length of feedback", plotOutput("n_char_plot")),
                               tabPanel("Wordcloud", 
                                        selectInput("Filter_words2", 
                                                    label = "If you want to remove words from the wordcloud, select them in the field below:", 
                                                    multiple = T, 
                                                    choices = NULL),
                                        plotOutput("FB_Wordcloud_a")),
                               tabPanel("Similarity plot", plotOutput("FB_Similarity_plot")),
                               
                               width = 12
                        )
                    ),
                    fluidRow(column(12,
                                    tableOutput("Feedback_given_Assessor")) 
                    )
            ),
            
            ###  1.2.5 Create csv tab ----
            
            tabItem(tabName = "create_csv_tab",
                    fluidRow(
                        box(p("In this table you can find some summary statistics for the assessors. If you want to download this dataset don't forget to first click on Show All."), 
                            br(),
                            (p("To view some more columns you can scroll to the right.")),
                            width = 12
                        )
                    ),
                    fluidRow(column(width = 12,
                                    div(style = 'width:1000px;margin:auto', DTOutput("summary_table_assessors"))
                    )
                    )                
            )
        )
    )
)


#   2. Define server logic ----

server <- function(input, output, session) {
    
    ## 2.1  Create files at server side ----
    
    ### 2.1.2   FB_File() ----
    
    FB_File <- reactive({
        
        req(input$FB_File)
        
        FB_File <- read_excel(input$FB_File$datapath)
        colnames(FB_File) <- c("Assessor", 
                               "Assessor_mail", 
                               "FB_date", 
                               "Product", 
                               "Participant", 
                               criteria())
        FB_File
    })
    
    ### 2.1.2   Proces_File() ----
    
    Proces_File <- reactive({
        req(input$Proces_File)
        
        Proces_File <- read_excel(input$Proces_File$datapath)
        colnames(Proces_File) <- c("Assessor", 
                                   "Assesor_email", 
                                   "Winner", 
                                   "Participant_win", 
                                   "Loser", 
                                   "Participant_loose", 
                                   "Compare_time", 
                                   "FB_time", 
                                   "Total_time", 
                                   "Submitted") 
        Proces_File
    })
    
    ### 2.1.3   Based on ncriteria input ----
    
    criteria <- reactive({
        
        req(input$ncriteria)
        
        C <- NULL
        
        for(i in 1:input$ncriteria){
            Cr <- paste0("Crit",i)
            C <- c(C, Cr)
        }
        
        C
        
    }) 
    
    
    output$criteria <- renderTable(
        criteria()
    )
    
    n_char_criteria <- reactive({
        
        req(input$ncriteria)
        
        C2 <- NULL
        for(i in 1:input$ncriteria){
            Cr2 <- paste0("n_char_","Crit",i)
            C2 <- c(C2, Cr2)
        }        
        
        C2
    })
    
    output$n_char_criteria <- renderTable(
        n_char_criteria()
    )
    
    not_empty_criteria <- reactive({
        
        req(input$ncriteria)
        
        C3<- NULL
        for(i in 1:input$ncriteria){
            Cr3 <- paste0("not_empty_","Crit",i)
            C3 <- c(C3, Cr3)
        }
        
        C3
        
    })
    
    output$not_empty_criteria <- renderTable(
        not_empty_criteria()
    )
    
    median_n_char_criteria <- reactive({
        
        req(input$ncriteria)
        
        C4 <- NULL
        for(i in 1:input$ncriteria){
            Cr4 <- paste0("median_n_char_","Crit",i)
            C4 <- c(C4, Cr4)
        }
        
        C4
        
    })
    
    output$median_n_char_criteria<- renderTable(
        median_n_char_criteria()
    )
    
    mean_n_char_criteria <- reactive({
        
        req(input$ncriteria)
        
        C5 <- NULL
        for(i in 1:input$ncriteria){
            Cr5 <- paste0("mean_n_char_","Crit",i)
            C5 <- c(C5, Cr5)
        }
        
        C5
        
    })
    
    output$mean_n_char_criteria<- renderTable(
        mean_n_char_criteria()
    )
    
    ### 2.1.3   List of assessors for Input ----
    
    Assessor.IDs <- reactive(
        FB_File() %>%
            select(
                Assessor
            )
    )
    
    n_assessors <- reactive(
        nrow(dat_general_merged())
    )
    
    observe({
        req(input$FB_File)
        
        unique_assessors <- FB_File() %>%
            group_by(Assessor) %>%
            select(Assessor)
        
        
        updateSelectInput(session = session, "Id_assessor_i", choices = unique_assessors$Assessor)
    })
    
    ## 2.3  General Stats dataframe ----
    
    dat_general1 <- reactive({
        
        req(input$FB_File)
        
        FB_File() %>%    
            mutate(
                across(.cols = criteria(), .fns = ~nzchar(.x)*1, .names = "not_empty_{.col}")
            ) %>%
            pivot_longer(
                not_empty_criteria()
            )  %>%
            group_by(name, Assessor) %>%
            summarise(
                n_FB_fields = n(),
                n_not_empty = sum(value)
            ) %>%
            mutate(
                perc_FBGiven = round((n_not_empty/n_FB_fields)*100, 2)
            ) %>%
            ungroup() %>%
            select(
                Assessor,
                name,
                n_not_empty,
                n_FB_fields,
                perc_FBGiven
            ) %>%
            mutate(
                # drop not_empty_ from the name fields
                Criterium = str_sub(name, start = 11, end = 15)
            ) %>%
            pivot_wider(
                id_cols = Assessor,
                names_from = Criterium,
                values_from = c(n_FB_fields, n_not_empty, perc_FBGiven)
            )
        
    })
    
    dat_general2 <- reactive({
        req(input$FB_File)
        
        FB_File() %>%    
            mutate(
                across(.cols = criteria(), .fns = ~nchar(.x)*1, .names = "n_char_{.col}")
            ) %>%
            pivot_longer(
                n_char_criteria()
            )  %>%
            group_by(name, Assessor) %>%
            summarise(
                mean_n_char = mean(value, na.rm = T),
                median_n_char = median(value, na.rm = T),
                min_n_char = min(value, na.rm = T),
                max_n_char = max(value, na.rm = T)
            ) %>%
            ungroup(
            ) %>%
            select(
                Assessor,
                name,
                mean_n_char,
                median_n_char,
                min_n_char,
                max_n_char
            ) %>%
            mutate(
                Criterium = str_sub(name, start = 8, end = 12)
            ) %>%
            pivot_wider(
                id_cols = Assessor,
                names_from = Criterium,
                values_from = c(mean_n_char, median_n_char, min_n_char, max_n_char)
            )
    })    
    
    dat_general3 <- reactive({
        req(input$Proces_File)
        
        Proces_File() %>%
            group_by(Assessor) %>%
            summarize(
                total_FB_time = sum(FB_time),
                median_FB_time = median(FB_time, na.rm = T),
                median_total_time = median(Total_time, na.rm = T),
                median_compare_time = median(Compare_time, na.rm = T)
            ) %>%
            ungroup() %>%
            select(
                Assessor,
                total_FB_time,
                median_FB_time,
                median_total_time,
                median_compare_time
            )
    })
    
    dat_general_merged <- reactive({
        
        req(input$FB_File)
        
        dat_general1() %>%
            left_join(dat_general2(), by = "Assessor") %>%
            left_join(dat_general3(), by = "Assessor")
    })
    
    ## 2.4  General stats tab ----
    
    ### 2.4.1   infoBoxes ----
    
    output$n_assessors_box <- renderInfoBox({
        infoBox(
            "Number of assessors", 
            value = n_assessors(), 
            icon = shiny::icon("people-arrows"), 
            fill = TRUE
        )}
    )
    
    median_FB_time <- reactive(
        dat_general_merged() %>%
            summarize(
                median_FB_time_all = mean(median_FB_time)
            )
    )
    
    mean_FB_time <- reactive(
        dat_general_merged() %>%
            summarize(
                mean_FB_time_all = mean(mean_FB_time, na.rm = T)
            )
    )   
    
    output$median_FB_time_box <- renderInfoBox({
        infoBox(
            "Median FB time of assessors", 
            value= paste0(round(median_FB_time(), 0)," " ,"secs"), 
            icon = shiny::icon("user-clock"), fill = TRUE
        )}
    )
    
    mean_n_char <- reactive({
        VAL <- dat_general_merged() %>%
            select(Assessor, mean_n_char_criteria()) %>%
            pivot_longer(
                mean_n_char_criteria()
            ) %>%
            group_by(Assessor) %>%
            summarize(
                mean_n_char_ass = mean(value, na.rm = T)
            ) %>%
            ungroup() %>%
            summarize(
                mean_n_char_all = mean(mean_n_char_ass, na.rm = T)
            )
        
        VAL$mean_n_char_all
        
    }
    )
    
    output$mean_n_char_box <- renderInfoBox({
        infoBox(
            "Mean number of characters per criterium", 
            value = round(mean_n_char(),0), 
            icon = shiny::icon("user-edit"), 
            fill = TRUE 
        )}
    )
    
    ### 2.4.2   Hist_mean_n_char ----
    
    dat_P2 <- reactive(
        FB_File() %>%
            mutate(across(.cols = criteria(), .fns = ~nchar(.x)*1, .names = "n_char_{.col}")
            ) %>%
            select(Assessor, n_char_criteria()) %>%
            pivot_longer(
                n_char_criteria()
            )
    )
    
    output$Hist_mean_n_char <- renderPlot({
        
        # First recode labels of n_char_criteria to criteria for the facet labels
        
        Key <- setNames(as.vector(criteria()),as.vector(n_char_criteria()))
        
        Hist <- dat_P2() %>%
            mutate(
                Criteria = recode(name, !!!Key)
            ) %>%
            ggplot(aes(value)) +
            geom_histogram() +
            facet_wrap(~Criteria) +
            labs(x = "number of characters in FB field", y = "number of FB fields")
        
        Hist
    })
    
    ### 2.4.3   Table summary ----
    
    output$summary_n_char_criterium <- renderTable({
        
        Key <- setNames(as.vector(criteria()),as.vector(n_char_criteria()))
        
        Means <- dat_P2() %>%
            mutate(
                Criterium = recode(name, !!!Key)
            ) %>%
            group_by(Criterium) %>%
            summarize(
                mean_number_characters = mean(value, na.rm = T)
            )
        
        Means    
        
    })
    
    ### 2.4.4   Wordcloud ----
    
    Feedback_text_overall <- reactive(
        FB_File() %>%
            select(
                criteria()
            ) %>%
            pivot_longer(
                criteria()
            ) %>%
            mutate(
                Feedback = value
            ) %>%
            select(
                Feedback
            )
    )
    
    observe({
        req(input$FB_File)
        
        docs1 <- Corpus(VectorSource(Feedback_text_overall()))
        docs1 <- docs1 %>%
            tm_map(removeNumbers) %>%
            tm_map(removePunctuation) %>%
            tm_map(stripWhitespace)
        docs1 <- tm_map(docs1, content_transformer(tolower))
        docs1 <- tm_map(docs1, removeWords, stopwords("dutch"))
        docs1 <- tm_map(docs1, stemDocument, language = "dutch")
        dtm1 <- DocumentTermMatrix(docs1)
        words1 <- findFreqTerms(dtm1, 5)
        
        updateSelectizeInput(session = session, "Filter_words1", choices = words1)
    })
    
    
    output$FB_Wordcloud_overall <- renderPlot(
        Feedback_wordcloud(Feedback_text_overall(), input$Filter_words1)
    )
    
    ### 2.4.5   RankAssessorsPlot ----
    
    dat_P3 <- reactive(
        FB_File() %>%
            mutate(across(.cols = criteria(), .fns = ~nchar(.x)*1, .names = "n_char_{.col}")
            ) %>%
            select(Assessor, n_char_criteria()) %>%
            pivot_longer(
                n_char_criteria()
            ) %>%
            group_by(Assessor) %>%
            summarize(
                Mean = mean(value, na.rm =T),
                Min = min(value, na.rm = T),
                Max = max(value, na.rm = T)
            ) %>%
            mutate(
                Rank = dense_rank(Mean)
            ) %>%
            ungroup() 
    )   
    
    output$RankAssessorsPlot <- renderPlotly(
        ggplotly(
            ggplot(
                data = dat_P3(),
                aes(
                    x = Rank,
                    y = Mean,
                    ymin = Min,
                    ymax = Max,
                    name = Assessor)) +
                geom_pointrange() +
                labs(
                    subtitle = "Every line is an assessor",
                    y = "number of characters",
                    x = "rank of assessor"
                )
        )
    )
    
    ## 2.5  Selected assessor output ----
    
    ### 2.5.1   Infoboxes ----
    
    Tot_FB_time <- reactive(
        dat_general_merged() %>%
            filter(Assessor == input$Id_assessor_i) %>%
            select(
                total_FB_time
            )
    )
    
    output$total_FB_time_box_a <- renderInfoBox({
        infoBox(
            "Total FB time",
            value = paste0(round(Tot_FB_time(),0), " ", "secs"),
            icon = shiny::icon("user-clock"),
            fill = TRUE
        )
    })
    
    Mean_n_char_As <- reactive(
        dat_general_merged() %>%
            filter(Assessor == input$Id_assessor_i) %>%
            select(
                Assessor, median_n_char_criteria()
            ) %>%
            pivot_longer(
                median_n_char_criteria()
            ) %>%
            group_by(Assessor) %>%
            summarise(
                Mean_n_char = median(value, na.rm = T)
            ) %>%
            select(Mean_n_char)
    )
    
    output$total_n_char_box_a <- renderInfoBox({
        infoBox(
            "Median number characters / criterium",
            value = round(Mean_n_char_As(),0),
            icon = shiny::icon("comments"),
            fill = TRUE
        )
    })
    
    
    ### 2.5.2   Wordcloud ----
    
    Feedback_text <- reactive(
        Feedback_Assessor_table() %>%
            select(Feedback)
    )
    
    observe({
        req(input$FB_File)
        
        docs2 <- Corpus(VectorSource(Feedback_text()))
        docs2 <- docs2 %>%
            tm_map(removeNumbers) %>%
            tm_map(removePunctuation) %>%
            tm_map(stripWhitespace)
        docs2 <- tm_map(docs2, content_transformer(tolower))
        docs2 <- tm_map(docs2, removeWords, stopwords("dutch"))
        docs2 <- tm_map(docs2, stemDocument, language = "dutch")
        dtm <- DocumentTermMatrix(docs2)
        words <- findFreqTerms(dtm, 2)
        
        updateSelectInput(session = session, "Filter_words2", choices = words)
    })
    
    
    output$FB_Wordcloud_a <- renderPlot(
        Feedback_wordcloud(Feedback_text(), input$Filter_words2)
    )
    
    ### 2.5.3  Similarity analysis ----
    
    Text_similarity <- reactive({
        
        req(input$ncriteria)
        
        text2 <- FB_File() %>%
            filter(Assessor == input$Id_assessor_i) %>%
            select(
                Assessor,
                criteria()) %>%
            pivot_longer(
                criteria()
            ) %>%
            mutate(
                Criterium = name,
                Feedback = value
            ) %>%
            select(
                Feedback
            )
        
        docs <- tm::Corpus(tm::VectorSource(text2$Feedback))
        docs <- docs %>%
            tm_map(removeNumbers) %>%
            tm_map(removePunctuation) %>%
            tm_map(stripWhitespace)
        docs <- tm_map(docs, content_transformer(tolower))
        docs <- tm_map(docs, removeWords, stopwords("dutch"))
        
        dtm <- tm::DocumentTermMatrix(docs) 
        matrix_dtm <- as.matrix(dtm) 
        
        dtm.tfidf <- tm::weightTfIdf(dtm)
        tfidf.matrix <- as.matrix(dtm.tfidf)
        dist.matrix <- proxy::simil(tfidf.matrix, method = "cosine", upper = F)
        
        dist.matrix_melted <- melt(as.matrix(dist.matrix))
        
        dist.matrix_melted
        dist.matrix_melted %>%
            select(Var1, Var2, value) %>%
            dplyr::rename(
                Criterium_A_ID = Var1,
                Criterium_B_ID = Var2,
                Similarity = value
            )
    })
    
    output$FB_Similarity_plot <- renderPlot({
        Text_similarity() %>%
            ggplot(aes(x = Criterium_A_ID, y = Criterium_B_ID, fill = Similarity)) +
            geom_tile() + scale_fill_gradient(low= "green", high = "red")
    })
    
    ## Create Infobox on similarity statistics
    
    Mean_Similarity_As <- reactive(
        Text_similarity() %>%
            summarize(
                Mean_Similarity = mean(Similarity, na.rm = TRUE)
            ) %>%
            select(
                Mean_Similarity
            )
    )
    
    output$Similarity_mean_Infobox_a <- renderInfoBox(
        infoBox(
            "Mean similarity",
            value = round(Mean_Similarity_As(),3),
            icon = shiny::icon("copy"),
            fill = TRUE
        )
    )
    
    Perc_Similar_As <- reactive({
        
        n_Sim <- Text_similarity() %>%
            filter(
                Similarity > 0.75
            ) %>%
            summarize(
                n_Sim = n()
            )
        
        n_FB <- Text_similarity() %>%
            summarize(
                n_FB = n()
            ) 
        round((n_Sim$n_Sim/n_FB$n_FB)*100, 1)
        
    })
    
    output$Similarity_perc_Infobox_a <- renderInfoBox(
        infoBox(
            "Percentage similar fields",
            value = paste0(Perc_Similar_As(), " ", "%"),
            icon = shiny::icon("paste"),
            fill = TRUE
        )
    )
    
    ### 2.5.4  Feedback_Assessor_table ----
    
    Feedback_Assessor_table <- reactive(
        FB_File() %>%
            filter(Assessor == input$Id_assessor_i) %>%
            select(
                criteria()
            ) %>%
            pivot_longer(
                criteria()
            ) %>%
            mutate(
                Criterium = name,
                Feedback = value
            ) %>%
            select(
                Criterium,
                Feedback
            )
    )
    
    output$Feedback_given_Assessor <- renderTable(
        FB_File() %>%
            filter(
                Assessor == input$Id_assessor_i
            ) %>%
            select(
                criteria()
            )
    )
    
    
    ### 2.5.5   Plot on n_char ----
    
    dat_P1 <- reactive(
        FB_File() %>%
            mutate(across(.cols = criteria(), .fns = ~nchar(.x)*1, .names = "n_char_{.col}")
            )
    )
    
    selected_assessor_dat_P1 <- reactive(
        dat_P1() %>%
            filter(
                Assessor == input$Id_assessor_i
            )
    )
    
    # Create output elements   
    
    output$n_char_plot  <- renderPlot({
        
        Key <- setNames(as.vector(criteria()),as.vector(n_char_criteria()))
        
        Selected_Assessor <-
            selected_assessor_dat_P1() %>%
            select(
                Assessor,
                n_char_criteria()
            ) %>%
            pivot_longer(
                n_char_criteria()
            ) %>%
            mutate(
                Criteria = recode(name, !!!Key)
            ) 
        
        P1 <- 
            dat_P1() %>% 
            select(
                Assessor,
                n_char_criteria()
            ) %>%
            pivot_longer(
                n_char_criteria()
            ) %>%
            mutate(
                Criteria = recode(name, !!!Key)
            ) %>%
            ggplot(
                aes(x = Criteria, 
                    y = value)
            ) +
            geom_jitter(alpha=.3, color = "grey") +
            geom_jitter(
                data = Selected_Assessor,
                aes(x = Criteria, y = value), color = "red", size = 2
            ) +
            scale_y_continuous(name = "number of characters in the feedback") +
            scale_x_discrete(name = "") +
            ggtitle("Number of characters in FB Fields")
        
        P1
    })
    
    ##   2.6 summary_table_assessors ----
    
    output$summary_table_assessors <- renderDataTable(
        DT::datatable({
            dat_general_merged()
        },
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
            autowidth = TRUE,
            dom = 'Bfrtip',
            scrollY=TRUE,
            scrollX= TRUE,
            lengthMenu = list(c(20,100,-1),c("20","100", "All")),
            pageLength = 20,
            buttons = list(c('copy', 'csv', 'excel'),
                           list(
                               extend = "collection",
                               text = "Show All",
                               action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
                           )
            )
        )
        )
    )
    
}

thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
