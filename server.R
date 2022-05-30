#
# Shiny Application for students early dropout prediction (server)
# Author: Ksenia Koroljova
#



# Used libraries  --------------------------------------------------------------

library(shiny)
library(caret)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(tidyverse)
library(lubridate)
library(fastAdaboost)
library(stringr)
library(quanteda)
library(quanteda.textstats)
library(googleLanguageR)


# Loaded files  ----------------------------------------------------------------

gl_auth("")

questions_model <- readRDS("R/questionnaire_model.rds")
essay_model <- readRDS("R/essay_conversation_model.rds")
conversation_model <- readRDS("R/conversation_model.rds")

loaded_models <- list()
loaded_models[[1]] <- questions_model
loaded_models[[2]] <- conversation_model
loaded_models[[3]] <- essay_model

questionnaire <- read_excel("R/kys_clean_62obj.xlsx")
essay_conversations <- read_excel("R/B_C.xlsx")
conversations <- read_excel("R/vest_19_21_p.xlsx")
town <- read_excel("R/linnad.xlsx")

variables<- read_excel("R/variables.xlsx")
variables$all <- variables

# Server realization  ----------------------------------------------------------

server <- function(input, output, session) {

  # Models page
  
  # 1. Questionnaire model  ----------------------------------------------------

  # Histogram
  output$QhistogramPlot <- renderPlot({
    
    # Column name variable
    numSelected <- ifelse(input$QhistogramVars == "Eesti keel", "K5_est",
                    ifelse(input$QhistogramVars == "Inglise keel", "K16_eng",
                    ifelse(input$QhistogramVars == "Eelteadmised matemaatikas", "K16_mat",
                    ifelse(input$QhistogramVars == "Informeeritus eriala kohta", "K6",
                    ifelse(input$QhistogramVars == "Meeskonnatöö", "K23_mskd",
                    ifelse(input$QhistogramVars == "Kallakus reaalteaduste poole", "K22_Reaal")
                  )))))
    
    ggplot(data = questionnaire,
           aes(x = .data[[numSelected]])) +
      geom_histogram(stat = "bin", color="#FFFFFF", fill="#342b60") +
      theme(panel.background = element_rect(fill = '#FFFFFF',
                                            colour = "#EBEBEB",size = 1, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F2F2F2"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "#F2F2F2")
      )+
      labs(title = sprintf("Graafik %s jaoks", numSelected),
           x = "Hinne",
           y = "Sagedus") +
      stat_bin(geom = "text", binwidth = 1, 
               aes(label = ifelse(..count.. != 0, as.character(..count..), '')),
               vjust = -0.6)
    }
    
    )# end histogram plot

  
  # Variable importance plot
  QvariableImportance <- varImp(questions_model)
  
  output$questFactorsImportancePlot <- renderPlot({
    ggplot(QvariableImportance) +
      ylab("Tunnuse tähtsus")+
      xlab("Tunnus")+
     geom_col(color="#FFFFFF", width = 0.85 , fill="#342b60", size=2) +
     theme(panel.background = element_rect(fill = "#FFFFFF",colour = "#EBEBEB", size = 1, linetype = "solid"),
       panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "#FFFFFF"),
       panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#FFFFFF")
     )
    })# end render plot
  
  output$questVariablesDT <- DT::renderDataTable({
    data <- filter(variables$all, Mudel == "küsimustiku")
    data <- data %>% select(-starts_with("Mudel"))
    DT::datatable(data, options = list(pageLength = 10))
  }) 

  # 2. Essay model  ------------------------------------------------------------

  # Essay histogram
  output$essayHistogramPlot <- renderPlot({
    
    # Column name variable
    numSelectedEssay <- ifelse(input$essayHistogramVars == "Õppevorm", "Vorm",
                        ifelse(input$essayHistogramVars == "Sugu", "Sugu",
                        ifelse(input$essayHistogramVars == "Kodakondsus", "Kodakondsus",
                        ifelse(input$essayHistogramVars == "E-posti host", "emailHost",
                        ifelse(input$essayHistogramVars == "Ametlik e-post", "ametlikEmail",
                        ifelse(input$essayHistogramVars == "Koolikeel", "Koolikeel",
                        ifelse(input$essayHistogramVars == "Vestluse aeg", "V_aeg")
                         ))))))
    
    ggplot(data = essay_conversations,
           aes(x = .data[[numSelectedEssay]])) +
      geom_histogram(stat = "count", color="#FFFFFF", fill="#342b60") +
      #geom_bar(stat = "count") +
      theme(panel.background = element_rect(fill = '#FFFFFF', colour = "#EBEBEB", size = 1, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F2F2F2"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "#F2F2F2"))+
      labs(title = sprintf("Graafik %s jaoks", numSelectedEssay),
           x = "Vastus",
           y = "Sagedus") +
      stat_count(geom = "text", aes(
        label = ifelse(..count.. != 0, as.character(..count..), '')),
        vjust = -0.6)
  })# end histogram plot
  
  
  
  # Variable importance plot
  essayVariableImportance <- varImp(essay_model)
  
  output$essayVariableImportancePlot <- renderPlot({
    ggplot(essayVariableImportance) +
      ylab("Tunnuse tähtsus")+
      xlab("Tunnus")+
      geom_col(color="#FFFFFF", width = 0.85 , fill="#342b60", size=2) +
      theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#EBEBEB", size = 1, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "#FFFFFF"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#FFFFFF")
      )
  })# end render plot
  
  output$essayVariablesDT <- DT::renderDataTable({
    data <- filter(variables$all, Mudel == "essee")
    data <- data %>% select(-starts_with("Mudel"))
    DT::datatable(data, options = list(pageLength = 10))
  }) 
  
  # 3. Conversation model  -----------------------------------------------------

  # Conversation histogram
  output$convHistogramPlot <- renderPlot({
    
    # Column name variable
    numSelectedConv <- ifelse(input$convHistogramVars == "Kutsekool", "Kutsekool",
                       ifelse(input$convHistogramVars == "Linna suurus", "LS_",
                       ifelse(input$convHistogramVars == "Ida-Virumaa", "Ida.Viru",
                       ifelse(input$convHistogramVars == "Kodakondsus", "Kodakondsus",
                       ifelse(input$convHistogramVars == "Vestluse aeg", "V_aeg",
                       ifelse(input$convHistogramVars == "Koolikeel", "Koolikeel")
                       )))))
    
    ggplot(data = conversations,
           aes(x = .data[[numSelectedConv]])) +
      geom_histogram(stat = "count", color="#FFFFFF", fill="#342b60") +
      #geom_bar(stat = "count") +
      theme(panel.background = element_rect(fill = '#FFFFFF', colour = "#EBEBEB", size = 1, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F2F2F2"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "#F2F2F2")
      )+
      labs(title = sprintf("Graafik %s jaoks", numSelectedConv),
           x = "Vastus",
           y = "Sagedus") +
      stat_count(geom = "text", aes(
        label = ifelse(..count.. != 0, as.character(..count..), '')),
                 vjust = -0.6)
  })# end histogram plot
  
  
  
  # Variable importance plot
  convVariableImportance <- varImp(conversation_model)
  
  output$convVariableImportancePlot <- renderPlot({
    ggplot(convVariableImportance) +
      ylab("Tunnuse tähtsus")+
      xlab("Tunnus")+
      geom_col(color="#FFFFFF", width = 0.85 , fill="#342b60", size=2) +
      theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#EBEBEB", size = 1, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "#FFFFFF"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#FFFFFF")
      )
  })# end render plot
  
  output$convVariablesDT <- DT::renderDataTable({
    data <- filter(variables$all, Mudel == "vestluse")
    data <- data %>% select(-starts_with("Mudel"))
    DT::datatable(data, options = list(pageLength = 10))
  }) 

  
  
  # Prediction page ---------
  
   # Questionnaire  -----------------------------------------------------------
  
  # React value when using the action button
  questionPrediction <- reactiveValues(result = NULL)
  questionPercentages <- reactiveValues(no = 0, yes = 0)
  
  observeEvent(input$buttonQuestionPredict, {
    answers <-  data.frame(
        K13_vNr=input$inputSpecialityChoice,
        K5_est=input$inputEstonian,
        K6=input$inputSpecialityInfo,
        K7_Nuti=input$inputSmarthome,
        K16_mat=input$inputMath,
        K16_eng=input$inputEnglish,
        K22_Reaal=input$inputReal,
        K23_mskd=input$inputTeamwork
      )#answers end
    answersConverted <- type.convert(answers, as.is = FALSE)
    tryCatch({
      questionPrediction$result <- predict(questions_model, answersConverted, type="prob")
      questionPercentages$no <- questionPrediction$result %>% transmute(round(no*100, 2))
      questionPercentages$yes <- questionPrediction$result %>% transmute(round(yes*100, 2))
    }, error = function(e) {
      showNotification("Ennustamine ebaõnnestus. Kontrollige, kas kõik väljad on õigesti täidetud.",
                       "",type = "error")
      return()
    }, silent=TRUE) 
  })
 
   # Questionnaire prediction results div
   output$questionsPredictionResult <- renderUI({
     if(is.null(questionPrediction$result))
       div(class = "models-info",
           p("Valige vastuseid ja vajutage nuppu Ennusta!")
       )#div
     else
       div(class = "models-info",
           p("Saadud tulemused:")
       )#div
   })
 
   # Info boxes result
   output$qPredictResult1 <- renderValueBox({
     valueBox(
       paste0(questionPercentages$no, "%"),
       "Jääb kolledžisse",
       icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green"
     )})
   output$qPredictResult2 <- renderValueBox({
     valueBox(
       paste0(questionPercentages$yes, "%"),
       "Langeb välja",
       icon = icon("thumbs-down", lib = "glyphicon"),
       color = "red"
     )
   })

   # Reset button
   observeEvent(input$buttonQuestionReset, {
     questionPrediction$result <- NULL
     questionPercentages$yes <- 0
     questionPercentages$no <- 0
     updateRadioButtons(inputId = "inputSpecialityChoice", selected = "jah")
     updateRadioButtons(inputId = "inputEstonian", selected = "1")
     updateRadioButtons(inputId = "inputSpecialityInfo", selected = "1")
     updateRadioButtons(inputId = "inputSmarthome", selected = "1")
     updateRadioButtons(inputId = "inputMath", selected = "1")
     updateRadioButtons(inputId = "inputEnglish", selected = "1")
     updateRadioButtons(inputId = "inputReal", selected = "1")
     updateRadioButtons(inputId = "inputTeamwork", selected = "1")
   })  
   
   # Essay  --------------------------------------------------------------------

   essayPrediction <- reactiveValues(result = NULL)
   essayPercentages <- reactiveValues(no = 0, yes = 0)
   essay <- reactiveValues()
   essaySummary <- reactiveValues(tokens = 0, types = 0, spam = 0, lexDiversity = 0)
   
   keyWords <- c("it","telemaatika","tulevik","tuleviku","tulevikus","aruka","iot","eriala", "süsteemide","töö","kindlasti", "arukad","arukat","süsteemi","süsteem","süsteemid","ит", "телематика","будущий","будущее","будущие","умные", "умный","умное","умная","системы","система","иот","работа","работу","работы","будущем","очень")
   
   observeEvent(input$buttonEssayPredict, {
     
     essay <- input$inputEssay
     essayOriginal <- input$inputEssay
     
     # Remove all special characters
     essay <- gsub("[[:punct:]]", " ", essay)
     
     # Remove numbers
     essay <- gsub("[0-9]+", "", essay)
     
     # Remove regular expressions
     essay <- str_replace_all(essay, "[^[:alnum:]]", " ")
     
     # Remove long spaces
     essay <- gsub("\\s+"," ",str_trim(essay))
     
     corpusEssay<- corpus(tolower(essay))
     
     # Tokens
     tokensEssayRem<- quanteda::tokens(corpusEssay, remove_punct = TRUE,remove_numbers = TRUE, remove_symbols = TRUE)
     
     tokensRem <- summary(tokensEssayRem)
     tokensRem <- as.data.frame(tokensRem)
     
     # Stopwords 
     stopwords <- read.csv("R/stopwords.csv",encoding = "UTF-8")
     colnames(stopwords) <- c("term")
     stopwordsAll <- stopwords$term
     
     essayRemStopwords <- tokensEssayRem %>% tokens_remove(stopwordsAll, padding = FALSE) #drop stopwords from essay tokens
     
     essayRemStopwordsDF <- data.frame(tokens=numeric())
     essayRemStopwordsDF[1,1] <- length(essayRemStopwords[[1]]) #number of tokens in "clean" essay
     
     #Spam
     spamDF <- data.frame(n=numeric())
     l<- str_locate_all(tolower(essay), keyWords)
     n <- 0
     for (k in 1:length(l)) {
       n <- n+dim(l[[k]])[1]
     }
     spamDF <- n
     spamPercent <- (spamDF/essayRemStopwordsDF$tokens)*100
     
     # Translation 
     essayTranslation <- gl_translate(
       essayOriginal,
       target = "en",
       format = c("text"))
     englishEssay <- essayTranslation[1]
     
     # Work with english essay
     englishEssay <- gsub("[[:punct:]]", " ", englishEssay)
     englishEssay <- gsub("[0-9]+", "", englishEssay)
     englishEssay <- str_replace_all(englishEssay, "[^[:alnum:]]", " ")
     englishEssay <- gsub("\\s+"," ",str_trim(englishEssay))
     corpusEnglishEssay <- corpus(tolower(englishEssay))
     tokensEnglish <- quanteda::tokens(corpusEnglishEssay, remove_punct = TRUE,remove_numbers = TRUE, remove_symbols = TRUE)
     
     tokensEnglishClean <- tokens_remove(tokensEnglish,pattern = stopwords("en"),padding = TRUE)
     
     # Lexical diversity
     lexDiv <- quanteda.textstats::textstat_lexdiv(tokensEnglishClean,c("U"))
     
     # Info for valueboxes 
     essaySummary$tokens <- ntoken(essay)
     essaySummary$types <- ntype(essay)
     essaySummary$lexDiversity <- lexDiv$U
     essaySummary$spam <- spamPercent

     answersEssay <- data.frame(
       unicTokens = essaySummary$types,
       spam = essaySummary$spam,
       Sugu_n = input$radioGenderEssay,
       Kkp = input$inputKKPEssay,
       VeP = input$radioVePEssay,
       Vanus = trunc((input$dateBirthday %--% Sys.Date()) / years(1)),
       LS__M = input$radioTownMedium,
       lexdiv_U = essaySummary$lexDiversity
     )
     answersConvertedE <- type.convert(answersEssay, as.is = FALSE)
     
     tryCatch({
       essayPrediction$result <- predict(essay_model,  answersConvertedE, type="prob")
       essayPercentages$no <-  essayPrediction$result %>% transmute(round(no*100, 2))
       essayPercentages$yes <-  essayPrediction$result %>% transmute(round(yes*100, 2))
     }, error = function(e) {
       showNotification("Ennustamine ebaõnnestus. Kontrollige, kas kõik väljad on õigesti täidetud.","",type = "error")
       return()
     }, silent=TRUE) 
   })
   
   
   # Essay prediction results div
   output$essayPredictionResult <- renderUI({
     if(is.null(essayPrediction$result))
       div(class = "models-info",
           p("Sisestage essee ja vajutage nuppu Ennusta!")
       )#div
     else
       div(class = "models-info",
           p("Saadud tulemused:")
       )#div
   })
   
   # Prediction result
   output$ePredictResult1 <- renderValueBox({
     valueBox(
       paste0(essayPercentages$no, "%"),
       "Jääb kolledžisse",
       icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green"
     )})
   output$ePredictResult2 <- renderValueBox({
     valueBox(
       paste0(essayPercentages$yes, "%"),
       "Langeb välja",
       icon = icon("thumbs-down", lib = "glyphicon"),
       color = "red"
     )
   })
   
   # Essay info
   output$essayTokens <- renderValueBox({
     valueBox(
       paste0(essaySummary$tokens,""),
       "sõnade arv", #tokens
       color = "purple"
     )})
   output$essayUniqueTokens <- renderValueBox({
     valueBox(
       paste0(essaySummary$types, ""),
       "unikaalsete sõnade arv", #unique tokens
       color = "purple"
     )
   })
   output$essaySpam<- renderValueBox({
     valueBox(
       paste0(round(essaySummary$spam,3), "%"),
       "rämps", #spam
       color = "purple"
     )
   })
   output$essayDiversity<- renderValueBox({
     valueBox(
       paste0(round(essaySummary$lexDiversity, 3), ""),
       "leksikaalne rikkus", #lexical diversity
       color = "purple"
     )
   })
   
   # Reset button
   observeEvent(input$buttonEssayReset, {
     essayPrediction$result <- NULL
     essayPercentages$yes <- 0
     essayPercentages$no <- 0
     updateTextAreaInput(inputId = "inputEssay", value = "")
     essaySummary$tokens = 0
     essay = NULL
     essaySummary$types = 0
     essaySummary$spam = 0
     essaySummary$lexDiversity = 0
     updateRadioButtons(inputId = "radioGenderEssay", selected = "0")
     updateDateInput(inputId = "dateBirthdayEssay", value = Sys.Date())
     updateRadioButtons(inputId = "radioVePEssay", selected = "1")
     updateNumericInput(inputId = "inputKKPEssay", value = "3")
     updateRadioButtons(inputId = "radioTownMedium", selected = "1")
   })  
  
   
   # Conversation  -------------------------------------------------------------
  
   convPrediction <- reactiveValues(result = NULL)
   convPercentages <- reactiveValues(no = 0, yes = 0)
   answers_converted  <- reactiveValues()
   
   # Get selected town name to find town size from df
   updateSelectInput(inputId = "selectTown", choices = town$Linn)
   
   observeEvent(input$buttonConvPredict, {
     townName <- input$selectTown
     townSize <- town$LinnaSuurus[town$Linn==townName]
     age <- trunc((input$dateBirthday %--% Sys.Date()) / years(1))
     
     answers <-  data.frame(
      Kuu = month(input$dateBirthday),
      Vanus = age,
      Ida.Viru = input$radioIdaVirumaa,
      VeP = input$radioVeP,
      V_aeg = input$radioVestluseAeg,
      Kodakondsus = input$radioCitizenship,
      LS_ = townSize,
      Koolikeel = input$radioSchoolLanguage,
      Kkp = input$inputKKP,
      ametlikEmail = input$radioEmail,
      Sugu = input$radioGender,
      emailHost = input$radioEmailHost,
      Kutsekool = input$radioSchool
     )#answers end
     
      answers_converted <- type.convert(answers, as.is = FALSE)
      tryCatch({
        convPrediction$result <- predict(conversation_model, answers_converted, type="prob")
        convPercentages$no <- convPrediction$result %>% transmute(round(no*100, 2))
        convPercentages$yes <- convPrediction$result %>% transmute(round(yes*100, 2))
      }, error = function(e) {
        showNotification("Ennustamine ebaõnnestus. Kontrollige, kas kõik väljad on õigesti täidetud.","",type = "error")
        return()
      }, silent=TRUE) 
      
   })
   
   # Conversation prediction results div
   output$convPredictionResult <- renderUI({
     if(is.null(convPrediction$result))
       div(class = "models-info",
           p("Valige või sisestage vastuseid ja vajutage nuppu Ennusta!")
       )#div
     else
       div(class = "models-info",
           p("Saadud tulemused:")
       )#div
   })
   
   # Info boxes result
   output$convPredictResult1 <- renderValueBox({
     valueBox(
       paste0(convPercentages$no, "%"),
       "Jääb kolledžisse",
       icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green"
     )})
   output$convPredictResult2 <- renderValueBox({
     valueBox(
       paste0(convPercentages$yes, "%"),
       "Langeb välja",
       icon = icon("thumbs-down", lib = "glyphicon"),
       color = "red"
     )
   })
   
   # Reset button
   observeEvent(input$buttonConvReset, {
     convPrediction$result <- NULL
     convPercentages$yes <- 0
     convPercentages$no <- 0
     updateRadioButtons(inputId = "radioIdaVirumaa", selected = "1")
     updateRadioButtons(inputId = "radioVeP", selected = "1")
     updateRadioButtons(inputId = "radioVestluseAeg", selected = "suvi")
     updateRadioButtons(inputId = "radioSchoolLanguage", selected = "eesti")
     updateRadioButtons(inputId = "radioSchool", selected = "jah")
     updateRadioButtons(inputId = "radioEmail", selected = "jah")
     updateRadioButtons(inputId = "radioEmailHost", selected = "com")
     updateDateInput(inputId = "dateBirthday", value = Sys.Date())
     updateRadioButtons(inputId = "radioCitizenship", selected = "Est")
     updateNumericInput(inputId = "inputKKP", value = 3)
     updateRadioButtons(inputId = "radioGender", selected = "m")
   })  
  
   
   
   
  # File upload page  -----------
   
  # Read data from excel file ------
   
   loadedData <- reactive({
     loadedFile <- input$inputFile
     if(is.null(loadedFile)){
       return(NULL)
     }
     readxl::read_excel(loadedFile$datapath, 1)
   }) # read data end
   
  # UI if file was loaded  --------
  output$loadedFile<- renderUI ({
    data <- loadedData()
    if(is.null(data))
      p("Tabel ilmub, kui fail on juba valitud.")
    else(
      DT::dataTableOutput("fileContentDT")
    ) #else 
  }) #loadedFile end

  # Uploaded table
  output$fileContentDT <- DT::renderDataTable({
    data <- loadedData()
    if(is.null(data))
      return (NULL)
    else (
    DT::datatable(data, extensions = 'FixedColumns',
                  options = list(pageLength = 3, scrollX = TRUE, fixedColumns = TRUE))
    )
  })

    # Predict button logic -------------
    filePrediction <- reactiveValues()
    
    observeEvent(input$buttonFilesPagePrediction, {
      data <- loadedData()
      model <- input$radioModels
      model <-as.numeric(model)
      
        tryCatch({
          filePrediction$model<- predict(loaded_models[[model]], data, type="prob") %>% transmute("Väljalangemise tõenäosus" = round(yes*100, 2))
        }, error = function(e) {
          showNotification("Ennustamine ebaõnnestus. Kas laetud failis on vajalikud tunnused valitud mudeli jaoks?","",type = "error")
          return()
        }, silent=TRUE) # check if loaded file can be used for selected prediction model
      
    })
    
    # Uploaded file prediction results -----------
    output$predictionResultsByModel<- renderUI ({
      data <- filePrediction$model
      if(is.null(data))
        p("Tabel ilmub, kui fail on juba valitud ja ennustamine on tehtud.")
      else(
        DT::dataTableOutput("predictionResultsDT")
      ) #else
    })
    
    output$predictionResultsDT <- DT::renderDataTable({
     data <- filePrediction$model
      if(is.null(data))
        return (NULL)
      else (
        DT::datatable(data, extensions = 'Buttons',
                      options = list(
                        pageLength = 15, 
                        dom = 'Brtp', 
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
      )
    }) #prediction results
    
    output$allVariablesDT <- DT::renderDataTable({
      data <- variables$all
        DT::datatable(data, options = list(pageLength = 10))
      
    }) #variables
    
    # Reset button  -----
    observeEvent(input$buttonFilesPageReset, {
      reset(id = "inputFile")
      loadedData <- NULL
      loadedFile <- NULL
      filePrediction$model <- NULL
      model <- NULL
      updateRadioButtons(inputId = "radioModels", selected = "1")
    }) 
    
  
}#server function end
