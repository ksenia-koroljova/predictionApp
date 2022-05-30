#
# Shiny Application for students early dropout prediction (ui)
# Author: Ksenia Koroljova
#


library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dashboardthemes)
library(shinyjs)

 # Custom theme colors -----------------------
customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#222222"
  ,primaryFontColor = "#FFFFFF"
  ,infoFontColor = "#FFFFFF"
  ,successFontColor = "#FFFFFF"
  ,warningFontColor = "#FFFFFF"
  ,dangerFontColor = "#FFFFFF"
  ,bodyBackColor = "#FFFFFF"
  
  ### header
  ,logoBackColor = "#E83E8C"
  
  ,headerButtonBackColor = "#E83E8C"
  ,headerButtonIconColor = "#FFFFFF"
  ,headerButtonBackColorHover = "#CC2D74"
  ,headerButtonIconColorHover = "#FFFFFF"
  
  ,headerBackColor = "#E83E8C"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#F2F2F2"
  ,sidebarPadding = "0"
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#222222"
  
  ,sidebarSearchBackColor = "#F0F0F0"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"
  
  ,sidebarTabTextColor = "#E83E8C"
  ,sidebarTabTextSize = "16"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "1"
  
  ,sidebarTabBackColorSelected = "#E83E8C"
  ,sidebarTabTextColorSelected = "#FFFFFF"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#E83E8C"
  ,sidebarTabTextColorHover = "#FFFFFF"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#FFFFFF"
  ,sidebarTabBorderWidthHover = "0"
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "#FFFFFF"
  ,boxBorderRadius = "0"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#FFFFFF"
  ,boxPrimaryColor = "#342b60"
  #,boxInfoColor = "#51BFD3" blue
  ,boxInfoColor = "#E83E8C"
  ,boxSuccessColor = "#70A53A"
  ,boxWarningColor = "#FFC75A"
  ,boxDangerColor = "#F34949"
  
  ,tabBoxTabColor = "#FFFFFF"
  ,tabBoxTabTextSize = "16"
  ,tabBoxTabTextColor = "#342B60"
  ,tabBoxTabTextColorSelected = "#E83E8C"
  ,tabBoxBackColor = "#FFFFFF"
  ,tabBoxHighlightColor = "#fff" #!!!
  ,tabBoxBorderRadius = "0"
  
  ### inputs
  ,buttonBackColor = "#E40D7E"
  ,buttonTextColor = "#FFFFFF"
  ,buttonBorderColor = "#E40D7E"
  ,buttonBorderRadius = "0"
  
  ,buttonBackColorHover = "#C70C6C"
  ,buttonTextColorHover = "#FFFFFF"
  ,buttonBorderColorHover = "#C70C6C"
  
  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#E40D7E"
  ,textboxBorderRadius = "0"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#E40D7E"
  
  ### tables
  ,tableBackColor = "#EBEBEB"
  ,tableBorderColor = "#FFFFFF"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)


# UI part

ui <- dashboardPage(
  title="Väljalangemise ennustamine — R Shiny Project",
  dashboardHeader(title = tags$img(src="logo.png")),
  
  # Menu ----------------------------------------------------------------------------
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Tutvumine", tabName = "about", icon = icon("info-sign", lib = "glyphicon")),
      menuItem("Mudelid", tabName = "models",  icon =icon("stats", lib = "glyphicon")),
      menuItem("Ennustamine", tabName = "prediction",  icon = icon("tasks", lib = "glyphicon")),
      menuItem("Faili laadimine", tabName = "fileUpload",  icon = icon("upload", lib = "glyphicon"))
      )),
  # ----------------------------------------------------------------------------
  # Main page -------------------------
  dashboardBody(
    useShinyjs(), 
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    customTheme,
    tabItems(
      # About
      tabItem(
        tabName = "about",
        fluidPage(  
          tabBox(
            id = "tabBoxAbout",
            width = 12,
            tabPanel(
              "Rakendusest",
              fluidRow(
                column(
                  width = 12,
                  div (
                    class = "blockInfo",
                    div(
                      class = "blockImg",
                      img(src="student.jpg")
                    ),
                    div (
                      class = "blockText",
                      h2("Taust"),
                      p("Tänapäeval IT’ga seotud erialad on hästi populaarsed mitte ainult noorte aga ka vanema põlvkonna seas: ülikoolide ja kolledžite IT valdkondadele asuvad õppima nii noored kui ka need kes juba omavad erialalisi teadmiseid mõnes muus valdkonnas. Vaatamata suurele nõudlusele, juba esimese semestri kestel osa tudengeid langevad välja, põhused võivad olla erinevad: ennekõike isiklikel põhustel ja mitteedasijõudmise tõttu. Tihtipeale selline olukord on seotud sellega, et tudengid ei oma selget pilti sellest, mida õppeprotsess endast kujutab."),
                      p("Iga õppeasutuse, sh Tallinna Tehnikaülikooli Virumaa kolledži huvides on lahkuvate tudengite arvu vähendada. Seepärast 2019-st aastast muudeti kolledži abiturientide vastuvõtutingimused: tõsteti minimaalne lõputunnistuse keskmine hinne, lisandus mentorlus. Peale sellele vastuvõtutingimusena lisandus vestlus, mille käigus tudengitele pakutakse vastata õpimotivatsiooniga seotud küsimustele, samuti esitatakse otseselt telemaatika ja arukate süsteemide erialaga seotud küsimusi.")
                    )
                  )
                )
                ),
                fluidRow(
                column(
                  width = 12,
                  div (
                    class = "blockText", 
                    h2("Mudelid")
                  ),
                 div(
                   class = "blockInfo",
                   div (
                     class = "blockImg",
                     img(src="chat.png")
                    ), # img conversation
                   
                   div (
                     class = "blockText",
                     p("Vestluse mudel põhineb nii läbi viidud vestluse andmetel, kui ka tudengite informatsioonil, mis võetakse SAIS süsteemist. Andmed SAIS’ist sisaldavad üldandmeid sisseastujate kohta, nagu kontaktandmed, kodakondsus, sugu, õppevorm, sünniaeg ning andmeid varem saadud keskhariduse kohta: õppekeel, lõputunnistuse keskmine hinne ja õppeasutuse nimetus, kuid puudub matemaatika riigieksami tulemus."),
                     p("Vestluse käigus sisseastujatele esitatakse küsimused seotud õpimotivatsiooniga, huviga eriala vastu ning eelteadmistega antud valdkonnas. Küsimuste vastused annavad teatud hulgas konkursipunkte, mida kasutatakse ennustamismudelis koos vestluse kuupäevadega.")
                     )
                   ), #conversation model end
                 div(
                   class = "blockInfo",
                   div (
                     class = "blockImg",
                     img(src="questionnaire.png")
                   ), # img questionnaire
                   
                   div (
                     class = "blockText",
                     p("Küsitlus oli loodud, et välja selgitada sisseastujate motivatsioon, kogemus ja teadmised valitud valdkonnast."),
                     p("Küsimustik on eesti ja vene keeles ning sisaldab erinevaid küsimusi kolmest kategooriast: üldandmed, eriala valik ning isiklikud omadused. Erinevalt esseest, küsimustik sisaldab ka sobivaid vastuseid ning vastaja ainult valib kõige sobivama varianti, mis ka lihtsustab järgmise töötlemist.")
                   )
                 ), #questionnaire model end
                 div(
                   class = "blockInfo",
                   div (
                     class = "blockImg",
                     img(src="contract.png")
                   ), # img essay
                   
                   div (
                     class = "blockText",
                     p("Esmakursuslased kirjutati esseed teemal „Minu nägemus tulevasest erialasest tööst“ õppeaine „Sissejuhatus erialasse“ raames esimese semestri lõpus. Esseed kontrollitakse plagiaadituvastussüsteemi abil ning esseede vormistamine on reeglistatud. "),
                     p("Ennustamiseks olid eraldatud järgmised karakteristikud: teksti leksikaalne rikkus; plagiaadi protsent; loetavuse hinnang, „vesise“ näitaja, mis on stoppsõnade arvu ja sõnade koguarvu suhe tekstis; teksti „rämps“, mis on kõige sagedamate sõnade arvu ja sõnade koguarvu suhe tekstis."),
                     p("Kuna ennustusmudel, mis baseerub ainult kirjutatud esseel ei anda tulemusi, mudelisse on lisatud tunnused vestluse mudelist, mis annab uue mudeli kõrgema täpsusega. ")
                   )
                 ) #essay model end
                 )#column
                )
              ),# about application
  # Models usage --------------------------------------------------------------
            tabPanel(
              "Kuidas kasutada ennustamise mudelit?",
              box(
                status = "primary", 
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 100,
                title = span( icon("question-sign", lib = "glyphicon"), "Üksikennustus"),
                div(class = "models-info",
                    p("Üksikennutust võib teha lehel \"Ennustamine\"."),
                fluidRow(
                  column(
                    width = 4,
                    box(
                      width = 100,
                      p(strong("1. samm")),
                      div (
                        class = "infoImg",
                        img(src="howTo/singlePr.gif")),
                      p("Valige sobiva ennutusmudeli ning vastake küsimustele. Seejärael vajutage nuppu \"Ennusta\".")
                    )),
                  column(
                    width = 4,
                    box(
                      width = 100,
                      p(strong("2. samm")),
                      div (
                        class = "infoImg",
                        img(src="howTo/singlePrResults.gif")
                      ),
                      p("Kui ennustamine on tehtud, ilmub väljalangemise tõenäosus. Kuna kalkuleerimise jaoks essee tõlkitakse inglise keelde kasutades Google poolne API, võib see natuke aega võtta.")
                    )),
                  column(
                    width = 4,
                    box(
                      width = 100,
                      p(strong("3. samm")),
                      div (
                        class = "infoImg",
                        img(src="howTo/singlePrInfo.gif")
                      ),
                      p("Lisainfo essee kohta on nähtav alamplokkides.")
                      )
                  )
                    )#fluidrow
                )
                
                ), # single prediction
              box(
                status = "primary", 
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 100,
                title = span( icon("question-sign", lib = "glyphicon"), "Ennustamine faili järgi"),
                div(class = "models-info",
                    p("Ennustamine faili järgi on paigaldatud lehel \"Faili laadimine\"."),
                    fluidRow(
                      box(
                        width = 4,
                        p(strong("1. samm")),
                        div (
                          class = "infoImg",
                          img(src="howTo/fileSelection.gif")),
                        p("Faili laadimise jaoks vajutage \"Ava\". Valige Excel faili. Kui fail on laaditud, ilmub roheline riba tekstiga \"Upload complete\".")
                        ),
                      box(
                        width = 4,
                        p(strong("2. samm")),
                        div (
                        class = "infoImg",
                        img(src="howTo/modelSelection.gif")
                        ),
                        p("Valige sobiva ennustusmudeli ning vajutage nuppu \"Ennusta\".")
                        ),
                      box(
                        width = 4,
                        p(strong("3. samm")),
                        div (
                          class = "infoImg",
                          img(src="howTo/filePrResults.gif")
                        ),
                        p("Kalkuleerutud väljalangemise tõenäosuse saate näha sakis \"Ennustamise tulemused\".")
                      )
                    )#fluidrow
                )
              ), # file prediction
              box(
                status = "primary", 
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 100,
                title = span( icon("question-sign", lib = "glyphicon"), "Mudelite kirjeldus"),
                div(class = "models-info",
                    p("Mudelite kirjeldus saab leida lehel \"Mudelid\". Seal on nähtav nii mudeli tunnuste mõjuvus, kui ka graafikud varemate tudengite vastustega. ")
                )# about models
              )
            )
          )#tabbox
        )#fluidPage About
      ),# end about
      
  # ------------------------------------------------------------------------
      
      # Models page 
  
      tabItem(
        tabName = "models",
        fluidPage(  
          tabBox(
            id = "tabBoxModels",
            width = 12,
            
            # Questionnaire model
            tabPanel(
              "Küsitluse mudel",
              fluidRow(
                column(
                  width = 6,
                  # Model description ---------------------
                  box(
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 100,
                    title = span( icon("info-sign", lib = "glyphicon"), "Küsitlusmudeli kirjeldus"),
                    div(class = "models-info",
                        p("Küsitlus oli loodud, et välja selgitada sisseastujate motivatsioon, kogemus ja teadmised valitud valdkonnast. Küsimustik on eesti ja vene keeles ning sisaldab erinevaid küsimusi kolmest kategooriast: üldandmed, eriala valik ning isiklikud omadused. Erinevalt esseest, küsimustik sisaldab ka sobivaid vastuseid ning vastaja ainult valib kõige sobivama varianti, mis ka lihtsustab järgmise töötlemist."))
                  ),# description box end
                  
                  # Plot -------------------
                  box(
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    width = 100,
                    title = span( icon("signal", lib = "glyphicon"), "Histogramm"),
                    selectInput('QhistogramVars', "Muutujad:",
                                c('Eesti keel',
                                  'Inglise keel',
                                  'Eelteadmised matemaatikas',
                                  'Informeeritus eriala kohta',
                                  'Meeskonnatöö',
                                  'Kallakus reaalteaduste poole')),
                    plotOutput("QhistogramPlot")
                  )#
                ),
                column(
                  width = 6,
                  # Factors plot ---------------------
                  box (
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 100,
                    title = span(icon("signal", lib = "glyphicon"), "Tunnuste mõjuvus"),
                    plotOutput("questFactorsImportancePlot")
                    
                  ), # factors box with plot end
                  
                  # Factors description ---------------------
                  box(
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 100,
                    title = span( icon("question-sign", lib = "glyphicon"), "Tunnuste kirjeldus"),
                    DT::dataTableOutput("questVariablesDT")
                  )#factors description
                )
              )
              ), #end questionnaire model
            
            
            
            # Essay model
            tabPanel(
              "Essee mudel",
                fluidRow(
                  column(
                    width = 6,
                    # Model description ----------
                    box(
                      status = "primary", 
                      solidHeader = TRUE,
                      width = 100,
                      title = span( icon("info-sign", lib = "glyphicon"), "Essee mudeli kirjeldus"),
                      div(class = "models-info",
                          p("Esmakursuslased kirjutati esseed teemal „Minu nägemus tulevasest erialasest tööst“ õppeaine „Sissejuhatus erialasse“ raames esimese semestri lõpus. Esseed kontrollitakse plagiaadituvastussüsteemi abil ning esseede vormistamine on reeglistatud."),
                          p("Kuna ennustusmudel, mis baseerub ainult kirjutatud esseel ei anda tulemusi, mudelisse on lisatud tunnused vestluse mudelist, mis annab uue mudeli kõrgema täpsusega. Mudeli täiendamiseks olid lisatud järgmised tunnused vestlusmudelist: vestluspunktid ja vestluse aeg.")
                      )
                    ),# description box end
                    
                    # Plot ------------
                    box(
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      collapsed = FALSE,
                      width = 100,
                      title = span( icon("signal", lib = "glyphicon"), "Histogramm"),
                      selectInput('essayHistogramVars', "Muutujad:",
                                  c('Õppevorm',
                                    'Sugu',
                                    'Kodakondsus',
                                    'E-posti host',
                                    'Ametlik e-post',
                                    'Koolikeel',
                                    'Vestluse aeg')),
                      plotOutput("essayHistogramPlot")
                    )
                  ),
                  
                  # Factors plot ----------
                  column(
                    width = 6,
                    box (
                      status = "primary", 
                      solidHeader = TRUE,
                      width = 100,
                      title = span(icon("signal", lib = "glyphicon"), "Tunnuste mõjuvus"),
                      plotOutput("essayVariableImportancePlot")
                      
                    ),# factors box with plot end
                    
                    # Factors description --------
                    box(
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      width = 100,
                      title = span( icon("question-sign", lib = "glyphicon"), "Tunnuste kirjeldus"),
                      DT::dataTableOutput("essayVariablesDT")
                    )#factors description
                  )
                )
            ), #end essay model
            
            
            
            # Conversation model 
            
            tabPanel(
              "Vestluse mudel",
              fluidRow(
                column(
                  width = 6,
                  
                  # Model description ------------
                  box(
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 100,
                    title = span( icon("info-sign", lib = "glyphicon"), "Vestlusmudeli kirjeldus"),
                    div(class = "models-info",
                        p("Vestluse mudel põhineb nii läbi viidud vestluse andmetel, kui ka tudengite informatsioonil, mis võetakse SAIS süsteemist.
Andmed SAIS’ist sisaldavad üldandmeid sisseastujate kohta, nagu kontaktandmed, kodakondsus, sugu, õppevorm, sünniaeg ning andmeid varem saadud keskhariduse kohta: õppekeel, lõputunnistuse keskmine hinne ja õppeasutuse nimetus, kuid puudub matemaatika riigieksami tulemus."),
                        p("Läbi viidud vestlusest sisseastujatega on võtnud andmeid nagu vestluse kuupäevad ning konkursipunktid vestluse eest, mis antakse esitatud küsimuste vastuste, motivatsiooni ja huvi eriala vastu ning eelnevate teadmiste eriala kohta eest.")
                    )
                  ),# description box end
                  
                  # Plot -------------
                  box(
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    width = 100,
                    title = span( icon("signal", lib = "glyphicon"), "Histogramm"),
                    selectInput('convHistogramVars', "Muutujad:",
                                c('Kutsekool',
                                  'Linna suurus',
                                  'Ida-Virumaa',
                                  'Kodakondsus',
                                  'Vestluse aeg',
                                  'Koolikeel')),
                    plotOutput("convHistogramPlot")
                  )
                ),
                
                # Factors plot ------------
                column(
                  width = 6,
                  box (
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 100,
                    title = span(icon("signal", lib = "glyphicon"), "Tunnuste mõjuvus"),
                    plotOutput("convVariableImportancePlot")
                    
                  ),# factors box with plot end
                  
                  # Factors description ---------
                  box(
                    status = "primary",
                    solidHeader = TRUE,
                    collapsed = TRUE,
                    collapsible = TRUE,
                    width = 100,
                    title = span( icon("question-sign", lib = "glyphicon"), "Tunnuste kirjeldus"),
                    DT::dataTableOutput("convVariablesDT")
                  )#factors description
                )
              )
            ) #end conversation model
            
          )#tabbox
       )#fluidPage Models
      ), # end models
      
      # ------------------------------------------------------------------------
      
      # Single prediction 
      tabItem(
        tabName = "prediction",
        fluidPage(
          tabBox(
            id = "tabBoxPrediction",
            width = 12,
            
# Questionnaire model PREDICTION  ----------------------------------------------
            tabPanel(
              "Küsitluse mudel",
              fluidRow(
                column(
                  width = 6,
                  box(
                    width = 100,
                    radioButtons(
                      inputId = "inputSpecialityChoice",
                      label = "1. Kas Telemaatika ja arukate süsteemide eriala on teie esimene valik edasiõppimiseks?",
                      choices = c("jah","ei"),
                      inline = TRUE
                    ), # question 13
                    radioButtons(
                      inputId = "inputEstonian",
                      label = "2. Hinnake oma eesti keele oskust:",
                      choices = lapply(1:5, function(i) {paste0(i)}),
                      inline = TRUE
                    ),# question 5 estonian
                    radioButtons(
                      inputId = "inputSpecialityInfo",
                      label = "3. Hinnake oma informeeritust Telemaatika ja arukate süsteemide eriala kohta:",
                      choices = lapply(1:5, function(i) {paste0(i)}),
                      inline = TRUE
                    ),# question 6
                    radioButtons(
                      inputId = "inputSmarthome",
                      label = "4. Kui palju telemaatika on seotud nutimajaga:",
                      choices = lapply(1:5, function(i) {paste0(i)}),
                      inline = TRUE
                    ),# question 7 nutimaja
                    radioButtons(
                      inputId = "inputMath",
                      label = "5. Hinnake oma eelteadmisi matemaatikas:",
                      choices = lapply(1:5, function(i) {paste0(i)}),
                      inline = TRUE
                    ),# question 16 math
                    radioButtons(
                      inputId = "inputEnglish",
                      label = "6. Hinnake oma eelteadmisi inglise keeles:",
                      choices = lapply(1:5, function(i) {paste0(i)}),
                      inline = TRUE
                    ),# question 16 eng
                    radioButtons(
                      inputId = "inputReal",
                      label = "7. Hinnake oma kallakut reaalteaduste poole:",
                      choices = lapply(1:5, function(i) {paste0(i)}),
                      inline = TRUE
                    ),# question 22 
                    radioButtons(
                      inputId = "inputTeamwork",
                      label = "8. Kas teile meeldib töötada meeskonnas?",
                      choices = lapply(1:5, function(i) {paste0(i)}),
                      inline = TRUE
                    ),# question 23
                    actionButton("buttonQuestionPredict", "Ennusta"),
                    actionButton("buttonQuestionReset", "Tühjenda")
                    )#box end
              ),#column 1
              
              
              column(
                width = 6,
                box(
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 100,
                  title = "Ennustamise tulemus",
                  uiOutput("questionsPredictionResult"),
                  
                  fluidRow(
                    column(
                      width = 6,
                      valueBoxOutput(width = 100, "qPredictResult1")
                    ),
                    column(
                      width = 6,
                      valueBoxOutput(width = 100, "qPredictResult2")
                    )
                  )
                ),#box
                
                box(
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 100,
                  title = "Informatsioon",
                  div(
                    class = "models-info",
                    p("Vastage vastustele skaalal 1-5, kus 1 on \"üldse mitte\" või \"üldse ei oska\" ning 5 on \"väga tugev\" või \"oskan suurepääraselt\".")
                  )
                )
              )#column 2
              
              )#fluidrow
            ),# questionnaire model prediction
            
            
# Essay model PREDICTION  ------------------------------------------------------
            tabPanel(
              "Essee mudel",
                fluidRow(
                  column(
                    width = 6,
                    box(
                      width = 100,
                      radioButtons(
                        inputId = "radioGenderEssay",
                        label = "1. Valige sisseastuja sugu:",
                        choiceNames = c("mees", "naine"),
                        choiceValues = c("0", "1"),
                        inline = TRUE
                      ),
                      dateInput(
                        inputId = "dateBirthdayEssay",
                        label = "2. Sisestage sisseastuja sünnipäev:",
                        language = "et",
                        weekstart = 1,
                        startview = "decade",
                        format = "yyyy-mm-dd"
                      ),
                      radioButtons(
                        inputId = "radioVePEssay",
                        choices = lapply(1:10, function(i) {paste0(i)}),
                        label = "3. Saadud vestluse punktid:",
                        inline = TRUE
                      ),
                      numericInput(
                        inputId = "inputKKPEssay",
                        label = "4. Keskrahiduse lõpptunnistuse keskminne hinne:",
                        min = 1,
                        max = 5,
                        step = 0.01,
                        value = 3
                      ),
                      radioButtons(
                        inputId = "radioTownMedium",
                        choiceNames = c("jah", "ei"),
                        choiceValues = c("1", "0"),
                        label = "5. Kas sisseastuja on pärit keskmise suurusega linnast?",
                        inline = TRUE
                      ),
                      textAreaInput(
                        inputId = "inputEssay",
                        label = "SIsestage essee:",
                        resize = "vertical",
                        width = "100%",
                        height = "400px",
                        placeholder = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec cursus cursus nunc, nec dictum purus auctor eget. Ut vel eros aliquam, pharetra velit quis, imperdiet nunc. Vivamus ultrices nibh sit amet consectetur mollis. Aliquam facilisis maximus sem. Ut cursus tempus elit, eget varius dolor egestas eget. Etiam dignissim tincidunt neque, efficitur rhoncus neque posuere nec. Sed hendrerit ante in purus condimentum placerat. Sed vel justo et mauris aliquam euismod. Praesent a leo a nunc fermentum rhoncus. Vestibulum ac volutpat metus. Proin lacinia tincidunt ligula, a suscipit odio facilisis eu. Donec leo mi, rhoncus ut consectetur aliquam, cursus sed sem."
                      ),
                      actionButton("buttonEssayPredict", "Ennusta"),
                      actionButton("buttonEssayReset", "Tühjenda")
                    )
                  ), #column1
                  
                  column(
                    width = 6,
                    box(
                      status = "primary", 
                      solidHeader = TRUE,
                      width = 100,
                      title = "Ennustamise tulemus",
                      uiOutput("essayPredictionResult"),
                      fluidRow(
                        column(
                          width = 6,
                          valueBoxOutput(width = 100, "ePredictResult1")
                        ),
                        column(
                          width = 6,
                          valueBoxOutput(width = 100, "ePredictResult2")
                        )
                      )
                    ),#result
                    
                    box(
                      status = "primary", 
                      solidHeader = TRUE,
                      width = 100,
                      title = "Essee karakteristikud",
                      fluidRow(
                        column(
                          width = 6,
                          valueBoxOutput(width = 100, "essayTokens") #tokens
                        ),
                        column(
                          width = 6,
                          valueBoxOutput(width = 100, "essayUniqueTokens")
                        )
                      ), # row
                    fluidRow(
                      column(
                        width = 6,
                        valueBoxOutput(width = 100, "essaySpam")
                      ),
                      column(
                        width = 6,
                        valueBoxOutput(width = 100, "essayDiversity")
                      )
                    )#row
                    )
                  )#column 2
                  
                )#fluidrow
            ),# essay model prediction
            
            
# Conversation model PREDICTION  ------------------------------------------------------
              tabPanel(
                "Vestluse mudel",
                fluidRow(
                  column(
                    width = 6,
                    box(
                      width = 100,
                      radioButtons(
                        inputId = "radioGender",
                        label = "1. Valige sisseastuja sugu:",
                        choiceNames = c("mees", "naine"),
                        choiceValues = c("m", "n"),
                        inline = TRUE
                      ),
                      dateInput(
                        inputId = "dateBirthday",
                        label = "2. Sisestage sisseastuja sünnipäev:",
                        language = "et",
                        weekstart = 1,
                        startview = "decade",
                        format = "yyyy-mm-dd"
                      ),
                      radioButtons(
                        inputId = "radioCitizenship",
                        label = "3. Valige sisseastuja kodakondsus:",
                        choiceNames = c("Eesti", "teine"),
                        choiceValues = c("Est", "teine"),
                        inline = TRUE
                      ),
                      radioButtons(
                        inputId = "radioIdaVirumaa",
                        choiceValues = c(1,0),
                        choiceNames = c("jah", "ei"),
                        label = "4. Kas sisseastuja on Ida-Virumaalt?",
                        inline = TRUE
                      ),
                      selectInput(
                        inputId = "selectTown",
                        label = "5. Valige linn või vald:",
                        choices = ""
                      ),
                      radioButtons(
                        inputId = "radioSchoolLanguage",
                        label = "6. Eelmise kooli keel:",
                        choices = c("eesti","vene"),
                        inline = TRUE
                      ),
                      radioButtons(
                        inputId = "radioSchool",
                        label = "7. Kas eelmise õppeasutusena oli kutsekool?",
                        choiceNames = c("jah","ei"),
                        choiceValues = c("1", "0"),
                        inline = TRUE
                      ),
                      radioButtons(
                        inputId = "radioVeP",
                        choices = lapply(1:10, function(i) {paste0(i)}),
                        label = "8. Saadud vestluse punktid:",
                        inline = TRUE
                      ),
                      radioButtons(
                        inputId = "radioVestluseAeg",
                        choiceNames = c("suvi", "muu aeg"),
                        choiceValues = c("suvi", "teine"),
                        label = "9. Vestluse aeg:",
                        inline = TRUE
                      ),
                      numericInput(
                        inputId = "inputKKP",
                        label = "10. Keskrahiduse lõpptunnistuse keskminne hinne:",
                        min = 1,
                        max = 5,
                        step = 0.01,
                        value = 3
                      ),
                      radioButtons(
                        inputId = "radioEmail",
                        choices = c("jah", "ei", "segane"),
                        label = "11. Kas sisseastuja e-post on ametlik?",
                        inline = TRUE
                      ),
                      radioButtons(
                        inputId = "radioEmailHost",
                        choiceNames = c(".com", ".ee", ".ru"),
                        choiceValues = c("com", "ee", "ru"),
                        label = "12. Valige e-posti host:",
                        inline = TRUE
                      ),
                      actionButton("buttonConvPredict", "Ennusta"),
                      actionButton("buttonConvReset", "Tühjenda")
                    )#box end
                  ),#column 1
                  
                  
                  column(
                    width = 6,
                    box(
                      status = "primary", 
                      solidHeader = TRUE,
                      width = 100,
                      title = "Ennustamise tulemus",
                      uiOutput("convPredictionResult"),
                      
                      fluidRow(
                        column(
                          width = 6,
                          valueBoxOutput(width = 100, "convPredictResult1")
                        ),
                        column(
                          width = 6,
                          valueBoxOutput(width = 100, "convPredictResult2")
                        )
                      )
                    )#box
                  )#column 2
                  
                )#fluidrow
              )# questionnaire model prediction

            
            
          ) #tabBoxPrediction end
        )#fluidPage Prediction
      ), # end prediction page
      


      # Prediction by file
      
      # File Upload ------------------------------------------------------------------------
      tabItem(
        tabName = "fileUpload",
        fluidPage(
            tabBox(
              id = "tabBoxFiles",
              width = 12,
              tabPanel(
                "Excel failid (.xls, .xlsx)",
                # First row 
                fluidRow(
                  column (
                    width = 6,
                    box(
                     status = "primary", 
                      solidHeader = TRUE,
                      width = 100,
                      title = "Faili laadimine",
                      collapsible = TRUE,
                      collapsed = FALSE,
                     div(
                       class = "models-info",
                       p("Faili allalaadimiseks klõpsake nuppu 'Ava'. Kuvatavas aknas valige Excel-faili kas .xls või .xlsx formaadis. Laaditud faili saab läbi vaadata allpool.")
                     ),
                     
                        fileInput(
                          inputId = "inputFile",
                          label = "Valige fail laadimiseks:",
                          accept = ".xls",
                          buttonLabel = "Ava...",
                          placeholder = "Fail ei ole valitud"
                        )
                     ) #box
                      ), #column 1 end
                      
                      column(
                        width = 6,
                        box(
                          status = "info", 
                          solidHeader = TRUE,
                          width = 100,
                          title = "Ennustamismudelid",
                          radioButtons(
                            inputId = "radioModels",
                            label = "Valige mudel ennustamiseks:",
                            choiceNames = c("Küsimustiku mudel", "Vestluse mudel", "Essee mudel"),
                            choiceValues = c("1", "2", "3")
                          ),
                          actionButton("buttonFilesPagePrediction", "Ennusta"),
                          actionButton("buttonFilesPageReset", "Tühjenda")
                        )#box
                      ) #column 2 end
                  
                ), # first row end
                
                # Second row 
                fluidRow(
                  column(
                    width = 12,
                    # Rendered table after upload
                    box(
                     status = "primary", 
                      solidHeader = TRUE,
                      width = 100,
                      title = "Laaditud faili vaatamine",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      uiOutput("loadedFile")
                    ) #box
                  ) #column 2 end
                  
                ), # 2nd row end
                
                # Third row
                fluidRow(
                  column(
                    width = 12,
                    box (
                      width = 100,
                      status = "primary", 
                      solidHeader = TRUE,
                      title = "Ennustamise tulemused",
                      uiOutput("predictionResultsByModel")
                    )
                  )
                ), # 3rd row end
                
                # Fourth row
                fluidRow(
                  column(
                    width = 12,
                    box (
                      width = 100,
                      status = "primary", 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      title = "Muutujate nimekiri",
                      DT::dataTableOutput("allVariablesDT")
                    )
                  )
                ) # 4th row end
                
              )#tabPanel
            )#tabBox
        )#fluidPage  file upload
      )# end file upload
      
    )#all tab items
  )#dashboardBody
)


