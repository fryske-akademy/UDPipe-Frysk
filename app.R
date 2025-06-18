library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinysky)
library(shinyAce)
library(readr)
library(readtext)
library(xml2)
library(rvest)
library(openxlsx)
library(stringr)
library(cld3)
library(udpipe)
library(DT)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(textrank)
library(ggwordcloud)

# sudo apt-get update
# sudo apt-get install -y libpoppler-cpp-dev libprotobuf-dev protobuf-compiler glpk-utils libglpk-dev

################################################################################

ui <- tagList(
  useShinyjs(),

  includeCSS("www/styles.css"), extendShinyjs(script = "extend.js", functions = c("clearUpload")),
  titlePanel(title = HTML("<div class='title'>UDPipe Frysk<div>"), windowTitle = "UDPipe Frysk"),

  tags$head(
    tags$link(rel="icon", href="FA2.png"),

    tags$meta(charset="UTF-8"),
    tags$meta(name   ="description", content="UDPipe Frysk is a web app for lemmatizing and POS-tagging Frisian texts. The tagger is trained on Frisian text that is annotated according to the guidelines of Universal Dependencies version 2 using the universal POS-tags."),
  ),

  navbarPage
  (
    title=NULL, id = "navBar", collapsible = TRUE,

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-cog' style='font-size: 100%'></span>&nbsp;Run"),
      value = "run",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 203px); background-color: #eeeefe",

        br(),

        fluidPage(
          align="left",
          style='max-width: 880px;',

          p("This service is freely available.
             Please respect the ", a("CC BY-NC-SA", href = "http://creativecommons.org/licenses/by-nc-sa/4.0/", target = "_blank"), "licence of the models – ",
             HTML("explicit written permission of the authors is required for any commercial exploitation of the system."),
             "If you use the service, you agree that data obtained by us during such use can be used for further improvements of this system. For Dutch and English the", a("UDPipe REST API", href = "https://lindat.mff.cuni.cz/services/udpipe/api-reference.php", target = "_blank"), "is used."),
        ),

        align="center",
        br(),

        HTML("<p style='font-weight: bold;'>Select Input:</p>"),
        br(),

        icon(NULL),

        radioGroupButtons(
          inputId    = "selInput",
          label      = NULL,
          choices    = c("<i class='glyphicon glyphicon-font' ></i> Text" = "Text" ,
                         "<i class='glyphicon glyphicon-file' ></i> File" = "File" ,
                         "<i class='glyphicon glyphicon-globe'></i> Web"  = "Web" ),
          individual = TRUE,
          selected   = "Text"
        ),

        br(),
        uiOutput("getInput"),
        br(),

        splitLayout
        (
          cellWidths = c("270px", "90px"),
          pickerInput('selLang', NULL, c("frisian_frysk-ud-1.00-250407", "dutch-alpino-ud-2.12-230717", "dutch-lassysmall-ud-2.12-230717", "english-partut-ud-2.12-230717"), selected="frisian_frysk-ud-1.00-250407", multiple=FALSE, width="255px", options = pickerOptions(title = "Language", dropupAuto = F, container = 'body')),
          shiny::actionButton("clearButton", HTML("<span class='glyphicon glyphicon-erase' style='font-size: 90%'></span>&nbsp;Clear"))
        ),

        br(),
        busyIndicator(text = NULL, wait = 1000),
        uiOutput("showResults"),

        br(), br(), br(), br()
      ),

      br()
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-signal' style='font-size: 100%'></span>&nbsp;Graphs"),
      value = "graphs",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 203px); background-color: #eeeefe",

        br(),

        splitLayout(
          cellWidths = c("340px", "30px", "auto"),
          cellArgs = list(style = "padding: 0px"),

          span(
            fluidPage(
              style = "border: 1px solid silver; background-color: #eeeefe",

              br(), br(),

              radioButtons("selGraph", NULL, choices = c(
                "UPOS frequencies",
                "Most occurring tokens",
                "Most occurring tokens wordcloud",
                "Keywords RAKE",
                "Keywords wordcloud",
                "Keywords PMI/MD/LFMD",
                "Co-occurrences within sentence",
                "Words following one another"),

                selected = "UPOS frequencies", inline = FALSE
              ),

              br()
            ),

            uiOutput("showInputs")
          ),

          div(),

          span(
            busyIndicator(text = NULL, wait = 1000),
            uiOutput("showAnalyses")
          )
        )
      ),

      br()
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-info-sign' style='font-size: 100%'></span>&nbsp;About"),
      value = "about",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 203px); background-color: #eeeefe",

        br(),

        h5(strong("About")),
        p("UDPipe Frysk is a web app for lemmatizing, PoS tagging and dependency parsing of Frisian texts. The lemmatizer/tagger/parser are trained on Frisian text that is annotated according to the ", a("guidelines of Universal Dependencies version 2", href = "https://universaldependencies.org/guidelines.html", target = "_blank"), ". We use", a("UDPipe", href = "http://dx.doi.org/10.18653/v1/K17-3009", target = "_blank"), "as available via the R package", a("udpipe", href = "https://cran.r-project.org/web/packages/udpipe/index.html", target = "_blank"), ". The following people were involved in the development of UDPipe Frysk: Hans Van de Velde (Fryske Akademy, project manager/financing), Wilbert Heeringa (Fryske Akademy, building training corpus/implementation UDPipe Frysk), Gosse Bouma (University of Groningen, building training corpus/advice), Martha Hofman (Fryske Akademy, building training corpus), Eduard Drenth (Fryske Akademy, building web service). UDPipe Frysk is still under develoment. Comments are welcome and can be sent to", img(src = 'email.png', height = 19, align = "center"),"."),
        p("In addition to the lemmatizer/tagger/dependency parser for Frisian, a lemmatizer/tagger/dependency parser for Dutch (", a("LassySmall", href = "https://universaldependencies.org/treebanks/nl_lassysmall/index.html", target = "_blank"), ", compiled by Gosse Bouma and Gertjan van Noord) and English (", a("ParTUT", href = "https://universaldependencies.org/treebanks/en_partut/index.html", target = "_blank"), ", compiled by Cristina Bosco and Manuela Sanguinetti) is provided. When entering or uploading some text, the appropriate tool is automatically chosen."),
        br(),

        h5(strong("Corpus compilation and model performance")),
        p("For details about the corpus compilation and the model performance see", a("this", href = "stats.pdf", target = "_blank"), "document."),
        br(),
        
        h5(strong("Web service")),
        p("A", a("web service", href = "https://frisian.eu/udpipeservice/", target = "_blank"), "has been developed by Eduard Drenth."),
        p("If you want to process a text file 'Frysk.txt' and save the output as a conllu file, give the command: "),
        p(style="font-family: Courier, Monaco, monospace", "curl -X POST -H 'Content-Type: text/plain' --data-binary '@Frysk.txt' https://frisian.eu/udpipeservice/udpipe/process/conllu"),
        p("If you want to process the text 'Praat mar Frysk' and save the output as TSV file, give the command:"),
        p(style="font-family: Courier, Monaco, monospace", "curl -X POST -H 'Content-Type: text/plain' --data-binary 'Praat mar Frysk' https://frisian.eu/udpipeservice/udpipe/process/tsv"),
        p("For bigger texts use the", a("command line script", href = "https://bitbucket.org/fryske-akademy/udpipe/src/master/", target = "_blank"), "."),
        br(),

        h5(strong("System requirements")),
        p("UDPipe Frysk runs best on a computer with a monitor with a minimum resolution of 1370 x 870 (width x height). The use of (a recent version of) Chrome, Chromium, Edge, Firefox or Opera as a web browser is to be preferred."),
        br(),

        h5(strong("How to cite this app")),
        p("Wilbert Heeringa, Gosse Bouma, Martha Hofman, Eduard Drenth, Jan Wijffels & Hans Van de Velde (2022),", a("POS tagging, lemmatization and dependency parsing of West Frisian", href = "https://aclanthology.org/2022.lrec-1.512/", target = "_blank"), ", In Proceedings of the Thirteenth Language Resources and Evaluation Conference, pages 4789–4798, Marseille, France. European Language Resources Association."),
        br(),

        h5(strong("Acknowledgements")),
        p("The development of this software was made possible by a CLARIAH-Plus project financed by the Dutch Research Council (Grant 184.034.023). We thank Milan Straka and Jana Straková", a("(Charles University, UFAL)", href = "http://ufal.mff.cuni.cz/about", target = "_blank"), "for developing", a("UDPipe", href = "http://dx.doi.org/10.18653/v1/K17-3009", target = "_blank"), ". We thank Jan Wijffels", a("(BNOSAC)", href = "http://www.bnosac.be/", target = "_blank"),"for developing and making available the R package ", a("udpipe", href = "https://bnosac.github.io/udpipe/en/index.html", target = "_blank"), ". In this web app we used glyphs from the ", a("Glyphicon Halflings", href = "https://www.glyphicons.com/", target = "_blank"), "set."),
        br(),

        h5(strong("Implementation")),
        p("Two existing web apps served as examples, namely ", a("UDPipe", href = "http://lindat.mff.cuni.cz/services/udpipe/", target = "_blank"), " and ", a("Linguakit", href = "https://linguakit.com/en/part-of-speech-tagging", target = "_blank"), ". ", "UDPipe Frysk is implemented as a Shiny app. Shiny was developed by RStudio. This web app uses the following R packages:"),
        br(),

        tags$div(tags$ul
        (
          tags$li(tags$span(HTML("<span style='color:blue'>base</span>"), p("R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shiny</span>"), p("Winston Chang, Joe Cheng, J.J. Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version 1.0.0. https://CRAN.R-project.org/package=shiny"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinyjs</span>"), p("Dean Attali (2018). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 1.0. https://CRAN.R-project.org/package=shinyjs"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinyWidgets</span>"), p("Victor Perrier, Fanny Meyer and David Granjon (2019). shinyWidgets: Custom Inputs Widgets for Shiny. R package version 0.5.0. https://CRAN.R-project.org/package=shinyWidgets"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinysky</span>"), p("Dai ZJ (2019). shinysky: A Set of Shiny Components and Widgets. R package version 0.1.3. https://github.com/AnalytixWare/ShinySky"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinyAce</span>"), p("Vincent Nijs, Forest Fang, Trestle Technology, LLC and Jeff Allen (2019). shinyAce: Ace Editor Bindings for Shiny. R package version 0.4.1. https://CRAN.R-project.org/package=shinyAce"))),
          tags$li(tags$span(HTML("<span style='color:blue'>readr</span>"), p("Hadley Wickham, Jim Hester and Romain Francois (2018). readr: Read Rectangular Text Data. R package version 1.3.1. https://CRAN.R-project.org/package=readr"))),
          tags$li(tags$span(HTML("<span style='color:blue'>readtext</span>"), p("Benoit K, Obeng A (2021). _readtext: Import and Handling for Plain and Formatted Text Files_. R package version 0.81, https://CRAN.R-project.org/package=readtext"))),
          tags$li(tags$span(HTML("<span style='color:blue'>xml2</span>"), p("Hadley Wickham, Jim Hester and Jeroen Ooms (2021). xml2: Parse XML. R package version 1.3.3. https://CRAN.R-project.org/package=xml2"))),
          tags$li(tags$span(HTML("<span style='color:blue'>rvest</span>"), p("Hadley Wickham (2021). rvest: Easily Harvest (Scrape) Web Pages. R package version 1.0.2. https://CRAN.R-project.org/package=rvest"))),
          tags$li(tags$span(HTML("<span style='color:blue'>openxlsx</span>"), p("Philipp Schauberger and Alexander Walker (2020). openxlsx: Read, Write and Edit xlsx Files. R package version 4.2.3. https://CRAN.R-project.org/package=openxlsx"))),
          tags$li(tags$span(HTML("<span style='color:blue'>stringr</span>"), p("Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0. https://CRAN.R-project.org/package=stringr"))),
          tags$li(tags$span(HTML("<span style='color:blue'>cld3</span>"), p("Jeroen Ooms (2019). cld3: Google's Compact Language Detector 3. R package version 1.2. https://CRAN.R-project.org/package=cld3"))),
          tags$li(tags$span(HTML("<span style='color:blue'>udpipe</span>"), p("Jan Wijffels (2019). udpipe: Tokenization, Parts of Speech Tagging, Lemmatization and Dependency Parsing with the 'UDPipe' 'NLP' Toolkit. R package version 0.8.3. https://CRAN.R-project.org/package=udpipe"))),
          tags$li(tags$span(HTML("<span style='color:blue'>DT</span>"), p("Yihui Xie, Joe Cheng and Xianying Tan (2019). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.11. https://CRAN.R-project.org/package=DT"))),
          tags$li(tags$span(HTML("<span style='color:blue'>lattice</span>"), p("Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R. Springer, New York. ISBN 978-0-387-75968-5"))),
          tags$li(tags$span(HTML("<span style='color:blue'>igraph</span>"), p("Csardi G, Nepusz T: The igraph software package for complex network research, InterJournal, Complex Systems 1695. 2006. http://igraph.org"))),
          tags$li(tags$span(HTML("<span style='color:blue'>ggraph</span>"), p("Thomas Lin Pedersen (2019). ggraph: An Implementation of Grammar of Graphics for Graphs and Networks. R package version 2.0.0. https://CRAN.R-project.org/package=ggraph"))),
          tags$li(tags$span(HTML("<span style='color:blue'>ggplot2</span>"), p("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016."))),
          tags$li(tags$span(HTML("<span style='color:blue'>textrank</span>"), p("Jan Wijffels (2019). textrank: Summarize Text by Ranking Sentences and Finding Keywords. R package version 0.3.0. https://CRAN.R-project.org/package=textrank"))),
          tags$li(tags$span(HTML("<span style='color:blue'>ggwordcloud</span>"), p("Erwan Le Pennec and Kamil Slowikowski (2019). ggwordcloud: A Word Cloud Geom for 'ggplot2'. R package version 0.5.0. https://CRAN.R-project.org/package=ggwordcloud")))
        )),

        br(),
        p("The icons used in this app are glyphs taken from the set of", a("Bootstrap Glyphicons", href = "https://getbootstrap.com/docs/3.3/components/", target = "_blank"), "which includes over 250 glyphs from the", a("Glyphicon", href = "https://glyphicons.com/", target = "_blank"), "Halflings set."),
        br(),
        br()
      ),

      br()
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-exclamation-sign' style='font-size: 100%'></span>&nbsp;Disclaimer"),
      value = "disclaimer",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 203px); background-color: #eeeefe",

        br(),
        h5(strong("Liability")),
        p("This app is provided 'as is' without warranty of any kind, either express or implied, including, but not limited to, the implied warranties of fitness for a purpose, or the warranty of non-infringement. Without limiting the foregoing, the Fryske Akademy makes no warranty that: 1) the app will meet your requirements, 2) the app will be uninterrupted, timely, secure or error-free, 3) the results that may be obtained from the use of the app will be effective, accurate or reliable, 4) the quality of the app will meet your expectations, 5) any errors in the app will be corrected."),
        br(),
        p("The app and its documentation could include technical or other mistakes, inaccuracies or typographical errors. The Fryske Akademy may make changes to the app or documentation made available on its web site. The app and its documentation may be out of date, and the Fryske Akademy makes no commitment to update such materials."),
        br(),
        p("The Fryske Akademy assumes no responsibility for errors or ommissions in the app or documentation available from its web site."),
        br(),
        p("In no event shall the Fryske Akademy be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, data or profits, whether or not the Fryske Akademy has been advised of the possibility of such damages, and on any theory of liability, arising out of or in connection with the use of this software."),
        br(),
        p("The use of the app is done at your own discretion and risk and with agreement that you will be solely responsible for any damage to your computer system or loss of data that results from such activities. No advice or information, whether oral or written, obtained by you from the Fryske Akademy shall create any warranty for the software."),
        br(),
        h5(strong("Other")),
        p("The disclaimer may be changed from time to time."),
        br()
      ),

      br()
    )
  ),

  tags$footer
  (
    tags$table(style = "width:100%",
               tags$tr
               (
                 tags$td(tags$a(tags$img(src="FA1.png", style = "height: 35px; margin-top: 9px; margin-left : 22px;"),
                                href    = "https://www.fryske-akademy.nl/en/"),
                                style   = "width: 33%; text-align: left;",
                                class   = "balk",
                                onclick = "window.open('https://www.fryske-akademy.nl/en/', '_blank'); return false;",
                                target  = "_blank"),
                 tags$td(tags$a(tags$img(src="RUG.png", style = "height: 35px; margin-top: 9px;"),
                                href    = "https://www.rug.nl/bachelors/information-science/"),
                                style   = "width: 34%; text-align: center;",
                                class   = "balk",
                                onclick = "window.open('https://www.rug.nl/bachelors/information-science/', '_blank'); return false;",
                                target  = "_blank"),
                 tags$td(tags$a(tags$img(src="Clariah.png", style = "height: 35px; margin-top: 9px; margin-right: 22px;"),
                                href    = "https://www.clariah.nl"),
                                style   = "width: 33%; text-align: right;",
                                class   = "balk",
                                onclick = "window.open('https://www.clariah.nl', '_blank'); return false;",
                                target  = "_blank"),
                 tags$td(textOutput("heartbeat"))
               )
    )
  )
)

################################################################################

server <- function(input, output, session)
{
  observeEvent(input$navBar,
  {
    if (getUrlHash() == paste0("#", input$navBar)) return()
    updateQueryString(paste0("#", input$navBar), mode = "push")
  })

  observeEvent(getUrlHash(),
  {
    Hash <- getUrlHash()
    if (Hash == paste0("#", input$navBar)) return()
    Hash <- gsub("#", "", Hash)
    updateNavbarPage(session, "navBar", selected=Hash)
  })

  output$heartbeat <- renderText(
  {
    invalidateLater(5000)
    Sys.time()
  })
  
  ##############################################################################

  global <- reactiveValues(model=NULL, modelLoaded=FALSE)

  output$aceEditor <- renderUI(
  {
    aceEditor(
      outputId            = "ace",
      value               = "",
      placeholder         = "Enter your text here.",
      mode                = "plain_text",
      height              = "200px",
      fontSize            = 17,
      wordWrap            = TRUE,
      showLineNumbers     = FALSE,
      highlightActiveLine = FALSE,
      showPrintMargin     = FALSE
    )
  })

  fileContent <- reactive(
  {
    req(input$fileInput)
    return(readtext(file = input$fileInput$datapath, encoding = "UTF-8")$text)
  })

  webContent <- reactive(
  {
    req(input$webURL)

    site <- NULL

    tryCatch(
      site <- read_html(input$webURL),
      error   = function(something) {},
      warning = function(something) {}
    )

    if (length(site)>0)
    {
      text <- html_text(html_nodes(site, 'p'))
      text <- gsub("\n", "", text)
      text <- gsub("([)[0-9]+(]))", "", text)

      text <- data.frame(text)
      text <- subset(text, str_count(text, "\\w+") > 1)
      text <- subset(text, grepl("[A-Z|a-z]", text))

      if (nrow(text) > 0)
        return(paste(text$text, sep = "", collapse = "\n\n"))
      else
        return(NULL)
    }
    else
      return(NULL)
  })

  output$getInput <- renderUI(
  {
    req(input$selInput)

    if (input$selInput=="Text")
      return(div(uiOutput("aceEditor"), class='format'))
    if (input$selInput=="File")
      return(div(fileInput('fileInput', NULL, accept = c(".txt", ".docx", ".html"), placeholder='Browse or drop your file here (.txt, .docx, .html)', width="100%"), class='format', style = "background-color: #eeeefe"))
    if (input$selInput=="Web")
      return(div(HTML("<input type='text' id='webURL' class='web' placeholder='Enter web address'><div style='height: 40px'></div>"), class='format', style = "background-color: #eeeefe"))
  })

  checkText <- reactive(
  {
    req(input$selInput)

    if (input$selInput=="Text")
      s <- input$ace
    if (input$selInput=="File")
      s <- fileContent()
    if (input$selInput=="Web" )
      s <- webContent()

    if (!is.null(s) && (trimws(s) == ""))
      return(NULL)

    if ((length(s)>0) && (s!=""))
    {
      lang <- detect_language(s)

      if (!is.na(lang))
      {
        if  (lang=="fy")
          updatePickerInput(session, 'selLang', selected=   "frisian_frysk-ud-1.00-250407")

        if ((lang=="nl") || (lang=="af"))
          updatePickerInput(session, 'selLang', selected="dutch-lassysmall-ud-2.12-230717")

        if  (lang=="en")
          updatePickerInput(session, 'selLang', selected=  "english-partut-ud-2.12-230717")
      }
    }

    return(s)
  })

  observeEvent(input$clearButton,
  {
    if (input$selInput=="Text")
      updateAceEditor(session, "ace", value = 1)
    if (input$selInput=="File")
      js$clearUpload()
    if (input$selInput=="Web")
      updateTextInput(session, "webURL", value = "")
  })

  UDPipe_annotate1 <- function(sentence, model)
  {
    if (!global$modelLoaded)
    {
      global$model <- udpipe_load_model(file = paste0("www/", model, ".udpipe"))
      global$modelLoaded <- TRUE
    }

    result <- as.data.frame(udpipe(x = sentence, object=global$model))
    result$term_id <- NULL

    return(result)
  }

  UDPipe_annotate2 <- function(sentence, model)
  {
    query <- paste0("curl --data 'tokenizer=&tagger=&parser=&data=", sentence, "&model=", model, "' http://lindat.mff.cuni.cz/services/udpipe/api/process")

    result <- system(query, intern = TRUE)
    result <- paste(result, collapse = " ")
    result <- str_extract(result, "sent_id[:print:]+")
    result <- unlist(strsplit(result, "\\n", fixed = TRUE))

    df <- data.frame()

    for (i in 3:(length(result)-2))
    {
      w <- unlist(strsplit(result[i], "\\t", fixed = TRUE))

      df0 <- data.frame(
        sentence_id   = result[1],
        sentence      = result[2],
        token_id      = w[ 1],
        token         = w[ 2],
        lemma         = w[ 3],
        upos          = w[ 4],
        xpos          = w[ 5],
        feats         = w[ 6],
        head_token_id = w[ 7],
        dep_rel       = w[ 8],
        deps          = w[ 9],
        misc          = w[10]
      )

      df <- rbind(df, df0)
    }

    df <- subset(df, !is.na(token_id))
    df <- subset(df, !grepl("# sent_id", token_id))
    df <- subset(df, !grepl("# text =" , token_id))

    return(df)
  }

  resultUD <- function()
  {
    req(checkText())

    if (input$selLang=="frisian_frysk-ud-1.00-250407")
      result <- UDPipe_annotate1(checkText(), input$selLang)
    else
      result <- UDPipe_annotate2(checkText(), input$selLang)

    return(result)
  }

  resultUD0 <- function()
  {
    req(resultUD())

    result <- resultUD()

    result$doc_id       <- NULL
    result$paragraph_id <- NULL
    result$sentence_id  <- NULL
    result$sentence     <- NULL
    result$start        <- NULL
    result$end          <- NULL
    result$misc         <- NULL

    return(result)
  }

  resultUD1 <- function()
  {
    req(resultUD())
    return(resultUD())
  }

  resultUD2 <- function()
  {
    req(resultUD())
    return(as_conllu(resultUD()))
  }

  output$resultUD <- DT::renderDataTable(
    resultUD0(),
    rownames = FALSE,
    options = list(scrollX = TRUE, searching = FALSE, pageLength = 500, lengthChange = FALSE,
                   columnDefs = list(list(className = 'dt-left', targets = "_all")))
  )

  output$showResults <- renderUI(
  {
    req(checkText())

    tagList(
      HTML("<p style='font-weight: bold;'>Output Table:</p>"),
      div(DT::dataTableOutput('resultUD'), class='format'),

      br(), br(),

      splitLayout
      (
        cellWidths = c("270px", "150px"),

        pickerInput('selFormat', label = "Format: ", c("Tab-delimited text", "Microsoft Excel", "CoNLL-U"), selected="Microsoft Excel", multiple=FALSE, width="150px", options = pickerOptions(title = "Format", dropupAuto = F, container = 'body')),
        downloadButton("downloadTable", "Download Table")
      )
    )
  })

  fileName <- function()
  {
    if (input$selFormat=="Tab-delimited text")
      ext <- "tsv"

    if (input$selFormat=="Microsoft Excel")
      ext <- "xlsx"

    if (input$selFormat=="CoNLL-U")
      ext <- "conllu"

    return(paste0("UDPipe.", ext))
  }

  output$downloadTable <- downloadHandler(filename = fileName, content = function(file)
  {
    if (input$selFormat=="Tab-delimited text")
      write.table(resultUD1(), file, sep = "\t", na = "", dec = ".", quote = TRUE, qmethod = "double", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

    if (input$selFormat=="Microsoft Excel")
      write.xlsx (resultUD1(), file, sheetName = "table", headerStyle = createStyle(textDecoration = "BOLD"), rowNames=FALSE, colNames=TRUE, na.string = "", firstRow = TRUE)

    if (input$selFormat=="CoNLL-U")
      write_file (resultUD2(), file)
  })

  ##############################################################################

  output$showInputs <- renderUI(
  {
    req(resultUD())

    if (input$selGraph=="Most occurring tokens")
    {
      return(
        fluidPage(
          style = "margin-left: 20px;",
          br(), br(),

          splitLayout
          (
            cellWidths = c("90px", "90px"),

            span("UPOS:\n", pickerInput('selUPOS2', NULL, unique(resultUD()$upos), selected="NOUN" , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("Form:\n", pickerInput('selForm2', NULL, c("token", "lemma")    , selected="token", multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body')))
          )
        )
      )
    }

    if (input$selGraph=="Most occurring tokens wordcloud")
    {
      return(
        fluidPage(
          style = "margin-left: 20px;",
          br(), br(),

          splitLayout
          (
            cellWidths = c("90px", "90px"),

            span("UPOS:\n", pickerInput('selUPOS7', NULL, unique(resultUD()$upos), selected="NOUN" , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("Form:\n", pickerInput('selForm7', NULL, c("token", "lemma")    , selected="token", multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body')))
          )
        )
      )
    }

    if (input$selGraph=="Keywords RAKE")
    {
      return(
        fluidPage(
          style = "margin-left: 20px;",
          br(), br(),

          splitLayout
          (
            cellWidths = c("90px", "90px", "90px"),

            span("UPOS:\n", pickerInput('selUPOS3a', NULL, unique(resultUD()$upos), selected="ADJ"  , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("UPOS:\n", pickerInput('selUPOS3b', NULL, unique(resultUD()$upos), selected="NOUN" , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("Form:\n", pickerInput('selForm3' , NULL, c("token", "lemma")    , selected="token", multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body')))
          )
        )
      )
    }

    if (input$selGraph=="Keywords wordcloud")
    {
      return(
        fluidPage(
          style = "margin-left: 20px;",
          br(), br(),

          splitLayout
          (
            cellWidths = c("90px", "90px"),

            span("UPOS:\n", pickerInput('selUPOS8a', NULL, unique(resultUD()$upos), selected="ADJ"  , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("UPOS:\n", pickerInput('selUPOS8b', NULL, unique(resultUD()$upos), selected="NOUN" , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("Form:\n", pickerInput('selForm8' , NULL, c("token", "lemma")    , selected="token", multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body')))
          )
        )
      )
    }

    if (input$selGraph=="Keywords PMI/MD/LFMD")
    {
      return(
        fluidPage(
          style = "margin-left: 20px;",
          br(), br(),

          splitLayout
          (
            cellWidths = c("90px", "90px"),

            span("n-gram:\n", pickerInput('selNgram4', NULL, c("2", "3", "4", "5")  , selected="2"    , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("Form:\n"  , pickerInput('selForm4' , NULL, c("token", "lemma")    , selected="token", multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("Stats:\n" , pickerInput('selStats4', NULL, c("PMI", "MD", "LFMD") , selected="PMI"  , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body')))
          )
        )
      )
    }

    if (input$selGraph=="Co-occurrences within sentence")
    {
      return(
        fluidPage(
          style = "margin-left: 20px;",
          br(), br(),

          splitLayout
          (
            cellWidths = c("90px", "90px", "90px"),

            span("UPOS:\n" , pickerInput('selUPOS5a', NULL, unique(resultUD()$upos), selected="ADJ"  , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("UPOS:\n" , pickerInput('selUPOS5b', NULL, unique(resultUD()$upos), selected="NOUN" , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("Form:\n" , pickerInput('selForm5' , NULL, c("token", "lemma")    , selected="token", multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body')))
          )
        )
      )
    }

    if (input$selGraph=="Words following one another")
    {
      return(
        fluidPage(
          style = "margin-left: 20px;",
          br(), br(),

          splitLayout
          (
            cellWidths = c("90px", "90px"),

            span("UPOS:\n"    , pickerInput('selUPOS6a', NULL, unique(resultUD()$upos), selected="ADJ"  , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("UPOS:\n"    , pickerInput('selUPOS6b', NULL, unique(resultUD()$upos), selected="NOUN" , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body')))
          ),

          splitLayout
          (
            cellWidths = c("90px", "90px"),

            span("Form:\n"    , pickerInput('selForm6' , NULL, c("token", "lemma")    , selected="token", multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body'))),
            span("Skip gram\n", pickerInput('skipGram6', NULL, 0:10                   , selected=0      , multiple=FALSE, options = pickerOptions(dropupAuto = F, container = 'body')))
          )
        )
      )
    }
  })

  output$graph1 <- renderPlot(res = 120,
  {
    stats <- txt_freq(resultUD()$upos)
    stats$key <- factor(stats$key, levels = rev(stats$key))

    barchart(key ~ freq,
             data   = stats,
             col    = "deepskyblue3",
             border = "transparent",

             panel = function(x, y, ...)
             {
               panel.grid(v = -11, h = -9)
               panel.barchart(x, y, ...)
             },

             main   = "UPOS (Universal Parts of Speech)\n frequency of occurrence",
             xlab   = "Frequency",

             origin = 0,
             xlim   = c(0, (max(stats$freq)+(0.05*max(stats$freq)))))
  })

  output$graph2 <- renderPlot(res = 120,
  {
    req(input$selUPOS2)
    req(input$selForm2)

    stats <- subset(resultUD(), upos %in% c(input$selUPOS2))

    if (input$selForm2=="token")
      stats <- txt_freq(stats$token)
    if (input$selForm2=="lemma")
      stats <- txt_freq(stats$lemma)

    stats$key <- factor(stats$key, levels = rev(stats$key))

    barchart(key ~ freq,
             data   = head(stats, 20),
             col    = "deepskyblue3",
             border = "transparent",

             panel = function(x, y, ...)
             {
               panel.grid(v = -11, h = -9)
               panel.barchart(x, y, ...)
             },

             main   = paste0("Most occurring ", input$selForm2, "s\nUPOS = ", input$selUPOS2),
             xlab   = "Frequency",

             origin = 0,
             xlim   = c(0, (max(stats$freq)+(0.05*max(stats$freq)))))
  })

  output$graph7 <- renderPlot(res = 120,
  {
    req(input$selUPOS7)
    req(input$selForm7)

    stats <- subset(resultUD(), upos %in% c(input$selUPOS7))

    if (input$selForm7=="token")
      stats <- txt_freq(stats$token)
    if (input$selForm7=="lemma")
      stats <- txt_freq(stats$lemma)

    stats <- subset(stats, freq >= 3)

    if (nrow(stats) > 0)
    {
      stats <- stats[1:min(100, nrow(stats)),]
      stats$angle <- sample(c(0, 0, 0, 90), nrow(stats), replace = TRUE)

      ggplot(stats, aes(label = key, size = freq, color = rainbow(n=nrow(stats)), angle = angle)) +
        geom_text_wordcloud(rm_outside = TRUE) +
        scale_size_area(max_size = 20) +
        theme_minimal() +
        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    }
  })

  output$graph3 <- renderPlot(res = 120,
  {
    req(input$selUPOS3a)
    req(input$selUPOS3b)
    req(input$selForm3)

    stats <- keywords_rake(resultUD(),
                           term = input$selForm3,
                           group = c("doc_id", "paragraph_id", "sentence_id"),
                           relevant = resultUD()$upos %in% c(input$selUPOS3a, input$selUPOS3b))

    stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

    if ((nrow(stats)>0) & (sum(stats$rake)>0))
      return(barchart(key ~ rake,
                      data   = head(subset(stats, freq > 3), 20),
                      col    = "deepskyblue3",
                      border = "transparent",

                      panel = function(x, y, ...)
                      {
                        panel.grid(v = -11, h = -9)
                        panel.barchart(x, y, ...)
                      },

                      main   = paste0("Keywords identified by RAKE\n", input$selUPOS3a, " & ", input$selUPOS3b),
                      xlab   = "RAKE",

                      origin = 0,
                      xlim   = c(0, (max(stats$rake)+(0.05*max(stats$rake))))))
    else
      return(NULL)
  })

  output$graph8 <- renderPlot(res = 120,
  {
    req(input$selUPOS8a)
    req(input$selUPOS8b)
    req(input$selForm8)

    x <- resultUD()

    if (input$selForm8=="token")
      stats <- textrank_keywords(x$token, relevant = x$upos %in% c(input$selUPOS8a, input$selUPOS8b), ngram_max = 8, sep = " ")
    if (input$selForm8=="lemma")
      stats <- textrank_keywords(x$lemma, relevant = x$upos %in% c(input$selUPOS8a, input$selUPOS8b), ngram_max = 8, sep = " ")

    stats <- subset(stats$keywords, (ngram > 1) & (freq >= 3))

    if (nrow(stats) > 0)
    {
      stats <- stats[1:min(100, nrow(stats)),]
      stats$angle <- sample(c(0, 90, 0, 90), nrow(stats), replace = TRUE)

      ggplot(stats, aes(label = keyword, size = freq, color = rainbow(n=nrow(stats)), angle = angle)) +
        geom_text_wordcloud(rm_outside = TRUE) +
        scale_size_area(max_size = 10) +
        theme_minimal() +
        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    }
  })

  output$graph4 <- renderPlot(res = 120,
  {
    req(input$selNgram4)
    req(input$selForm4)

    x <- resultUD()

    if (input$selForm4=="token")
      x$word <- tolower(x$token)
    if (input$selForm4=="lemma")
      x$word <- tolower(x$lemma)

    stats <- keywords_collocation(x         = x,
                                  term      = "word",
                                  ngram_max = as.numeric(input$selNgram4),
                                  group     = c("doc_id", "paragraph_id", "sentence_id"))
    stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

    if ((nrow(stats)>0) && (input$selStats4=="PMI"))
      return(barchart(reorder(key, pmi) ~ pmi,
                      data   = head(subset(stats, freq > 3), 20),
                      col    = "deepskyblue3",
                      border = "transparent",

                      panel = function(x, y, ...)
                      {
                        panel.grid(v = -11, h = -9)
                        panel.barchart(x, y, ...)
                      },

                      main   = "Keywords identified by PMI collocation",
                      xlab   = "PMI (Pointwise Mutual Information)",

                      origin = 0,
                      xlim   = c(0, (max(stats$pmi)+(0.05*max(stats$pmi))))))
    else

    if ((nrow(stats)>0) && (input$selStats4=="MD"))
      return(barchart(reorder(key, md) ~ md,
                      data   = head(subset(stats, freq > 3), 20),
                      col    = "deepskyblue3",
                      border = "transparent",

                      panel = function(x, y, ...)
                      {
                        panel.grid(v = -11, h = -9)
                        panel.barchart(x, y, ...)
                      },

                      main   = "Keywords identified by MD collocation",
                      xlab   = "MD (Mutual Dependency)",

                      origin = 0,
                      xlim   = c(0, (min(stats$md)+(0.05*min(stats$md))))))
    else

    if ((nrow(stats)>0) && (input$selStats4=="LFMD"))
      return(barchart(reorder(key, lfmd) ~ lfmd,
                      data   = head(subset(stats, freq > 3), 20),
                      col    = "deepskyblue3",
                      border = "transparent",

                      panel = function(x, y, ...)
                      {
                        panel.grid(v = -11, h = -9)
                        panel.barchart(x, y, ...)
                      },

                      main   = "Keywords identified by LFMD collocation",
                      xlab   = "LFMD (Log-Frequency biased Mutual Dependency)",

                      origin = 0,
                      xlim   = c(0, (min(stats$lfmd)+(0.05*min(stats$lfmd))))))
    else
      return(NULL)
  })

  output$graph5 <- renderPlot(res = 120,
  {
    req(input$selUPOS5a)
    req(input$selUPOS5b)
    req(input$selForm5)

    stats <- cooccurrence(x     = subset(resultUD(),
                          upos %in% c(input$selUPOS5a, input$selUPOS5b)),
                          term  = input$selForm5,
                          group = c("doc_id", "paragraph_id", "sentence_id"))

    if (nrow(stats) > 0)
    {
      wordnetwork <- head(stats, 30)
      names(wordnetwork)[names(wordnetwork) == "cooc"] <- "frequency"

      wordnetwork <- graph_from_data_frame(wordnetwork)

      ggraph(wordnetwork, layout = "fr") +
        geom_edge_link(aes(width = frequency, edge_alpha = frequency), edge_colour = "pink") +
        geom_node_point(colour = "black", size = 2) +
        geom_node_text(aes(label = name), col = "darkgreen", size = 4, repel = TRUE) +
        guides(x.sec = "axis", y.sec = "axis") +
        theme(plot.title         = element_text(size = 14, hjust = 0.5, face="bold"),
              plot.subtitle      = element_text(size = 12, hjust = 0.5),
              panel.background   = element_rect(fill  = "white"),
              axis.line.y.left   = element_line(color = "black"),
              axis.line.y.right  = element_line(color = "black"),
              axis.line.x.top    = element_line(color = "black"),
              axis.line.x.bottom = element_line(color = "black")) +
        labs(title = "Co-occurrences within sentence", subtitle = paste0("UPOS: ", input$selUPOS5a, " & ", input$selUPOS5b))
    }
    else
      return(NULL)
  })

  output$graph6 <- renderPlot(res = 120,
  {
    req(input$selUPOS6a)
    req(input$selUPOS6b)
    req(input$selForm6)
    req(input$skipGram6)

    if (input$selForm6=="token")
      stats <- cooccurrence(resultUD()$token,
                            relevant = resultUD()$upos %in% c(input$selUPOS6a, input$selUPOS6b),
                            skipgram = input$skipGram6)
    if (input$selForm6=="lemma")
      stats <- cooccurrence(resultUD()$lemma,
                            relevant = resultUD()$upos %in% c(input$selUPOS6a, input$selUPOS6b),
                            skipgram = input$skipGram6)

    if (nrow(stats) > 0)
    {
      wordnetwork <- head(stats, 30)
      names(wordnetwork)[names(wordnetwork) == "cooc"] <- "frequency"

      wordnetwork <- graph_from_data_frame(wordnetwork)

      ggraph(wordnetwork, layout = "fr") +
        geom_edge_link(aes(width = frequency, edge_alpha = frequency), edge_colour = "pink") +
        geom_node_point(colour = "black", size = 2) +
        geom_node_text(aes(label = name), col = "darkgreen", size = 4, repel = TRUE) +
        guides(x.sec = "axis", y.sec = "axis") +
        theme(plot.title         = element_text(size = 14, hjust = 0.5, face="bold"),
              plot.subtitle      = element_text(size = 12, hjust = 0.5),
              panel.background   = element_rect(fill  = "white"),
              axis.line.y.left   = element_line(color = "black"),
              axis.line.y.right  = element_line(color = "black"),
              axis.line.x.top    = element_line(color = "black"),
              axis.line.x.bottom = element_line(color = "black")) +
        labs(title = "Words following one another", subtitle = paste0("UPOS: ", input$selUPOS6a, " & ", input$selUPOS6b))
    }
    else
      return(NULL)
  })

  output$showAnalyses <- renderUI(
  {
    req(resultUD())

    if (input$selGraph=="UPOS frequencies")
    {
      return(
        fluidPage(
          style="min-width: calc(100vw - 610px);",
          align="center",
          plotOutput("graph1", width = "600px", height = "600px")
        )
      )
    }

    if (input$selGraph=="Most occurring tokens")
    {
      return(
        fluidPage(
          style="min-width: calc(100vw - 610px);",
          align="center",
          plotOutput("graph2", width = "600px", height = "600px"),
        )
      )
    }

    if (input$selGraph=="Most occurring tokens wordcloud")
    {
      return(
        fluidPage(
          style="min-width: calc(100vw - 610px);",
          align="center",

          div(
            style="width: 600px; height: 600px; background-color: #ffffff;",

            div(
              style = "padding-left: 49px; padding-right: 49px; padding-top: 49px; padding-bottom: 49px;",

              div(
                style = "border: 1px solid silver;",
                plotOutput("graph7", width = "500px", height = "500px")
              )
            )
          )
        )
      )
    }

    if (input$selGraph=="Keywords RAKE")
    {
      return(
        fluidPage(
          style="min-width: calc(100vw - 610px);",
          align="center",
          plotOutput("graph3", width = "600px", height = "600px"),
        )
      )
    }

    if (input$selGraph=="Keywords wordcloud")
    {
      return(
        fluidPage(
          style="min-width: calc(100vw - 610px);",
          align="center",

          div(
            style="width: 600px; height: 600px; background-color: #ffffff;",

            div(
              style = "padding-left: 49px; padding-right: 49px; padding-top: 49px; padding-bottom: 49px;",

              div(
                style = "border: 1px solid silver;",
                plotOutput("graph8", width = "500px", height = "500px")
              )
            )
          )
        )
      )
    }

    if (input$selGraph=="Keywords PMI/MD/LFMD")
    {
      return(
        fluidPage(
          style="min-width: calc(100vw - 610px);",
          align="center",
          plotOutput("graph4", width = "600px", height = "600px"),
        )
      )
    }

    if (input$selGraph=="Co-occurrences within sentence")
    {
      return(
        fluidPage(
          style="min-width: calc(100vw - 610px);",
          align="center",
          plotOutput("graph5", width = "620px", height = "570px"),
        )
      )
    }

    if (input$selGraph=="Words following one another")
    {
      return(
        fluidPage(
          style="min-width: calc(100vw - 610px);",
          align="center",
          plotOutput("graph6", width = "620px", height = "570px"),
        )
      )
    }
  })
}

################################################################################

options(shiny.sanitize.errors = TRUE)
options(shiny.usecairo=FALSE)
options(shiny.maxRequestSize=20*1024^2)

shinyApp(ui = ui, server = server)

################################################################################
