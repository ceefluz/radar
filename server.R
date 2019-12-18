
server <- function(input, output, session) {
  
  
  # DEFINE SETS -------------------------------------------------
  
  # define whether diagnostics have been performed in selected interval
  set_select <- reactive({
    input$confirm # confirm buttons needs to be pressed to initiate this code
    isolate({
      if (input$diagnosticsInput == "bc_timing") {
        antimicrobials %>%
          mutate(check =
                   if_else(bc_timing %in% c(min(input$checkInput):max(input$checkInput)),
                           "Taken", 
                           "Not taken"))
      }
      else {
        antimicrobials %>%
          mutate(check =
                   if_else(uc_timing %in% c(min(input$checkInput):max(input$checkInput)),
                           "Taken", 
                           "Not taken")) 
      }
    })
  })
  
  # base set with sidebar input: adminstration route, first prescription, antimicrobials, max use all, max use single
  # origin, year, specialty, subspecialty (not excluded), age, gender,
  
  # Filtering the dataset based on the selection from the sidebar
  
  set_base <- reactive({
    input$confirm # confirm buttons needs to be pressed to initiate this code
    isolate({
      set_base <- set_select() %>%
        filter(
          ab_route %in% input$adminInput, # route of intake: IV, oral or both
          ab_first %in% c(input$firstInput, TRUE), # select first prescriptions or all
          ab_timing %in% c(min(input$ab_timingInput):max(input$ab_timingInput)), # time interval of treatment start
          ab_type %in% input$abInput, # select antimicrobials
          ab_days %in% if (input$ab_any_singleInput == TRUE) { # treatment duration of ...
            input$ab_singleInput # ... single antimicrobial
          } else {
            c(1:max(input$ab_singleInput)) # if all antimicrobials selected
          } & ab_days_all %in% if (input$ab_any_allInput == TRUE) {
            input$ab_allInput # entire treatment course
          } else {
            c(1:max(input$ab_allInput))
          },
          adm_route %in% input$admissionInput, # origin
          year %in% c(min(input$yearInput):max(input$yearInput)), # years selected
          specialty %in% input$specInput, # specialty groups (surgery, ...)
          !(sub_specialty %in% input$exInput), # exclude single specialties
          age %in% c(min(input$ageInput):max(input$ageInput)), # select age
          gender %in% input$genderInput # select gender
        )
      
      if (!is.null(input$inInput)) {
        # include subspecialty selection only
        filter(set_base, sub_specialty %in% input$inInput)
      } else {
        set_base
      }
      
      # filter minimum number per subspecialty
      
      set_base_n <- set_base %>%
        distinct(id, adm_id, .keep_all = TRUE) %>%
        group_by(sub_specialty) %>%
        summarise(n = n()) %>%
        filter(n >= input$nInput)
      
      set_base %>%
        filter(sub_specialty %in% set_base_n$sub_specialty)
    })
  })
  
  test_results <- reactive({
    input$confirm
    isolate({
      microbiology %>% 
        semi_join(set_base(), by = c("id", "adm_id")) %>%
        left_join(set_base() %>% select(id, adm_id, check)) %>% 
        filter(check == "Taken")
    })
  })
  
  # set_1 for
  set_reac_1 <- reactive({
    input$confirm
    isolate({
      set_base() %>%
        distinct(id, adm_id, .keep_all = TRUE)
    })
  })
  
  set_reac_2 <- reactive({
    input$confirm
    isolate({
      set_base()
    })
  })
  
  
  
  
  # UI - GENERAL --------------------------------------------------------------
  
  
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # show intro tour
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "Alright. Let's go"))
  )
  
  # use action buttons as tab selectors
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Patients", "Antimicrobial consumption", "Diagnostics", "Outcome"),
                      label = "",
                      selected = x
    )
  }
  
  observeEvent(input$patients, {
    update_all("Patients")
  })
  observeEvent(input$antimicrobials, {
    update_all("Antimicrobial consumption")
  })
  observeEvent(input$diagnostics, {
    update_all("Diagnostics")
  })
  observeEvent(input$outcome, {
    update_all("Outcome")
  })
  
  # update confirm button
  
  observeEvent(input$confirm, {
    updateButton(
      session, 
      inputId = "confirm", 
      label = "CONFIRM SELECTION", 
      icon = icon("bar-chart-o"), 
      style = "primary")
  })
  
  # hide the underlying selectInput in sidebar for better design
  observeEvent("", {
    hide("tab")
  })
  
  # update all/none group in sidebar and antimicrobials by group
  observe({
    x <- input$allInput
    if (!is.null(x)) {
      x <- ab_groups$ab_group
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "abGroupInput",
      label = NULL, 
      choices = ab_groups$ab_group,
      selected = x
    )
  })
  
  observe({
    x <- input$abGroupInput
    if (!is.null(x)) {
      update_ab <-
        update_ab %>% filter(ab_group %in% input$abGroupInput)
      x <- update_ab$ab_type
    }
    else {
      x <- character(0)
    }
    updateCheckboxGroupInput(
      session,
      inputId = "abInput",
      label = "ANTIMICROBIALS",
      choices = ab$ab_type,
      selected = x
    )
  })
  
  # gray out slider input when input "any" in antimicrobial selection
  observeEvent(input$ab_anyInput, {
    if (input$ab_anyInput == FALSE) {
      disable("ab_timingInput")
      updateSliderInput(session,
                        inputId = "ab_timingInput",
                        label = NULL,
                        value = c(0, max(antimicrobials$ab_timing, na.rm = TRUE)),
                        min = 0,
                        max = max(antimicrobials$ab_timing, na.rm = TRUE),
                        step = 1
      )
    } else {
      enable("ab_timingInput")
      updateSliderInput(session,
                        inputId = "ab_timingInput",
                        label = NULL,
                        value = c(0, 1),
                        min = 0,
                        max = max(antimicrobials$ab_timing, na.rm = TRUE),
                        step = 1
      )
    }
  })
  
  observeEvent(input$ab_any_allInput, {
    if (input$ab_any_allInput == FALSE) {
      disable("ab_allInput")
      updateSliderInput(session,
                        inputId = "ab_allInput",
                        label = NULL,
                        value = max(antimicrobials$ab_days_all, na.rm = TRUE),
                        min = 0,
                        max = max(antimicrobials$ab_days_all, na.rm = TRUE),
                        step = 1
      )
    } else {
      enable("ab_allInput")
      updateSliderInput(session,
                        inputId = "ab_allInput",
                        label = NULL,
                        value = 2,
                        min = 0,
                        max = max(antimicrobials$ab_days_all, na.rm = TRUE),
                        step = 1
      )
    }
  })
  
  observeEvent(input$ab_any_singleInput, {
    if (input$ab_any_singleInput == FALSE) {
      disable("ab_singleInput")
      updateSliderInput(session, 
                        inputId = "ab_singleInput",
                        label = NULL, 
                        value = max(antimicrobials$ab_days_all, na.rm = TRUE),
                        min = 0, 
                        max = max(antimicrobials$ab_days_all, na.rm = TRUE), 
                        step = 1
      )
    } else {
      enable("ab_singleInput")
      updateSliderInput(session, 
                        inputId = "ab_singleInput",
                        label = NULL, 
                        value = 2,
                        min = 0, 
                        max = max(antimicrobials$ab_days_all, na.rm = TRUE), 
                        step = 1 
      )
    }
  })
  
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent("", {
    show("patients_panel")
    hide("antimicrobials_panel")
    hide("diagnostics_panel")
    hide("outcome_panel")
  }, once = TRUE)
  
  observeEvent(input$patients, {
    show("patients_panel")
    hide("diagnostics_panel")
    hide("antimicrobials_panel")
    hide("outcome_panel")
  })
  observeEvent(input$antimicrobials, {
    show("antimicrobials_panel")
    hide("diagnostics_panel")
    hide("outcome_panel")
    hide("patients_panel")
  })
  observeEvent(input$diagnostics, {
    show("diagnostics_panel")
    hide("antimicrobials_panel")
    hide("outcome_panel")
    hide("patients_panel")
  })
  observeEvent(input$outcome, {
    show("outcome_panel")
    hide("diagnostics_panel")
    hide("antimicrobials_panel")
    hide("patients_panel")
  })
  
  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "patients", style = {
      if (x == "Patients") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "antimicrobials", style = {
      if (x == "Antimicrobial consumption") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "diagnostics", style = {
      if (x == "Diagnostics") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "outcome", style = {
      if (x == "Outcome") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })
  
  # UI - PATIENTS - 1 ----------------------------------------------------------
  
  output$box_pat <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Subspecialties in selection",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            introBox(data.step = 5, data.intro = intro$text[5],
                     dropdown(
                       radioGroupButtons(
                         inputId = "box_pat1",
                         label = NULL, 
                         choices = c("Show all", "Show top 10 only"), 
                         selected = "Show all", 
                         direction = "vertical"
                       ),
                       size = "xs",
                       icon = icon("gear", class = "opt"), 
                       up = TRUE
                     )
            )
          ),
          withSpinner(
            plotlyOutput("plot_pat_select", height = 230),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        )
      )
    )
  })
  
  
  # UI - PATIENTS - 2 -------------------------------------------------------
  
  output$box_pat2 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Subspecialties - table",
          htmlOutput("patients_total"),
          withSpinner(
            DT::dataTableOutput("table_pat_all"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Patient age",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_pat1.1",
                label = "Select group", 
                choiceNames = c("All", "Gender"),
                choiceValues = c("all", "gender"), 
                selected = "all", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_age_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_age_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  output$patients_total <- renderText({
    HTML(
      paste("Total number of admissions:", 
            strong(
              unique(
                paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                  length()
              )
            )
      )
    )
  })
  
  
  
  # UI - PATIENTS - 3 -------------------------------------------------------
  
  output$box_year <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_year",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Number of patients per year",
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_year1",
                label = "Select time period", 
                choiceNames = c("Years", "Quarter", "Months"),
                choiceValues = c("years", "yearquarter_adm", "yearmonth_adm"), 
                selected = "years", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_year_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_year_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  
  
  # UI - AB - 1 ------------------------------------------------------------------
  output$box1 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box1",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Antimicrobials",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box1.0",
                label = "Choose groups", 
                choiceNames = c("Antimicrobial - Groups", "Antimicrobials"), 
                choiceValues = c("ab_group", "ab_type"), 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box1.1",
                label = "Show", 
                choiceNames = c("Prescriptions", "DDD per 100 bed days", "DOT per 100 bed days"),
                choiceValues = c("prescriptions", "DDD_100", "DOT_100"),
                selected = "prescriptions", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_ab", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box1 == 'Antimicrobials'",
            actionBttn(
              inputId = "ab",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; left: 4em; bottom: 0.5em;",
          dropdown(
            downloadButton(outputId = "down_box_1", label = "Download plot"),
            size = "xs",
            icon = icon("download", class = "opt"), 
            up = TRUE
          )
        )
      )
    )
  })
  
  observeEvent((input$ab), {
    showModal(modalDialog(
      renderPlot({
        plot_ab() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  # UI - AB - 2 -------------------------------------------------------------
  
  output$box2 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Defined daily doses (DDD)",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box2.2",
                label = "Select time", 
                choiceNames = c("per year", "per month", "per quarter"),
                choiceValues = c("year", "yearmonth_adm", "yearquarter_adm"), 
                selected = "yearmonth_adm", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box2.3",
                label = "Select grouping", 
                choiceNames = c("None", "Specialty", "Sub-specialty", "Origin"),
                choiceValues = c("none", "specialty", "sub_specialty", "adm_route"),
                selected = "none",
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_ddd_ts", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("ddd_ts", height = 300),
            type = 4,
            color = "#CC0000",
            size = 0.7
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em",
          conditionalPanel(
            "input.box2 == 'Defined daily doses (DDD)'",
            actionBttn(
              inputId = "ddd",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          ) 
        )
      )
    )
  })
  
  observeEvent((input$ddd), {
    showModal(modalDialog(
      renderPlot({
        ddd_ts() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  
  # UI - AB - 3 --------------------------------------------------------------
  
  output$box3 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box3",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Days of therapy (DOT)",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box3.2",
                label = "Select time", 
                choiceNames = c("per year", "per month", "per quarter"),
                choiceValues = c("year", "yearmonth_adm", "yearquarter_adm"), 
                selected = "yearmonth_adm", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box3.3",
                label = "Select grouping", 
                choiceNames = c("None", "Specialty", "Sub-specialty", "Origin"),
                choiceValues = c("NULL", "specialty", "sub_specialty", "adm_route"),
                selected = "NULL",
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_dot_ts", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("dot_ts", height = 300),
            type = 4,
            color = "#CC0000",
            size = 0.7
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em",
          conditionalPanel(
            "input.box3 == 'Days of therapy (DOT)'",
            actionBttn(
              inputId = "days",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$days), {
    showModal(modalDialog(
      renderPlot({
        dot_ts() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  
  # UI - AB - 4 -----------------------------------------------------------
  
  output$box4 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box4",
        width = NULL,
        height = 400,
        tabPanel(
          title = "DDD / DOT table",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box4.0",
                label = "Select group", 
                choiceNames = c("Antimicrobial - Groups", "Antimicrobials", "Year", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("ab_group", "ab_type", "year", "specialty", "sub_specialty", "adm_route"), 
                selected = "ab_type", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box4.1",
                label = "Select group", 
                choices = c("DDD per 100 bed days", "DOT per 100 bed days"), 
                selected = "DDD per 100 bed days", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            DT::dataTableOutput("table_ab"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  
  # UI - DIAGNOSTICS - 1 ------------------------------------------------------------------
  
  output$box5 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box5",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Diagnostics in selected patients",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box5.1",
                label = "Change time", 
                choiceNames = c("Year", "Quarter", "Month"), 
                choiceValues = c("year", "yearquarter_adm", "yearmonth_adm"), 
                selected = "year", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box5.2",
                label = "Change plot", 
                choiceNames = c("Count", "Proportion"), 
                choiceValues = c("dodge", "fill"), 
                selected = "dodge", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_5", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_dia_adm", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Timing of selected diagnostics",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_6", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_dia_timing", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box5 == 'Diagnostics in selected patients'",
            actionBttn(
              inputId = "dia_adm",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box5 == 'Timing of selected diagnostics'",
            actionBttn(
              inputId = "dia_timing",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$dia_adm), {
    showModal(modalDialog(
      renderPlot({
        dia_adm() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  observeEvent((input$dia_timing), {
    showModal(modalDialog(
      renderPlot({
        plot_dia_timing() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  # UI - DIAGNOSTICS - 2 ------------------------------------------------------------------
  
  output$box6 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box6",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Diagnostics in relation",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box6.1",
                label = "Select group", 
                choiceNames = c("Antimicrobial - Groups", "Antimicrobials", "Year", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("ab_group", "ab_type", "year", "specialty", "sub_specialty", "adm_route"), 
                selected = "year", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position:absolute;left:4em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_7", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_dia_perform", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Table - Proportion performed",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box6.3",
                label = "Select group", 
                choiceNames = c("Antimicrobials", "Antimicrobial - Groups", "Year", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("ab_group", "ab_type", "year", "specialty", "sub_specialty", "adm_route"), 
                selected = "year", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            DT::dataTableOutput("dia_table"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position:absolute;right:0.5em;bottom: 0.5em;",
          conditionalPanel(
            "input.box6 == 'Diagnostics in relation'",
            actionBttn(
              inputId = "dia_perform",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$dia_perform), {
    showModal(modalDialog(
      renderPlot({ 
        plot_dia_perform() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  # UI - DIAGNOSTICS - 3 ------------------------------------------------------------------
  
  output$box7 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box7",
        width = NULL,
        height = 400,
        tabPanel(
          title = "First isolates in selected diagnostics", 
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box7.0",
                label = "Select group", 
                choiceNames = c("All", "Antimicrobial - Groups", "Antimicrobials", "Year", "Gender", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("fullname", "ab_group", "ab_type", "year", "gender", "specialty", "sub_specialty", "adm_route"), 
                selected = "fullname", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown(
              sliderInput(
                inputId = "box7.1",
                label = "Show top ...", 
                min = 0, 
                max = 50, 
                value = c(25), 
                step = 5
              ),
              size = "xs",
              icon = icon("search-plus", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 7.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_micro", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("micro_plot", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "First isolates - table",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box7.2",
                label = "Select group", 
                choiceNames = c("All", "Year", "Gender", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("fullname", "year", "gender", "specialty", "sub_specialty", "adm_route"), 
                selected = "fullname", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            DT::dataTableOutput("micro_table", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position:absolute;right:0.5em;bottom: 0.5em;",
          conditionalPanel(
            "input.box7 == 'First isolates in selected diagnostics'",
            actionBttn(
              inputId = "micro_plus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$micro_plus), {
    showModal(modalDialog(
      renderPlot({
        micro_plot() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  # UI - DIAGNOSTICS - 4 ------------------------------------------------------------------
  
  output$box8 <- renderUI({
    div(
      style = "position: relative",
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        dropdown(
          selectizeInput(
            inputId = "box8.1",
            label = "Select isolates",
            choices = sort(unique(microbiology$fullname)[!is.na(unique(microbiology$fullname))]),
            multiple = TRUE
          ),
          size = "xs",
          label = "Isolates",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 9.5em; bottom: 0.5em;",
        dropdown(
          checkboxGroupButtons(
            width = "300px", 
            inputId = "box8.2",
            label = "Select antimicrobials",
            size = "xs", checkIcon = list("yes" = icon("check")),
            individual = TRUE,
            choiceValues = 
              sort(colnames(microbiology %>% select_if(is.rsi))),
            choiceNames = paste(
              ab_name(
                sort(colnames(microbiology %>% select_if(is.rsi)))),
              sep = ", "
            )
          ),
          size = "xs",
          label = "Antimicrobials",
          up = TRUE
        )
      ),
      tabBox(
        id = "box8",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Resistance",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box8.0",
                label = "Select group",
                choiceNames = c("All", "Year", "Gender", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("fullname", "year", "gender", "specialty", "sub_specialty", "adm_route"),
                selected = "fullname",
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box8.0.1",
                label = "Select calculation",
                choices = c("Count", "Proportion"),
                selected = "Count",
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"),
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 17.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_res", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("isolate_plot", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Resistance - over time",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box8.3",
                label = "Select time", 
                choiceNames = c("per year", "per month", "per quarter"),
                choiceValues = c("year", "yearmonth_test", "yearquarter_test"), 
                selected = "yearquarter_test", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 17.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_res_ts", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("isolate_ts", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Resistance - table",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box8.7",
                label = "Select group", 
                choiceNames = c("None", "Year", "Month", "Quarter", "Gender", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("fullname", "year", "yearmonth_test", "yearquarter_test", "gender", "specialty", "sub_specialty", "adm_route"), 
                selected = "fullname", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box8.8",
                label = "Select calculation", 
                choices = c("Count", "Proportion"),
                selected = "Count", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            DT::dataTableOutput("isolate_table", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position:absolute;right:0.5em;bottom: 0.5em;",
          conditionalPanel(
            "input.box8 == 'Resistance'",
            actionBttn(
              inputId = "res_plus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position:absolute;right:0.5em;bottom: 0.5em;",
          conditionalPanel(
            "input.box8 == 'Resistance - over time'",
            actionBttn(
              inputId = "res_ts",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$res_plus), {
    showModal(modalDialog(
      renderPlot({
        isolate_plot() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  observeEvent((input$res_ts), {
    showModal(modalDialog(
      renderPlot({
        isolate_ts() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  # UI - OUTCOME - 1 -----------------------------------------------------
  
  output$box_los1 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_los1",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Length of stay",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_los1.0",
                label = "Select group", 
                choiceNames = c("All", "Gender", "Year", "Antimicrobial - Groups", "Antimicrobials", "Diagnostics", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("1", "gender", "year", "ab_group", "ab_type", "check", "specialty", "sub_specialty", "adm_route"), 
                selected = "1", 
                direction = "vertical",
                size = "sm"
              ),
              prettySwitch(
                inputId = "box_los1.2",
                label = "Show histogram", 
                value = FALSE,
                slim = TRUE
              ),
              prettySwitch(
                inputId = "box_los1.1", 
                label = "Show legend", 
                value = FALSE, 
                slim = TRUE
              ),
              prettySwitch(
                inputId = "box_los1.3",
                label = "SPREAD OUT TO REMOVE OVERLAPS", 
                value = FALSE,
                slim = TRUE
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown(
              sliderInput(
                inputId = "zoom",
                label = "Zoom (days)",
                min = 0,
                max = max(as.numeric(set_reac_1()$LOS), na.rm = TRUE),
                value = c(0, 30),
                step = 10
              ),
              size = "xs",
              icon = icon("search-plus", class = "opt"),
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 7.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_los1", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_los", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box_los1 == 'Length of stay'",
            actionBttn(
              inputId = "los",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$los), {
    showModal(modalDialog(
      renderPlot({
        plot_los() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  # UI - OUTCOME - 2 ---------------------------------------------------
  
  output$box_los2 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_los2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Length of stay - Kaplan-Meier",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_los2", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("kaplan_los", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          ),
          div(
            style = "position: absolute; left: 4.5em; bottom: 0.5em;",
            htmlOutput("text_box_los2")
          ),
          div(
            style = "position: absolute; right: 0.5em; bottom: 0.5em;",
            conditionalPanel(
              "input.box_los2 == 'Length of stay - Kaplan-Meier'",
              actionBttn(
                inputId = "los_km",
                icon = icon("search-plus", class = "opt"),
                style = "fill",
                color = "danger",
                size = "xs"
              )
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$los_km), {
    showModal(modalDialog(
      renderPlot({
        validate(
          need(!is.null(kaplan_los()), 'Please select at least two groups in box (left)'),
          need(length(unique(kaplan_los()$plot$data$strata)) <= 5, 'Max. number of groups = 5; please adjust selection')
        )
        kaplan_los()
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  output$text_box_los2 <- renderText({
    
    text <- function(x){
      if(input$box_los1.0 == "1"){y <- "All"}
      if(input$box_los1.0 == "gender"){y <- "Gender"}
      if(input$box_los1.0 == "year"){y <- "Year"}
      if(input$box_los1.0 == "specialty"){y <- "Specialty"}
      if(input$box_los1.0 == "sub_specialty"){y <- "Subspecialty"}
      if(input$box_los1.0 == "ab_group"){y <- "Antimicrobial - Groups"}
      if(input$box_los1.0 == "ab_type"){y <- "Antimicrobials"}
      if(input$box_los1.0 == "check"){y <- "Diagnostics"}
      if(input$box_los1.0 == "adm_route"){y <- "Origin"}
      y
    }
    
    paste0("Groups: ","<b>",text(input$box_los1.0), "<br>", "</b>", "(Select groups and legend in box to the left.)")
  })
  
  
  # UI - OUTCOME - 3 ---------------------------------------------------
  
  output$box_los3 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_los3",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Length of stay - table",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_los3",
                label = "Select group", 
                choiceNames = c("Gender", "Year", "Antimicrobial - Groups", "Antimicrobials", "Diagnostics", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("gender", "year", "ab_group", "ab_type", "check", "specialty", "sub_specialty", "adm_route"), 
                selected = "gender", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            DT::dataTableOutput("table_los", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  
  # BOX PATIENTS - 1 ------------------------------------------------------------
  
  plot_pat_select <- reactive({
    
    pat_select <- set_reac_1() %>%
      group_by(sub_specialty) %>%
      summarise(n = n()) %>% 
      arrange(desc(n))
    
    if (input$box_pat1 == "Show top 10 only") {
      pat_select <- pat_select[1:10,]
    } else{
      pat_select
    }
    
    plot <- 
      ggplot(pat_select, 
             aes(n, 
                 y = 0, 
                 group = sub_specialty, 
                 text = sub_specialty, 
                 count = n)) +
      geom_point(aes(size = n, fill = sub_specialty), 
                 alpha = 0.6, 
                 color = "black", 
                 shape = 21) +
      coord_cartesian(ylim = c(-2, 2)) +
      scale_size_area(max_size = 25) +
      guides(fill = FALSE, size = FALSE) +
      labs(x = "Number of patients (use mouse to identify subspecialties)") +
      scale_x_continuous(
        name = "Number of patients (use mouse to identify subspecialties)", 
        trans = "log10", 
        breaks = c(1, 10, 100, 1000, 10000)) +
      scale_fill_viridis_d() + 
      theme(
        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 10, hjust = 0),
        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1)
      )
    
    
    style(
      hide_legend(
        ggplotly(tooltip = c("text", "Count"))), 
      hoverlabel = list(bgcolor = "white")
    )
  })
  
  output$plot_pat_select <- renderPlotly({
    plot_pat_select()
  })
  
  
  # BOX PATIENTS - 2 ----------------------------------------------------------
  
  table_pat_all <- reactive(
    DT::datatable(
      set_reac_1() %>% 
        count(sub_specialty) %>% 
        arrange(desc(n)) %>% 
        rename("Subspecialty" = sub_specialty),
      rownames = FALSE,
      options = list(
        dom = 'frtp',
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )
  )
  
  output$table_pat_all <- DT::renderDataTable({
    table_pat_all()
  })
  
  plot_age_select <- reactive({
    if (input$box_pat1.1 == "gender") {
      ggplot(set_reac_1(), aes(age, fill = gender)) +
        geom_area(stat = "count", 
                  position = "identity", 
                  alpha = 0.6) +
        geom_line(stat = "count", 
                  color = "black") +
        scale_x_continuous(breaks = seq(0, 100, 10)) +
        labs(x = "Age", y = "Count") +
        ggtitle("Patient age at admission") +
        scale_fill_manual(values = c("#d1351b", "#f39c12"), 
                          name = "Gender", 
                          breaks = unique(set_reac_1()$gender), 
                          label = unique(set_reac_1()$gender)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12))
      
    }
    else {
      ggplot(set_reac_1(), aes(age)) +
        geom_area(stat = "count", 
                  position = "identity", 
                  fill = "#d1351b", 
                  alpha = 0.6) +
        geom_line(stat = "count", 
                  color = "black") +
        scale_x_continuous(breaks = seq(0, 100, 10)) +
        labs(x = "Age", y = "Count") +
        ggtitle("Patient age at admission") +
        guides(fill = FALSE) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12))
    }
  })
  
  output$plot_age_select <- renderPlot({
    plot_age_select()
  })
  
  
  # BOX PATIENTS - 3 ----------------------------------------------------------
  
  plot_year_select <- reactive({
    
    years <- set_reac_1() %>% 
      count(year)
    
    months <- set_reac_1() %>% 
      count(yearmonth_adm)
    
    quarter <- set_reac_1() %>% 
      count(yearquarter_adm)
    
    if (input$box_year1 == "years") {
      plot <- qic(year, n,
                  data = years, 
                  agg.fun = "sum",
                  decimals = 2,
                  xlab = "Year",
                  ylab = "Count",
                  title = "Admissions per year") +
        scale_x_continuous(breaks = 
                             c(min(set_reac_2()$year, na.rm = TRUE):max(set_reac_2()$year, na.rm = TRUE)))
      
    } else {
      if (input$box_year1 == "yearquarter_adm") {
        plot <- qic(yearquarter_adm, n,
                    data = quarter,
                    agg.fun = "sum",
                    decimals = 2,
                    xlab = "Quarter",
                    ylab = "Count",
                    title = "Admissions per quarter") +
          scale_x_yearqtr(n = length(quarter$yearquarter_adm)/4) 
        
      } else {
        plot <- qic(yearmonth_adm, n,
                    data = months,
                    agg.fun = "sum",
                    decimals = 2,
                    xlab = "Month",
                    ylab = "Count",
                    title = "Admissions per month") +
          scale_x_yearmon(n = length(months$yearmonth_adm)/12)
      }
    }
    
    plot +
      labs(caption = "(Line represents median; if red-dotted = signal for non-random variation)") +
      theme(plot.caption = element_text(size = 10, colour = "darkgrey"), 
            plot.title = element_text(face = "bold", size = 12))
    
  })
  
  output$plot_year_select <- renderPlot({
    plot_year_select()
  })
  
  
  # BOX AB - 1 -------------------------------------------------------------------
  
  plot_ab <- reactive({
    
    data <- set_reac_2() %>% 
      group_by_("id", "adm_id", input$box1.0) %>% 
      summarise(
        DDD_sum = sum(ddd_per_prescription, na.rm = TRUE),
        DOT_sum = sum(ab_days, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      left_join(
        set_reac_2() %>% 
          select(id, adm_id, LOS) %>% 
          distinct()
      ) %>% 
      mutate(DDD_per_day_100 = DDD_sum/LOS/100,
             DOT_per_day_100 = DOT_sum/LOS/100) %>% 
      group_by_(input$box1.0) %>% 
      summarise(DDD_100 = sum(DDD_per_day_100),
                DOT_100 = sum(DOT_per_day_100))
    
    prescriptions <- 
      ggplot(data = set_reac_2() %>% count_(input$box1.0), 
             aes_string(
               x = paste0("reorder(", input$box1.0, ", n)"),
               y = "n",
               fill = "n")) +
      geom_bar(stat = "identity",
               color = "black",
               alpha = 0.8) +
      scale_fill_continuous(high = "#8c8c8c", low = "#cccccc") +
      labs(x = NULL, y = "No. of prescriptions") +
      guides(fill = "none") +
      theme_minimal() +
      coord_flip() +
      ggtitle("Total number of prescriptions") +
      theme(plot.title = element_text(face = "bold", size = 12))
    
    
    ddd_dot <- 
      ggplot(data, aes_string(
        x = paste0("reorder(", input$box1.0, ",", input$box1.1, ")"),
        y = input$box1.1,
        fill = input$box1.1)) +
      geom_bar(stat = "identity",
               color = "black",
               alpha = 0.8) +
      scale_fill_continuous(high = "#8c8c8c", low = "#cccccc") +
      labs(x = NULL, y = if(input$box1.1 == "DDD_100") {"DDD per 100 bed days"} else {"DOT per 100 bed days"}) +
      guides(fill = "none") +
      theme_minimal() +
      coord_flip() + 
      ggtitle(if(input$box1.1 == "DDD_100") {"Total amount of DDD per 100 bed days"} else {"Total amount of DOT per 100 bed days"}) +
      theme(plot.title = element_text(face = "bold", size = 12))
    
    
    if (input$box1.1 == "prescriptions") {
      prescriptions
    } else {
      ddd_dot
    }
  })
  
  
  output$plot_ab <- renderPlot({
    plot_ab()
  })
  
  
  
  # BOX AB - 2 --------------------------------------------------------------
  
  # DDD & DOT calculation
  
  ddd_dot <- reactive({
    
    set_reac_2() %>% 
      group_by(id, adm_id, year, 
               yearmonth_adm, yearquarter_adm, specialty, sub_specialty, adm_route) %>% 
      summarise(
        DDD_sum = sum(ddd_per_prescription, na.rm = TRUE),
        DOT_sum = sum(ab_days, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      left_join(
        set_reac_2() %>% select(id, adm_id, LOS)
      ) %>% 
      distinct() %>% 
      mutate(DDD_per_day_100 = DDD_sum/LOS/100,
             DOT_per_day_100 = DOT_sum/LOS/100)
    
  })
  
  # Defined daily doses
  
  ddd_ts <- reactive({
    
    plot_year <- qic(x = year, y = DDD_per_day_100, 
                     data = ddd_dot(),
                     agg.fun = "sum",
                     decimals = 2,
                     xlab = "Year",
                     ylab = "DDD per 100 bed days",
                     title = "Defined daily doses (DDD) / 100 bed days per year",
                     facets = 
                       if (input$box2.3 == "specialty") {
                         ~ specialty
                       } else { 
                         if (input$box2.3 == "sub_specialty") {
                           ~ sub_specialty
                         } else {
                           if (input$box2.3 == "adm_route") {
                             ~ adm_route
                           } else {
                             NULL
                           }
                         }
                       }
    ) + scale_x_continuous(breaks = 
                             c(min(set_reac_2()$year, na.rm = TRUE):max(set_reac_2()$year, na.rm = TRUE)))
    
    plot_month <- qic(x = yearmonth_adm, y = DDD_per_day_100,
                      data = ddd_dot(), 
                      agg.fun = "sum",
                      decimals = 2,
                      xlab = "Months",
                      ylab = "DDD per 100 bed days",
                      title = "Defined daily doses (DDD) / 100 bed days per month",
                      facets =
                        if (input$box2.3 == "specialty") {
                          ~ specialty
                        } else {
                          if (input$box2.3 == "sub_specialty") {
                            ~ sub_specialty
                          } else {
                            if (input$box2.3 == "adm_route") {
                              ~ adm_route
                            } else {
                              NULL
                            }
                          }
                        }
    ) + scale_x_yearmon(n = length(unique(set_reac_2()$yearmonth_adm))/4)
    
    plot_quarter <- qic(x = yearquarter_adm, y = DDD_per_day_100,
                        data = ddd_dot(),
                        agg.fun = "sum",
                        decimals = 2,
                        xlab = "Quarter",
                        ylab = "DDD per 100 bed days",
                        title = "Defined daily doses (DDD) / 100 bed days per quarter",
                        facets =
                          if (input$box2.3 == "specialty") {
                            ~ specialty
                          } else {
                            if (input$box2.3 == "sub_specialty") {
                              ~ sub_specialty
                            } else {
                              if (input$box2.3 == "adm_route") {
                                ~ adm_route
                              } else {
                                NULL
                              }
                            }
                          }
    ) + scale_x_yearqtr(n = length(unique(set_reac_2()$yearquarter_adm))/4)
    
    if (input$box2.2 == "yearmonth_adm") {
      plot <- plot_month
    } else { 
      if (input$box2.2 == "yearquarter_adm") {
        plot <- plot_quarter
      } else {
        plot <- plot_year
      }
    }
    
    plot + 
      labs(caption = "(Line represents median; if red-dotted = signal for non-random variation)") +
      theme(plot.caption = element_text(size = 10, colour = "darkgrey"), 
            plot.title = element_text(face = "bold", size = 12),
            axis.text.x = element_text(angle = 90))
    
  })
  
  output$ddd_ts <- renderPlot({
    ddd_ts()
  })
  
  
  # BOX AB - 3 --------------------------------------------------------------
  
  # Days of therapy
  
  dot_ts <- reactive({
    
    plot_year <- qic(x = year, y = DOT_per_day_100,
                     data = ddd_dot(),
                     agg.fun = "sum",
                     decimals = 2,
                     xlab = "Year",
                     ylab = "DOT per 100 bed days",
                     title = "Days of therapy (DOT) / 100 bed days per year",
                     facets = 
                       if (input$box3.3 == "specialty") {
                         ~ specialty
                       } else { 
                         if (input$box3.3 == "sub_specialty") {
                           ~ sub_specialty
                         } else {
                           if (input$box3.3 == "adm_route") {
                             ~ adm_route
                           } else {
                             NULL
                           }
                         }
                       }
    ) + scale_x_continuous(breaks = 
                             c(min(set_reac_2()$year, na.rm = TRUE):max(set_reac_2()$year, na.rm = TRUE)))
    
    plot_month <- qic(x = yearmonth_adm, y = DOT_per_day_100,
                      data = ddd_dot(),
                      agg.fun = "sum",
                      decimals = 2,
                      xlab = "Months",
                      ylab = "DOT per 100 bed days",
                      title = "Days of therapie (DOT) / 100 bed days per month",
                      facets = 
                        if (input$box3.3 == "specialty") {
                          ~ specialty
                        } else { 
                          if (input$box3.3 == "sub_specialty") {
                            ~ sub_specialty
                          } else {
                            if (input$box3.3 == "adm_route") {
                              ~ adm_route
                            } else {
                              NULL
                            }
                          }
                        }
    ) + scale_x_yearmon(n = length(unique(set_reac_2()$yearmonth_adm))/4)
    
    plot_quarter <- qic(x = yearquarter_adm, y = DOT_per_day_100,
                        data = ddd_dot(),
                        agg.fun = "sum",
                        decimals = 2,
                        xlab = "Quarter",
                        ylab = "DOT per 100 bed days",
                        title = "Days of therapy (DOT) / 100 bed days per quarter",
                        facets = 
                          if (input$box3.3 == "specialty") {
                            ~ specialty
                          } else { 
                            if (input$box3.3 == "sub_specialty") {
                              ~ sub_specialty
                            } else {
                              if (input$box3.3 == "adm_route") {
                                ~ adm_route
                              } else {
                                NULL
                              }
                            }
                          }
    ) + scale_x_yearqtr(n = length(unique(set_reac_2()$yearquarter_adm))/4)
    
    if (input$box3.2 == "yearmonth_adm") {
      plot <- plot_month
    } else { 
      if (input$box3.2 == "yearquarter_adm") {
        plot <- plot_quarter
      } else {
        plot <- plot_year
      }
    }
    
    plot + 
      labs(caption = "(Line represents median; if red-dotted = signal for non-random variation)") +
      theme(plot.caption = element_text(size = 10, colour = "darkgrey"), 
            plot.title = element_text(face = "bold", size = 12),
            axis.text.x = element_text(angle = 90))
    
  })
  
  output$dot_ts <- renderPlot({
    dot_ts()
  })
  
  
  
  
  # BOX AB - 4 --------------------------------------------------------------
  
  output$table_ab <- DT::renderDataTable({
    
    table <- set_reac_2() %>% 
      group_by_("id", "adm_id", input$box4.0) %>% 
      summarise(
        DDD_sum = sum(ddd_per_prescription, na.rm = TRUE),
        DOT_sum = sum(ab_days, na.rm = TRUE)
      ) %>% 
      left_join(
        set_reac_2() %>% select(id, adm_id, LOS)
      ) %>% 
      distinct() %>% 
      ungroup() %>% 
      mutate(DDD_per_day = DDD_sum/LOS,
             DOT_per_day = DOT_sum/LOS) %>% 
      group_by_(input$box4.0) %>% 
      summarise(
        "DDD per 100 bed days" = round(sum(DDD_per_day)/100, 2),
        "DOT per 100 bed days" = round(sum(DOT_per_day)/100, 2)
      ) %>% 
      rename("Selected variables" = input$box4.0)
    
    
    datatable(
      if (input$box4.1 == "DDD per 100 bed days") {
        table[,c(1,2)]
      }
      else {
        table[,c(1,3)]
      },
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtp',
        buttons = c('csv', 'excel', 'pdf'),
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )
  }, server = FALSE)
  
  
  # BOX DIAGNOSTICS - 1  ------------------------------------------------------------------
  
  dia_adm <- reactive({
    input$confirm # confirm buttons needs to be pressed to initiate this code
    isolate({
      ts <- set_reac_1() %>% 
        group_by(id, adm_id) %>% 
        distinct(check, .keep_all = TRUE) %>% 
        ungroup() %>% 
        group_by_(input$box5.1, "check") %>% 
        summarise(n = n())
      
      plot <-
        ggplot(ts, aes_string(input$box5.1, "n", fill = "check")) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12)) +
        ggtitle(
          paste0("Diagnostic performed within ",
                 min(input$checkInput),
                 " and ",
                 max(input$checkInput),
                 " days \nfrom start of antimicrobials")) +
        labs(x = if (input$box5.1 == "year") {
          paste("Per year")
        } else {
          if (input$box5.1 == "yearquarter_adm") {
            paste("Per quarter")
          } else {
            paste("Per month")
          }
        }, y = if (input$box5.2 == "dodge") {
          paste("Count")
        } else {
          paste("Proportion")
        }) +
        if (input$box5.1 == "yearquarter_adm") { 
          scale_x_yearqtr(n = 12)
        } else {
          if (input$box5.1 == "yearmonth_adm") {
            scale_x_yearmon(n = 12)
          }
        }
      
      plot <- plot + 
        if (input$diagnosticsInput == "bc_timing") {
          scale_fill_manual(
            values = c("#a6a6a6", "#d1351b"),
            name = "Blood cultures"
          )
        }
      else {
        scale_fill_manual(
          values = c("#a6a6a6", "#f39c12"),
          name = "Urine cultures"
        )
      }
      
      plot + 
        if(input$box5.1 == "year") {
          geom_col(position = input$box5.2,
                   color = "black",
                   alpha = 0.8)
        } else {
          geom_area(position = input$box5.2,
                    color = "black",
                    alpha = 0.8)
        }
    })
  })
  
  output$plot_dia_adm <- renderPlot({
    dia_adm()
  })
  
  # timing plot
  
  plot_dia_timing <- reactive({
    input$confirm # confirm buttons needs to be pressed to initiate this code
    isolate({
      test_timing <- set_reac_1() %>%
        count(uc_timing, check) 
      
      ggplot(test_timing, aes(x = uc_timing, fill = check)) +
        geom_bar(color = "black", 
                 alpha = 0.8) +
        scale_fill_manual(
          breaks = c("Not taken", "Taken"),
          values =
            if (input$diagnosticsInput == "bc_timing") {
              c("#a6a6a6", "#d1351b")
            } else {
              c("#a6a6a6", "#f39c12")
            },
          labels = c("Not selected", "Selected"),
          name = " ") +
        labs(x = "Days", y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12)) +
        ggtitle("Timing in relation to start of antimicrobials")
    })
  })
  
  output$plot_dia_timing <- renderPlot({
    plot_dia_timing()
    
  })
  
  
  # BOX DIAGNOSTICS - 2 --------------------------------------------------------
  
  plot_dia_perform <- reactive({
    input$confirm # confirm buttons needs to be pressed to initiate this code
    isolate({
    perform_all <- set_reac_1() %>%
      mutate(dia_perform_all = round(sum(check == "Taken") / n() * 100, 1)) %>%
      dplyr::select(input$box6.1, dia_perform_all)
    
    perform_group <- set_reac_1() %>%
      group_by_(input$box6.1) %>%
      summarise(dia_perform = round(sum(check == "Taken") / n() * 100, 1)) %>%
      left_join(perform_all) %>%
      mutate(dia_perform_diff = dia_perform - dia_perform_all) %>%
      arrange(-dia_perform_diff) %>%
      distinct(.keep_all = TRUE)
    
    ggplot(perform_group, 
           aes_string(
             x = paste0("reorder(", input$box6.1, ", dia_perform_diff)"), 
             y = "dia_perform_diff", 
             fill = "dia_perform_diff", 
             text = paste0(input$box6.1))) +
      geom_hline(
        yintercept = 0, 
        linetype = 2, 
        color = "darkgrey") +
      geom_bar(
        stat = "identity", 
        color = "black",
        alpha = 0.8) +
      scale_fill_continuous(high = "#706f6f", low = "#cccccc")+
      labs(
        y = paste0("Absolute diff. from average [", perform_all[1, 2], "%]"), 
        x = " ") +
      guides(fill = FALSE) +
      coord_flip() +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 12)) +
      ggtitle(
        paste0("Diagnostic performed within ",
               min(input$checkInput),
               " and ",
               max(input$checkInput),
               " days \nfrom start of antimicrobials - comparison"))
  })
  })
  
  output$plot_dia_perform <- renderPlot({
    plot_dia_perform()
  })
  
  # performance table
  
  output$dia_table <- DT::renderDataTable({
    datatable(
      set_reac_1() %>%
        group_by_(input$box6.3, "check") %>%
        tally() %>%
        mutate(Proportion = paste0(round(n / sum(n) * 100, 1), "%")),
      rownames = FALSE,
      colnames =
        c("Group",
          if (input$diagnosticsInput == "bc_timing") {
            paste("Blood cultures")
          } else {
            paste("Urine cultures")
          },
          "n",
          "Proportion"),
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtp',
        buttons = c('csv', 'excel', 'pdf'),
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )
    
  })
  
  
  # BOX DIAGNOSTICS - 3  ------------------------------------------------------------------
  
  micro_plot <- reactive({ 
    
    micro_count <- 
      test_results() %>% 
      filter(
        first_isolate == TRUE, 
        material == 
          if (input$diagnosticsInput == "bc_timing") {
            "blood"
          } else {
            "urine"
          }) %>% 
      group_by_("fullname", input$box7.0) %>%
      summarise(n = n()) %>%
      arrange(-n)
    
    micro_count <- micro_count[1:input$box7.1,]
    
    plot <- 
      ggplot(micro_count,
             aes_string(
               x = paste0("reorder(fullname ,n)"), 
               y = "n", 
               fill = "n")) +
      geom_bar(
        stat = "identity", 
        color = "black", 
        alpha = 0.8) +
      scale_fill_continuous(high = "#706f6f", low = "#cccccc")+
      labs(x = NULL, y = "Count") +
      guides(fill = "none") +
      theme_minimal() +
      coord_flip() +
      theme(plot.title = element_text(face = "bold", size = 12)) +
      ggtitle(
        paste0("Frequency of first isolates in ",
               if (input$diagnosticsInput == "bc_timing") {
                 "Blood cultures"
               } else {
                 "Urine cultures"
               }))
    
    if (input$box7.0 != "fullname") {
      plot <- plot + facet_wrap(input$box7.0)
    }
    
    plot
  })
  
  output$micro_plot <- renderPlot({
    micro_plot()
  })
  
  # microorganisms table
  
  micro_table <- reactive({
    micro_table <- test_results() %>% 
      filter(first_isolate == TRUE,
             material == 
               if (input$diagnosticsInput == "bc_timing") {
                 "blood"
               } else {
                 "urine"
               }) %>% 
      group_by_("fullname", input$box7.2) %>%
      summarise(n = n()) %>%
      arrange(-n) %>% 
      rename("Isolates" = "fullname")
    
    
    if (input$box7.2 == "gender") {
      micro_table <- micro_table %>% rename("Gender" = gender)
    }
    if (input$box7.2 == "year") {
      micro_table <- micro_table %>% rename("Year" = year)
    }
    if (input$box7.2 == "specialty") {
      micro_table <- micro_table %>% rename("Specialty" = specialty)
    }
    if (input$box7.2 == "sub_specialty") {
      micro_table <- micro_table %>% rename("Subspecialty" = sub_specialty)
    }
    if (input$box7.2 == "adm_route") {
      micro_table <- micro_table %>% rename("Origin" = adm_route)
    } else {
      micro_table <- micro_table
    }
    
    datatable(
      micro_table,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtp',
        buttons = c('csv', 'excel', 'pdf'),
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )
  })
  
  output$micro_table <- DT::renderDataTable({
    micro_table()
  }, server = FALSE)
  
  # BOX DIAGNOSTICS - 4 -------------------------------------------------------
  
  isolate_data <- reactive({
    test_results() %>% 
      filter(
        fullname == input$box8.1,
        first_isolate == TRUE, 
        material == 
          if (input$diagnosticsInput == "bc_timing") {
            "blood"
          } else {
            "urine"
          }) 
  })
  
  isolate_plot <- reactive({
    
    ts <- isolate_data() %>% 
      dplyr::select(input$box8.0, input$box8.2) %>% 
      group_by_(input$box8.0)
  
    if (length(input$box8.2) == 1) {
      ts <- ts %>% count_df(combine_SI = FALSE)
    } else {
      ts <- ts %>% 
        mutate(value = count_R(.),
               interpretation = "R")
    }
    
    plot <- 
      ggplot(ts,
             aes_string(input$box8.0, "value", fill = "interpretation")) +
      geom_bar(
        stat = "identity",
        position = 
          if (input$box8.0.1 == "Count") {
            position_dodge() 
          } else {
            position_fill()
          },
        color = "black",
        alpha = 0.8
      ) +
      scale_fill_manual(
        limits = c("S", "I", "R"),
        breaks = c("S", "I", "R"),
        values = c("#21908CFF", "#FDE725FF", "#440154FF"),
        na.value = "lightgrey"
      ) +
      ggtitle(
        label =
          paste(
            "Selected isolate: ",
            input$box8.1,
            if (length(input$box8.2) == 1) {
              "\nSelected antimicrobial: "
            } else {
              "\nCo-resistance to: "
            },
            paste(ab_name(c(input$box8.2)), collapse = " & ")
          )
      ) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 5)),
        axis.title.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 12)
      ) +
      if (input$box8.0 == "fullname") {
        theme(axis.text.y = element_blank())
      }
    
    if (input$box8.0.1 == "Proportion") {
      plot +
        scale_y_continuous(
          breaks = seq(0,1,0.1), 
          labels = scales::percent(seq(0, 1 ,0.1))
        ) +
        labs(y = "Percentage") +
        coord_flip()
    } else {
      plot +
        labs(y = "Count") +
        coord_flip() 
    }
    
  })
  
  output$isolate_plot <- renderPlot({
    validate(
      need(input$box8.1, 'Please select isolate below'),
      need(input$box8.2, 'Please select antimicrobial below')
    )
    isolate_plot()
  })
  
  # over time
  
  isolate_ts <- reactive({
    
    col_select <- paste(c(input$box8.2, input$box8.3), sep = ",")
    
    ts <- isolate_data() %>% 
      select(col_select) %>% 
      group_by_(input$box8.3) 
    
    if (length(input$box8.2) == 1) {
      ts <- ts %>% count_df(combine_SI = FALSE)
    } else {
      ts <- ts %>% 
        mutate(value = count_R(.),
               interpretation = "R") 
    }
    
    plot <- qic(
      if (input$box8.3 == "year") {
        year
      } else {
        if (input$box8.3 == "yearquarter_test") {
          yearquarter_test
        } else {
          yearmonth_test
        }
      },
      value,
      data = ts, 
      agg.fun = "sum",
      decimals = 2,
      facets = ~ interpretation,
      ncol = 1
    ) + 
      labs(caption = "(Line represents median; if red-dotted = signal for non-random variation)") +
      theme(strip.text = element_text(face = "bold"),
            plot.title = element_text(face = "bold", size = 12),
            plot.caption = element_text(size = 10, colour = "darkgrey"))
    
    if (input$box8.3 == "yearmonth_test") {
      plot +scale_x_yearmon(n = length(ts$yearmonth_test)/12) +
        labs(x = "Months", y = "Count") +
        ggtitle(
          paste0(input$box8.1, ": ",
                 if (length(input$box8.2) == 1) {
                   ("Susceptibility to ")
                 } else {
                   "Co-resistance to "
                 },
                 paste(ab_name(c(input$box8.2)), collapse = " & "), " - Count per month"
          ))
    } else {
      if (input$box8.3 == "yearquarter_test") {
        plot + scale_x_yearmon(n = length(ts$yearquarter_test)/12) +
          labs(x = "Quarter", y = "Count") +
          ggtitle(
            paste0(input$box8.1, ": ",
                   if (length(input$box8.2) == 1) {
                     ("Susceptibility to ")
                   } else {
                     "Co-resistance to "
                   },
                   paste(ab_name(c(input$box8.2)), collapse = " & "), " - Count per quarter"
            ))
        
      } else {
        plot + scale_x_continuous(breaks = c(min(ts$year, na.rm = TRUE):max(ts$year, na.rm = TRUE))) +
          labs(x = "Year", y = "Count") +
          ggtitle(
            paste0(input$box8.1, ": ",
                   if (length(input$box8.2) == 1) {
                     ("Susceptibility to ")
                   } else {
                     "Co-resistance to "
                   },
                   paste(ab_name(c(input$box8.2)), collapse = " & "), " - Count per year"
            ))
      }
    }
    
  })
  
  output$isolate_ts <- renderPlot({
    validate(
      need(input$box8.1, 'Please select isolate below'),
      need(input$box8.2, 'Please select antimicrobial below')
    )
    isolate_ts()
  })
  
  # isolates table
  
  isolate_table <- reactive({
    
    table <- isolate_data() %>% 
      dplyr::select(input$box8.2, input$box8.7) %>% 
      group_by_(input$box8.7) %>% 
      nest()
    
    if (length(input$box8.2) == 1) {
      table <- table %>% 
        mutate(Value = map(data, 
                           ~ if(input$box8.8 == "Count") {
                             count_df(.)
                           } else {
                             portion_df(., as_percent = TRUE)
                           })) %>% 
        select(-data) %>% 
        unnest() %>% 
        rename(Antimicrobial = Antibiotic)
    } else {
      table <- table %>% 
        mutate(Value = map(data, 
                           ~ if(input$box8.8 == "Count") {
                             count_R(.)
                           } else {
                             portion_R(., as_percent = TRUE)
                           }),
               Interpretation = "R", 
               Antimicrobial = paste0(ab_name(c(input$box8.2)), collapse = " & ")) %>% 
        select(-data) %>% 
        unnest()
    }
    
    table <- table %>% 
      spread(Interpretation, Value) %>% 
      mutate(Isolate = input$box8.1) %>% 
      select(Group = input$box8.7, Isolate, everything()) 
    
    if (input$box8.7 == "yearmonth_test") {
      table <- table %>%
        mutate(Group = as.character(as.yearmon(Group)))
    } 
    if (input$box8.7 == "yearquarter_test") {
      table <- table %>%
        mutate(Group = as.character(as.yearqtr(Group)))
    }
    
    if (input$box8.7 == "fullname") {
      table <- table %>% 
        select(-Isolate)
    } else {
      table 
    }
    
    datatable(
      table,
      extensions = 'Buttons',
      escape = FALSE,
      rownames = FALSE, 
      caption = 
        if (length(input$box8.2) != 1) {
          "Co-resistance"
        } else { 
          NULL 
        },
      fillContainer = TRUE,
      options = list(
        dom = 'Bfrtp',
        buttons = c('csv', 'excel', 'pdf'))
    )
  })
  
  output$isolate_table <- DT::renderDataTable({
    validate(
      need(input$box8.1, 'Please select isolate below'),
      need(input$box8.2, 'Please select isolate below')
    )
    isolate_table()
  }, server = FALSE)
  
  
  # BOX OUTCOME - 1 -------------------------------------------------------------
  
  plot_los <- reactive({
    
    p <- set_reac_1() %>% 
      mutate(year = as.character(year)) # needed for proper visualization
    
    p <-
      ggplot(p) +
      coord_cartesian(xlim = input$zoom) +
      ggtitle("Length of stay - distribution") +
      labs(x = "Days") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        axis.title.y = if (input$box_los1.3 == TRUE) {
          element_blank()
        } else {
          element_text(angle = 90)
        }
      )
    
    if (input$box_los1.3 == TRUE) {
      p <- p + geom_density_ridges(
        aes_string(
          x = "LOS",
          y = input$box_los1.0,
          fill = input$box_los1.0,
          height = "..density.."
        ),
        alpha = 0.6
      )
    } else {
      if (input$box_los1.2) {
        p <- p + geom_histogram(
          aes_string(
            x = "LOS",
            fill = input$box_los1.0
          ),
          alpha = 0.6,
          binwidth = 1,
          position = "dodge",
          color = "black"
        ) +
          labs(y = "Count")
      } else {
        p <- p + geom_density(
          aes_string(
            x = "LOS",
            fill = input$box_los1.0
          ),
          position = "identity",
          alpha = 0.3,
          bw = 1
        ) + labs(y = "Density")
      }
    }
    
    if(input$box_los1.0 == "check"){
      p <-
        p + if (input$diagnosticsInput == "bc_timing") {
          scale_fill_manual(
            values = c("#a6a6a6", "#d1351b"),
            name = "Blood cultures"
          )
        }
      else {
        scale_fill_manual(
          values = c("#a6a6a6", "#f39c12"),
          name = "Urine cultures"
        )
      }
    } else {
      if (input$box_los1.0 == "1") {
        p <- p + scale_fill_continuous(low = "darkblue", high = "darkblue")
      } else {
        p <- p + scale_fill_viridis_d()
      }
    }
    
    if (input$box_los1.1 == FALSE) {
      p <- p + guides(fill = FALSE)
    }
    p
  })
  
  output$plot_los <- renderPlot({
    plot_los()
  })
  
  
  # BOX OUTCOME - 2 -------------------------------------------------------------
  
  kaplan_los <- reactive({
    
    kaplan_set <- set_reac_1() %>% 
      mutate(status = if_else(death_during_adm == FALSE, 1, 0))
    
    if (input$box_los1.0 != 1) {
      ggsurvplot(
        surv_fit(as.formula(
          paste0("Surv(LOS, event = status)", "~", input$box_los1.0)),
          data = kaplan_set),
        data = kaplan_set,
        color = "strata",
        pval = TRUE,
        conf.int = FALSE,
        pval.method = TRUE,
        break.time.by = 5,
        palette = {
          if (input$box_los1.0 == "1") {
            c("lightgrey")
          }
          if (input$box_los1.0 == "check") {
            if (input$diagnosticsInput == "bc_timing") {
              c("#a6a6a6", "#d1351b")
            }
            else {
              c("#a6a6a6", "#f39c12")
            }
          }
          else {
            viridis(n_distinct(set_reac_1() %>% select(input$box_los1.0)))
          }
        },
        ylab = "Probablility to stay in hospital",
        xlab = "Length of stay (days)",
        ggtheme = theme_minimal(),
        legend = "none",
        pval.coord = c(1, 0.15),
        pval.method.coord = c(1, 0.25),
        xlim = input$zoom, # 20 for show purposes
        break.x.by = 5
      )
    } else {
      NULL
    }
  })
  
  output$kaplan_los <- renderPlot({
    validate(
      need(!is.null(kaplan_los()), 'Please select at least two groups in box (left)'),
      need(length(unique(kaplan_los()$plot$data$strata)) <= 5, 'Max. number of groups = 5; please adjust selection')
    )
    kaplan_los()
  })
  
  
  # BOX OUTCOME - 3 -------------------------------------------------------------
  
  table_los <- reactive({
    
    set_table <- set_reac_1() %>%
      group_by_(input$box_los3) %>%
      summarise(
        "Mean LOS" = round(mean(LOS), 1),
        "Median LOS" = round(median(LOS), 1),
        Q1 = round(quantile(LOS, 0.25), 1),
        Q3 = round(quantile(LOS, 0.75), 1),
        n = n()
      ) %>%
      mutate(Proportion = paste((round(
        n / sum(n) * 100, 1
      )), "%"))
    
    if (input$diagnosticsInput == "bc_timing" & input$box_los3 == "check") {
      set_table <- set_table %>% rename("Blood cultures" = check)
    }
    if (input$diagnosticsInput == "uc_timing" & input$box_los3 == "check") {
      set_table <- set_table %>% rename("Urine cultures" = check)
    }
    if (input$box_los3 == "gender") {
      set_table <- set_table %>% rename("Gender" = gender)
    }
    if (input$box_los3 == "year") {
      set_table <- set_table %>% rename("Year" = year)
    }
    if (input$box_los3 == "ab_group") {
      set_table <- set_table %>% rename("Antimicrobial - Groups" = ab_group)
    }
    if (input$box_los3 == "ab_type") {
      set_table <- set_table %>% rename("Antimicrobials" = ab_type)
    }
    if (input$box_los3 == "specialty") {
      set_table <- set_table %>% rename("Specialty" = specialty)
    }
    if (input$box_los3 == "sub_specialty") {
      set_table <- set_table %>% rename("Subspecialty" = sub_specialty)
    }
    if (input$box_los3 == "adm_route") {
      set_table <- set_table %>% rename("Origin" = adm_route)
    }
    
    datatable(
      set_table, 
      rownames = FALSE, 
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtp',
        buttons = c('csv', 'excel', 'pdf'),
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )
  })
  
  output$table_los <- DT::renderDataTable({
    table_los()
  }, server = FALSE)
  
  
  # DOWNLOAD ----------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, "_anti_add_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(set_base(), file)
    }
  )
  
  output$downloadMicroData <- downloadHandler(
    filename = function() {
      paste(input$filename, "_microbiology_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(test_results(), file)
    }
  )
  
  download_box <- function(exportname, plot) {
    downloadHandler(
      filename = function() {
        paste(exportname, Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot, device = "png", width = 8)
      }
    )
  }
  
  output$down_age_select <- download_box("pat_age", plot_age_select())
  output$down_year_select <- download_box("pat_year", plot_year_select())
  output$down_box_1 <- download_box("antimicrobial_use", plot_ab())
  output$down_box_2 <- download_box("DDD_groups", plot_DDD_all())
  output$down_box_3 <- download_box("DOT_groups", plot_DOT_all())
  output$down_box_5 <- download_box("diagnostics_year", dia_adm())
  output$down_box_6 <- download_box("diagnostics_timing", plot_dia_timing())
  output$down_box_7 <- download_box("diagnostics_perform", plot_dia_perform())
  output$down_box_micro <- download_box("first_isolates", micro_plot())
  output$down_box_res <- download_box("resistance", isolate_plot())
  output$down_box_res_ts <- download_box("resistance_time", isolate_ts())
  output$down_box_ddd_ts <- download_box("ddd time", ddd_ts())
  output$down_box_dot_ts <- download_box("dot time", dot_ts())
  output$down_box_los1.0 <- download_box("los_groups", plot_los()) 
  output$down_box_los2 <- download_box("km-curve", kaplan_los()$plot)
  
  
}
