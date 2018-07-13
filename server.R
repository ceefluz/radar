
server <- function(input, output, session) {
  
  
  # DEFINE SETS -------------------------------------------------
  
  # define whether diagnostics have been performed in selected interval
  set_select <- reactive({
    input$confirm
    isolate(
      if (input$diagnosticsInput == "bc_timing") {
        set %>%
          mutate(
            check =
              if_else(
                bc_timing %in% c(min(input$checkInput):max(input$checkInput)),
                "Taken", "Not taken"
              )
          )
      }
      else {
        set %>%
          mutate(
            check =
              if_else(uc_timing %in% c(min(input$checkInput):max(input$checkInput)),
                      "Taken", "Not taken"
              )
          )
      }
    )
  })
  # base set with sidebar input: adminstration route, first prescription, antimicrobials, max use all, max use single
  # Admitting department, year, specialty, subspecialty (not excluded), age, gender,
  
  # Filtering the dataset based on the selection from the sidebar
  
  set_base <- reactive({
    input$confirm # confirm buttons needs to be pressed to initiate this code
    isolate({
      set_base <- set_select() %>%
        filter(
          (ab_route %in% input$adminInput & # route of intake: IV, oral or both
             ab_first %in% c(input$firstInput, TRUE) & # select first prescriptions or all
             ab_timing %in% min(input$ab_timingInput):max(input$ab_timingInput) & # time interval of treatment start
             ab_type %in% input$abInput) & # select antimicrobials
            ab_days %in% if (input$ab_any_singleInput == TRUE) { # treatment duration of ...
              input$ab_singleInput # ... single antimicrobial
            } else {
              c(1:max(input$ab_singleInput)) # if all antimicrobials selected
            } &
            ab_days_all %in% if (input$ab_any_allInput == TRUE) {
              input$ab_allInput # entire treatment course
            } else {
              c(1:max(input$ab_allInput))
            },
          (adm_route %in% input$admissionInput), # admitting department
          (year %in% c(min(input$yearInput):max(input$yearInput))), # years selected
          (specialty %in% input$specInput), # specialty groups (surgery, ...)
          (!(sub_specialty %in% input$exInput)), # exclude single specialties
          (age %in% min(input$ageInput):max(input$ageInput)), # select age
          (gender %in% input$genderInput) # select gender
        )
      
      if (!is.null(input$inInput)) { # include subspecialty selection only
        filter(set_base, sub_specialty %in% input$inInput)
      } else {
        set_base
      }
      
      # filter minimum number per subspecialty
      
      set_base_n <- set_base %>% 
        distinct(id, .keep_all = TRUE) %>% 
        group_by(sub_specialty) %>% 
        summarise(n = n()) %>% 
        filter(n >= input$nInput) 
      
      set_base <- set_base %>%
        filter(sub_specialty %in% set_base_n$sub_specialty)
    })
  })
  
  # set_1 for
  set_reac_1 <- reactive({
    input$confirm
    isolate(
      set_base() %>%
        distinct(id, .keep_all = TRUE)
    )
  })
  
  set_reac_2 <- reactive({
    input$confirm
    isolate(
      set_base()
    )
  })
  
  set_ab <- reactive({
    input$confirm
    isolate(
      set_reac_2() %>%
        group_by(ab_type) %>%
        count(ab_type)
    )
  })
  set_ab_group <- reactive({
    input$confirm
    isolate(
      set_reac_2() %>%
        group_by(ab_group) %>%
        count(ab_group)
    )
  })
  
  
  
  # UI - GENERAL --------------------------------------------------------------
  
  
  # show intro modal
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
  observeEvent(input$ab_any_allInput, {
    if (input$ab_any_allInput == FALSE) {
      disable("ab_allInput")
      updateSliderInput(session,
                        inputId = "ab_allInput",
                        label = NULL,
                        value = max(set$ab_days_all, na.rm = TRUE),
                        min = 0,
                        max = max(set$ab_days_all, na.rm = TRUE),
                        step = 1
      )
    } else {
      enable("ab_allInput")
      updateSliderInput(session,
                        inputId = "ab_allInput",
                        label = NULL,
                        value = 2,
                        min = 0,
                        max = max(set$ab_days_all, na.rm = TRUE),
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
                        value = max(set$ab_days_all, na.rm = TRUE),
                        min = 0, 
                        max = max(set$ab_days_all, na.rm = TRUE), 
                        step = 1
      )
    } else {
      enable("ab_singleInput")
      updateSliderInput(session, 
                        inputId = "ab_singleInput",
                        label = NULL, 
                        value = 2,
                        min = 0, 
                        max = max(set$ab_days_all, na.rm = TRUE), 
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
    hide("box_log_advanced")
    hide("box_cox_advanced")
  }, once = TRUE)
  
  observeEvent(input$patients, {
    show("patients_panel")
    hide("diagnostics_panel")
    hide("antimicrobials_panel")
    hide("outcome_panel")
    hide("box_log_advanced")
    hide("box_cox_advanced")
  })
  observeEvent(input$antimicrobials, {
    show("antimicrobials_panel")
    hide("diagnostics_panel")
    hide("outcome_panel")
    hide("patients_panel")
    hide("box_log_advanced")
    hide("box_cox_advanced")
  })
  observeEvent(input$diagnostics, {
    show("diagnostics_panel")
    hide("antimicrobials_panel")
    hide("outcome_panel")
    hide("patients_panel")
    hide("box_log_advanced")
    hide("box_cox_advanced")
  })
  observeEvent(input$outcome, {
    show("outcome_panel")
    hide("diagnostics_panel")
    hide("antimicrobials_panel")
    hide("patients_panel")
    hide("box_log_advanced")
    hide("box_cox_advanced")
  })
  observeEvent(input$tab2, {
    if (input$tab2 == "Logistic regression analysis" & input$diagnostics == TRUE) {
      show("box_log_advanced")
      hide("antimicrobial_panel")
      hide("diagnostics_panel")
      hide("outcome_panel")
      hide("patients_panel")
      hide("box_cox_advanced")
    }
  })
  observeEvent(input$tab3, {
    if (input$tab3 == "Cox regression analysis" & input$outcome == TRUE) {
      show("box_cox_advanced")
      hide("antimicrobial_panel")
      hide("diagnostics_panel")
      hide("outcome_panel")
      hide("patients_panel")
      hide("box_log_advanced")
    }
  })
  
  observeEvent({
    input$patients
    input$antimicrobials
    input$diagnostics
    input$outcome
  }, {
    updateSelectInput(
      session,
      "tab2",
      choices = c("-", "Logistic regression analysis"),
      label = "CHOOSE OPTIONS",
      selected = "-"
    )
    updateSelectInput(
      session,
      "tab3",
      choices = c("-", "Cox regression analysis"),
      label = "CHOOSE OPTIONS",
      selected = "-"
    )
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
            color = radar_colors["red"], 
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
            color = radar_colors["red"],
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
            color = radar_colors["red"],
            size = 0.7
          )
        )
      )
    )
  })
  
  output$patients_total <- renderText({
    HTML(paste("Total number of patients:", strong(n_distinct(set_reac_1()$id))))
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
                label = "Select group", 
                choiceNames = c("All", "Gender"),
                choiceValues = c("NULL", "gender"), 
                selected = "all", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box_year2",
                label = "Change plot", 
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
            color = radar_colors["red"],
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
                choices = c("Prescriptions", "DDD per 100 bed days", "DOT per 100 bed days"), 
                selected = "Prescriptions", 
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
            color = radar_colors["red"],
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
          axis.title = element_text(size = 26),
          text = element_text(size = 20)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  observeEvent((input$ab_los), {
    showModal(modalDialog(
      renderPlot({
        plot_ab_los() + theme(
          axis.title = element_text(size = 26),
          text = element_text(size = 20)
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
                inputId = "box2.1",
                label = "Select group", 
                choiceNames = c("Year", "Specialty", "Subspecialty", "Admitting department"),
                choiceValues = c("year", "specialty", "sub_specialty", "adm_route"), 
                selected = "year", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box2.0",
                label = "Outliers", 
                choices = c("Top 3", "No outliers"), 
                selected = "No outliers", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box2.2",
                label = "Labels", 
                choices = c("Show labels", "No labels"), 
                selected = "No labels", 
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
              downloadButton(outputId = "down_box_2", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_DDD_all", height = 300),
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
        plot_DDD_all() + theme(
          axis.title = element_text(size = 26),
          text = element_text(size = 20)
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
                inputId = "box3.1.0",
                label = "Select group", 
                choiceNames = c("Year", "Specialty", "Subspecialty", "Admitting department"),
                choiceValues = c("year", "specialty", "sub_specialty", "adm_route"), 
                selected = "year", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box3.1.1",
                label = "Outliers", 
                choices = c("Top 3", "No outliers"), 
                selected = "No outliers", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box3.1.2",
                label = "Labels", 
                choices = c("Show labels", "No labels"), 
                selected = "No labels", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute;left: 4em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_3", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_DOT_all", height = 300),
            type = 4,
            color = radar_colors["red"],
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
        plot_DOT_all() + theme(
          axis.title = element_text(size = 26),
          text = element_text(size = 20)
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
          title = "DDD/DOT table",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box4.0",
                label = "Select group", 
                choiceNames = c("Antimicrobial - Groups", "Antimicrobials", "Year", "Specialty", "Subspecialty", "Admitting department"),
                choiceValues = c("ab_group", "ab_type", "year", "specialty", "sub_specialty", "adm_route"), 
                selected = "ab_type", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box4.1",
                label = "Select group", 
                choices = c("DDD per 100 bed days", "DOT per 100 bed days"), 
                selected = "DDD per 100 bed days", direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            DT::dataTableOutput("table_ab"),
            type = 4,
            color = radar_colors["red"],
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
                choiceNames = c("Year", "Quarter"), 
                choiceValues = c("year", "yearquarter"), 
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
            color = radar_colors["red"],
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
        )
      )
    )
  })
  
  observeEvent((input$dia_adm), {
    showModal(modalDialog(
      renderPlot({
        dia_adm() + theme(
          axis.title = element_text(size = 26),
          text = element_text(size = 20)
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
          title = "Diagnostics - timing",
          withSpinner(
            plotOutput("plot_dia_timing", height = 300),
            type = 4,
            color = radar_colors["red"],
            size = 0.7
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box6 == 'Diagnostics - timing'",
            actionBttn(
              inputId = "dia_timing",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position:absolute;left:0.5em;bottom: 0.5em;",
          dropdown(
            downloadButton(outputId = "down_box_6", label = "Download plot"),
            size = "xs",
            icon = icon("download", class = "opt"), 
            up = TRUE
          )
        )
      )
    )
  })
  
  observeEvent((input$dia_timing), {
    showModal(modalDialog(
      renderPlot({
        plot_dia_timing() + theme(
          axis.title = element_text(size = 26),
          text = element_text(size = 20)
        )
      }, height = 600),
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
          title = "Diagnostics in relation",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box7.1",
                label = "Select group", 
                choiceNames = c("Antimicrobial - Groups", "Antimicrobials", "Year", "Specialty", "Subspecialty", "Admitting department"),
                choiceValues = c("ab_group", "ab_type", "year", "specialty", "sub_specialty", "adm_route"), 
                selected = "year", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box7.2",
                label = "Labels", 
                choices = c("Show labels", "Hide labels"), 
                selected = "Hide labels", 
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
            plotlyOutput("plot_dia_perform", height = 300),
            type = 4,
            color = radar_colors["red"],
            size = 0.7
          )
        ),
        tabPanel(
          title = "Table - Proportion performed",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box8.0",
                label = "Select group", 
                choiceNames = c("Antimicrobials", "Antimicrobial - Groups", "Year", "Specialty", "Subspecialty", "Admitting department"),
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
            color = radar_colors["red"],
            size = 0.7
          )
        ),
        div(
          style = "position:absolute;right:0.5em;bottom: 0.5em;",
          conditionalPanel(
            "input.box7 == 'Diagnostic performance'",
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
      renderPlotly({ 
        plot_dia_perform()
      }),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  # UI - DIAGNOSTICS - 4 ------------------------------------------------------------------
  
  output$box8 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box8",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Microorganisms found",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box8",
                label = "Select group", 
                choiceNames = c("All", "Antimicrobial - Groups", "Antimicrobials", "Year", "Gender", "Specialty", "Subspecialty", "Admitting department"),
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
                inputId = "box8.1",
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
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_8", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("micro_plot", height = 300),
            type = 4,
            color = radar_colors["red"],
            size = 0.7
          )
        ),
        div(
          style = "position:absolute;right:0.5em;bottom: 0.5em;",
          conditionalPanel(
            "input.box7 == 'Diagnostic performance'",
            actionBttn(
              inputId = "dia_micro",
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
  
  observeEvent((input$dia_micro), {
    showModal(modalDialog(
      renderPlot({
        micro_plot()
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  # UI - DIAGNOSTICS - ADVANCED ---------------------------------------------------
  
  output$box_log_advanced <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_log_advanced",
        width = NULL,
        height = NULL,
        tabPanel(
          title = "Advanced options",
          h5(strong("Logistic regression model \n (dependent variable = Diagnostics perfomed):")),
          withSpinner(
            verbatimTextOutput("logreg", placeholder = TRUE),
            type = 4,
            color = radar_colors["red"],
            size = 0.7,
            proxy.height = "200px"
          ),
          h5(strong("Summary statistics")),
          verbatimTextOutput("logreg_aic", placeholder = TRUE)
        )
      )
    )
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
                inputId = "box_los1",
                label = "Select group", 
                choiceNames = c("All", "Gender", "Year", "Antimicrobial - Groups", "Antimicrobials", "Diagnostics", "Specialty", "Subspecialty", "Admitting department"),
                choiceValues = c("1", "gender", "year", "ab_group", "ab_type", "check", "specialty", "sub_specialty", "adm_route"), 
                selected = , 
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
            color = radar_colors["red"],
            size = 0.7
          )
        )
      )
    )
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
            color = radar_colors["red"],
            size = 0.7
          ),
          div(
            style = "position: absolute; left: 4.5em; bottom: 0.5em;",
            htmlOutput("text_box_los2")
          )
        )
      )
    )
  })
  
  output$text_box_los2 <- renderText({
    
    text <- function(x){
      if(input$box_los1 == "1"){y <- "All"}
      if(input$box_los1 == "gender"){y <- "Gender"}
      if(input$box_los1 == "year"){y <- "Year"}
      if(input$box_los1 == "specialty"){y <- "Specialty"}
      if(input$box_los1 == "sub_specialty"){y <- "Subspecialty"}
      if(input$box_los1 == "ab_group"){y <- "Antimicrobial - Groups"}
      if(input$box_los1 == "ab_type"){y <- "Antimicrobials"}
      if(input$box_los1 == "check"){y <- "Diagnostics"}
      if(input$box_los1 == "adm_route"){y <- "Admitting department"}
      y
    }
    
    paste0("Groups: ","<b>",text(input$box_los1), "<br>", "</b>", "(Select groups and legend in box to the left.)")
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
                choiceNames = c("Gender", "Year", "Antimicrobial - Groups", "Antimicrobials", "Diagnostics", "Specialty", "Subspecialty", "Admitting department"),
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
            DT::dataTableOutput("table_los"),
            type = 4,
            color = radar_colors["red"],
            size = 0.7
          )
        )
      )
    )
  })
  
  
  # UI - OUTCOME - ADVANCED ---------------------------------------------------
  
  output$box_cox_advanced <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_cox_advanced",
        width = NULL,
        height = 800,
        tabPanel(
          title = "Advanced options",
          h5(strong("Cox regression model \ (lenght of stay):")),
          withSpinner(
            verbatimTextOutput("coxreg", placeholder = TRUE),
            type = 4,
            color = radar_colors["red"],
            size = 0.7,
            proxy.height = "200px"
          ),
          h5(strong("Summary statistics:")),
          verbatimTextOutput("coxreg_aic", placeholder = TRUE)
        )
      )
    )
  })
  
  # BOX PATIENTS - 1 ------------------------------------------------------------
  
  plot_pat_select <- reactive({
    
    pat_select <- set_reac_1() %>%
      group_by(sub_specialty, specialty) %>%
      summarise(n = n()) %>% arrange(desc(n))
    
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
                 fill = n,
                 text = sub_specialty, 
                 count = n)) +
      geom_point(aes(size = n, fill = sub_specialty), 
                 alpha = 0.8, 
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
      scale_fill_radar() +
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
      hoverlabel = list(bgcolor = "white"), 
      layout = list(hovermode = "compare")) %>%
      layout(margin = list(b = 60))
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
      options = list(
        dom = "ftpr",
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
        scale_fill_manual(values = c("#d1351b", "#f39c12"), 
                          name = "Gender", 
                          breaks = c("M", "F"), 
                          label = c("Male", "Female")) +
        theme_minimal()
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
        guides(fill = FALSE) +
        theme_minimal() 
    }
  })
  
  output$plot_age_select <- renderPlot({
    plot_age_select()
  })
  
  
  # BOX PATIENTS - 3 ----------------------------------------------------------
  
  plot_year_select <- reactive({
    ggplot(set_reac_1(), aes_string("factor(year)", fill = input$box_year1)) +
      geom_bar(color = "black", alpha = 0.6, position = if (input$box_year2 == "Count") {
        "stack" 
      } else {
        "fill"
      }) +
      labs(x = "Year", y = "Count") +
      scale_fill_manual(values = c("#d1351b", "#f39c12"), 
                        name = "Gender", 
                        breaks = c("M", "F"), 
                        label = c("Male", "Female")) +
      theme_minimal()
  })
  
  output$plot_year_select <- renderPlot({
    plot_year_select()
  })
  
  
  # BOX AB - 1 -------------------------------------------------------------------
  
  plot_ab <- reactive({
    if (input$box1.1 == "Prescriptions") {
      ggplot(
        data = 
          if (input$box1.0 == "ab_type") {
            set_ab()
          } 
        else {
          set_ab_group()
        },
        aes_string(
          x = paste0("reorder(", input$box1.0, ",n)"),
          y = "n",
          fill = "n"
        )) +
        geom_bar(stat = "identity",
                 color = "black",
                 alpha = 0.8) +
        scale_fill_continuous(high = "#8c8c8c", low = "#cccccc") +
        labs(x = NULL, y = "No. of prescriptions") +
        guides(fill = "none") +
        theme_minimal() +
        coord_flip()
    }
    else {
      if (input$box1.1 == "DDD per 100 bed days") {
        
        LOS_sum <- as.numeric(sum(set_reac_2()$LOS)) / 100
        
        set_reac_2() %>%
          filter(!is.na(ddd_per_prescription)) %>%
          group_by_(input$box1.0) %>%
          summarise(DDD_sum_100 = sum(ddd_per_prescription) / LOS_sum) %>%
          ggplot(aes_string(
            x = paste0("reorder(", input$box1.0, ",DDD_sum_100)"),
            y = "DDD_sum_100",
            fill = "DDD_sum_100"
          )) +
          geom_bar(stat = "identity",
                   color = "black",
                   alpha = 0.8) +
          scale_fill_radar(discrete = FALSE, palette = "hot") +
          labs(x = NULL, y = "DDD per 100 bed days") +
          guides(fill = "none") +
          theme_minimal() +
          coord_flip()
      }
      else {
        
        LOS_sum <- sum(set_reac_2()$LOS) / 100
        
        set_reac_2() %>%
          group_by_(input$box1.0) %>%
          summarise(DOT_sum_100 = as.numeric(sum(ab_days)) / LOS_sum) %>%
          ggplot(aes_string(
            x = paste0("reorder(", input$box1.0, ",DOT_sum_100)"),
            y = "DOT_sum_100",
            fill = "DOT_sum_100"
          )) +
          geom_bar(stat = "identity",
                   color = "black",
                   alpha = 0.8) +
          scale_fill_radar(discrete = FALSE,
                           palette = "cool",
                           reverse = TRUE) +
          labs(x = NULL, y = "DOT per 100 bed days") +
          guides(fill = "none") +
          theme_minimal() +
          coord_flip()
      }
    }
  })
  
  
  output$plot_ab <- renderPlot({
    plot_ab()
  })
  
  
  
  # BOX AB - 2 --------------------------------------------------------------
  
  plot_DDD_all <- reactive({
    
    DDD_set <- set_reac_1() %>%
      filter(!is.na(ddd_per_prescription)) %>%
      group_by_(input$box2.1) %>%
      mutate(LOS_sum_100 = as.numeric(sum(LOS)) / 100, 
             DDD_sum_all = sum(ddd_per_prescription)) %>%
      mutate(DDD_100 = DDD_sum_all / LOS_sum_100)
    
    plot <- 
      ggplot(DDD_set, aes_string(input$box2.1, "DDD_100", fill = "DDD_100")) +
      geom_col(position = "identity", 
               color = "black", 
               alpha = 0.8) +
      scale_fill_radar(discrete = FALSE, palette = "hot") +
      guides(fill = FALSE) +
      theme_minimal() +
      labs(x = NULL, y = "DDD per 100 patient days") +
      if (input$box2.1 == "sub_specialty" | input$box2.1 == "adm_route") {
        if (input$box2.2 == "Show labels") {
          theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                legend.position = "none")
        } else {
          theme(axis.text.x = element_blank())
        }
      }
    
    if (input$box2.0 == "Top 3") {
      plot +
        geom_label_repel(data = DDD_set %>% 
                           arrange(-DDD_100) %>% 
                           distinct(input$box2.0, .keep_all = TRUE) %>% 
                           head(3), 
                         aes_string(label = input$box2.1))
    } else {
      plot
    }
  })
  
  output$plot_DDD_all <- renderPlot({
    plot_DDD_all()
  })
  
  
  # BOX AB - 3 --------------------------------------------------------------
  
  plot_DOT_all <- reactive({
    DOT_set <- set_reac_1() %>%
      filter(!is.na(ab_days)) %>%
      group_by_(input$box3.1.0) %>%
      mutate(LOS_sum_100 = as.numeric(sum(LOS)) / 100, DOT_sum_all = as.numeric(sum(ab_days))) %>%
      mutate(DOT_100 = DOT_sum_all / LOS_sum_100)
    
    plot <- 
      ggplot(DOT_set, aes_string(input$box3.1.0, "DOT_100", fill = "DOT_100")) +
      geom_col(position = "identity", 
               color = "black", 
               alpha = 0.8) +
      scale_fill_radar(discrete = FALSE, palette = "cool", reverse = TRUE) +
      guides(fill = FALSE) +
      theme_minimal() +
      labs(x = NULL, y = "DOT per 100 patient days") +
      if (input$box3.1.0 == "sub_specialty" | input$box3.1.0 == "adm_route") {
        if (input$box3.1.1 == "Show labels") {
          theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                legend.position = "none")
        }
        else {
          theme(axis.text.x = element_blank())
        }
      }
    
    if (input$box3.1.2 == "Top 3") {
      plot +
        geom_label_repel(data = DOT_set %>% arrange(-DOT_100) %>% 
                           distinct(input$box3.1.0, .keep_all = TRUE) %>% 
                           head(3), 
                         aes_string(label = input$box3.1.0))
    }
    else {
      plot
    }
  })
  
  output$plot_DOT_all <- renderPlot({
    plot_DOT_all()
  })
  
  
  # BOX AB - 4 --------------------------------------------------------------
  
  output$table_ab <- DT::renderDataTable({
    DDD_set_ab <-
      set_reac_2() %>%
      filter(!is.na(ddd_per_prescription)) %>%
      group_by_(if (input$box4.0 == "ab_type") {
        "ab_type"
      } else {
        (input$box4.0)
      }) %>%
      mutate(
        LOS_sum_100 = as.numeric(sum(LOS)) / 100,
        DDD_sum_all = sum(ddd_per_prescription)
      ) %>%
      mutate(DDD_100 = round(DDD_sum_all / LOS_sum_100), 1) %>%
      select_(if (input$box4.0 == "ab_type") {
        "ab_type"
      } else {
        (input$box4.0)
      }, "DDD_100") %>%
      rename("DDD per 100 bed days" = DDD_100)
    
    DOT_set_ab <-
      set_reac_2() %>%
      filter(!is.na(ab_days)) %>%
      group_by_(if (input$box4.0 == "ab_type") {
        "ab_type"
      } else {
        (input$box4.0)
      }) %>%
      mutate(
        LOS_sum_100 = as.numeric(sum(LOS)) / 100,
        DOT_sum_all = as.numeric(sum(ab_days))
      ) %>%
      mutate(DOT_100 = round(DOT_sum_all / LOS_sum_100), 1) %>%
      select_(if (input$box4.0 == "ab_type") {
        "ab_type"
      } else {
        (input$box4.0)
      }, "DOT_100") %>%
      rename("DOT per 100 bed days" = DOT_100)
    
    
    datatable(
      if (input$box4.1 == "DDD per 100 bed days") {
        DDD_set_ab %>% distinct_(input$box4.0, .keep_all = TRUE)
      }
      else {
        DOT_set_ab %>% distinct_(input$box4.0, .keep_all = TRUE)
      },
      rownames = FALSE,
      colnames = c("Group", input$box4.1),
      options = list(
        dom = "ftpr",
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )
  })
  
  
  # BOX DIAGNOSTICS - 1  ------------------------------------------------------------------
  
  dia_adm <- reactive({
    ggplot(set_reac_1(), aes_string(input$box5.1, fill = "check")) +
      geom_bar(
        position = input$box5.2, 
        color = "black", 
        alpha = 0.8) +
      theme_minimal() +
      theme(axis.text.x = if (input$box5.1 == "yearquarter") {
        element_text(angle = 90, hjust = 1)
      } else {
        element_text(angle = 0, hjust = 1)
      }) +
      labs(x = if (input$box5.1 == "year") {
        paste("Year")
      } else {
        paste("Quarter")
      }, y = if (input$box5.2 == "dodge") {
        paste("Count")
      } else {
        paste("Proportion")
      }) +
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
  })
  
  output$plot_dia_adm <- renderPlot({
    if (req(input$tab) == "Diagnostics") {
      dia_adm()
    }
    else {
      Sys.sleep(0)
    }
  })
  
  
  
  # BOX DIAGNOSTICS - 2 -------------------------------------------------------
  
  plot_dia_timing <- reactive({
    if (input$diagnosticsInput == "bc_timing") {
      timing <- set_reac_1() %>%
        filter(check == "Taken") %>%
        group_by(bc_timing) %>%
        summarise(n = n()) %>%
        rename(timing = bc_timing)
    }
    else {
      timing <- set_reac_1() %>%
        filter(check == "Taken") %>%
        group_by(uc_timing) %>%
        summarise(n = n()) %>%
        rename(timing = uc_timing)
    }
    
    ggplot(timing, aes(timing, y = n, fill = n)) +
      geom_bar(
        stat = "identity", 
        color = "black", 
        alpha = 0.8) +
      labs(x = "Timing in relation to start of antimicrobials (in days)", y = "Count") +
      guides(fill = "none") +
      scale_fill_radar(discrete = FALSE, palette = "cool") +
      theme_minimal()
  })
  
  output$plot_dia_timing <- renderPlot({
    plot_dia_timing()
    
  })
  
  
  # BOX DIAGNOSTICS - 3 --------------------------------------------------------
  
  plot_dia_perform <- reactive({
    perform_all <- set_reac_1() %>%
      mutate(dia_perform_all = round(sum(check == "Taken") / n() * 100, 1)) %>%
      dplyr::select(input$box7.1, dia_perform_all)
    
    perform_group <- set_reac_1() %>%
      group_by_(input$box7.1) %>%
      summarise(dia_perform = round(sum(check == "Taken") / n() * 100, 1)) %>%
      left_join(perform_all) %>%
      mutate(dia_perform_diff = dia_perform - dia_perform_all) %>%
      arrange(-dia_perform_diff) %>%
      distinct(.keep_all = TRUE)
    
    ggplot(perform_group, aes_string(
      x = paste0("fct_inorder(factor(", input$box7.1, "))"), 
      y = "dia_perform_diff", 
      fill = "dia_perform_diff", 
      text = paste0(input$box7.1))) +
      geom_hline(
        yintercept = 0, 
        linetype = 2, 
        color = "darkgrey") +
      geom_bar(
        stat = "identity", 
        color = "black", 
        size = 0.1, 
        alpha = 0.8) +
      scale_fill_radar(discrete = FALSE, palette = "cool") +
      labs(
        y = paste0("Diff. from average [", perform_all[1, 2], "%]"), 
        x = "") +
      guides(fill = FALSE) +
      theme_minimal() +
      if (input$box7.2 == "Show labels") {
        theme(axis.text.x = element_text(angle = 90, hjust = 1), 
              legend.position = "none")
      }
    else {
      theme(axis.text.x = element_blank())
    }
    
    style(
      hide_legend(
        ggplotly(tooltip = c("text", "value"))), 
      hoverlabel = list(bgcolor = "white"))
  })
  
  output$plot_dia_perform <- renderPlotly({
    plot_dia_perform()
  })
  
  output$dia_table <- DT::renderDataTable({
    datatable(
      set_reac_1() %>%
        group_by_(input$box8.0, "check") %>%
        tally() %>%
        mutate(Proportion = paste0(round(n / sum(
          n
        ) * 100, 1), "%")),
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
      options = list(
        dom = "ftpr",
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )
  })
  
  
  # BOX DIAGNOSTICS - 4  ------------------------------------------------------------------
  
  micro_plot <- reactive({ 
    
    micro_count <- set_reac_2() %>% ungroup()
    micro_count <- 
      micro_count %>%
      group_by_("fullname", input$box8) %>%
      summarise(n = n()) %>%
      arrange(-n)
    
    micro_count <- micro_count[1:input$box8.1,]
    
    
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
      scale_color_viridis(option = "A") +
      labs(x = NULL, y = "Count") +
      guides(fill = "none") +
      theme_minimal() +
      coord_flip()
    
    if (input$box8 != "fullname") {
      plot <- plot + facet_wrap(input$box8)
    }
    
    plot
  })
  
  output$micro_plot <- renderPlot({
    micro_plot()
  })
  
  
  
  # BOX DIAGNOSTICS -  ADVANCED --------------------------------------------------------
  
  logreg <-
    reactive({
      glm(as.formula(paste0("as.factor(check) ~", paste(input$modelInput, collapse = "+"))),
          data = set_reac_1(), family = binomial
      )
    })
  
  output$logreg <- renderPrint({
    req(input$modelInput)
    logreg <- tidy(logreg()) %>%
      select(term, estimate, std.error, p.value) %>%
      bind_cols(as.tibble(exp(coefficients(logreg())))) %>%
      select(term, value, p.value, std.error) %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      rename(Variable = term, OR = value, "p-value" = p.value)
    
    logreg$Variable <- str_replace(
      logreg$Variable,
      pattern = paste(
        c("ab_type", "ab_group", "specialty", "sub_specialty", "gender", "adm_route", "adm_weekday"),
        collapse = "|"
      ),
      replacement = ""
    )
    logreg
  })
  
  output$logreg_aic <- renderPrint({
    req(input$modelInput)
    glance(logreg())
  })
  
  
  # BOX OUTCOME - 1 -------------------------------------------------------------
  
  plot_los <- reactive({
    p <-
      ggplot(set_reac_1()) +
      coord_cartesian(xlim = input$zoom) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title.y = if (input$box_los1.3 == TRUE) {
          element_blank()
        }
      ) +
      if (input$box_los1.3 == TRUE) {
        geom_density_ridges(
          aes_string(
            x = "LOS",
            y = input$box_los1,
            group = input$box_los1,
            fill = paste(as.factor(input$box_los1)),
            height = "..density.."
          ),
          alpha = 0.3
        )
      } else {
        geom_density(
          aes_string(
            x = "LOS",
            group = input$box_los1,
            fill = paste(as.factor(input$box_los1))
          ),
          position = "identity",
          alpha = 0.3,
          bw = 1
        )
      }
    
    if(input$box_los1 == "check"){
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
    }
    if ( input$box_los1 == "1") {
      p <- p + scale_fill_continuous(low = "lightgrey", high = "lightgrey")
    }
    if (!(input$box_los1 %in% c("1", "check"))) {
      p <- p + scale_fill_manual(values = viridis(n_distinct(set %>% select_(input$box_los1))))
    }
    if (input$box_los1.1 == FALSE) {
      p <- p + guides(fill = FALSE)
    }
    if(input$box_los1.2 == TRUE & input$box_los1.3 != TRUE) {
      p <- p + geom_histogram(
        aes_string(y = "..density.."),
        alpha = 0.6,
        binwidth = 1,
        position = "dodge",
        color = "black"
      )
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
    
    ggsurvplot(
      surv_fit(as.formula(
        paste0("Surv(LOS, event = status)", "~", input$box_los1)),
        data = kaplan_set),
      data = kaplan_set,
      color = "strata",
      pval = TRUE,
      conf.int = FALSE,
      pval.method = TRUE,
      break.time.by = 5,
      palette = {
        if (input$box_los1 == "1") {
          c("lightgrey")
        }
        if (input$box_los1 == "check") {
          if (input$diagnosticsInput == "bc_timing") {
            c("#a6a6a6", "#d1351b")
          }
          else {
            c("#a6a6a6", "#f39c12")
          }
        }
        else{
          viridis(n_distinct(set %>% select_(input$box_los1)))
        }
      },
      ylab = "Probablility to stay in hospital",
      xlab = "Length of stay (days)",
      ggtheme = theme_blank(),
      legend = "none",
      pval.coord = c(1, 0.15),
      pval.method.coord = c(1, 0.25),
      xlim = input$zoom, # 20 for show purposes
      break.x.by = 5
    )
  })
  
  output$kaplan_los <- renderPlot({
    kaplan_los()
  })
  
  
  # BOX OUTCOME - 3 -------------------------------------------------------------
  
  table_los <- reactive({
    set_table <- set_reac_1()
    set_table$gender <- factor(set_table$gender, levels = c("M", "F"), labels = c("Male", "Female"))
    
    set_table <- set_table %>%
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
      set_table <- set_table %>% rename("Admitting department" = adm_route)
    }
    
    datatable(set_table, rownames = FALSE, fillContainer = TRUE, options = list(
      dom = "tpr",
      style = "bootstrap"
    ))
  })
  
  output$table_los <- DT::renderDataTable({
    table_los()
  })
  
  
  
  # BOX OUTCOME - ADVANCED -------------------------------------------------------------
  
  fml_cox <- reactive({
    as.formula(paste0(
      "Surv(LOS, event = status)",
      "~",
      paste(input$coxInput, collapse = "+")
    )) 
  })
  
  coxreg <- reactive({
    coxph(fml_cox(), data = set_reac_1())
  })
  
  output$coxreg <- renderPrint({
    req(input$coxInput)
    coxreg <- tidy(coxreg()) %>% 
      select(term, estimate, std.error, p.value) %>%
      bind_cols(as.tibble(exp(coefficients(coxreg())))) %>%
      select(term, value, p.value, std.error) %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      rename(Variable = term, HR = value, "p-value" = p.value)
    coxreg$Variable <- str_replace(
      coxreg$Variable,
      pattern = paste(
        c("check", "ab_type", "ab_group", "specialty", "sub_specialty", "gender", "adm_route", "adm_weekday", "check"),
        collapse = "|"
      ),
      replacement = ""
    )
    coxreg
  })
  
  output$coxreg_aic <- renderPrint({
    req(input$coxInput)
    glance(coxreg())
  })
  
  
  # DOWNLOAD ----------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(set_base(), file, row.names = FALSE)
    }
  )
  
  
  download_box <- function(exportname, plot) {
    downloadHandler(
      filename = function() {
        paste(exportname, Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot, device = "png")
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
  output$down_box8 <- download_box("mircoorganisms", micro_plot())
  output$down_box_los1 <- download_box("los_groups", plot_los()) 
  output$down_box_los2 <- download_box("km-curve", kaplan_los()$plot)
  
  
}