

ui <- dashboardPage(
  skin = "black",
  title = "RadaR",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title = span(img(src = "radar.svg", height = 35), "RadaR"),
    titleWidth = 300,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = (steps$text[1]),
        icon = icon("spinner")
      ),
      notificationItem(
        text = steps$text[2],
        icon = icon("address-card")
      ),
      notificationItem(
        text = steps$text[3],
        icon = icon("calendar")
      ),
      notificationItem(
        text = steps$text[4],
        icon = icon("user-md")
      ),
      notificationItem(
        text = steps$text[5],
        icon = icon("ambulance")
      ),
      notificationItem(
        text = steps$text[6],
        icon = icon("flask")
      ),
      notificationItem(
        text = strong(steps$text[7]),
        icon = icon("exclamation")
      )
    ),
    tags$li(
      a(
        strong("ABOUT RadaR"),
        height = 40,
        href = "https://github.com/ceefluz/radar/blob/master/README.md",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    width = 300,
    introBox(data.step = 3, data.intro = intro$text[3], #  intro tour
             div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
             sidebarMenu(
               introBox(data.step = 1, data.intro = intro$text[1], # intro tour
                        div(id = "sidebar_button",
                            bsButton(inputId = "confirm", 
                                     label = "START RADAR", 
                                     icon = icon("play-circle"), 
                                     style = "danger")
                        )
               ),
               div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
               menuItem(
                 "ANTIMICROBIALS",
                 tabName = "antimicrobials",
                 icon = icon("spinner"),
                 menuItem(
                   "START OF ANTIMICROBIALS \n (IN RELATION TO START OF ADMISSION)",
                   sliderInput(
                     inputId = "ab_timingInput",
                     label = "",
                     min = 0,
                     max = max(antimicrobials$ab_timing, na.rm = TRUE),
                     value = c(0, max(antimicrobials$ab_timing, na.rm = TRUE)),
                     step = 1
                   ),
                   switchInput(
                     inputId = "ab_anyInput",
                     label = "SELECTION",
                     value = FALSE, 
                     onLabel = "SLIDER", 
                     offLabel = "ANY", 
                     inline = TRUE, 
                     size = "mini", 
                     width = "100%", 
                     offStatus = "danger"
                   )
                 ),
                 menuItem(
                   "MINIMUM TREATMENT DURATION (DAYS) \n ALL ANTIMICROBIALS",
                   sliderInput(
                     inputId = "ab_allInput",
                     label = "",
                     value = 2L,
                     min = 1L,
                     max = 10L,
                     step = 1L
                   ),
                   switchInput(
                     inputId = "ab_any_allInput",
                     label = "SELECTION",
                     value = FALSE, 
                     onLabel = "SLIDER", 
                     offLabel = "ANY", 
                     inline = TRUE, 
                     size = "mini", 
                     width = "100%", 
                     offStatus = "danger"
                   )
                 ),
                 menuItem(
                   "MINIMUM DURATION OF USE (DAYS) \n SINGLE ANTIMICROBIAL",
                   sliderInput(
                     inputId = "ab_singleInput",
                     label = "",
                     value = 2L,
                     min = 1L,
                     max = 10L,
                     step = 1L
                   ),
                   switchInput(
                     inputId = "ab_any_singleInput",
                     label = "SELECTION",
                     value = FALSE, 
                     onLabel = "SLIDER", 
                     offLabel = "ANY", 
                     inline = TRUE, 
                     size = "mini", 
                     width = "100%", 
                     offStatus = "danger"
                   )
                 ),
                 menuItem(
                   "ADMINISTRATION ROUTE",
                   checkboxGroupInput(
                     inputId = "adminInput",
                     label = "",
                     choices = unique(antimicrobials$ab_route[!is.na(antimicrobials$ab_route)]),
                     selected = unique(antimicrobials$ab_route[!is.na(antimicrobials$ab_route)])
                   )
                 ),
                 menuItem(
                   "FIRST ANTIMICROBIALS ONLY",
                   switchInput(
                     inputId = "firstInput",
                     label = "",
                     value = FALSE, 
                     onLabel = "FIRST", 
                     offLabel = "ALL", 
                     size = "mini", 
                     width = "100%", 
                     offStatus = "danger"
                   )
                 ),
                 checkboxGroupButtons(
                   inputId = "allInput",
                   label = "CHOOSE GROUPS OF ANTIMICROBIALS",
                   choices = "ALL / NONE",
                   size = "sm",
                   selected = "ALL / NONE"
                 ),
                 checkboxGroupInput(
                   inputId = "abGroupInput",
                   label = "",
                   choices = sort(ab_groups$ab_group)
                 ),
                 checkboxGroupInput(
                   inputId = "abInput",
                   label = "ANTIMICROBIALS",
                   choices = ab$ab_type,
                   selected = ab$ab_type
                 )
               )
               ,
               br(),
               br(),
               menuItem(
                 "PATIENTS",
                 tabName = "patients",
                 icon = icon("address-card"),
                 checkboxGroupInput(
                   inputId = "genderInput",
                   label = "",
                   choices = unique(admissions$gender),
                   selected = unique(admissions$gender),
                   inline = TRUE
                 ),
                 sliderInput(
                   inputId = "ageInput",
                   label = "Age",
                   value = c(min(admissions$age, na.rm = TRUE), max(admissions$age, na.rm = TRUE)),
                   min = min(admissions$age, na.rm = TRUE),
                   max = max(admissions$age, na.rm = TRUE),
                   step = 1,
                   sep = ""
                 )
               ),
               br(),
               br(),
               menuItem(
                 "YEAR",
                 tabName = "year",
                 icon = icon("calendar"),
                 sliderInput(
                   inputId = "yearInput",
                   label = "Year",
                   value = c(min(antimicrobials$year, na.rm = TRUE), max(antimicrobials$year, na.rm = TRUE)),
                   min = min(antimicrobials$year, na.rm = TRUE),
                   max = max(antimicrobials$year, na.rm = TRUE),
                   step = 1L,
                   sep = ""
                 )
               ),
               br(),
               br(),
               menuItem(
                 "SPECIALTY",
                 tabName = "specialty",
                 icon = icon("user-md"),
                 checkboxGroupInput(
                   inputId = "specInput",
                   label = "SPECIALTY",
                   choices = unique(admissions$specialty),
                   selected = unique(admissions$specialty)
                 ),
                 sliderTextInput(
                   inputId = "nInput",
                   label = "MINIMUM NUMBER OF PATIENTS PER SUBSPECIALTY",
                   choices = c("0", "10", "100", "1000", "10000"),
                   selected = "0",
                   grid = TRUE
                 ),
                 selectizeInput(
                   inputId = "inInput",
                   label = "INCLUDE ONLY THIS SUBSPECIALTY:",
                   choices = c(levels(as.factor(admissions$sub_specialty))),
                   multiple = TRUE
                 ),
                 selectizeInput(
                   inputId = "exInput",
                   label = "EXCLUDE SUBSPECIALTY",
                   choices = c(levels(as.factor(admissions$sub_specialty))),
                   multiple = TRUE
                 )
               ),
               br(),
               br(),
               menuItem(
                 "ORIGIN",
                 tabName = "admission",
                 icon = icon("ambulance"),
                 checkboxGroupInput(
                   inputId = "admissionInput",
                   label = "",
                   choices = levels(as.factor(admissions$adm_route)),
                   selected = levels(as.factor(admissions$adm_route))
                 )
               ),
               br(),
               br(),
               menuItem(
                 "DIAGNOSTICS",
                 tabName = "diagnostics",
                 icon = icon("flask"),
                 selectInput(
                   inputId = "diagnosticsInput",
                   label = "",
                   choices = list("Blood cultures" = "bc_timing", "Urine cultures" = "uc_timing")
                 ),
                 sliderInput(
                   inputId = "checkInput",
                   label = "DAYS TO FIRST TEST (IN RELATION TO START OF ANTIMICROBIALS)",
                   value = c(-1L, 1L),
                   min = min(c(admissions$bc_timing, admissions$uc_timing), na.rm = TRUE),
                   max = max(c(admissions$bc_timing, admissions$uc_timing), na.rm = TRUE),
                   step = 1L
                 )
               ),
               br(),
               br(),
               menuItem(
                 "DOWNLOAD SELECTION",
                 tabName = "download",
                 icon = icon("download"),
                 textInput(
                   inputId = "filename",
                   placeholder = "Name download file",
                   label = ""
                 ),
                 div(
                   downloadButton(
                     outputId = "downloadData",
                     label = "Save Antimicrobial/Admission Data",
                     icon = icon("download"),
                     style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                   )
                 ),
                 div(
                   downloadButton(
                     outputId = "downloadMicroData",
                     label = "Save Microbiology Data",
                     icon = icon("download"),
                     style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                   )
                 )
               ),
               br()
               
             )
    )),
  
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "radar_style.css")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton("patients", 
                   label = "PATIENTS", 
                   icon = icon("user"), 
                   style = "success"),
          bsButton("antimicrobials", 
                   label = "ANTIMICROBIALS", 
                   icon = icon("spinner", class = "spinner-box"), 
                   style = "success"),
          bsButton("diagnostics", 
                   label = "DIAGNOSTICS", 
                   icon = icon("flask", class = "flask-box"), 
                   style = "success"),
          bsButton("outcome", 
                   label = "OUTCOME", 
                   icon = icon("thumbs-o-up"), 
                   style = "success"),
          data.step = 2, data.intro = intro$text[2])
      )
    ),
    
    fluid_design("antimicrobials_panel", "box1", "box2", "box3", "box4"),
    fluid_design("diagnostics_panel", "box5", "box6", "box7", "box8"),
    fluid_design("outcome_panel", "box_los1", "box_los2", "box_los3", NULL),
    
    fluidRow(
      div(
        id = "patients_panel", 
        column(
          width = 12,
          introBox(data.step = 4, data.intro = intro$text[4],
                   uiOutput("box_pat")
          )
        ),
        column(
          width = 6,
          uiOutput("box_pat2")
        ),
        column(
          width = 6,
          uiOutput("box_year")
        )
      )
    )
  )
)
