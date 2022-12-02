#' Harmonized PRiSMA Dashboard (Pregnancy Surveillance)
#'
#'This function helps to create a shiny dashboard for you using Prisma data.
#'
#'@example Prisma_dashboard()
#'
#' @export
#' @param  Prisma_dat - Prisma dataset,
#' @param  ... - any additional function



Prisma_dashboard <- function(Prisma_dat,...){
  suppressMessages({
    suppressWarnings({
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  if(!require(lubridate)){install.packages(lubridate); library(lubridate)}
  if(!require(writexl)){install.packages(writexl); library(writexl)}
  if(!require(labelled)){install.packages(labelled); library(labelled)}
  if(!require(dlookr)){install.packages(dlookr); library(dlookr)}
  if(!require(SmartEDA)){install.packages(SmartEDA); library(SmartEDA)}
  if(!require(gtsummary)){install.packages(gtsummary); library(gtsummary)}
  if(!require(gt)){install.packages(gt); library(gt)}
  #if(!require(CleanEasy)){install.packages(CleanEasy); library(CleanEasy)}
  if(!require(janitor)){install.packages(janitor); library(janitor)}
  if(!require(ggstatsplot)){install.packages(ggstatsplot); library(ggstatsplot)}
  if(!require(here)){install.packages(here); library(here)}
  if(!require(shinythemes)){install.packages(shinythemes); library(shinythemes)}
  if(!require(shiny)){install.packages(shiny); library(shiny)}
  if(!require(shinydashboard)){install.packages(shinydashboard); library(shinydashboard)}
  if(!require(ggplot2)){install.packages(ggplot2); library(ggplot2)}
  if(!require(esquisse)){install.packages(esquisse); library(esquisse)}
  if(!require(shinyjs)){install.packages(shinyjs); library(shinyjs)}
})})
  dict <- labelled::look_for(Prisma_dat %>% clean_names(.)) %>% select(2:3) %>%
    mutate(label = replace(label,variable =="scrnid","Screening ID"),
           label = replace(label,variable =="scrn_obsloc","Screening location"),
           label = replace(label,variable =="age_ieorres","Does the woman meet required age"),
           label = replace(label,variable =="catchment_ieorres","Woman lives and will remain in study area"),
           label = replace(label,variable == "type_visit","Type of Visit"))

  # --- Start by creating ANC 20 for those enrolled at 18> to less 20 GA
  Prisma_data <- Prisma_dat %>% clean_names(.) %>%
    rbind(Prisma_dat %>% clean_names(.) %>%
            filter(type_visit =="Enrollment",(us_ga_wks_age_fts1 >= 18 & us_ga_wks_age_fts1 < 20)) %>%
            mutate(type_visit = replace(type_visit, type_visit=="Enrollment","ANC-20"))) %>%
    set_variable_labels(.labels = setNames(as.list(dict$label),dict$variable)) %>%
    arrange(scrnid) %>%
    mutate(brthdat = replace(brthdat,brthdat == "1907-07-07",NA)) %>%
    mutate(Mum_age = round(as.numeric(scrn_obsstdat - brthdat)/365,2),
           Mum_age_cat = factor(case_when(Mum_age < 20 ~ 1, Mum_age >= 20 & Mum_age < 25 ~ 2,
                                          Mum_age >= 25 & Mum_age < 30 ~ 3, Mum_age >= 30 & Mum_age < 35 ~ 4,
                                          Mum_age >= 35 & Mum_age < 40 ~ 5,Mum_age > 40 ~ 6),levels = 1:6,
                                labels = c("<20yrs","20 - <25","25 - <30","30 -<35","35 -<40",">=40yrs")),
           BMI = round(weight_peres/(height_peres/100)^2,2)) %>%
    mutate(screen_facility = case_when(scrn_fac_spfy_obsloc == "Ahava Medical Centre" ~ "Ahava Medical \n Center",
                                       scrn_fac_spfy_obsloc == "Kisumu County Hospital" ~ "Kisumu County \n Hospital",
                                       scrn_fac_spfy_obsloc == "Kuoyo Health Center" ~ "Kuoyo Health \n Center",
                                       scrn_fac_spfy_obsloc == "Lumumba Sub County Hospital" ~ "Lumumba Sub \n County Hospital",
                                       scrn_fac_spfy_obsloc == "Siaya County Referral Hospital" ~ "Siaya County \n Referral Hospital",
                                       scrn_fac_spfy_obsloc == "JOOTRH" ~ "JOOTRH"))

# --- Load Data
#Prisma_data <- CleanEasy::load_multipl_df(str_replace(here(),pattern = "Prisma Dashboard","Cleaning scripts/Final Data"))[[1]]

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(
                      title = "HARMONIZED PRiSMA DASHBOARD : (Pregnancy Surveillance)" ,
                      titleWidth = 800,disable = FALSE,
                      dropdownMenu(
                        messageItem(from = "Statistic department",message = "Data updated on 29/11/2022",
                                    time = paste(Sys.time())),
                        messageItem(from = "Stat team",message = "Analysis up-to-date",icon = icon("line-chart"),
                                    time = paste(Sys.time())),
                        type = "messages",badgeStatus = "primary")
                    ),
                    dashboardSidebar( "",
                                      sidebarMenu(
                                        menuItem(
                                          text = "Dashboard 1",tabName = "dashboard1",icon = icon("dashboard"),
                                          menuSubItem(
                                            text = "Info Dashboard",tabName = "subdashboard1",icon = icon("list-alt")
                                          ),
                                          menuSubItem(
                                            text = "Anthropometric Uni-Exploration",tabName = "subdashboard2",icon = icon("bar-chart")
                                          ),
                                          menuSubItem(
                                            text = "Anthropometric Bi-Exploration",tabName = "subdashboard3",icon = icon("line-chart")
                                          )
                                        ),
                                        menuItem(
                                          text = "Data Dashboard",tabName = "dashboard2",icon = icon("dashboard"),
                                          menuSubItem(
                                            text = "Prisma Data Overview",tabName = "subdashboard2a",icon = icon("list-alt")),
                                          menuSubItem(
                                            text = "Prisma Scheduler",tabName = "subdashboard2b",icon = icon("list-alt"))
                                        ),
                                        menuItem(text = "POC Diagnostics",tabName = "dashboard3",icon = icon("dashboard"),
                                                 menuSubItem(text = "Maternal POC Diagnostics",
                                                             tabName = "POC_mat",icon = icon("pie-chart")),
                                                 menuSubItem(text = "Infant POC Diagnostics",
                                                             tabName = "POC_inf",icon = icon("bar-chart"))),
                                        menuItem(text = "Laboratory Results",tabName = "dashboard4",icon = icon("dashboard"),
                                                 menuSubItem(text = "Lab Result Overview",
                                                             tabName = "lab_1",icon = icon("cog")),
                                                 menuSubItem(text = "Maternal lab Results",
                                                             tabName = "Mat_lab",icon = icon("list-alt")),
                                                 menuSubItem(text = "Infant Lab Results",
                                                             tabName = "Inf_lab",icon = icon("line-chart")))
                                      ),
                                      useShinyjs(),
                                      actionButton("refresh", "Refresh",icon = icon("redo")),
                                      submitButton(text = "Apply Changes",icon = icon("angle-double-right"))

                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(shinythemes::themeSelector(),
                                tabName = "subdashboard1",
                                fluidPage(
                                  titlePanel("Summary and Status of the Data :"),
                                  tabsetPanel(
                                    tabPanel(
                                      title = "Summary Reports on Screening and Enrollment :",
                                      icon = icon("pie-chart"),
                                      sidebarLayout(
                                        sidebarPanel(
                                          theme = shinytheme("readable"),
                                          width = 2
                                        ),
                                        mainPanel(h3(strong("SUMMARY OF ENROLLMENT STATUS  :")),
                                                  fluidRow(infoBoxOutput(outputId = "TotScreened"),
                                                           infoBoxOutput(outputId = "TotEnrolled"),
                                                           infoBoxOutput(outputId = "Totdeferred")
                                                  ),
                                                  h3(strong("SUMMARY OF ULTRASOUND GA AT ENROLLMENT:  :")),
                                                  fluidRow(valueBoxOutput(outputId = "LowestGA",width = 5),
                                                           valueBoxOutput(outputId = "HighestGA",width = 5),
                                                  )
                                        )
                                      )
                                    ),
                                    tabPanel(
                                      title = "Cummulative Screening and Enrollment Trend :",
                                      icon = icon("line-chart"),
                                      sidebarLayout(
                                        sidebarPanel(
                                          fluidRow(
                                            box(title = "Select Date Range (Period) :",
                                                #  ---Widget of Date of Screening
                                                textInput(inputId = "start_date",
                                                          label = "Start Date (YMD format) :",
                                                          value = "",
                                                          placeholder = "2022-11-23"),
                                                textInput(inputId = "end_date",
                                                          label = "End Date (YMD format) :",
                                                          value = "",
                                                          placeholder = "2022-12-25"),height = 1,
                                                width = 12,status = "success",collapsible = TRUE,solidHeader = TRUE)
                                          ),width = 3
                                        ),
                                        mainPanel(
                                          fluidRow(
                                            box(title = "Screening & Enrollment Trend :",
                                                ggplot_output(id = "scrnenrol_trend",height = 500),width = 12,
                                                status = "info",collapsible = TRUE,collapsed = FALSE,
                                                solidHeader = TRUE,footer = "The trend of screening and enrolment overtime")
                                          )
                                        )
                                      ),
                                      sidebarLayout(
                                        sidebarPanel(
                                          fluidRow(
                                            box( title = "Select Year & Month :",
                                                 uiOutput(outputId = "years_widget"),
                                                 uiOutput(outputId = "months_widget"),
                                                 submitButton(text = "Execute change",icon("angle-double-right")),height = 1,
                                                 width = 12,status = "warning",collapsible = TRUE,solidHeader = TRUE )
                                          ) ,
                                          width = 3
                                        ),
                                        mainPanel(
                                          box(title = "Trend of Enrolmments and Screening by Months :",
                                              ggplot_output(id = "month_trend",height = 500),width = 12,status = "warning",collapsible = TRUE,collapsed = FALSE,
                                              solidHeader = TRUE,footer = "The trend of screening and enrolment overtime by Months")
                                        )
                                      )
                                    ),
                                    tabPanel(title = "Screened & Enrollment by Facilities",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 fluidRow(
                                                   box(title = "Select Plot Option :",
                                                       uiOutput(outputId = "screen_enrol"),status = "info",width = 12,
                                                       solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                                 ),
                                                 width = 3
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box(title = "Number Screened/Enrolled per facilty:",
                                                       ggplot_output(id = "fac_screened",height = 500),status = "danger",width = 12,
                                                       solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                                 )

                                               )
                                             )
                                    ),
                                    tabPanel(title = "Trend of enrolment and screening By Facilities",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 fluidRow(
                                                   box( title = "Year & Month :",
                                                        uiOutput(outputId = "years_widget_2"),
                                                        uiOutput(outputId = "months_widget_2"),status = "danger",width = 12,
                                                        solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                                 ),
                                                 fluidRow(
                                                   box(title = "Select Plot Option :",
                                                       uiOutput(outputId = "screen_enrol_2"),status = "info",width = 12,
                                                       solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                                 ) ,
                                                 width = 3
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box(title = "Number Screened/Enrolled per facilty (Each Month):",
                                                       ggplot_output(id = "fac_screened_Month",height = 500),status = "primary",width = 12,
                                                       solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                       footer = textOutput(outputId = "selec_mont"))
                                                 )
                                               )
                                             )
                                    )
                                  )
                                )
                        ),
                        tabItem(
                          tabName = "subdashboard2",
                          fluidPage(
                            titlePanel("Graphical Representations :"),

                            tabsetPanel(
                              tabPanel(title = "Women Age Distribution",

                                       sidebarLayout(
                                         sidebarPanel(
                                           fluidRow(
                                             box(title = "Age widget",
                                                 uiOutput(outputId = "age_widget"),
                                                 uiOutput(outputId = "age_Hist_bins"),status = "info",width = 12,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                           ),
                                           width = 2
                                         ),
                                         mainPanel(
                                           fluidRow(
                                             box(title = "Density Plot of Age Distribution",
                                                 ggplot_output(id = "Age_dist"),status = "warning",width = 6,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE
                                             ),
                                             box(title = "Histogram of Age distribution",
                                                 ggplot_output(id = "hist_age_dist"),status = "info",width = 6,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                 background = "olive")
                                           ),
                                           fluidRow(
                                             box(title = "Description of Participants Ages",
                                                 tableOutput(outputId = "age_table"),status = "success",width = 12,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                           ),
                                           fluidRow(
                                             box(title = "Frequency Distribution of Age Categories",
                                                 ggplot_output(id = "Age_cat",height = 500),status = "primary",width = 12,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE)
                                           )
                                         )
                                       )
                              ),
                              tabPanel(title = "Women Weight Distribution",

                                       sidebarLayout(
                                         sidebarPanel(
                                           fluidRow(
                                             box(title = "Widgets",
                                                 uiOutput(outputId = "Weight_widget"),width = 12,solidHeader = TRUE,
                                                 collapsible = TRUE,collapsed = FALSE,status = "success")
                                           ),width = 3),
                                         mainPanel(
                                           fluidRow(
                                             box(title = "Distibution of Weight by ANC visit",
                                                 ggplot_output(id = "weight_cont",height = 500),status = "primary",width = 12,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                           ),fluidRow(
                                             box(title = "Distibution of Weight per ANC visit",
                                                 tableOutput(outputId = "weight_table"), width = 12,solidHeader = TRUE,
                                                 collapsible = TRUE,collapsed = FALSE)
                                           ))
                                       )),
                              tabPanel(title = "Women Muac Distribution",

                                       sidebarLayout(
                                         sidebarPanel(
                                           fluidRow(
                                             box(title = "Widgets",
                                                 uiOutput(outputId = "Muac_widget"),width = 12,solidHeader = TRUE,
                                                 collapsible = TRUE,collapsed = FALSE,status = "info")
                                           ),width = 3),
                                         mainPanel(
                                           fluidRow(
                                             box(title = "Distibution of Muac by ANC visit",
                                                 ggplot_output(id = "Muac_cont",height = 500),status = "warning",width = 12,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                           ),fluidRow(
                                             box(title = "Distibution of Muac per ANC visit",
                                                 tableOutput(outputId = "Muac_table"), width = 12,solidHeader = TRUE,
                                                 collapsible = TRUE,collapsed = FALSE)
                                           ))
                                       )),
                              tabPanel(title = "Women BMI Distribution",

                                       sidebarLayout(
                                         sidebarPanel(
                                           fluidRow(
                                             box(title = "Widgets",
                                                 uiOutput(outputId = "BMI_widget"),width = 12,solidHeader = TRUE,
                                                 collapsible = TRUE,collapsed = FALSE,status = "info")
                                           ),width = 3),
                                         mainPanel(
                                           fluidRow(
                                             box(title = "Distibution of BMI by ANC visit",
                                                 ggplot_output(id = "BMI_cont",height = 500),status = "info",width = 12,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                           ),fluidRow(
                                             box(title = "Distibution of BMI per ANC visit",
                                                 tableOutput(outputId = "BMI_table"), width = 12,solidHeader = TRUE,
                                                 collapsible = TRUE,collapsed = FALSE)
                                           ))
                                       )),
                              tabPanel(title = "Women Heights Distribution",

                                       sidebarLayout(
                                         sidebarPanel(
                                           fluidRow(
                                             box(title = "Widgets",
                                                 uiOutput(outputId = "height_widget"),width = 12,solidHeader = TRUE,
                                                 collapsible = TRUE,collapsed = FALSE,status = "danger")
                                           ),width = 3),
                                         mainPanel(
                                           fluidRow(
                                             box(title = "Distibution of Heights",
                                                 ggplot_output(id = "height_cont",height = 500),status = "danger",width = 12,
                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                           ),fluidRow(
                                             box(title = "Distibution of Height (table format)",
                                                 tableOutput(outputId = "height_table"), width = 12,solidHeader = TRUE,
                                                 collapsible = TRUE,collapsed = FALSE)
                                           ))
                                       ))
                            )
                          )
                        ),
                        tabItem(tabName = "subdashboard3",
                                fluidPage(
                                  titlePanel("Bivariates :"),
                                  tabsetPanel(
                                    tabPanel( title = "Anthropometrics Bivariates",
                                              box(title = "Test of Association between BMI $ Muac",width = 10,status = "info",
                                                  ggplot_output(id = "bmi_muac",height = 600),solidHeader = TRUE,collapsible = TRUE)
                                    )
                                  )
                                )
                        ),
                        tabItem(tabName = "subdashboard2a",
                                fluidPage(
                                  titlePanel("Data Preview :"),
                                  shinythemes::shinytheme("readable"),

                                  tabsetPanel(
                                    tabPanel(title = "PRiSMA dataset",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 fluidRow(
                                                   box(title = "Choose Visite Type |& facility :",
                                                       uiOutput(outputId = "anc_pnc_widget"),
                                                       uiOutput(outputId = "Visit_type_widget"),
                                                       uiOutput(outputId = "facility_widget"),width = 12,status = "warning",
                                                       solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                                 ),
                                                 width = 3
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box(title = "Prisma Dataset Preview :",
                                                       dataTableOutput(outputId = "data_1"),width = NULL,status = "success",
                                                       solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE)
                                                 )
                                               )
                                             )
                                    )
                                  )
                                )
                        ),
                        tabItem(tabName = "subdashboard2b",
                                fluidPage(
                                  titlePanel("SCHEDULER download:"),
                                  #shinythemes::shinytheme("readable"),

                                  tabsetPanel(
                                    tabPanel(title = "PRiSMA Scheduler",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 fluidRow(box( title = "Click to Download !",
                                                               downloadButton('downloadData', 'Download data'),
                                                               width = 12,background = "red")),
                                                 width = 2
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "scheduler"))
                                               )
                                             )
                                    )
                                  )
                                )
                        ),
                        tabItem(tabName = "POC_mat",
                                fluidPage(
                                  titlePanel("Maternal Point-of-Care (PoC) Diagnostics :"),
                                  shinythemes::shinytheme("readable"),

                                  tabsetPanel(
                                    tabPanel(title = "Systolic & diastolic results",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "100px",
                                                        downloadButton('dwnld_sys_dia_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "sys_dia")),
                                                 fluidRow(
                                                   box(title = "Select Visit type:",
                                                       uiOutput(outputId = "sys_dia_widget"),height = "100px",
                                                       width = 3,status = "info",solidHeader = T,collapsible = T),
                                                 ),submitButton(),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "dist_table"))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Heart Rates/Resp Rates/Oxygen saturation etc",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "100px",
                                                        downloadButton('dwnld_hr_rr_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "hr_rr")),
                                                 fluidRow(
                                                   box(title = "Select Visit type:",
                                                       uiOutput(outputId = "hr_widget"),height = "100px",
                                                       width = 3,status = "warning",solidHeader = T,collapsible = T),
                                                 ),submitButton(),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "hr_dist_table"))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Hb POC Results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "100px",
                                                        downloadButton('dwnld_hb_bg_missng', 'Download'))),
                                                 fluidRow(box(width = 6,status = "warning",solidHeader = T,collapsible = T,
                                                              DT::dataTableOutput(outputId = "hb_bg"))),
                                                 fluidRow(
                                                   box(title = "Select Visit type:",
                                                       uiOutput(outputId = "hb_bg_widget"),height = "100px",
                                                       width = 3,status = "warning",solidHeader = T,collapsible = T),
                                                 ),submitButton(),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "hb_bg_dist_table"))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Malaria/syphilis/HIV/HBV/HCV/Covid results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "100px",
                                                        downloadButton('dwnld_mal_hiv_missng', 'Download'))),
                                                 fluidRow(box(width = 11,status = "warning",solidHeader = T,collapsible = T,
                                                              DT::dataTableOutput(outputId = "mal_hiv_"))),
                                                 fluidRow(
                                                   box(title = "Select Visit type:",
                                                       uiOutput(outputId = "mal_hiv_widget"), height = "100px",
                                                       width = 3,status = "warning",solidHeader = T,collapsible = T),
                                                 ),
                                                 fluidRow(
                                                   box(title = "PoC Key diagnostics variable completeness",
                                                       ggplot_output(id = "Mal_syph",height = "550px"),width = 12,
                                                       status = "info",solidHeader = T,collapsible = T)
                                                 ),
                                                 fluidRow(box(title = "Distribution of PoC Diagnostics results :",
                                                              gt_output(outputId = "mal_hiv_dist_table"),width = 12))
                                               )
                                             )
                                    )
                                  )
                                )
                        ),
                        tabItem(tabName = "lab_1",
                                fluidPage(
                                  titlePanel("Laboratory Components Completeness :"),
                                  shinythemes::shinytheme("readable"),
                                  h2(strong("Maternal Laboratory Results :")),

                                  sidebarLayout(
                                    sidebarPanel(
                                      fluidRow(
                                        uiOutput(outputId = "lab_comp_widget")),
                                      width = 2
                                    ),
                                    mainPanel(
                                      fluidRow(
                                        #box(title = "Results Completeness :",
                                        ggplot_output(id = "lab_com",height = "650px",width = "110%")
                                        #width = 12,status = "warning",solidHeader = T,collapsible = T)
                                      )
                                    )
                                  ),
                                  h2(strong("Infant Laboratory Results :")),
                                  sidebarLayout(
                                    sidebarPanel(width = 2
                                    ),
                                    mainPanel(
                                      fluidRow(
                                        box(title = "Results Completeness :",
                                            h5(strong(" No infant data.. No infant script::!!! Yet")),
                                            width = 12,status = "success",solidHeader = T,collapsible = T,collapsed = TRUE)
                                      )
                                    )
                                  )
                                )
                        ),

                        tabItem(tabName = "Mat_lab",
                                fluidPage(
                                  titlePanel("Maternal Laboratory Results :"),
                                  theme = shinythemes::shinytheme("sandstone"),

                                  tabsetPanel(
                                    tabPanel(title = "CBC Results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "40px",
                                                        downloadButton('dwnld_cbc_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "cbc")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "cbc_paleto",height = "650px"),
                                                       width = 12,status = "warning",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Urinalysis Results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "40px",
                                                        downloadButton('dwnld_ur_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "ur")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "UR_paleto",height = "650px"),
                                                       width = 12,status = "success",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Kidney Function Test :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "40px",
                                                        downloadButton('dwnld_kft_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "kft")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "KFT_paleto",height = "650px"),
                                                       width = 12,status = "primary",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Liver Function Test :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "40px",
                                                        downloadButton('dwnld_lft_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "lft")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "LFT_paleto",height = "650px"),
                                                       width = 12,status = "info",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Micronutrient Test Results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "40px",
                                                        downloadButton('dwnld_Micronut_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "Micronut")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "Micronutrient_paleto",height = "650px"),
                                                       width = 12,status = "info",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Thyroid Panel Results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "40px",
                                                        downloadButton('dwnld_thyroid_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "thyroid")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "Thyroid_paleto",height = "650px"),
                                                       width = 11,status = "primary",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Malaria Laboratory Results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "40px",
                                                        downloadButton('dwnld_malaria_lab_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "malaria_lab")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "Malaria_paleto",height = "650px"),
                                                       width = 12,status = "primary",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Schistosomiasis Test Results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "50px",
                                                        downloadButton('dwnld_Schisto_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "Schisto")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "Schisto_paleto",height = "650px"),
                                                       width = 11,status = "primary",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    ),
                                    tabPanel(title = "Helminths Test Results :",

                                             sidebarLayout(
                                               sidebarPanel(
                                                 width = 1
                                               ),
                                               mainPanel(
                                                 fluidRow(
                                                   box( title = "Missing Results Query",height = "50px",
                                                        downloadButton('dwnld_helminths_missng', 'Download'))),
                                                 fluidRow(
                                                   DT::dataTableOutput(outputId = "helminths")),
                                                 fluidRow(
                                                   box(title = "Paleto chart :",
                                                       ggplot_output(id = "Helminths_paleto",height = "650px"),
                                                       width = 11,status = "warning",solidHeader = T,collapsible = T))
                                               )
                                             )
                                    )
                                  )
                                )
                        )
                      )
                    )

)

#--Example of background color:: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.


server <- function(input, output, session) {

  observeEvent(input$refresh,{
    shinyjs::refresh()
  })

  # ----Total Screened
  output$TotScreened <- renderInfoBox({
    infoBox(
      title = "Total Screened :",
      value = Prisma_data %>% drop_na(scrnid) %>% distinct(scrnid) %>% pull(scrnid) %>% length(),
      subtitle = "Number of Screened women",
      icon = icon("angle-double-right"),
      fill = TRUE,
    )
  })

  # ---- Total Enrollments
  output$TotEnrolled <- renderInfoBox({
    infoBox(
      title = "Total enrollement",
      value = paste0(Prisma_data %>% drop_na(scrnid) %>% distinct(momid) %>% drop_na() %>% pull(momid) %>% length(),
                     "(",round(Prisma_data %>% distinct(momid) %>% drop_na() %>% pull(momid) %>% length()/(
                       Prisma_data %>% distinct(scrnid) %>% pull(scrnid) %>% length() -
                         Prisma_data %>% filter(othr_reason_ieorres == "Deferred—to come for enrollment on later date") %>%
                         drop_na(scrnid) %>% pull(scrnid) %>% length())*100,1),"%)"),
      subtitle = "Based on total screened",
      fill = TRUE,color = "yellow"
    )
  })


  # ----- Number Deferred
  output$Totdeferred <- renderInfoBox({
    infoBox(
      title = "Total Deferred",
      value = paste0((Prisma_data %>% filter(othr_reason_ieorres == "Deferred—to come for enrollment on later date") %>%
                        drop_na(scrnid) %>% pull(scrnid) %>% length()),
                     "(",round((Prisma_data %>% filter(othr_reason_ieorres == "Deferred—to come for enrollment on later date") %>%
                                  drop_na(scrnid) %>% pull(scrnid) %>% length())/(
                                    Prisma_data %>% drop_na(scrnid) %>% distinct(scrnid) %>% pull(scrnid) %>% length() -
                                      Prisma_data %>% distinct(momid) %>% drop_na() %>% pull(momid) %>% length() )*100,1),"%)"),
      subtitle = "Based on total screened",
      fill = TRUE,color = "red",
      icon = icon("credit-card")
    )
  })


  # ----- Number Unenrolled
  output$TotUnenrolled <- renderInfoBox({
    infoBox(
      title = "Total Unenrolled",
      value = paste0((Prisma_data %>% drop_na(scrnid) %>% distinct(scrnid) %>% pull(scrnid) %>% length() -
                        Prisma_data %>% distinct(momid) %>% drop_na() %>% pull(momid) %>% length()),
                     "(",round((Prisma_data %>% distinct(scrnid) %>% pull(scrnid) %>% length() -
                                  Prisma_data %>% distinct(momid) %>% drop_na() %>% pull(momid) %>% length())/
                                 Prisma_data %>% distinct(scrnid) %>% pull(scrnid) %>% length()*100,1),"%)"),
      subtitle = "Based on total screened",
      fill = TRUE,color = "red",
      icon = icon("credit-card")
    )
  })

  # ---- Lowest at enrollment GA
  output$LowestGA <- renderValueBox({
    valueBox(
      value = paste0(Prisma_data %>% filter(type_visit =="Enrollment") %>% drop_na(momid) %>%
                       distinct(momid,.keep_all = TRUE) %>%
                       mutate(us_ga_screening = round(((us_ga_wks_age_fts1*7) + us_ga_days_age_fts1)/7,1)) %>%
                       pull(us_ga_screening) %>% min(na.rm = TRUE)," (weeks Old)"),
      subtitle = "Current Lowest GA at Enrollment by Ultrasound",
      color = "purple",icon = icon("arrow-down")
    )
  })

  # ---- Highest GA  at enrollment
  output$HighestGA <- renderValueBox({
    valueBox(
      value = paste0(Prisma_data %>% filter(us_visit =="Screening") %>% drop_na(momid) %>%
                       distinct(momid,.keep_all = TRUE) %>%
                       mutate(us_ga_screening = round(((us_ga_wks_age_fts1*7) + us_ga_days_age_fts1)/7,1)) %>%
                       select(us_ga_screening) %>%
                       pull(us_ga_screening) %>% max(na.rm = TRUE)," (weeks Old)"),
      subtitle = "Current Highest GA at Enrollment by Ultrasound",
      color = "green",icon = icon("arrow-up")
    )
  })


  # ------ Trend of Screening and Enrollment
  render_ggplot("scrnenrol_trend",{
    Screen_dat = Prisma_data %>% select(scrnid,scrn_obsstdat,momid,con_dsstdat) %>% drop_na(scrnid) %>%
      distinct(scrnid,.keep_all = T) %>%
      group_by(scrn_obsstdat) %>% summarise(freq1 = n()) %>% ungroup() %>% mutate(freq = cumsum(freq1))
    Enrol_dat = Prisma_data %>% select(scrnid,scrn_obsstdat,momid,con_dsstdat) %>% drop_na(momid) %>%
      distinct(momid,.keep_all = T) %>%
      group_by(con_dsstdat) %>% summarise(freq1 = n()) %>% ungroup() %>% mutate(freq = cumsum(freq1))

    Screen_dat  %>% ggplot(aes(x = scrn_obsstdat,y = freq)) +
      geom_line(aes(y = freq,color = "Screened"),group = 1, size = 1.3,data = Screen_dat) +
      geom_line(aes( y = freq,x = con_dsstdat,color = "Enrollments"),group = 1, size = 1.3,data = Enrol_dat) +
      geom_text(aes(y = freq, x = scrn_obsstdat,label = freq),vjust = -0.5,data = Screen_dat) +
      geom_text(aes(y = freq, x = con_dsstdat,label = freq),vjust = -0.5,data = Enrol_dat) +
      theme_classic() + labs(title = paste("Cummalative Number Screened verses Enrolled \n between ",
                                           input$start_date," to ",input$end_date),
                             y = "Counts of Participants",
                             x = "Period (Duration)") +
      theme(plot.title = element_text(hjust = 0.4,face = "bold",size = 18),
            axis.title = element_text(size = 14,face = "italic",colour = "purple"),
            axis.text = element_text(size = 14,face = "bold",colour = "black"),
            legend.text = element_text(face = "bold",size = 13),
            legend.position = "top") +
      scale_color_manual(
        name = "",values = c("Screened" = "#CD3700", "Enrollments" = "#008B8B")) +
      scale_x_date(limits = c(ymd(input$start_date),ymd(input$end_date)))

  })


  # --- Screening and Enrollment Trends By Months
  render_ggplot(id ="month_trend",{
    Enrol_data2 = Prisma_data %>% mutate(YEAR = year(con_dsstdat),MONTH = months(con_dsstdat),
                                         month_year = zoo::as.yearmon(con_dsstdat))%>%
      drop_na(momid) %>% distinct(momid,.keep_all = T) %>%
      select(momid,YEAR,MONTH,month_year) %>% filter(YEAR == as.numeric(input$year_s) & MONTH %in%c(input$month_s)) %>%
      mutate(MONTH = factor(MONTH),
             MONTH = fct_relevel(MONTH,ref = "January","February","March","April","May","June","July","August",
                                 "September","October","November","December")) %>%
      group_by(MONTH) %>% summarise(freq = n()) %>% ungroup()


    Enrol_data2  %>% ggplot(aes(x = MONTH,y = freq)) +
      geom_line(aes(color = "Enrollments"),group = 1, size = 1.3) +
      theme_classic() + labs(title = "Enrollments Trend \n by Months",
                             y = "Counts of Participants",
                             x = "Period (Months)") +
      theme(plot.title = element_text(hjust = 0.4,face = "bold",size = 19),
            axis.title = element_text(size = 16,face = "italic",colour = "red"),
            axis.text = element_text(size = 16,face = "bold",colour = "black"),
            legend.text = element_text(face = "bold",size = 13),
            legend.position = "top") +
      scale_color_manual(  # ----- "Screened" = "#7CFC00",
        name = "",values = c("Enrollments" = "#CD00CD"))
  })



  # ---- Screening & Enrollments by Facilities
  render_ggplot("fac_screened",{
    if(input$screened == "Screened"){
      Prisma_data %>% distinct(scrnid,.keep_all = TRUE) %>% drop_na(scrnid) %>%
        group_by(screen_facility) %>%
        summarise(freq = n()) %>% ungroup() %>%
        mutate(prop = round(freq/sum(freq),3)) %>% drop_na() %>%
        ggplot(aes(x = screen_facility, y = prop)) +
        geom_col(width = 0.4,fill = c("#7B68EE")) + ggpubr::theme_pubclean() +
        geom_text(aes(label = freq),position = position_dodge(0.9),
                  size = 5.2,vjust = 1.5,color = "black") +
        geom_text(aes(label = paste0("(",prop*100,"%)")),position = position_dodge(0.9),
                  size = 5.2,vjust = 3,color = "black",alpha =2) +
        labs(title = " Total number Screened by Each Facility",
             y = " Proportion screened",
             x = " Facilities") +
        theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 19),
              axis.text = element_text(color = "black",face = "bold",size = 15),
              axis.title = element_text(face = "italic",size = 15),
              legend.text = element_text(face = "italic",size = 13)) +
        scale_y_continuous(labels = scales::percent)
    }else if(input$screened == "Enrolled"){
      Prisma_data %>% distinct(momid,.keep_all = TRUE) %>%  drop_na(momid) %>%
        group_by(screen_facility) %>%
        summarise(freq = n()) %>% ungroup() %>%
        mutate(prop = round(freq/sum(freq),3)) %>% drop_na() %>%
        ggplot(aes(x = screen_facility, y = prop)) +
        geom_col(width = 0.4,fill = c("#008B45")) + ggpubr::theme_pubclean() +
        geom_text(aes(label = freq),position = position_dodge(0.9),
                  size = 5.2,vjust = 1.5,color = "black") +
        geom_text(aes(label = paste0("(",prop*100,"%)")),position = position_dodge(0.9),
                  size = 5.2,vjust = 3,color = "black",alpha =2) +
        labs(title = " Total number of Enrollments by Each Facility",
             subtitle = paste0("Total Enrollments (N) = ",Prisma_data %>% drop_na(scrnid) %>% distinct(momid) %>%
                                 drop_na() %>% pull(momid) %>% length()),
             y = " Proportion enrolled",
             x = " Facilities") +
        theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 19),
              axis.text = element_text(color = "black",face = "bold",size = 15),
              axis.title = element_text(face = "italic",size = 15),
              legend.text = element_text(face = "italic",size = 13)) +
        scale_y_continuous(labels = scales::percent)
    }else if(input$screened == "Enrolled & Unenrolled proportion"){
      Prisma_data %>% distinct(scrnid,.keep_all = TRUE) %>%  drop_na(scrnid) %>%
        select(scrnid,momid,screen_facility) %>%
        mutate(enrol = if_else(is.na(momid),"Unenrolled","Enrolled")) %>%
        group_by(screen_facility,enrol) %>%
        summarise(freq = n()) %>% ungroup() %>% group_by(screen_facility) %>%
        mutate(prop = round(freq/sum(freq),3)) %>% drop_na() %>%
        ggplot(aes(x = screen_facility, y = prop, fill = enrol)) +
        geom_col(width = 0.4) + ggpubr::theme_pubclean() +
        geom_text(aes(label = freq),position = position_stack(0.9),
                  size = 5.2,vjust = 1.5,color = "black") +
        geom_text(aes(label = paste0("(",prop*100,"%)")),position = position_stack(0.9),
                  size = 5.2,vjust = 3,color = "black",alpha =2) +
        labs(title = " Distribution of Enrollment status by Each Facility",
             y = " Proportion Enrollments status",
             x = " Facilities",fill = "") +
        theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 19),
              axis.text = element_text(color = "black",face = "bold",size = 15),
              axis.title = element_text(face = "italic",size = 15),
              legend.text = element_text(face = "italic",size = 13)) +
        scale_y_continuous(labels = scales::percent)
    }
  })



  # ---- Screening & Enrollments by Facilities Per Month per year
  render_ggplot("fac_screened_Month",{
    if(input$screened_2 == "Screened"){
      Prisma_data %>% mutate(YEAR = year(scrn_obsstdat),MONTH = months(scrn_obsstdat),
                             month_year = zoo::as.yearmon(scrn_obsstdat)) %>%
        drop_na(scrnid) %>% distinct(scrnid,.keep_all = T) %>%
        select(scrnid,YEAR,MONTH,month_year,screen_facility) %>%
        filter(YEAR %in%c(as.numeric(input$year_s_2)) & MONTH %in%c(input$month_s_2)) %>%
        group_by(screen_facility) %>%
        summarise(freq = n()) %>% ungroup() %>%
        mutate(prop = round(freq/sum(freq),3)) %>% drop_na() %>%
        ggplot(aes(x = screen_facility, y = prop)) +
        geom_col(width = 0.4,fill = c("#00EE76")) + ggpubr::theme_pubclean() +
        geom_text(aes(label = freq),position = position_stack(0.9),
                  size = 5.3,vjust = 1.5,color = "black") +
        geom_text(aes(label = paste0("(",prop*100,"%)")),position = position_stack(0.9),
                  size = 5.3,vjust = 3,color = "black",alpha =2) +
        labs(title =paste0("Total number Screened by Each Facility \n","(",input$month_s_2," ",input$year_s_2,")"),
             y = " Proportion screened",
             x = " Facilities") +
        theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 19),
              axis.text = element_text(color = "black",face = "bold",size = 15),
              axis.title = element_text(face = "italic",size = 15),
              legend.text = element_text(face = "italic",size = 13)) +
        scale_y_continuous(labels = scales::percent)
    }else if(input$screened_2 == "Enrolled"){
      Enrl_mth_data <- Prisma_data %>% mutate(YEAR = year(scrn_obsstdat),MONTH = months(scrn_obsstdat),
                                              month_year = zoo::as.yearmon(scrn_obsstdat)) %>%
        drop_na(momid) %>% distinct(momid,.keep_all = T) %>%
        select(momid,YEAR,MONTH,month_year,screen_facility)
      Enrl_mth_data %>%
        filter(YEAR %in%c(as.numeric(input$year_s_2)) & MONTH %in%c(input$month_s_2)) %>%
        group_by(screen_facility) %>%
        summarise(freq = n()) %>% ungroup() %>%
        mutate(prop = round(freq/sum(freq),3)) %>% drop_na() %>%
        ggplot(aes(x = screen_facility, y = prop)) +
        geom_col(width = 0.4,fill = c("#009ACD")) + ggpubr::theme_pubclean() +
        geom_text(aes(label = freq),position = position_stack(0.9),
                  size = 5.3,vjust = 1.5,color = "black") +
        geom_text(aes(label = paste0("(",prop*100,"%)")),position = position_stack(0.9),
                  size = 5.3,vjust = 3,color = "black",alpha =2) +
        labs(title = paste0("Enrollments status by Each Facility \n","(",input$month_s_2," ",input$year_s_2,")"),
             subtitle = paste0("Total Enrollments(N) = ",Enrl_mth_data %>%
                                 filter(YEAR %in%c(as.numeric(input$year_s_2)) & MONTH %in%c(input$month_s_2)) %>%
                                 pull(momid) %>% length()),
             y = " Proportion enrolled",
             x = " Facilities") +
        theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 19),
              axis.text = element_text(color = "black",face = "bold",size = 15),
              axis.title = element_text(face = "italic",size = 15),
              legend.text = element_text(face = "italic",size = 13)) +
        scale_y_continuous(labels = scales::percent)
    }else if(input$screened_2 == "Enrolled & Unenrolled proportion"){
      Prisma_data %>% mutate(YEAR = year(scrn_obsstdat),MONTH = months(scrn_obsstdat),
                             month_year = zoo::as.yearmon(scrn_obsstdat)) %>%
        drop_na(scrnid) %>% distinct(scrnid,.keep_all = T) %>%
        select(scrnid,YEAR,MONTH,month_year,screen_facility,momid) %>%
        mutate(enrol = if_else(is.na(momid),"Unenrolled","Enrolled")) %>%
        filter(YEAR %in%c(as.numeric(input$year_s_2)) & MONTH %in%c(input$month_s_2)) %>%
        group_by(screen_facility,enrol) %>%
        summarise(freq = n()) %>% ungroup() %>% group_by(screen_facility) %>%
        mutate(prop = round(freq/sum(freq),3)) %>% drop_na() %>%
        ggplot(aes(x = screen_facility, y = prop, fill = enrol)) +
        geom_col(width = 0.4) + ggpubr::theme_pubclean() +
        geom_text(aes(label = freq),position = position_stack(0.9),
                  size = 5.3,vjust = 1.5,color = "black") +
        geom_text(aes(label = paste0("(",prop*100,"%)")),position = position_stack(0.9),
                  size = 5.3,vjust = 3,color = "black",alpha =2) +
        labs(title =paste0("Enrollments status by Each Facility \n","(",input$month_s_2," ",input$year_s_2,")"),
             y = " Proportion of Enrollents status",
             x = " Facilities", fill = "") +
        theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 19),
              axis.text = element_text(color = "black",face = "bold",size = 15),
              axis.title = element_text(face = "italic",size = 15),
              legend.text = element_text(face = "italic",size = 13)) +
        scale_fill_manual(values = c("#CDCD00", "#B8860B")) +
        scale_y_continuous(labels = scales::percent)
    }
  })


  # ----- Years widget selection 2 for facility
  output$years_widget_2 <- renderUI({
    selectInput(inputId = "year_s_2",
                label = "Choose Year :",
                choices = list("2022" = 2022,"2023" = 2023,"2024"  = 2024,"2025" = 2025,"2026" = 2026,
                               "2027" = 2027,"2028" = 2028,"2029" = 2029,"2030" = 2030),
                multiple = FALSE,selected = 2022)
  })

  # --- Months widget selection 2 for facility
  output$months_widget_2 <- renderUI({
    selectInput(inputId = "month_s_2",
                label = "Choose Month :",
                choices = list("January","February","March","April","May","June","July","August",
                               "September","October","November","December"),
                multiple = FALSE,selected = "November")
  })

  #----  widget for Screening & Enrollments for facility
  output$screen_enrol_2 <- renderUI({
    screened <- c("Screened","Enrolled","Enrolled & Unenrolled proportion")
    selectInput(inputId = "screened_2",
                label = "Choose what to plot :",
                choices = list("Screened","Enrolled","Enrolled & Unenrolled proportion"),
                selected = "Screened")
  })

  #---- selected months for facility footer
  output$selec_mont <- renderPrint({
    input$month_s_2
  })

  #-----  Distribution of age continuous
  render_ggplot("Age_dist",{
    Prisma_data %>% mutate(Mum_age = round(as.numeric(scrn_obsstdat - brthdat)/365,2)) %>%
      drop_na(momid) %>% distinct(momid,.keep_all = T) %>%
      drop_na(Mum_age) %>% ggplot(aes(x = Mum_age)) +
      geom_density(stat = "density",color = "#EE9A00") + theme_classic() +
      labs(title = "Distribution of Ages at Enrolment",x = "Ages") +
      theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 16),
            axis.text = element_text(color = "black",face = "bold",size = 14),
            axis.title = element_text(face = "italic",size = 14)) +
      scale_x_continuous(limits = input$Mum_age)
  })


  # --- Distribution of age categories
  render_ggplot("Age_cat",{
    Prisma_data %>%  drop_na(momid) %>% filter(type_visit == "Enrollment") %>%
      drop_na(Mum_age_cat) %>% group_by(Mum_age_cat) %>% summarise(freq = n()) %>% ungroup() %>%
      mutate(prop = round(freq/sum(freq),3)) %>%
      ggplot(aes(x = Mum_age_cat, y = prop)) +
      geom_col(width = 0.4,fill = c("#00C5CD")) + ggpubr::theme_pubclean() +
      geom_text(aes(label = freq),position = position_dodge(0.9),
                size = 4.8,vjust = 1.5,color = "red") +
      geom_text(aes(label = paste0("(",prop*100,"%)")),position = position_dodge(0.9),
                size = 4.8,vjust = 3,color = "black",alpha =2) +
      labs(title = " Distribution of Age categories",
           y = " Age Distribution",
           x = " Age Categories") +
      theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 18),
            axis.text = element_text(color = "black",face = "bold",size = 14),
            axis.title = element_text(face = "italic",size = 15),
            legend.text = element_text(face = "italic",size = 13))
  })


  #-----  Distribution of age continuous  histogram       #  ---- Activate drop na momid to only view age of enrolled cases
  render_ggplot("hist_age_dist",{
    Prisma_data %>% mutate(Mum_age = round(as.numeric(scrn_obsstdat - brthdat)/365,2)) %>%
      drop_na(momid) %>% filter(type_visit == "Enrollment") %>%
      drop_na(Mum_age) %>% ggplot(aes(x = Mum_age)) +
      geom_histogram(bins = input$bins,color = "#EE9A00") + theme_classic() +
      labs(title = "Distribution of Ages at Enrolment",x = "Ages") +
      theme(plot.title = element_text(hjust = 0.5,color = "black",face = "bold",size = 16),
            axis.text = element_text(color = "black",face = "bold",size = 14),
            axis.title = element_text(face = "italic",size = 14),
            legend.text = element_text(face = "italic",size = 13)) +
      scale_x_continuous(limits = input$Mum_age)
  })


  # --- Age distribution table
  output$age_table <- renderTable({
    Prisma_data %>% mutate(Mum_age = round(as.numeric(scrn_obsstdat - brthdat)/365,2)) %>%
      drop_na(momid) %>% filter(type_visit == "Enrollment") %>% distinct(momid,.keep_all = T) %>%
      drop_na(Mum_age) %>% select(Mum_age) %>%
      dlookr::describe(quantiles = c(0.00,0.25,0.5,0.75,1))%>%
      rename("N" = n,"NA" = na,"Characteristic" = described_variables,
             min = "p00",max ="p100",median = "p50",Q1 = "p25",Q3 = "p75")
  })


  #   ---- Widget for age
  output$age_widget <- renderUI({
    sliderInput(inputId = "Mum_age",
                label = "Mum Age Range :",
                min = min(Prisma_data$Mum_age,na.rm = TRUE),
                max = max(Prisma_data$Mum_age,na.rm = TRUE),
                value = c(min(Prisma_data$Mum_age,na.rm = TRUE),
                          max(Prisma_data$Mum_age,na.rm = TRUE)),
                sep = "")
  })

  output$age_Hist_bins <- renderUI({
    sliderInput(inputId =  "bins",
                label = "Number of hist bins :",
                min = 1,
                max = 100,
                value = 30)
  })

  #----  widget for Screening & Enrollments
  output$screen_enrol <- renderUI({
    screened <- c("Screened","Enrolled")
    selectInput(inputId = "screened",
                label = "Choose what to plot :",
                choices = list("Screened","Enrolled","Enrolled & Unenrolled proportion"),
                selected = "Screened")
  })


  # -----  Facility Widget
  output$facility_widget <- renderUI({
    selectInput(inputId = "scrn_fac_spfy_obsloc",
                label = "Screened Facility",
                choices = list(Prisma_data$scrn_fac_spfy_obsloc,All = Prisma_data$scrn_fac_spfy_obsloc),
                selected = "JOOTRH",
                multiple = TRUE

    )
  })

  # ----- Years widget selection
  output$years_widget <- renderUI({
    selectInput(inputId = "year_s",
                label = "Choose Year :",
                choices = list("2022" = 2022,"2023" = 2023,"2024"  = 2024,"2025" = 2025,"2026" = 2026,
                               "2027" = 2027,"2028" = 2028,"2029" = 2029,"2030" = 2030),
                multiple = FALSE,selected = 2022)
  })

  # --- Months widget selection
  output$months_widget <- renderUI({
    selectInput(inputId = "month_s",
                label = "Choose Month :",
                choices = list("January","February","March","April","May","June","July","August",
                               "September","October","November","December"),
                multiple = TRUE,selected = "November")
  })


  output$anc_pnc_widget <- renderUI({
    anc_pnc <- c("ANC","PNC")
    selectInput(inputId = "anc_pnc",
                label = "Choose Visit type :",
                choices = list("ANC","PNC"),
                selected = "ANC")
  })


  # ---- Visit type widget
  output$Visit_type_widget <- renderUI({
    if(input$anc_pnc == "ANC"){
      selectInput(inputId = "type_visit",
                  label = "ANC Visit Type :",
                  choices = list(Enrollment = "Enrollment",`ANC-20` = "ANC-20",`ANC-28` = "ANC-28",`ANC-32` = "ANC-32",
                                 `ANC-36` = "ANC-36",`IPC (labor &delivery)` = "IPC (labor &delivery)"),
                  selected = "Enrollment",
                  multiple = TRUE
      )
    }else if(input$anc_pnc == "PNC"){
      selectInput(inputId = "type_visit",
                  label = "PNC Visit Type :",
                  choices = list(`PNC-0` = "PNC-0",`PNC-1` = "PNC-1",`PNC-4` = "PNC-4",`PNC-6` = "PNC-6",`PNC-26` = "PNC-26",
                                 `PNC-52` = "PNC-52"),
                  selected = "PNC-0",
                  multiple = TRUE
      )
    }
  })


  # ---- Facility widget
  output$facility_widget <- renderUI({
    selectInput(inputId = "scrn_fac_spfy_obsloc",
                label = "Screened Facility",
                choices = list(JOOTRH = "JOOTRH",
                               `Ahava Medical Centre` = "Ahava Medical Centre",
                               `Kuoyo Health Center` = "Kuoyo Health Center",
                               `Kisumu County Hospital` = "Kisumu County Hospital",
                               `Siaya County Referral Hospital` = "Siaya County Referral Hospital",
                               `Lumumba Sub County Hospital` = "Lumumba Sub County Hospital"),
                selected = "JOOTRH",
                multiple = TRUE
    )
  })

  # --- Data set

  output$data_1 <- renderDataTable({
    if(input$anc_pnc == "ANC"){
      Prisma_data %>% filter(type_visit %in%c(input$type_visit),
                             scrn_fac_spfy_obsloc %in%c(input$scrn_fac_spfy_obsloc)) %>%
        select(momid,scrn_obsstdat,con_dsstdat,scrn_fac_spfy_obsloc,type_visit,
               us_ga_wks_age_fts1,us_ga_days_age_fts1) %>%
        rename("Screened date" = scrn_obsstdat,"Facility" = scrn_fac_spfy_obsloc,
               "Visit type" = type_visit,"GA wks" = us_ga_wks_age_fts1,
               "GA days" = us_ga_days_age_fts1,"Mother ID" = momid,"Consent Date" =con_dsstdat)
    }else if(input$anc_pnc == "PNC"){
      Prisma_data %>% filter(type_visit %in%c(input$type_visit),
                             scrn_fac_spfy_obsloc %in%c(input$scrn_fac_spfy_obsloc)) %>%
        select(momid,scrn_obsstdat,con_dsstdat,scrn_fac_spfy_obsloc,type_visit,
               us_ga_wks_age_fts1,us_ga_days_age_fts1) %>%
        rename("Screened date" = scrn_obsstdat,"Facility" = scrn_fac_spfy_obsloc,
               "Visit type" = type_visit,"GA wks" = us_ga_wks_age_fts1,
               "GA days" = us_ga_days_age_fts1,"Mother ID" = momid,"Consent Date" =con_dsstdat)
    }
  })

  #  --- Weight distribution by ANC Visit type
  render_ggplot(id = "weight_cont",{
    Prisma_data %>% select(type_visit,weight_peres) %>%
      filter(type_visit %in%c("Enrollment","ANC-20","ANC-28","ANC-32","ANC-36","IPC (labor &delivery)")) %>%
      ggplot(aes(x = weight_peres,color = type_visit)) +
      geom_density(size = 1.5) + theme_classic() + labs(title = "Density Plot of Mothers Weight by each ANC visit",
                                                        x = "Mothers Weight", y = "Density", color = "") +
      theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
            legend.text = element_text(size = 12,),legend.position = "top",
            axis.title = element_text(size = 15,face = "bold"),
            axis.text =  element_text(size = 15,face = "italic")) +
      scale_color_manual(values = c("#006400", "#ADFF2F", "#FFFF00", "#228B22")) +
      scale_x_continuous(limits = input$weight_peres)
  })

  # --- Weight widget distribution
  output$Weight_widget <- renderUI({
    sliderInput(inputId = "weight_peres",label = "Weight Widget",
                min = min(Prisma_data$weight_peres,na.rm = T),
                max = max(Prisma_data$weight_peres,na.rm = T),
                value = c(min(Prisma_data$weight_peres,na.rm = T),max(Prisma_data$weight_peres,na.rm = T)),
                sep = "")
  })

  # --- Weight distribution table
  output$weight_table <- renderTable({
    Prisma_data %>% drop_na(momid) %>%
      drop_na(weight_peres) %>% select(type_visit,weight_peres) %>% group_by(type_visit) %>%
      dlookr::describe(quantiles = c(0.00,0.25,0.5,0.75,1))%>%
      rename("N" = n,"NA" = na,"Characteristic" = described_variables,
             "Visit type" = "type_visit",min = "p00",max ="p100",median = "p50",Q1 = "p25",Q3 = "p75") %>%
      mutate(Characteristic = recode(Characteristic,"weight_peres" = "Weight"))
  })


  #  --- Weight distribution by ANC Visit type
  render_ggplot(id = "Muac_cont",{
    Prisma_data %>% select(type_visit,muac_peres) %>%
      filter(type_visit %in%c("Enrollment","ANC-20","ANC-28","ANC-32","ANC-36","IPC (labor &delivery)")) %>%
      ggplot(aes(x = muac_peres,color = type_visit)) +
      geom_density(size = 1.5) + theme_classic() + labs(title = "Density Plot of Mothers Muac by each ANC visit",
                                                        x = "Mothers Muac", y = "Density", color = "") +
      theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
            legend.text = element_text(size = 12,),legend.position = "top",
            axis.title = element_text(size = 15,face = "bold"),
            axis.text =  element_text(size = 15,face = "italic")) +
      scale_color_manual(values = c("#00688B", "#00FF00", "#00B2EE", "#66CDAA")) +
      scale_x_continuous(limits = input$muac_peres)
  })

  # --- Muac widget distribution
  output$Muac_widget <- renderUI({
    sliderInput(inputId = "muac_peres",label = "Muac Widget",
                min = min(Prisma_data$muac_peres,na.rm = T),
                max = max(Prisma_data$muac_peres,na.rm = T),
                value = c(min(Prisma_data$muac_peres,na.rm = T),max(Prisma_data$muac_peres,na.rm = T)),
                sep = "")
  })

  # --- Muac distribution table
  output$Muac_table <- renderTable({
    Prisma_data %>% drop_na(momid) %>%
      drop_na(muac_peres) %>% select(type_visit,muac_peres) %>% group_by(type_visit) %>%
      dlookr::describe(quantiles = c(0.00,0.25,0.5,0.75,1))%>%
      rename("N" = n,"NA" = na,"Characteristic" = described_variables,
             "Visit type" = "type_visit",min = "p00",max ="p100",median = "p50",Q1 = "p25",Q3 = "p75") %>%
      mutate(Characteristic = recode(Characteristic,"muac_peres" = "Muac"))
  })



  #  --- BMI distribution by ANC Visit type
  render_ggplot(id = "BMI_cont",{
    Prisma_data %>% select(type_visit,BMI) %>%
      filter(type_visit %in%c("Enrollment","ANC-20","ANC-28","ANC-32","ANC-36","IPC (labor &delivery)")) %>%
      ggplot(aes(x = BMI,color = type_visit)) +
      geom_density(size = 1.5) + theme_classic() + labs(title = "Density Plot of Mothers BMI by each ANC visit",
                                                        x = "Mothers BMI", y = "Density", color = "") +
      theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
            legend.text = element_text(size = 13,),legend.position = "top",
            axis.title = element_text(size = 15,face = "bold"),
            axis.text =  element_text(size = 15,face = "italic")) +
      scale_color_manual(values = c("#EE7600", "#FFFF00", "#CDCD00", "#8B8B00")) +
      scale_x_continuous(limits = input$BMI)
  })

  # --- BMI widget distribution
  output$BMI_widget <- renderUI({
    sliderInput(inputId = "BMI",label = "BMI Widget",
                min = min(Prisma_data$BMI,na.rm = T),
                max = max(Prisma_data$BMI,na.rm = T),
                value = c(min(Prisma_data$BMI,na.rm = T),max(Prisma_data$BMI,na.rm = T)),
                sep = "")
  })

  # --- BMI distribution table
  output$BMI_table <- renderTable({
    Prisma_data %>% drop_na(momid) %>%
      drop_na(BMI) %>% select(type_visit,BMI) %>%  group_by(type_visit) %>%
      dlookr::describe(quantiles = c(0.00,0.25,0.5,0.75,1)) %>%
      rename("N" = n,"NA" = na,"Characteristic" = described_variables,
             "Visit type" = "type_visit",min = "p00",max ="p100",median = "p50",Q1 = "p25",Q3 = "p75") %>%
      mutate(Characteristic = recode(Characteristic,"BMI" = "BMI"))
  })



  #  --- Height distribution
  render_ggplot(id = "height_cont",{
    Prisma_data %>% select(type_visit,height_peres,momid) %>%
      drop_na(momid) %>% filter(type_visit == "Enrollment") %>%
      ggplot(aes(x = height_peres)) +
      geom_density(size = 1.5) + theme_classic() + labs(title = "Distribution of Women Height",
                                                        x = "Mothers Height", y = "Density", color = "") +
      theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
            legend.text = element_text(size = 13,),legend.position = "top",
            axis.title = element_text(size = 15,face = "bold"),
            axis.text =  element_text(size = 15,face = "italic")) +
      scale_x_continuous(limits = input$height_peres)
  })

  # --- Height widget distribution
  output$height_widget <- renderUI({
    sliderInput(inputId = "height_peres",label = "Height Widget",
                min = min(Prisma_data$height_peres,na.rm = T),
                max = max(Prisma_data$height_peres,na.rm = T),
                value = c(min(Prisma_data$height_peres,na.rm = T),max(Prisma_data$height_peres,na.rm = T)),
                sep = "")
  })

  # --- Height distribution table
  output$height_table <- renderTable({
    Prisma_data %>% drop_na(momid) %>% filter(type_visit == "Enrollment") %>%
      drop_na(height_peres) %>% select(type_visit,height_peres) %>%
      dlookr::describe(quantiles = c(0.00,0.25,0.5,0.75,1)) %>%
      rename("N" = n,"NA" = na,"Characteristic" = described_variables,
             min = "p00",max ="p100",median = "p50",Q1 = "p25",Q3 = "p75") %>%
      mutate(Characteristic = recode(Characteristic,"height_peres" = "Height"))
  })


  #  --- Scatter plot of Muac and BMI distribution
  render_ggplot(id = "bmi_muac",{
    Prisma_data %>% select(type_visit,muac_peres,BMI) %>%
      filter(type_visit %in%c("Enrollment","ANC-20","ANC-28","ANC-32","ANC-36","IPC (labor &delivery)")) %>%
      ggplot(aes(x = muac_peres, y = BMI,color = type_visit)) +
      geom_point(size = 1.5) + geom_smooth(method = "lm") +
      theme_classic() + labs(title = "Scatter Plot of BMI & Muac by ANC Visit",
                             x = "MUAC", y = "BMI", color = "") +
      ggpubr::stat_cor(method = "pearson",color = "blue",size = 5) +
      theme(plot.title = element_text(hjust = 0.5,size = 19,face = "bold"),
            axis.title = element_text(size = 15,face = "bold"),
            axis.text =  element_text(size = 15,face = "italic"),
            legend.position = "none",
            strip.background = element_blank(), strip.placement = "outside",
            strip.text = element_text(size = 15,face = "italic")) +
      facet_wrap(~type_visit,scales = "free") +
      scale_x_continuous(limits = input$height_peres)
  })




  # ---- SCHEDULER SCRIPT

  Scheduler_data = Prisma_data %>% filter(type_visit =="Enrollment") %>% distinct(momid,.keep_all = TRUE) %>%
    mutate(us_ga_screening = ((us_ga_wks_age_fts1*7) + us_ga_days_age_fts1)) %>%
    mutate(# ---- ANC 20 Window
      `ANC 20 Schedule date` = (us_ohostdat + (20*7 - us_ga_screening)),
      `ANC 20 Earliest date` = (`ANC 20 Schedule date` - 14),
      `ANC 20 Latest date` = (`ANC 20 Schedule date` + 20),
      # --------- ANC 28 Window
      `ANC 28 Schedule date` = (us_ohostdat + (28*7 - us_ga_screening)),
      `ANC 28 Earliest date` = (`ANC 28 Schedule date` - 14),
      `ANC 28 Latest date` = (`ANC 28 Schedule date` + 20),
      # --------- ANC 32 Window
      `ANC 32 Schedule date` = (us_ohostdat + (32*7 - us_ga_screening)),
      `ANC 32 Earliest date` = (`ANC 32 Schedule date` - 7),
      `ANC 32 Latest date` = (`ANC 32 Schedule date` + 13),
      # -------- ANC 36 Window
      `ANC 36 Schedule date` = (us_ohostdat + (36*7 - us_ga_screening)),
      `ANC 36 Earliest date` = (`ANC 36 Schedule date` - 14),
      `ANC 36 Latest date` = (`ANC 36 Schedule date` + 20)) %>%
    select(momid,scrn_fac_spfy_obsloc,us_ohostdat,us_ga_wks_age_fts1,us_ga_days_age_fts1,
           `ANC 20 Earliest date`,`ANC 20 Schedule date`,`ANC 20 Latest date`,
           `ANC 28 Earliest date`,`ANC 28 Schedule date`,`ANC 28 Latest date`,
           `ANC 32 Earliest date`,`ANC 32 Schedule date`,`ANC 32 Latest date`,
           `ANC 36 Earliest date`,`ANC 36 Schedule date`,`ANC 36 Latest date`) %>%
    rename(`Mothers ID` = momid, `Ultrasound Date` = us_ohostdat,`Ultrsound GA Weeks` = us_ga_wks_age_fts1,
           `Ultrsound GA Days` = us_ga_days_age_fts1, `Facility` = scrn_fac_spfy_obsloc)
  # ---- Scheduler render table
  output$scheduler <- DT::renderDataTable({
    DT::datatable(Scheduler_data,
                  extensions = c("FixedColumns","FixedHeader","Scroller","Buttons"),
                  options = list(
                    paging = TRUE,
                    pagelength = 10,
                    scroller = TRUE,
                    scrollY = '400px',
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                    # dom = 'Bfrtip',
                    # buttons = c('csv','excel')
                  ),
                  selection = 'single',
                  rownames = FALSE)
  })
  #  ---- Scheduler downloadbutton
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Scheduler-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Scheduler_data %>% arrange(`Facility`), file)
    })




  # ------ Maternal Point-of-Care (PoC) Diagnostics

  #  --- Systolic & diastolic results  :: --POC PART 1

  Sys_dia <- Prisma_data %>% drop_na(momid) %>% distinct(momid,type_visit,.keep_all = T) %>%
    select(momid,type_visit,bp_sys_vsorres_1,bp_sys_vsorres_2,bp_sys_vsorres_3,bp_dia_vsorres_1,bp_dia_vsorres_2,bp_dia_vsorres_3)
  Sys_dia[Sys_dia==-7] = NA

  output$sys_dia <- DT::renderDataTable({
    DT::datatable(Sys_dia,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "400px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE,
                    fixedColumns = list(
                      leftColumns = 1:2,
                      heightMatch = "none"
                    )
                  ),
                  rownames = FALSE)
  })

  output$dwnld_sys_dia_missng <- downloadHandler(
    filename = function() {
      paste("Missing Systolic & diastolic-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Sys_dia %>% filter(!complete.cases(.)), file)
    })

  Sys_dia_distr = Sys_dia %>% select(-momid) %>% group_by(type_visit) %>%
    dlookr::describe(quantiles = c(0.00,0.25,0.5,0.75,1)) %>%
    rename("N" = n,"NA" = na,"Systolic & Diastolic" = described_variables,
           min = "p00",max ="p100",median = "p50",Q1 = "p25",Q3 = "p75") %>%
    mutate_if(is.double,~round(.x,digits = 1))
  output$dist_table <- DT::renderDataTable({
    DT::datatable(Sys_dia_distr %>% filter(type_visit %in%c(input$type_visit)),
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })


  output$sys_dia_widget <- renderUI({
    selectInput(inputId = "type_visit",
                label = "Visit Type",
                choices = c(Prisma_data %>% select(type_visit) %>%
                              drop_na(.) %>% unique()),
                multiple = TRUE,
                selected = "Enrollment")
  })



  #  ---- Heart rates/resp rates/oxygen saturation/Non invasive Hg/Fetal heart rate :: --POC PART 2
  HR_RR <- Prisma_data %>% drop_na(momid) %>%
    select(momid,type_visit,mhr_vsorres,rr_vsorres,pulseox_vsorres,sphb_lborres,fhr_vsorres_1) %>%
    rename(`Maternal heart rate` = mhr_vsorres,`Respiratory rate` = rr_vsorres, `Oxygen saturation` = pulseox_vsorres,
           `Non inversive Hemoglobin(SpHb)` = sphb_lborres,`Fetal heart rate` = fhr_vsorres_1)
  HR_RR[HR_RR==-7] = NA

  output$hr_rr <- DT::renderDataTable({
    DT::datatable(HR_RR,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "400px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_hr_rr_missng <- downloadHandler(
    filename = function() {
      paste("Missing Heart rates/resp rates/oxygen saturation", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(HR_RR %>% filter(!complete.cases(.)), file)
    })


  output$hr_dist_table <- DT::renderDataTable({
    DT::datatable(HR_RR %>% select(-momid) %>% group_by(type_visit) %>%
                    dlookr::describe(quantiles = c(0.00,0.25,0.5,0.75,1)) %>%
                    rename("N" = n,"NA" = na,"Systolic & Diastolic" = described_variables,
                           min = "p00",max ="p100",median = "p50",Q1 = "p25",Q3 = "p75") %>%
                    mutate_if(is.double,~round(.x,digits = 1)) %>%
                    filter(type_visit %in%c(input$type_visit)),
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE,
                    fixedColumns = list(
                      leftColumns = 1:2,
                      heightMatch = "none"
                    )
                  ),
                  rownames = FALSE)
  })


  output$hr_widget <- renderUI({
    selectInput(inputId = "type_visit_2",
                label = "Visit Type",
                choices = c(Prisma_data %>% select(type_visit) %>%
                              drop_na(.) %>% unique()),
                multiple = TRUE,
                selected = "Enrollment")
  })



  # --- Hb POC   :: PART 3

  Hb_bg <- Prisma_data %>% drop_na(momid) %>%
    select(momid,type_visit,hb_poc_lborres) %>%
    rename(`HB POC` = hb_poc_lborres)
  Hb_bg[Hb_bg==-7] = NA

  output$hb_bg <- DT::renderDataTable({
    DT::datatable(Hb_bg,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "400px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_hb_bg_missng <- downloadHandler(
    filename = function() {
      paste("Missing Hb POC/Blood glucose", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Hb_bg %>% filter(!complete.cases(.)), file)
    })


  output$hb_bg_dist_table <- DT::renderDataTable({
    DT::datatable(Hb_bg %>% select(-momid) %>% group_by(type_visit) %>%
                    dlookr::describe(quantiles = c(0.00,0.25,0.5,0.75,1)) %>%
                    rename("N" = n,"NA" = na,"Systolic & Diastolic" = described_variables,
                           min = "p00",max ="p100",median = "p50",Q1 = "p25",Q3 = "p75") %>%
                    mutate_if(is.double,~round(.x,digits = 1)) %>%
                    filter(type_visit %in%c(input$type_visit)),
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })


  output$hb_bg_widget <- renderUI({
    selectInput(inputId = "type_visit_3",
                label = "Visit Type",
                choices = c(Prisma_data %>% select(type_visit) %>%
                              drop_na(.) %>% unique()),
                multiple = TRUE,
                selected = "Enrollment")
  })



  # --- Malaria/syphilis/HIV/HBV/HCV/Covid  :: PART 4

  Mal_hiv_ <- Prisma_data %>% drop_na(momid) %>%
    select(momid,type_visit,malaria_poc_lborres,hiv_poc_lborres,syph_poc_lborres,hbv_poc_lborres,hcv_poc_lborres,covid_poc_lborres) %>%
    rename(`Malaria POC` = malaria_poc_lborres,`HIV POC` = hiv_poc_lborres,`Syphilis POC` = syph_poc_lborres,
           `HBV POC` = hbv_poc_lborres,`HCV POC` = hcv_poc_lborres,`Covid POC` = covid_poc_lborres)

  output$mal_hiv_ <- DT::renderDataTable({
    DT::datatable(Mal_hiv_,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "400px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_mal_hiv_missng <- downloadHandler(
    filename = function() {
      paste("Missing Malaria/syphilis/HIV/HBV/HCV ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Mal_hiv_ %>% filter(!complete.cases(.)), file)
    })

  #  ----   PoC completeness status
  render_ggplot(id = "Mal_syph",{
    Mal_hiv_ %>% pivot_longer(names_to = "Diagnosis", values_to = "Results",cols = 3:8) %>%
      mutate(comple_result = if_else(is.na(Results),'Incomplete','Complete')) %>%
      filter(type_visit %in% c(input$type_visit_4)) %>%
      group_by(Diagnosis,comple_result) %>% summarise(freq = n()) %>% ungroup() %>% group_by(Diagnosis) %>%
      mutate(prop = round(freq/sum(freq),3)) %>%
      ggplot(aes(x = Diagnosis, y = freq, fill = comple_result)) +
      geom_col(position = position_dodge(0.9)) +
      geom_text(aes(label = paste0(freq,"\n(",prop*100,"%)")),position = position_dodge(0.9),
                size = 4.5, vjust = 1.2,alpha = 2) +
      theme_classic() + labs(title = paste0("Diagnostic Key Variable Results Completeness status \n as per",
                                            input$type_visit_4),
                             y = "Results Completeness status", x = "Diagnostics",
                             fill = "Complete status") +
      theme(plot.title = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.title.y = element_text(size = 15,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 15,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold"),
            axis.text.y = element_text(size = 15,face = "bold"),
            legend.text = element_text(size = 12,color = "#CD6600"),
            legend.position = "top") +
      scale_fill_manual(values = c("#8EE5EE", "#008B45"))
  })

  #---- gt summary
  output$mal_hiv_dist_table <- render_gt({
    expr = Mal_hiv_ %>% select(-momid) %>% droplevels(.) %>%
      gtsummary::tbl_summary(by = type_visit,missing_text = "Missing Results") %>%
      italicize_levels() %>% bold_labels() %>% as_gt()
  })


  output$mal_hiv_widget <- renderUI({
    selectInput(inputId = "type_visit_4",
                label = "Visit Type",
                choices = c(Prisma_data %>% select(type_visit) %>%
                              drop_na(.) %>% unique()),
                multiple = TRUE,
                selected = "Enrollment")
  })




  #  --- MATERNAL LABORATORY RESULTS

  # ---- CBC RESULTS
  CBC <- Prisma_data %>% select(momid,type_visit,starts_with("CBC")) %>%  select(momid,type_visit,ends_with("LBORRES")) %>%
    drop_na(momid) %>% filter(type_visit == 'Enrollment') %>%
    rename(`Hb results` = cbc_hb_lborres,`HCT  %` = cbc_hct_lborres,`WBC ` = cbc_wbc_lborres,
           `Neutrophils %` = cbc_neu_pct_lborres,`Neutrophils fcc ` = cbc_neu_fcc_lborres,
           `Lymphocyte %` = cbc_lymph_pct_lborres,`Erythrocyte ` = cbc_eryth_mm_lborres,
           `MCV ` = cbc_mcv_lborres,`MCH results` = cbc_mch_lborres,`MCHC results` = cbc_mchc_gdl_lborres,
           `Platelets count ` = cbc_plate_lborres,`Monocyte  %` = cbc_mono_pct_lborres,
           `Monocyte fcc` = cbc_mono_fcc_lborres,`Eosinophils  %` = cbc_eos_pct_lborres,`Eosinophils fcc` = cbc_eos_fcc_lborres,
           `Red cell width  % ` = cbc_rdw_pct_lborres,`PDW results` = cbc_pdw_ct_lborres,`PCT results %` = cbc_pct_pct_lborres,
           `Lymphocyte fcc` = cbc_lymph_fcc_lborres)
  CBC[CBC ==-7] =NA

  output$cbc <- DT::renderDataTable({
    DT::datatable(CBC,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_cbc_missng <- downloadHandler(
    filename = function() {
      paste("Missing CBC results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(CBC %>% filter(!complete.cases(.)), file)
    })


  render_ggplot(id = "cbc_paleto",{
    CBC %>% select(-c(1:2)) %>% plot_na_pareto( ) +
      labs(title = "Pareto Chart for Missing CBC results",
           x = "CBC Key Variables") +
      theme(axis.title.y = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 18,face = "bold"),
            axis.text.y = element_text(size = 18,face = "bold"),
            plot.title = element_text(size = 21,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 12,face = "italic"))
  })



  # ---- URINALYSIS RESULTS
  UR <- Prisma_data %>% select(momid,type_visit,starts_with("UA")) %>% select(momid,type_visit,ends_with("LBORRES")) %>%
    drop_na(momid) %>%
    rename(`Protein` = ua_prot_lborres,`Leukocytes` = ua_leuk_lborres,
           `Nitrites` = ua_nitrite_lborres)
  UR[UR ==-7] = NA

  output$ur <- DT::renderDataTable({
    DT::datatable(UR,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_ur_missng <- downloadHandler(
    filename = function() {
      paste("Missing Urinalysis results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(UR %>% filter(!complete.cases(.)), file)
    })



  render_ggplot(id = "UR_paleto",{
    UR %>% select(-c(1:2)) %>% plot_na_pareto() +
      labs(title = "Pareto Chart for Missing Urinalysis results",
           x = "Urinalysis Key Variables") +
      theme(axis.title.y = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold",angle = 0),
            axis.text.y = element_text(size = 15,face = "bold"),
            plot.title = element_text(size = 21,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 12,face = "italic"))
  })


  #  ---  KFT RESULTS
  KFT <- Prisma_data %>% select(momid,type_visit,bun_mmoll_lborres,creat_umoll_lborres,sodium_lborres,potassium_lborres,
                                chloride_lborres,phosphorus_lborres,calcium_lborres,carb_diox_lborres) %>%
    drop_na(momid) %>% filter(type_visit %in%c('Enrollment','ANC-32')) %>%
    rename(`BUN test` = bun_mmoll_lborres,`Serum creatine` = creat_umoll_lborres,`Sodium` = sodium_lborres,
           Potassium = potassium_lborres,Chloride = chloride_lborres,Phosphorus = phosphorus_lborres,Calcium = calcium_lborres,
           `Carbon dioxide` = carb_diox_lborres)
  KFT[KFT ==-7] = NA

  output$kft <- DT::renderDataTable({
    DT::datatable(KFT,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_kft_missng <- downloadHandler(
    filename = function() {
      paste("Missing KFT results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(KFT %>% filter(!complete.cases(.)), file)
    })



  render_ggplot(id = "KFT_paleto",{
    KFT %>% select(-c(1:2)) %>% plot_na_pareto() +
      labs(title = "Pareto Chart for Missing Kidney Function Test results",
           x = "KFT Key Variables") +
      theme(axis.title.y = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold"),
            axis.text.y = element_text(size = 15,face = "bold"),
            plot.title = element_text(size = 21,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 11,face = "italic"))
  })

  # ------LFT RESULTS
  LFT <- Prisma_data %>% select(momid,type_visit,ast_ul_lborres,alt_ul_lborres,alp_lborres,tbilirubin_lborres,dbilirubin_lborres,
                                tprotein_lborres,albumin_lborres,gammagt_lborres,ibilirubin_lborres) %>%
    drop_na(momid) %>% filter(type_visit %in%c('Enrollment','ANC-32')) %>%
    rename(`AST/SGOT` = ast_ul_lborres,`ALT/SGPT` = alt_ul_lborres,`ALP results` = alp_lborres,`Total Bilirubin` = tbilirubin_lborres,
           `Albumin test` = albumin_lborres,`Gamma GT` = gammagt_lborres,`Indirect Bilirubin` = ibilirubin_lborres,
           `Direct Bilirubin` = dbilirubin_lborres,`Total protein test` = tprotein_lborres)
  LFT[LFT==-7] = NA

  output$lft <- DT::renderDataTable({
    DT::datatable(LFT,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_lft_missng <- downloadHandler(
    filename = function() {
      paste("Missing LFT results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(LFT %>% filter(!complete.cases(.)), file)
    })



  render_ggplot(id = "LFT_paleto",{
    LFT %>% select(-c(1:2)) %>% plot_na_pareto() +
      labs(title = "Pareto Chart for Missing Liver Function Test results",
           x = "LFT Key Variables") +
      theme(axis.title.y = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold"),
            axis.text.y = element_text(size = 15,face = "bold"),
            plot.title = element_text(size = 21,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 11,face = "italic"))
  })


  #  ----- Micronutrient Test Results
  Micronutrient <- Prisma_data %>% select(momid,type_visit,vitb12_cob_lborres,vitb12_hol_lborres,folate_plasma_nmoll_lborres,
                                          zinc_lborres,iron_hep_lborres,iron_tot_ugdl_lborres,vita_ugdl_lborres,ferritin_lborres,
                                          iodine_lborres,transferrin_lborres,rbp4_lborres,crp_lborres,agp_lborres,hrp_lborres) %>%
    drop_na(momid) %>% filter(type_visit %in%c('Enrollment','ANC-32')) %>%
    rename(`Total cobalamin` = vitb12_cob_lborres,"Holotranscobalamin \n II" = vitb12_hol_lborres,
           "Folate blood \n serum" = folate_plasma_nmoll_lborres,`Zinc test` = zinc_lborres,`Hepcidin` = iron_hep_lborres,
           "Total iron \n binding capacity" = iron_tot_ugdl_lborres,"Total vitamin A \n serum retinol" = vita_ugdl_lborres,
           Ferritin = ferritin_lborres,Iodine = iodine_lborres,sTfR = transferrin_lborres,`RBP4 results` = rbp4_lborres,
           `CRP results` = crp_lborres,`AGP results` = agp_lborres,`HRP 2 results` = hrp_lborres)
  Micronutrient[Micronutrient==-7] = NA

  output$Micronut <- DT::renderDataTable({
    DT::datatable(Micronutrient,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_Micronut_missng <- downloadHandler(
    filename = function() {
      paste("Missing Micronutrient results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Micronutrient %>% filter(!complete.cases(.)), file)
    })



  render_ggplot(id = "Micronutrient_paleto",{
    Micronutrient %>% select(-c(1:2)) %>% plot_na_pareto() +
      labs(title = "Pareto Chart for Missing Micronutrient Test Results",
           x = "Micronutrient Test Variables") +
      theme(axis.title.y = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold"),
            axis.text.y = element_text(size = 15,face = "bold"),
            plot.title = element_text(size = 21,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 11,face = "italic"))
  })


  #  ---- Thyroid Panel Results
  Thyroid <- Prisma_data %>% select(momid,type_visit,thyroid_tsh_lborres,thyroid_freet4_lborres,thyroid_freet3_lborres) %>%
    drop_na(momid) %>% filter(type_visit %in%c('Enrollment','ANC-32')) %>%
    rename(`TSH results` = thyroid_tsh_lborres,`Free T4` = thyroid_freet4_lborres,`Free T3` = thyroid_freet3_lborres)
  Thyroid[Thyroid==-7] =NA

  output$thyroid <- DT::renderDataTable({
    DT::datatable(Thyroid,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_thyroid_missng <- downloadHandler(
    filename = function() {
      paste("Missing Thyroid results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Thyroid %>% filter(!complete.cases(.)), file)
    })



  render_ggplot(id = "Thyroid_paleto",{
    Thyroid %>% select(-c(1:2)) %>% plot_na_pareto() +
      labs(title = "Pareto Chart for Missing Thyroid Panel Results",
           x = "Thyroid Panel Variables") +
      theme(axis.title.y = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold",angle = 0),
            axis.text.y = element_text(size = 15,face = "bold"),
            plot.title = element_text(size = 21,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 11,face = "italic"))
  })


  #   ---- Malaria Laboratory
  Malaria_lab <- Prisma_data %>% select(momid,type_visit,malbl_lborres,contains("_lborres_")) %>%
    select(momid,type_visit,starts_with("malbl")) %>%
    drop_na(momid)
  mal_var <-  Malaria_lab %>% labelled::look_for() %>% select(2:3)
  colnames(Malaria_lab) <- mal_var$label
  Malaria_lab <- Malaria_lab %>% rename(`Malaria blood slide` = `malaria blood slide test results`,
                                        momid = "mom id",type_visit = "Type of Visit")
  Malaria_lab[Malaria_lab ==-7] = NA

  output$malaria_lab <- DT::renderDataTable({
    DT::datatable(Malaria_lab,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_malaria_lab_missng <- downloadHandler(
    filename = function() {
      paste("Missing Malaria lab results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Malaria_lab %>% filter(!complete.cases(.)), file)
    })



  render_ggplot(id = "Malaria_paleto",{
    Malaria_lab %>% select(-c(1:2)) %>% plot_na_pareto() +
      labs(title = "Pareto Chart for Missing Malaria Laboratory Test Results",
           x = "Malaria test Variables") +
      theme(axis.title.y = element_text(size = 16,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 16,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,color = "black"),
            axis.text.y = element_text(size = 15,color = "black"),
            plot.title = element_text(size = 21,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 11))
  })


  #    ----  Schistosomiasis Test Results (Urine)
  Schistosomiasis <- Prisma_data %>% select(momid,type_visit,schisto_lborres,schisto_cnt,schisto_lborres_stool,
                                            schisto_stool_ct_1,schisto_stool_ct_2,schisto_stool_ct_3,schisto_stool_ct_4,
                                            schisto_stool_ct_5) %>%
    drop_na(momid) %>% filter(type_visit == 'Enrollment') %>%
    rename(`Urine test` = schisto_lborres,`schisto count` =schisto_cnt,`Stool test` = schisto_lborres_stool,
           "Mansoni count" = schisto_stool_ct_1,`Japonicum count` = schisto_stool_ct_2,`Mekongi count` = schisto_stool_ct_3,
           "Intercalatum \n count" = schisto_stool_ct_4,"Haematobium \n count" = schisto_stool_ct_5)
  Schistosomiasis[Schistosomiasis == -7] =NA


  output$Schisto <- DT::renderDataTable({
    DT::datatable(Schistosomiasis,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  })

  output$dwnld_Schisto_missng <- downloadHandler(
    filename = function() {
      paste("Missing Schistosomiasis results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Schistosomiasis %>% filter(!complete.cases(.)), file)
    })



  render_ggplot(id = "Schisto_paleto",{
    Schistosomiasis %>% select(-c(1:2)) %>% plot_na_pareto() +
      labs(title = "Pareto Chart for Missing Schistosomiasis Test Results",
           x = "Schistosomiasis counts Variables") +
      theme(axis.title.y = element_text(size = 17,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 17,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold"),
            axis.text.y = element_text(size = 15,face = "bold"),
            plot.title = element_text(size = 21,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 11))
  })


  #  ------ Helminths Test Results
  Helminths <- Prisma_data %>% select(momid,type_visit,helm_lborres,helm_cnt_1,helm_cnt_2,helm_cnt_3,helm_cnt_4,
                                      helm_cnt_5,helm_cnt_6,helm_cnt_7,helm_cnt_8,helm_cnt_9,helm_cnt_10,helm_cnt_11,
                                      helm_cnt_12) %>%
    drop_na(momid) %>% filter(type_visit == 'Enrollment') %>%
    rename(`Helminth Test` = helm_lborres,Whipworm = helm_cnt_1,`Ascaris lumbricoides` = helm_cnt_2,Hookworm = helm_cnt_3,
           Pinworm = helm_cnt_4, threadworm = helm_cnt_5,`Taenia saginata` = helm_cnt_6,`T. solium` = helm_cnt_7,
           `T. Latum` = helm_cnt_8,`Hymenolepsis/diminuta` = helm_cnt_9,`Fasciolopsis buski` = helm_cnt_10,
           `Paragonimus species` = helm_cnt_11, Clonorchis = helm_cnt_12)
  Helminths[Helminths == -7] = NA


  output$helminths <- DT::renderDataTable({
    DT::datatable(Helminths,
                  extensions = c("FixedColumns","FixedHeader","Scroller"),
                  options = list(
                    scroller = TRUE,
                    scrollY = "300px",
                    scrollX =TRUE,
                    searching = TRUE,
                    autoWidth = TRUE
                  ))
  })

  output$dwnld_helminths_missng <- downloadHandler(
    filename = function() {
      paste("Missing Helminths results ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(Helminths %>% filter(!complete.cases(.)), file)
    })

  render_ggplot(id = "Helminths_paleto",{
    Helminths %>% select(-c(1:2)) %>% plot_na_pareto() +
      labs(title = "Pareto Chart for Missing Helminths Test Results",
           x = "Helminths counts Variables") +
      theme(axis.title.y = element_text(size = 17,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 17,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold"),
            axis.text.y = element_text(size = 15,face = "bold"),
            plot.title = element_text(size = 20,face = "bold",hjust = 0.5),
            legend.text = element_text(size = 10))
  })


  ### --- Create  A complete check Data sets for lab components
  cbc_comp <- CBC %>% mutate_at(vars(`Hb results`:`PCT results %`),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(CBC)]),
           CBC = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,CBC)
  ur_comp <- UR %>% mutate_at(vars(`Protein`:`Nitrites`),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(UR)]),
           UR = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,UR)
  lft_comp <- LFT %>% mutate_at(vars(`AST/SGOT`:`Indirect Bilirubin`),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(LFT)]),
           LFT = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,LFT)
  kft_comp <- KFT %>% mutate_at(vars(`BUN test`:`Carbon dioxide`),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(KFT)]),
           KFT = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,KFT)
  Thyroid_comp <- Thyroid %>% mutate_at(vars(`TSH results`:`Free T3`),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(Thyroid)]),
           Thyroid = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,Thyroid)
  Mala_comp <- Malaria_lab %>% mutate_at(vars("Malaria blood slide":"Other thin RBC "),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(Malaria_lab)]),
           Malaria = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,Malaria)
  Micro_comp <- Micronutrient %>% mutate_at(vars(`Total cobalamin`:`HRP 2 results`),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(Micronutrient)]),
           "Micro \n nutrient" = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,"Micro \n nutrient")
  Schisto_comp <- Schistosomiasis %>% mutate_at(vars(`Urine test`:"Haematobium \n count"),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(Schistosomiasis)]),
           "Schisto \n somiasis" = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,"Schisto \n somiasis")
  Helminths_comp <- Helminths %>% mutate_at(vars(`Helminth Test`:Clonorchis),~if_else(is.na(.x),0,1)) %>%
    mutate(entries = rowSums(.[3:length(Helminths)]),
           Helminths = if_else(entries==0,"Incomplete","Complete")) %>% select(momid,type_visit,Helminths)

  ##  ---- Merge all the lab completeness variables
  suppressMessages({
  Lab_complete <- cbc_comp %>% full_join(ur_comp) %>% full_join(lft_comp) %>% full_join(kft_comp) %>%
    full_join(Thyroid_comp) %>% full_join(Mala_comp) %>% full_join(Micro_comp) %>% full_join(Schisto_comp) %>%
    full_join(Helminths_comp) %>% pivot_longer(names_to = "Lab_Component",values_to = "Complete_status",cols = CBC:Helminths)
})

  # --- LABORATOTY COMPONENTS COMPLETENESS plot
  render_ggplot(id ="lab_com",{

    Lab_complete %>% filter(type_visit == input$type_visit_5) %>%
      group_by(Lab_Component,Complete_status) %>% drop_na() %>%
      summarise(freq = n()) %>% ungroup() %>% group_by(Lab_Component) %>%
      mutate(prop = round(freq/sum(freq),3)) %>%
      ggplot(aes(x = Lab_Component, y = freq, fill = Complete_status)) +
      geom_col(position = position_dodge(0.9)) +
      geom_text(aes(label = paste0(freq,"\n(",prop*100,"%)")),position = position_dodge(0.9),
                size = 4.2, vjust = 1.2,alpha = 2,color ="white") +
      theme_classic() +
      labs(title = paste0("Laboratory Component Results Completeness status \n (", input$type_visit_5,")" ),
           y = "Results Completeness status", x = "Laboratory Component",
           fill = "Complete status") +
      theme(plot.title = element_text(size = 18,face = "bold",hjust = 0.5),
            axis.title.y = element_text(size = 15,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size = 15,face = "bold",hjust = 0.5),
            axis.text.x = element_text(size = 13,face = "italic",color =  "red"),
            axis.text.y = element_text(size = 15,face = "bold"),
            legend.text = element_text(size = 12,color = "#CD6600"),
            legend.position = "top") +
      scale_fill_manual(values = c("#3A5FCD", "#00868B"))
  })

  output$lab_comp_widget <- renderUI({
    selectInput(inputId = "type_visit_5",
                label = "Visit Type",
                choices = c(Prisma_data %>% select(type_visit) %>%
                              drop_na(.) %>% unique()),
                multiple = FALSE,
                selected = "Enrollment")
  })

}

shinyApp(ui, server)
}
