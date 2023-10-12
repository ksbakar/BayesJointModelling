

################################################################################
## Bayesian Joint Modelling for Allograft and patients' death
## Date: 2023-08-01
## Update: 2023-08-24
################################################################################

#rm(list = ls())

## load saved models

load_rdata <- function(loading_model){
  useShinyalert(force = TRUE)  # Initialize shinyalert
  shinyalert(
    title = "Loading Model",
    text = "The model is currently loading...",
    timer = 5000,
    type = "info"
  )
  #Sys.sleep(5)  # Simulate model running time
  #load("mod_graftloss.RData")
  #load("mod_alivestatus.RData")
  options(timeout=1200) # in secs
  #mod_graftloss = readRDS("mod_graftloss_shiny.rds")
  #mod_alivestatus = readRDS("mod_alivestatus_shiny.rds")
  mod_graftloss = readRDS(gzcon(url("https://www.dropbox.com/scl/fi/fcd8z5sohyezqs4xc0n51/mod_graftloss_shiny.rds?rlkey=a7jwepud15jc6r4cgpronueji&dl=1")))
  mod_alivestatus = readRDS(gzcon(url("https://www.dropbox.com/scl/fi/xcxa28aqr9joj879h92sq/mod_alivestatus_shiny.rds?rlkey=a3dynlv17h8imc95r88ed9co3&dl=1")))
  # JavaScript code to update the countdown timer
  shinyalert(
    title = "Finished",
    text = "The model has finished running.",
    type = "success"
  )
  loading_model(FALSE)  # Model has finished running
  return(list(mod_graftloss=mod_graftloss,mod_alivestatus=mod_alivestatus))
  #return(list(mod_graftloss=1,mod_alivestatus=2))
}

## create pred data from input

predData_Surv <- function(gendercode = input$gendercode,
                          bmi = input$bmi,
                          smokingcode = input$smokingcode,
                          racialorigincode = input$racialorigincode,
                          donorage = input$donorage,
                          donorgendercode = input$donorgendercode,
                          donor_death = input$donor_death,
                          coronaryarterycode = input$coronaryarterycode,
                          cerebrovasularcode = input$cerebrovasularcode,
                          diabetescode_text = input$diabetescode_text,
                          primaryrenaldiseasecode = input$primaryrenaldiseasecode,
                          timeondialysis = input$timeondialysis,
                          hlamismatchesdr = input$hlamismatchesdr,
                          egfr_ckdepi = c(input$egfr1,
                                          input$egfr2,
                                          input$egfr3,
                                          input$egfr4,
                                          input$egfr5,
                                          input$egfr6)){
  ##
  newdatSurv = readRDS(file="shiny_newdatSurv.rds")
  newdatSurv[,c("donorage")] = as.double(donorage)
  newdatSurv[,c("donorgendercode")] = as.double(donorgendercode)
  newdatSurv[,c("donor_death")] = as.double(donor_death)
  newdatSurv[,c("gendercode")] = as.double(gendercode) 
  newdatSurv[,c("bmi")] = as.double(bmi)
  newdatSurv[,c("smokingcode")] = as.double(smokingcode)
  newdatSurv[,c("racialorigincode")] = as.double(racialorigincode) 
  newdatSurv[,c("coronaryarterycode")] = as.double(coronaryarterycode)
  newdatSurv[,c("cerebrovasularcode")] = as.double(cerebrovasularcode)
  newdatSurv[,c("diabetescode_text")] = as.double(diabetescode_text)
  newdatSurv[,c("primaryrenaldiseasecode")] = as.double(primaryrenaldiseasecode)
  newdatSurv[,c("timeondialysis")] = as.double(timeondialysis)
  newdatSurv[,c("hlamismatchesdr")] = as.double(hlamismatchesdr)
  ##
  loop <- length(egfr_ckdepi[!is.na(egfr_ckdepi)])
  newdatSurv[1:loop,]
  ##
}

predData_Long <- function(egfr_ckdepi = c(input$egfr1,
                                          input$egfr2,
                                          input$egfr3,
                                          input$egfr4,
                                          input$egfr5,
                                          input$egfr6),
                          age = input$age,
                          gendercode = input$gendercode,
                          bmi = input$bmi,
                          smokingcode = input$smokingcode,
                          racialorigincode = input$racialorigincode,
                          donorage = input$donorage,
                          donorgendercode = input$donorgendercode,
                          donor_death = input$donor_death,
                          coronaryarterycode = input$coronaryarterycode,
                          cerebrovasularcode = input$cerebrovasularcode,
                          diabetescode_text = input$diabetescode_text,
                          primaryrenaldiseasecode = input$primaryrenaldiseasecode,
                          timeondialysis = input$timeondialysis,
                          hlamismatchesdr = input$hlamismatchesdr){
  ##
  newdatLong = readRDS(file="shiny_newdatLong.rds")
  newdatLong[,c("donorage")] = as.double(donorage)
  newdatLong[,c("donorgendercode")] = as.double(donorgendercode)
  newdatLong[,c("donor_death")] = as.double(donor_death)
  newdatLong[,c("gendercode")] = as.double(gendercode)
  newdatLong[,c("bmi")] = as.double(bmi)
  newdatLong[,c("smokingcode")] = as.double(smokingcode)
  newdatLong[,c("racialorigincode")] = as.double(racialorigincode)
  newdatLong[,c("coronaryarterycode")] = as.double(coronaryarterycode)
  newdatLong[,c("cerebrovasularcode")] = as.double(cerebrovasularcode)
  newdatLong[,c("diabetescode_text")] = as.double(diabetescode_text)
  newdatLong[,c("primaryrenaldiseasecode")] = as.double(primaryrenaldiseasecode)
  newdatLong[,c("timeondialysis")] = as.double(timeondialysis)
  newdatLong[,c("hlamismatchesdr")] = as.double(hlamismatchesdr)
  ##
  id <- unique(newdatLong$id)
  loop <- length(egfr_ckdepi[!is.na(egfr_ckdepi)])
  age_val <- unique(newdatLong$monthcode_yr)
  age <- age + age_val
  for(i in 1:loop){
    newdatLong[newdatLong$id==id[i],"egfr_ckdepi"] = as.double(egfr_ckdepi[1:i])
    newdatLong[newdatLong$id==id[i],"age"] = age[1:i]
  }
  ##
  newdatLong[complete.cases(newdatLong$egfr_ckdepi),]
  ##
}

## add year variable in pred data

dat_add_fnc <- function(data){
  data$year = NA
  ck_list <- unique(data$id)
  yr_list <- c(0.25,0.5,1,2,3,5)
  for(i in 1:6){
    data[data$id == ck_list[i],]$year = yr_list[i]
  }
  data
}

## prod egfr 

eGFR_plot_fnc <- function(data,input_data){
  data = dat_add_fnc(data = data)
  input_data = dat_add_fnc(data = input_data)
  fig1 <- data %>%
    plot_ly(
      x = ~monthcode_yr, 
      y = ~yfit, 
      frame = ~year, 
      #type = 'scatter',
      mode = 'lines'
    ) %>%
    layout(xaxis = list(title = 'Year'), 
           yaxis = list(title = 'eGFR (mL/min per 1.73m2)')) %>%
    add_fun(function(plot) {
      plot %>% 
        add_ribbons(x = ~monthcode_yr,
                    ymin = ~ci_lb,
                    ymax = ~ci_ub
        ) %>%
        add_lines(
          x = ~monthcode_yr,
          y = ~yfit,
          line = list(
            color = "black",
            dash = "dot"
          )
        )
    }
    ) %>% 
    add_markers(data = input_data, x=~monthcode_yr, y=~egfr_ckdepi)
  fig1
}

#eGFR_plot_fnc(data=p1oo,input_data=newdatLong)


## pred allograft

allograft_plot_fnc <- function(data){
  data = dat_add_fnc(data = data)
  ck_data = data %>% 
    group_by(year) %>%
    filter(row_number()==n())
  ck_data$y = 0; ck = ck_data; ck$y = 1
  ck_data = rbind(ck_data,ck)
  fig2 <- data %>%
    plot_ly(
      x = ~monthcode_yr, 
      y = ~survpred, 
      frame = ~year, 
      #type = 'scatter',
      mode = 'lines',
      linetype = I("dash")
    ) %>%
    layout(xaxis = list(title = 'Year'), 
           yaxis = list(title = 'Probability of allograft survival')) %>%
    add_fun(function(plot) {
      plot %>% 
        add_ribbons(x = ~monthcode_yr,
                    ymin = ~ci_lb,
                    ymax = ~ci_ub
        )
    }
    ) %>% 
    add_lines(
      x = ~monthcode_yr,
      y = ~survpred,
      line = list(
        color = "black",
        dash = "dot"
      )
    ) %>%
    add_lines(data = ck_data, x=~year, y=~y, mode = 'line',
              line = list(widthh=0.5, dash="dot"))
  fig2
}

#allograft_plot_fnc(data = p2oo)

## pred survival 

death_plot_fnc <- function(data){
  data = dat_add_fnc(data = data)
  ck_data = data %>% 
    group_by(year) %>%
    filter(row_number()==n())
  ck_data$y = 0; ck = ck_data; ck$y = 1
  ck_data = rbind(ck_data,ck)
  fig3 <- data %>%
    plot_ly(
      x = ~monthcode_yr, 
      y = ~survpred, 
      frame = ~year, 
      #type = 'scatter',
      mode = 'lines',
      linetype = I("dash")
    )%>%
    layout(xaxis = list(title = 'Year'), 
           yaxis = list(title = "Patients' Survival probability")) %>%
    add_fun(function(plot) {
      plot %>% 
        add_ribbons(x = ~monthcode_yr,
                    ymin = ~ci_lb,
                    ymax = ~ci_ub
        )
    }
    ) %>% 
    add_lines(
      x = ~monthcode_yr,
      y = ~survpred,
      line = list(
        color = "black",
        dash = "dot"
      )
    ) %>%    
    add_lines(data = ck_data, x=~year, y=~y, mode = 'line',
              line = list(widthh=0.5, dash="dot"))
  fig3
}

#death_plot_fnc(data = p3oo)

## combine all 3 plots 

plot_fnc <- function(eGFR,Graft,Death,inputLong){
  fig <- subplot(eGFR_plot_fnc(data=eGFR,input_data=inputLong), 
                 allograft_plot_fnc(data=Graft), 
                 death_plot_fnc(data=Death), 
                 nrows = 1, titleY = TRUE, titleX = TRUE, margin = 0.05) %>% 
    layout(title = 'Dynamic Projections') %>%
    animation_slider(
      currentvalue = list(prefix = "YEAR ", font = list(color="black"))
    ) 
  fig <- fig %>%  
    animation_opts(
      frame = 3000, easing = "elastic", redraw = FALSE
    ) 
  fig |> layout(showlegend = FALSE)  
}

#plot_fnc(p1oo,p2oo,p3oo,newdatLong)

## front-end page design for data input

data_page <- tabPanel(
  title = "Data & Model",
  titlePanel("Dynamic Projection of Allograft & Patients' Survival"),
  br(),
  hr(),
  #textOutput("time"),
  actionButton("run_button_modelLoad", "Load Model", icon = icon("play")),
  br(),
  hr(),
  fluidRow(
    column(12,
           h3("Patient Characteristics")
           #p("This is some content below the title.")
    ),    
    column(4,numericInput("age", "Age (in years):", 42, min=0, max=150)),
    column(4,numericInput("bmi", "Body mass index:", 21.56, min=0, max=150)),
    column(4,numericInput("timeondialysis", "Dialysis duration (in years):", 0.56, min=0, max=150)),
    column(4,radioButtons("gendercode", "Sex:",
                          c("Female" = "1", 
                            "Male" = "2"))),
    column(4,radioButtons("smokingcode", "Smoking:",
                          c("Never" = "1", 
                            "Former/Current" = "2"))),
    column(4,radioButtons("diabetescode_text", "Diabetes:",
                          c("No" = "1", 
                            "Yes" = "2"))),
    column(4,radioButtons("racialorigincode", "Ethnicity:",
                          c("Caucasian" = "1", 
                            "Indigenous Australian" = "2",
                            "New Zealand Maori" = "3",
                            "Others/Not Recorded" = "4"))),
    column(4,radioButtons("coronaryarterycode", "Prior Coronary Artery Disease:",
                          c("No" = "1", 
                            "Yes" = "3",
                            "Suspected" = "2"))),
    column(4,radioButtons("cerebrovasularcode", "Prior Cerebrovascular Disease:",
                          c("No" = "1", 
                            "Yes" = "3",
                            "Suspected" = "2"))),
    column(4,radioButtons("primaryrenaldiseasecode", "Causes of Kidney Failure:",
                          c("Hyper/Renovascular Disease" = "1", 
                            "Glomerulonephritis" = "3",
                            "Diabetes" = "5",
                            "Cystic" = "4",
                            "Analgesic Nephropathy" = "2",
                            "Others" = "6"))),    
    column(4,radioButtons("hlamismatchesdr", "HLA-DR Mismatches:",
                          c("0" = "1", 
                            "1" = "3",
                            "2" = "2")))
  ),
  br(),
  hr(),
  fluidRow(
    column(12,
           h3("Donor Data")
           #p("This is some content below the title.")
    ),    
    column(4,numericInput("donorage", "Donor age (in years):", 56, min=0, max=150)),
    column(4,radioButtons("donorgendercode", "Donor sex:",
                          c("Female" = "1", 
                            "Male" = "2"))),
    column(4,radioButtons("donor_death", "Donor death:",
                          c("Yes" = "1", 
                            "No" = "2")))
  ),
  br(),
  hr(),
  fluidRow(
    column(12,
           h3("Post-transplant eGFR (ml/min/1.73 m2)")
           #p("This is some content below the title.")
    ),    
    column(4,numericInput("egfr1", "3 months", 43.75, min=0, max=1000)),
    column(4,numericInput("egfr2", "6 months", 36.80, min=0, max=1000)),
    column(4,numericInput("egfr3", "12 months", 33.80, min=0, max=1000)),
    column(4,numericInput("egfr4", "24 months", 33.56, min=0, max=1000)),
    column(4,numericInput("egfr5", "36 months", 30.97, min=0, max=1000)),
    column(4,numericInput("egfr6", "60 months", 30.54, min=0, max=1000))
  ),
  br(),
  hr(),
  #actionButton("run_button_proj", "Run Projection", icon = icon("play")),
  #br(),
  #hr()
)

## front-end page design for survival plot visualisation

proj_page <- tabPanel(
  title = "Projection",
  titlePanel("Dynamic Projections of Survival Probability & eGFR Trends"),
  br(),
  hr(),
  actionButton("run_button_proj", "Projection Plots", icon = icon("play")),
  br(),
  hr(),
  fluidPage(
    plotlyOutput("plot_output")
  )
  #mainPanel(
  #  plotlyOutput("plot_output")
  #tabsetPanel(type = "tabs",
  #            tabPanel("Survival Probability & eGFR Trends", 
  #                     plotlyOutput("plot_output")
  #            ),
  #            tabPanel("Raw Data", tableOutput("table_dataSurv"))
  #)
  #)
)


## front-end page design for visualisation

ui <- navbarPage(
  title = "",
  #theme = shinytheme('flatly'),
  theme = shinytheme('united'),
  data_page,
  proj_page
)

## back-end surver to run model

server <- function(input, output) {
  #output$time <- renderText({
  #  invalidateLater(1000, session)
  #  paste(format(Sys.time(), "%a %b %d %X %Y"))    
  #})
  loading_model <- reactiveVal(FALSE)
  model_output <- reactiveVal(NULL)  # To store the output from load_rdata
  # load *.RData model 
  observeEvent(input$run_button_modelLoad, {
    loading_model(TRUE)
    output <- load_rdata(loading_model)
    model_output(output) # Store the output
    loading_model(FALSE)
  })
  #observe({
  #  if (!is.null(model_output)) {
  #    invalidateLater(1000)  # Update every 1 second (1000 milliseconds)
  #  }
  #})  
  #output$time <- renderText({
  #invalidateLater(1000)  # Update every 1 second (1000 milliseconds)
  #if (!is.null(model_output)) {
  #  "Model is running..."
  #} else {
  #  "Model is not running."
  #}
  #})
  ##
  data_inputSurv <- reactive({
    predData_Surv(gendercode = input$gendercode,
                  bmi = input$bmi,
                  smokingcode = input$smokingcode,
                  racialorigincode = input$racialorigincode,
                  donorage = input$donorage,
                  donorgendercode = input$donorgendercode,
                  donor_death = input$donor_death,
                  coronaryarterycode = input$coronaryarterycode,
                  cerebrovasularcode = input$cerebrovasularcode,
                  diabetescode_text = input$diabetescode_text,
                  primaryrenaldiseasecode = input$primaryrenaldiseasecode,
                  timeondialysis = input$timeondialysis,
                  hlamismatchesdr = input$hlamismatchesdr,
                  egfr_ckdepi = c(input$egfr1,
                                  input$egfr2,
                                  input$egfr3,
                                  input$egfr4,
                                  input$egfr5,
                                  input$egfr6))    
  })
  output$table_dataSurv <- renderTable({
    t(data_inputSurv())
  }, rownames = TRUE, colnames = FALSE)
  data_inputLong <- reactive({
    predData_Long(egfr_ckdepi = c(input$egfr1,
                                  input$egfr2,
                                  input$egfr3,
                                  input$egfr4,
                                  input$egfr5,
                                  input$egfr6),
                  age = input$age,
                  gendercode = input$gendercode,
                  bmi = input$bmi,
                  smokingcode = input$smokingcode,
                  racialorigincode = input$racialorigincode,
                  donorage = input$donorage,
                  donorgendercode = input$donorgendercode,
                  donor_death = input$donor_death,
                  coronaryarterycode = input$coronaryarterycode,
                  cerebrovasularcode = input$cerebrovasularcode,
                  diabetescode_text = input$diabetescode_text,
                  primaryrenaldiseasecode = input$primaryrenaldiseasecode,
                  timeondialysis = input$timeondialysis,
                  hlamismatchesdr = input$hlamismatchesdr)
  })
  ##
  projections <- eventReactive(input$run_button_proj,{
    #useShinyalert(force = TRUE)  # Initialize shinyalert
    shinyalert(
      title = "Running Model",
      text = "The model is currently running...",
      timer = 5000,
      type = "info"
    )
    eGFR = posterior_traj(model_output()$mod_graftloss, 
                          m = 1,
                          newdataLong = data_inputLong(),
                          newdataEvent = data_inputSurv(),
                          extrapolate = TRUE,
                          last_time = "transplantperiod_yr")
    Graft = posterior_survfit(model_output()$mod_graftloss,
                              newdataLong = data_inputLong(),
                              newdataEvent = data_inputSurv(),
                              last_time = "transplantperiod_yr")    
    Death = posterior_survfit(model_output()$mod_alivestatus,
                              newdataLong = data_inputLong(),
                              newdataEvent = data_inputSurv(),
                              last_time = "aliveperiod_yr")
    #eGFR = read.csv("p1oo.csv")
    #Graft = read.csv("p2oo.csv")
    #Death = read.csv("p3oo.csv")
    shinyalert(
      title = "Finished",
      text = "The model has finished running.",
      type = "success"
    )    
    return(list(eGFR=eGFR, Graft=Graft, Death=Death))
  })
  ##
  options(warn = - 1)
  output$plot_output <- renderPlotly({
    eGFR = projections()$eGFR 
    Graft = projections()$Graft 
    Death = projections()$Death 
    inputLong = data_inputLong()
    plot_fnc(eGFR, Graft,  Death, inputLong)
  })
  ##
}

#shinyApp(ui, server)
