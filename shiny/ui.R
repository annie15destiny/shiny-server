source('helpers.R')
print('source in ui.R')

library(ggplot2)

fluidPage(
  
  # Some custom CSS
  tags$head(
    tags$style(HTML("
                    /* Smaller font for preformatted text */
                    pre, table.table {
                    font-size: smaller;
                    }
                    
                    body {
                    min-height: 2000px;
                    }
                    
                    .option-group {
                    border: 1px solid #ccc;
                    border-radius: 6px;
                    padding: 0px 5px;
                    margin: 5px -10px;
                    background-color: #f5f5f5;
                    height: 390px;
                    }
                    
                    .option-header {
                    color: #79d;
                    text-transform: uppercase;
                    margin-bottom: 5px;
                    }
                    "))
    ),
  
  
  
  
  title = "TCM Study",
  
  titlePanel("Transitional Care Model Flight Simulator"),   
  
  fluidRow(
    
    column(8, #offset = 1,
           
           tabsetPanel(id = "OutTabset",
                       
                       tabPanel("Cost Analysis Integrated View", #h2("This is the second panel."),
                                #column(4,plotOutput('cost_analysis_p')))
                                plotOutput('cost_analysis_p')),
                                #checkboxInput("show_text", "Detailed View", value=FALSE)),
                       

                       tabPanel("Cost Analysis Text View", h4("Details:"),
                                verbatimTextOutput("text"))

                       # conditionalPanel(condition="show_text=='TRUE'",
                       #                  tabPanel("Cost Analysis Text View", h4("Details:"),
                       #                           verbatimTextOutput("text")))                       
           )# end of panel            
           
    )# end of column
    
    
    
    
  ),  
  
  fluidRow(
    column(4,
           #h4(""),
           div(class = "option-group",
               div(class = "option-header", "Provider"),
               selectizeInput('search_hosp', "Search Hospital", choices = Hospital_General_Information_simp$Hospital_Name,
                              selected="PENN PRESBYTERIAN MEDICAL CENTER",width=400),
               sliderInput("Operating_Margin", "Operating Margin (percentage)",
                           min=0, max=50, value=5, step=1),
               selectInput("bed_refill", "New Patients Bed Occupancy/Loss of Admission", c("Default" = "Default", "Enter a Number" = "Enter_num"),selected = "Default"),
               conditionalPanel("input.bed_refill==='Enter_num'",numericInput("bed_refill_num", "Enter the Rate (percentage)",value = 0))#submitButton("Submit")
               
           )
    ),
    
    
    column(width = 4,
           div(class = "option-group",
               div(class = "option-header", "TCM candidates"),
               sliderInput("Multiple_Chronic_Conditions_Perc", "Percentage of Medicare Inpatient Beneficiaries with Multiple Chronic Conditions (percentage)",
                           min=20, max=100, value=70, step=5),
               sliderInput("Enrollee_candidates_ratio", "TCM Enrollment (percentage)",
                           min=0, max=100, value=80, step=5),
               checkboxInput("select_DRG", "Selected Diagnoses", value=FALSE)
               #selectInput("TCM_Enrollee", "Number of TCM Enrollees", c("Default" = "Default", "Enter a Number" = "Enter_num"),selected = "Default")
       
           )
           
    )
    
       
  ),  
  
  fluidRow(  
    column(width = 4,
           div(class = "option-group",
               div(class = "option-header", "TCM Operation"),
               selectInput("Staffing", "Staffing",c("Advanced Practice Nurse" = 1,"Registered Nurse" = 2,"Community Health Worker" = 3,"Combined Workforce" = 4),
                           selected=4),
               selectInput("Intervention_Length", "Intervention Length",c("1 Month" = 1,"2 Months" = 2,"9 Months" = 9),selected=9),
               selectInput("start_up_cost_ui", "Initial Cost", c("Default" = "Default", "Enter a Number" = "Enter_num"),selected = "Default"),
               conditionalPanel("input.start_up_cost_ui==='Enter_num'",numericInput("start_up_cost_num", "Enter Initial Cost",value = 1000))
           )
           
    ),
    
    
    column(width = 4,
           div(class = "option-group",
               div(class = "option-header", "Payment Related"),
               selectInput("Payment_System", "Payment System",c("FFS System" = "FFS","Capitation System" = "Capitation","Revenue_Approach" = "Revenue Approach"),selected="FFS"),
               sliderInput("Hosp_perc", "Hospital's proportion of TCM Expenses",min=0, max=1, value=1, step=0.01),
               conditionalPanel("input.Payment_System=='FFS'",sliderInput("Penalty_Scale", "Penalty Scale",min=1, max=30, value=1, step=1),
               sliderInput("Penalty_Limit_value", "Readmissions Adjustment Factor",min=0.9, max=1, value=0.97, step=0.01))
               
           )
           
    )
    
    
    
  ),
  
#   hr(),
  br(),
  
  fluidRow(
    column(8, #offset = 1,
           
           tabsetPanel(id = "inTabset",
                       tabPanel("30-Day Readmission Reduction", #h2("30-Day Readmission Reduction"),
                                
                                column(4,
                                       sliderInput("read_reduction_min", "30-Day Readmission Reduction (percentage) Minimum",
                                                   min=-10, max=50, value=1, step=0.1),
                                       sliderInput("read_reduction_max", "30-Day Readmission Reduction (percentage) Maximum",
                                                   min=-10, max=50, value=6, step=0.1),
                                       sliderInput("read_reduction_mode", "30-Day Readmission Reduction (percentage) Most Likely",
                                                   min=-10, max=50, value=4, step=0.1),
                                       sliderInput("read_reduction_scale", "30-Day Readmission Reduction Distribution Shape",
                                                   min=1, max=20, value=4, step=1)
                                ),
                                column(6,plotOutput('Read_reduction_plot'))),
                       
                       
                       tabPanel("Annual Admission Reduction", #h2("30-Day Readmission Reduction"),
                                
                                column(4,
                                       sliderInput("read_reduction_annual_min", "Annual Admission Reduction (percentage) Minimum",
                                                   min=-10, max=50, value=1, step=0.1),
                                       sliderInput("read_reduction_annual_max", "Annual Admission Reduction (percentage) Maximum",
                                                   min=-10, max=50, value=15, step=0.1),
                                       sliderInput("read_reduction_annual_mode", "Annual Admission Reduction (percentage) Most Likely",
                                                   min=-10, max=50, value=5, step=0.1),
                                       sliderInput("read_reduction_annual_scale", "Annual Admission Reduction Distribution Shape",
                                                   min=1, max=20, value=4, step=1)
                                ),
                                column(6,plotOutput('Read_reduction_annual_plot')))
                       
                       
           )# end of panel            
           
    )# end of column
    
  )#end of fluidRow
  
  
  
    )#fluidPage
