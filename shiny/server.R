source('helpers.R')
print('source in server.R')


library(ggplot2)


function(input, output) {
  
  
  search_i<-reactive({
    as.numeric(as.character(Hospital_General_Information[which(Hospital_General_Information$Hospital_Name == input$search_hosp),1]))
  })


  
  Read_reduction_n_year<-reactive({data.frame("value"=sort(rpert(1000,input$read_reduction_min,input$read_reduction_max,
                                               input$read_reduction_mode,input$read_reduction_scale)))
  })
  
  
  output$Read_reduction_plot<- renderPlot({
    p<-ggplot(Read_reduction_n_year(), aes(x=value,y=..density..)) + geom_histogram(alpha=0.3,bins = 20)+xlab("30-Day Readmission Reduction")+
      geom_density(fill="blue",alpha=0.1)+#scale_y_continuous(limits = c(input$read_reduction_min,input$read_reduction_max))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    print(p)
  })

  
  Read_reduction_annual_n_year<-reactive({data.frame("value"=sort(rpert(1000,input$read_reduction_annual_min,input$read_reduction_annual_max,
                                                                 input$read_reduction_annual_mode,input$read_reduction_annual_scale)))
  })
  
  
  output$Read_reduction_annual_plot<- renderPlot({
    p<-ggplot(Read_reduction_annual_n_year(), aes(x=value,y=..density..)) + geom_histogram(alpha=0.3,bins = 20)+xlab("Admission Reduction")+
      geom_density(fill="blue",alpha=0.1)+#scale_y_continuous(limits = c(input$read_reduction_min,input$read_reduction_max))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    print(p)
  })
  
  
  
  output$text <- renderPrint({
    search_hosp<-as.character(Hospital_General_Information[which(Hospital_General_Information$PROVIDER_NUMBER == search_i()),2])
    #writeLines(paste(search_hosp))
    Operating_Margin<-input$Operating_Margin/100
    select_MCC=FALSE ### MCC defined in DRG
    select_DRG<-input$select_DRG
    Multiple_Chronic_Conditions_Perc<-input$Multiple_Chronic_Conditions_Perc/100
    Enrollee_candidates_ratio<-input$Enrollee_candidates_ratio/100
    bed_refill<-input$bed_refill
    Payment_System<-input$Payment_System
    
    Staffing<-input$Staffing
    Intervention_Length<-input$Intervention_Length
    start_up_cost_ui<-input$start_up_cost_ui
    
    read_reduction_min=input$read_reduction_min/100
    read_reduction_max=input$read_reduction_max/100
    read_reduction_mean=input$read_reduction_mode/100
    read_reduction_scale=input$read_reduction_scale
    Read_reduction_n_year<-data.frame(sort(rpert(1000,read_reduction_min,read_reduction_max,read_reduction_mean,read_reduction_scale)))
    colnames(Read_reduction_n_year)<-"value"
    
    read_reduction_annual_min=input$read_reduction_annual_min/100
    read_reduction_annual_max=input$read_reduction_annual_max/100
    read_reduction_annual_mean=input$read_reduction_annual_mode/100
    read_reduction_annual_scale=input$read_reduction_annual_scale
    Read_reduction_annual_n_year<-data.frame(sort(rpert(1000,read_reduction_annual_min,read_reduction_annual_max,read_reduction_annual_mean,read_reduction_annual_scale)))
    colnames(Read_reduction_annual_n_year)<-"value"
    number_yrs=1
    
    
    Penalty_Scale<-input$Penalty_Scale
    Penalty_Limit_value<-input$Penalty_Limit_value
    Payment_System<-input$Payment_System
    Hosp_perc<-input$Hosp_perc
    
    writeLines(paste("Total Number of Medciare FFS Beneficiary Discharges in 2014:"))
    
    ############################################################################################
    ##########################       Patient Input TCM Exclusion        ########################
    TCM_Inpatients<- subset(DRGData, !(DRG_Number %in% TCM_Exclusion))
    
    writeLines(paste("Not in the TCM exclusion list: ", 
                     format(sum(TCM_Inpatients$Total_Discharges),big.mark=",",scientific=FALSE),
          ", exclude", round(1-sum(TCM_Inpatients$Total_Discharges)/sum(DRGData$Total_Discharges),4)*100,'%' ))
    
    
    
    
    ############################################################################################
    #######################       Patient Input Selected Diagnosis        ######################
    
    if (select_DRG==TRUE)
    {TCM_Inpatients<- subset(DRGData, DRG_Number %in% Selected_DRG)
     
     writeLines(paste("With selected diagnoses: ", 
                      format(sum(TCM_Inpatients$Total_Discharges),big.mark=",",scientific=FALSE),
           ", exclude", round(1-sum(TCM_Inpatients$Total_Discharges)/sum(DRGData$Total_Discharges),4)*100,'%' ))}
    
    
    
    
    ############################################################################################
    ##########################       Patient Input MCC        ########################
    if (select_MCC==TRUE){
      TCM_Inpatients<- subset(TCM_Inpatients, DRG_Severity== "W MCC")}
    
    
    ####### Select Patients with Multiple Chronic Conditions     ###########################################
    #if (select_MCC==FALSE){
      TCM_Inpatients$Total_Discharges<-round(TCM_Inpatients$Total_Discharges*Multiple_Chronic_Conditions_Perc,0)
    #}
    
    writeLines(paste("Multiple Chronic Conditions: ", 
                     format(sum(TCM_Inpatients$Total_Discharges),big.mark=",",scientific=FALSE),
          ", exclude", round(1-sum(TCM_Inpatients$Total_Discharges)/sum(DRGData$Total_Discharges),4)*100,'%' ))
    
    
    
    
    ############################################################################################
    #######################       Patient willing to enroll in TCM       ######################
    
    TCM_Inpatients$Total_Discharges<-round(TCM_Inpatients$Total_Discharges*Enrollee_candidates_ratio,0)
    writeLines(paste("Willing to enroll in TCM: ", 
                     format(sum(TCM_Inpatients$Total_Discharges),big.mark=",",scientific=FALSE),
          ", exclude", round(1-sum(TCM_Inpatients$Total_Discharges)/sum(DRGData$Total_Discharges),4)*100,'%' ))
    
    
    
    
    writeLines(paste("                                "))
    writeLines(paste("#####################################################   "))
    writeLines(paste(search_hosp))
    Est_Num_TCM_enrollees<-round(sum(TCM_Inpatients$Total_Discharges[which(TCM_Inpatients$PROVIDER_NUMBER==search_i())])/Discharges_Case_Ratio,0)
    
  writeLines(paste("Number of TCM enrollees", format(Est_Num_TCM_enrollees,big.mark=",",scientific=FALSE)))
    
    ################################################################################################################################
    #####################################          Admission Loss Calculation         ##############################################
    
    ###############      Organize  TCM_Inpatients Data       ################
    DRGData_dt<-data.table(TCM_Inpatients)
    Hospitals_TCM<-DRGData_dt[,list(Average_Total_Payments=sum(Total_Discharges*Average_Total_Payments)/sum(Total_Discharges),
                                    Sum_Total_Discharges=sum(Total_Discharges),
                                    Sum_Total_Payment=sum(Total_Discharges*Average_Total_Payments)),
                              by=PROVIDER_NUMBER]
    
    
    Hospitals_TCM<-merge(Hospital_General_Information_simp, Hospitals_TCM, by= "PROVIDER_NUMBER",all.x=TRUE)
    Hospitals_TCM$Hospital_Name<-as.character(Hospitals_TCM$Hospital_Name)
    ##########################################################################
  if (is.na(Base_data0802$Bed_Utilization[which(Base_data0802$PROVIDER_NUMBER==search_i())])){
    Base_data0802$Bed_Utilization[which(Base_data0802$PROVIDER_NUMBER==search_i())]<-
      IME_GME2013$TOTAL_HOSPITAL_DAYS[which(IME_GME2013$PROVIDER_NUMBER==search_i())]/IME_GME2013$TOTAL_HOSPITAL_BED_DAYS_AVAILABLE[which(IME_GME2013$PROVIDER_NUMBER==search_i())]}
  
    
    if (bed_refill=="Default"){
      bed_refill<-bed_refill_fc(Base_data0802$Bed_Utilization[which(Base_data0802$PROVIDER_NUMBER == search_i())])}

    if (bed_refill=="Enter_num"){
    bed_refill<-input$bed_refill_num/100}
    
    
    
    writeLines(paste("                                "))
    writeLines(paste("#####################################################   "))
    uti<-sprintf("%1.2f%%", Base_data0802$Bed_Utilization[which(Base_data0802$PROVIDER_NUMBER == search_i())]*100)
  

    writeLines(paste("Bed Utilization:",uti))
    writeLines(paste(sprintf("%1.2f%%", bed_refill*100),"of available bed-days due to admission loss are occupied by new patients."))
    
    
    
    search_i_row=which(Hospitals_TCM$PROVIDER_NUMBER == search_i())
    if (Payment_System %in% c("FFS","Revenue_Approach")){
      Hospitals_TCM_Ad_loss<-Hospitals_TCM$Sum_Total_Payment[search_i_row]*Read_reduction_annual_n_year*(1-bed_refill)*Read_pay_adjust ## already include the willingness to enroll
    }
    if (Payment_System=="Capitation"){
      Hospitals_TCM_Ad_loss<-Hospitals_TCM$Sum_Total_Payment[search_i_row]*Read_reduction_annual_n_year*Read_pay_adjust ## already include the willingness to enroll
    }
    Hospitals_TCM_Ad_loss<-as.data.frame(Hospitals_TCM_Ad_loss)


    AD_annual_change<-1
    for (i in 1:number_yrs){
      AD_annual_change<-cbind(AD_annual_change,AD_annual_change[i]*Discharge_annual_change*Ad_payment_annual_change)
      Hospitals_TCM_Ad_loss[,i]=Hospitals_TCM_Ad_loss[,i]*AD_annual_change[i]}

    # ggplot(Hospitals_TCM_Ad_loss, aes(x=Hospitals_TCM_Ad_loss, y=..density..)) +
    #   geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
    #   geom_density()+scale_x_continuous(paste(simpleCap(Hospitals_TCM$Hospital_Name[search_i_row]),"Admission Reduction Loss"),labels = scales::dollar)+
    #   theme(axis.title.y=element_blank(),
    #         axis.text.y=element_blank(),
    #         axis.ticks.y=element_blank())


    writeLines(paste("                                "))
    
    
    if (Payment_System=="FFS"){
    writeLines(paste("Average Annual Loss of Income due to Admission Reduction: ", format_money(mean(Hospitals_TCM_Ad_loss[,1]*Operating_Margin))))}
    if (Payment_System=="Capitation"){
    writeLines(paste("Average Annual Cost Saving due to Admission Reduction: ", format_money(mean(Hospitals_TCM_Ad_loss[,1]*(1-Operating_Margin))),"(No bed utilization effect)"))}
    if (Payment_System=="Revenue_Approach"){
      writeLines(paste("Average Annual Revenue Loss due to Admission Reduction: ", format_money(mean(Hospitals_TCM_Ad_loss[,1]))))}


    
    
    
    
    
    
    
    
    
    ##############################################################################################################################
    #####################################             Penalty Calculation           ##############################################
    
    #READMISSION_REDUCTION_RATE_2015reshape <- read.csv("READMISSION_REDUCTION_RATE_2015reshape.csv")
    
    
    Penalty_Inpatients<- subset(DRGData, DRG_Number %in% Penalty_DRG)
    
    Penalty_Inpatient_sub<-subset(Penalty_Inpatients,select=c('PROVIDER_NUMBER','Diagnosis','Total_Discharges','Average_Total_Payments'))
    #Penalty_Inpatient_sub$Total_Discharges<-round(Penalty_Inpatient_sub$Total_Discharges*Enrollee_candidates_ratio,0) #double count for the TCM_enrollees_Total_cases_ratio
    Penalty_Inpatient_sub_dt<-data.table(Penalty_Inpatient_sub)
    Penalty_Inpatient_sub_reshape<-Penalty_Inpatient_sub_dt[,list(
      Sum_Total_Discharges=sum(Total_Discharges),
      Sum_Total_Payments=sum(Total_Discharges*Average_Total_Payments)),
      by=list(PROVIDER_NUMBER, Diagnosis)]
    ## the sum is for the same diagnosis but different level of complications
    Penalty_Inpatient_sub_reshape<-as.data.frame(Penalty_Inpatient_sub_reshape)
    
    if (Year==2014){
      
      READMISSION_REDUCTION_RATE_penalty<-subset(READMISSION_REDUCTION_RATE,select=
                                                   c('Provider_Number','Measure_Name','Number_of_Discharges_2014','Expected_Readmission_Rate_2014','Actual_Readmission_Rate_2014'))
      Penalty_Inpatient_sub_reshape<-merge(Penalty_Inpatient_sub_reshape,READMISSION_REDUCTION_RATE_penalty,by.x =c('PROVIDER_NUMBER','Diagnosis'),by.y = c('Provider_Number','Measure_Name'),all=TRUE )                                           
    }
    
    colnames(Penalty_Inpatient_sub_reshape)<-c("PROVIDER_NUMBER" ,"Diagnosis","Sum_Total_Discharges","Sum_Total_Payments","Number_of_Discharges","Expected_Readmission_Rate","Actual_Readmission_Rate" )

    
    
    #convert discharges to cases
    #Penalty_Inpatient_sub_reshape$Sum_Total_Cases<-Penalty_Inpatient_sub_reshape$Sum_Total_Discharges/Discharges_Case_Ratio
    
    ## duplicated boring code
    ## Penalty_Inpatient_sub<-subset(Penalty_Inpatients,select=c('PROVIDER_NUMBER','Diagnosis','Total_Discharges'))
    ## Penalty_Inpatient_sub$Diagnosis<-as.factor(Penalty_Inpatient_sub$Diagnosis)
    ## Penalty_Inpatient_sub_reshape<-cast(Penalty_Inpatient_sub, PROVIDER_NUMBER~Diagnosis,sum)
    
    
    if (Year==2014){Actual_Factor<-PUF2014$X2014_Factor[which( PUF2014$PROV==search_i())]}
    #READMISSION_REDUCTION_RATE$expected_read_change[which(READMISSION_REDUCTION_RATE$Provider_Number==search_i&READMISSION_REDUCTION_RATE$Measure_Name=="AMI")]
    if (Payment_System %in% c("FFS","Revenue_Approach")){
      writeLines(paste("                                "))
    writeLines(paste("#####################################################   "))
    writeLines(paste("Readmissions Adjustment Factor in 2014:",Actual_Factor))}
    
    
    READMISSION_REDUCTION_RATE_selectedHosp<-subset(READMISSION_REDUCTION_RATE,select=c(Provider_Number,Measure_Name,expected_read_change,Y15_14_actual_read_change), Provider_Number==search_i())
    Penalty_Inpatient_selectedHosp<-Penalty_Inpatient_sub_reshape[which(Penalty_Inpatient_sub_reshape$PROVIDER_NUMBER==search_i()),]
    Penalty_Inpatient_selectedHosp<-merge(Penalty_Inpatient_selectedHosp,READMISSION_REDUCTION_RATE_selectedHosp,
                                          by.x =c('PROVIDER_NUMBER','Diagnosis'),by.y = c('Provider_Number','Measure_Name'))
    

    
    
    ###############adding regression to calcuate annual change forth e actual readmission changes begin ##################
    
    Penalty_Inpatient_selectedHosp$reg_actual_read_change<-Penalty_Inpatient_selectedHosp$Y15_14_actual_read_change
    Penalty_Inpatient_selectedHosp$final_actual_read_change<-Penalty_Inpatient_selectedHosp$Y15_14_actual_read_change
    for (j in 1:nrow(Penalty_Inpatient_selectedHosp)){
      match_read<-which(Readmission_Rate_Change$Readmission_Rate==round(Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[j],3))
      if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="AMI"){
        AMI_L<-Readmission_Rate_Change$AMI_L[match_read]
        AMI_H<-Readmission_Rate_Change$AMI_H[match_read]
        Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,AMI_L,AMI_H)}else 
          if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="COPD"){
            COPD_L<-Readmission_Rate_Change$COPD_L[match_read]
            COPD_H<-Readmission_Rate_Change$COPD_H[match_read]
            Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,COPD_L,COPD_H)}else 
              if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="HF"){
                HF_L<-Readmission_Rate_Change$HF_L[match_read]
                HF_H<-Readmission_Rate_Change$HF_H[match_read]
                Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,HF_L,HF_H)}else
                  if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="HIP"){
                    HIP_L<-Readmission_Rate_Change$HIP_L[match_read]
                    HIP_H<-Readmission_Rate_Change$HIP_H[match_read]
                    Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,HIP_L,HIP_H)}else 
                      if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="PNE"){
                        PNE_L<-Readmission_Rate_Change$PNE_L[match_read]
                        PNE_H<-Readmission_Rate_Change$PNE_H[match_read]
                        Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,PNE_L,PNE_H)}
      
    }
    Penalty_Inpatient_selectedHosp$final_actual_read_change<-reg_read_change_perc*Penalty_Inpatient_selectedHosp$reg_actual_read_change+
      (1-reg_read_change_perc)*Penalty_Inpatient_selectedHosp$Y15_14_actual_read_change
    Penalty_Inpatient_selectedHosp$year<-2014
    
    
    
    if (Payment_System %in% c("FFS","Revenue_Approach")){

    writeLines(paste("                                "))
    writeLines(paste("Diagnosis   ","30-Day Expected Readmission    ", "30-Day Acutal Readmission"))
    # for (wl in 1: nrow(Penalty_Inpatient_selectedHosp)){
    #   writeLines(paste(Penalty_Inpatient_selectedHosp$Diagnosis[wl],"                   ",
    #                    sprintf("%1.2f%%",Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[wl]),"                       ",
    #                    sprintf("%1.2f%%", Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[wl]*100)))}

    
      writeLines(paste(Penalty_Inpatient_selectedHosp$Diagnosis[1],"                   ",
                       sprintf("%1.2f%%",Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[1]),"                       ",
                       sprintf("%1.2f%%", Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[1]*100)))

      writeLines(paste(Penalty_Inpatient_selectedHosp$Diagnosis[2],"                  ",
                       sprintf("%1.2f%%",Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[2]),"                       ",
                       sprintf("%1.2f%%", Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[2]*100)))    
    
      writeLines(paste(Penalty_Inpatient_selectedHosp$Diagnosis[3],"                    ",
                       sprintf("%1.2f%%",Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[3]),"                       ",
                       sprintf("%1.2f%%", Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[3]*100)))  
      
      writeLines(paste(Penalty_Inpatient_selectedHosp$Diagnosis[4],"                   ",
                       sprintf("%1.2f%%",Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[4]),"                        ",
                       sprintf("%1.2f%%", Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[4]*100)))
      
      writeLines(paste(Penalty_Inpatient_selectedHosp$Diagnosis[5],"                   ",
                       sprintf("%1.2f%%",Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[5]),"                       ",
                       sprintf("%1.2f%%", Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[5]*100)))
      
    }
      
      Penalty_Inpatient_selectedHosp_allyr<-Penalty_Inpatient_selectedHosp
    ###############adding regression to calcuate annual change forth e actual readmission changes end ##################
    
    #save all the readmission information for future years
    if (number_yrs>1){
      for (i in 2:number_yrs) {
        Penalty_Inpatient_selectedHosp$year<-2013+i
        Penalty_Inpatient_selectedHosp$Sum_Total_Discharges<-round(Penalty_Inpatient_selectedHosp$Sum_Total_Discharges*(Discharge_annual_change^(i-1)),0)
        Penalty_Inpatient_selectedHosp$Number_of_Discharges<-round(Penalty_Inpatient_selectedHosp$Number_of_Discharges*(Discharge_annual_change^(i-1)),0)
        Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate<-Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate*(Penalty_Inpatient_selectedHosp$expected_read_change^(i-1))
        Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate<-Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate*(Penalty_Inpatient_selectedHosp$final_actual_read_change^(i-1))
        Penalty_Inpatient_selectedHosp_allyr<-rbind(Penalty_Inpatient_selectedHosp_allyr,Penalty_Inpatient_selectedHosp)   
        
      }
    }
    # PROVIDER_NUMBER Diagnosis Sum_Total_Discharges Sum_Total_Payments Number_of_Discharges Expected_Readmission_Rate Actual_Readmission_Rate
    # 13044          390223       AMI                   75             904806                  594                      17.9              0.16835017
    # 13045          390223      COPD                   38             293450                  121                      21.5              0.24793388
    # 13046          390223        HF                  174            1771871                  606                      22.9              0.19636964
    # 13047          390223       HIP                   NA                 NA                  373                       5.6              0.08579088
    # 13048          390223       PNE                   44             449171                  118                      18.2              0.22881356
    #   # # 
    # TCM_HF_enrollees<-round(Penalty_Inpatient_sub_reshape$HF[which(Penalty_Inpatient_sub_reshape$PROVIDER_NUMBER==search_i)],0)
    # Total_HF_cases<-READMISSION_REDUCTION_RATE_reshape$Number_of_Discharges_2015_HF[which(READMISSION_REDUCTION_RATE_reshape$Provider_Number==search_i)]
    
    Penalty_Inpatient_selectedHosp$TCM_enrollees_Total_cases_ratio<-Enrollee_candidates_ratio*Penalty_Inpatient_selectedHosp$Sum_Total_Discharges/Penalty_Inpatient_selectedHosp$Number_of_Discharges 
    
    
    if(all(DRG_COPD %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="COPD")]=0}
    if(all(DRG_HF %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="HF")]=0}
    if(all(DRG_AMI %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="AMI")]=0}
    if(all(DRG_PNE %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="PNE")]=0}
    if(all(DRG_HIP %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="HIP")]=0}
    
    # because the sum of total discharges DRG under one diagnosis is different in Readmission Rate file and Inpatient cost file, thus we have to adjust the ratio
    if (Readmt_ApplyAll<-TRUE){TCM_enrollees_Total_cases_ratio=rep(Enrollee_candidates_ratio,nrow(Penalty_Inpatient_selectedHosp_allyr))}
    
    
    
    
    
    EST_penalty_reduction_n_years<-data.frame(matrix(NA, nrow = NumPert,ncol=number_yrs))
    
    for (k in 1:number_yrs){
      ###################                   Calculate Excess ratio for all diagnosis             #######################################################
      excess_ratio<-data.frame(matrix(NA, nrow = NumPert, ncol = nrow(Penalty_Inpatient_selectedHosp)))
      actual_read<-data.frame(matrix(NA, nrow = NumPert, ncol = nrow(Penalty_Inpatient_selectedHosp)))
      
      
      Penalty_Inpatient_selectedHosp<-subset(Penalty_Inpatient_selectedHosp_allyr,year==2013+k)
      
      for (i in 1: nrow(Penalty_Inpatient_selectedHosp)){
        
        actual_read[,i]<-as.data.frame((TCM_enrollees_Total_cases_ratio[i]*(Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i]-Read_reduction_n_year[,k])+
                                          (1-TCM_enrollees_Total_cases_ratio[i])*Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i]))
        
        excess_ratio[,i]<-as.data.frame((TCM_enrollees_Total_cases_ratio[i]*(Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i]-Read_reduction_n_year[,k])+
                                           (1-TCM_enrollees_Total_cases_ratio[i])*Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i])*100/
                                          Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[i])}
      
      colnames(actual_read)=Penalty_Inpatient_selectedHosp$Diagnosis
      colnames(excess_ratio)=Penalty_Inpatient_selectedHosp$Diagnosis
      actual_read_mean<-colMeans(actual_read, na.rm = TRUE)
      excess_ratio[excess_ratio<=1]<-0
      excess_ratio[excess_ratio>1]<-excess_ratio[excess_ratio>1]-1
      
      
      ########   using the same number to replicate the excess ratio, in order to make penalty compariable   ###########################################
      excess_ratio_actual<-data.frame(matrix(NA, nrow = 1, ncol = nrow(Penalty_Inpatient_selectedHosp)))
      for (i in 1: nrow(Penalty_Inpatient_selectedHosp)){
        excess_ratio_actual[,i]<-Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i]*100/
          Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[i]}
      
      excess_ratio_actual[excess_ratio_actual<=1]<-0
      excess_ratio_actual[excess_ratio_actual>1]<-excess_ratio_actual[excess_ratio_actual>1]-1   
      
      
      
      
      ###################                   Calculate EST factor and penalty for selected hosp  #######################################################
      for (i in 1: nrow(Penalty_Inpatient_selectedHosp)){
        excess_ratio[,i]<- excess_ratio[,i]*Penalty_Inpatient_selectedHosp$Sum_Total_Payments[i]
        excess_ratio_actual[,i]<-excess_ratio_actual[,i]*Penalty_Inpatient_selectedHosp$Sum_Total_Payments[i]}
      
      excess_ratio$Aggregate_payments_excess<-rowSums(excess_ratio,na.rm=TRUE)##1000 samples
      excess_ratio_actual$Aggregate_payments_excess<-sum(excess_ratio_actual,na.rm = TRUE)##1 actual number
      
      
      
      ####assume effect of readmission reduction on Aggregate_payments_all_discharge is ignorable
      #if missing data####
      if (is.na(Base_data0802$TOTAL_HOSPITAL_MEDICARE_DISCHARGES[which(Base_data0802$PROVIDER_NUMBER==search_i())])){
        Base_data0802$TOTAL_HOSPITAL_MEDICARE_DISCHARGES[which(Base_data0802$PROVIDER_NUMBER==search_i())]<-IME_GME2013$TOTAL_HOSPITAL_MEDICARE_DISCHARGES[which(IME_GME2013$PROVIDER_NUMBER==search_i())]}
      
      Aggregate_payments_all_discharge<-Base_data0802$TOTAL_HOSPITAL_MEDICARE_DISCHARGES[which(Base_data0802$PROVIDER_NUMBER==search_i())]*
        Base_data0802$Average_100DRG_Total_Payments[which(Base_data0802$PROVIDER_NUMBER==search_i())]   
      
      excess_ratio$EST_Factor<-round(1-excess_ratio$Aggregate_payments_excess/Aggregate_payments_all_discharge,4)
      excess_ratio_actual$EST_Factor<-round(1-excess_ratio_actual$Aggregate_payments_excess/Aggregate_payments_all_discharge,4)
      
      if (Penalty_Limit==TRUE){
        excess_ratio$EST_Factor<-max(excess_ratio$EST_Factor,Penalty_Limit_value)
        excess_ratio_actual$EST_Factor<-max(excess_ratio_actual$EST_Factor,Penalty_Limit_value)
      }
      excess_ratio$EST_penalty<-excess_ratio$EST_Factor*Aggregate_payments_all_discharge
      excess_ratio$EST_penalty_reduction<-(excess_ratio$EST_Factor-excess_ratio_actual$EST_Factor)*Aggregate_payments_all_discharge*Penalty_Scale
      
      #update the new readmission rate for next year, do not need to update, because without TCM actual readmission going back to norm
      #Penalty_Inpatient_selectedHosp_allyr$Actual_Readmission_Rate[which( Penalty_Inpatient_selectedHosp_allyr$year==2014+k)]<-actual_read_mean
      
      EST_penalty_reduction_n_years[,k]<-excess_ratio$EST_penalty_reduction
    }
    
    if (Payment_System %in% c("FFS","Revenue_Approach")){
      writeLines(paste("                                "))
    writeLines(paste("Average Annual Savings from Readmission Penalty: ", format_money(mean(EST_penalty_reduction_n_years[,1]))))}
    
    
    
    
    ########################################################################################################################
    #####################################            TCM Cost Analysis           ##############################################
    
    
    
    NURSE_PRACTIONER_H_SALARY=rpert(1000,67,80,73,4)
    REGISTERED_NURSE_H_SALARY=rpert(1000,46,59,52,4)
    COMMUNITY_HEALTH_WORKER_H_SALARY=rpert(1000,19,34,23.5,4)
    Combined_H_SALARY=rpert(1000,55,75,67,4)
    
    #No adjustment
    one_month_hour=rpert(1000,10,22,14,4)
    two_month_intensive_hours=rpert(1000,13,27,18,4)
    densityrank_i=1
    
    seven_month_level_1_hours=rpert(1000,3,5,3.8,4)
    seven_month_level_2_hours=rpert(1000,10,24,11.4,4) 
    seven_month_level_3_hours=rpert(1000,18,35,20,4)
    seven_month_level_1_perc=0.4
    seven_month_level_2_perc=0.4
    seven_month_level_3_perc=0.2
    
    #With density adjustment:
    one_month_hour=rpert(1000,12,15,13,4)
    two_month_intensive_hours=rpert(1000,15,18,16,4)
    
    seven_month_level_1_hours=rpert(1000,3,4,3.25,4)
    seven_month_level_2_hours=rpert(1000,8.5,16,10.5,4) 
    seven_month_level_3_hours=rpert(1000,15.5,24,17.2,4)
    seven_month_level_1_perc=0.4
    seven_month_level_2_perc=0.4
    seven_month_level_3_perc=0.2
    
    #three_month_hours=rpert(1000,16,33,22,4)
    #six_month_hours=rpert(1000,22,40,26,4)
    #nine_month_hours=rpert(1000,22,40,26,4)
    
    # 1 month intervention hours = uniform (12,15)*[0.6*(1-population density)+0.9] (default) range(10-22)
    # 2 month intervention hours = uniform (15,18)*[0.6*(1-population density)+0.9] (default) range(13-27)
    # 3 month intervention hours = uniform (18,22)*[0.6*(1-population density)+0.9] (default) range(16-33)
    # 6 month intervention hours = uniform (22,27)*[0.6*(1-population density)+0.9] (default) range(20-40)
    # population density look for excel, "densityrank" column
    
    
    
    # Staffing=1
    # 
    # # Advanced Practice Nurse = 1 
    # # Registered Nurse = 2
    # # Community Health Worker = 3
    # # Combined Workforce =4
    # 
    # Intervention_Length =2 
    # 
    # # 1 Month 
    # # 2 Month 
    # # 9 Month 
    #   # Under 9 month intervention: in the first 2 months, using Advanced Practice Nurses for intensive care,
    #   # in the following 7 months, using community health workers for level 1 care, using Registered Nurse for level 2 care, and Advanced Practice Nurse for level 3 care.
    #   
    
    ###################TCM Cost###########################################
    
    temp_i<-which(Base_data0802$PROVIDER_NUMBER==search_i())
    Salary_Adjusted_Factor_i<-Base_data0802$Salary_Adjusted_Factor[temp_i]
    densityrank_i<-Base_data0802$densityrank[temp_i]
    
    
    if (Staffing==1){Staff_SALARY=NURSE_PRACTIONER_H_SALARY} else 
      if (Staffing==2){Staff_SALARY=REGISTERED_NURSE_H_SALARY}else
        if (Staffing==3){Staff_SALARY=COMMUNITY_HEALTH_WORKER_H_SALARY}else 
          if (Staffing==4){Staff_SALARY=Combined_H_SALARY}else {
            print("Wrong staffing!")}
    
    TCM_cost_per_patient_1month<-Salary_Adjusted_Factor_i*Staff_SALARY*one_month_hour*densityrank_i
    TCM_cost_per_patient_2month<-Salary_Adjusted_Factor_i*Staff_SALARY*two_month_intensive_hours*densityrank_i
    TCM_cost_per_patient_9month<-Salary_Adjusted_Factor_i*densityrank_i*(Staff_SALARY*(two_month_intensive_hours+
                                                                                         seven_month_level_1_perc*seven_month_level_1_hours+
                                                                                         seven_month_level_2_perc*seven_month_level_2_hours+
                                                                                         seven_month_level_3_perc*seven_month_level_3_hours))
    
    
    
    
    if (Staffing==4 & Intervention_Length ==9 ){
      TCM_cost_per_patient_9month<-Salary_Adjusted_Factor_i*densityrank_i*(NURSE_PRACTIONER_H_SALARY*two_month_intensive_hours+
                                                                             COMMUNITY_HEALTH_WORKER_H_SALARY*seven_month_level_1_perc*seven_month_level_1_hours+
                                                                             REGISTERED_NURSE_H_SALARY*seven_month_level_2_perc*seven_month_level_2_hours+
                                                                             NURSE_PRACTIONER_H_SALARY*seven_month_level_3_perc*seven_month_level_3_hours)
      
    }
    
      #Do not need to *Enrollee_candidates_ratio because already included in TCM_Inpatients file
    
   
    
    writeLines(paste("                                "))
    writeLines(paste("#####################################################   "))
    
    
    if (Intervention_Length==9 ){TCM_cost=data.frame(TCM_cost_per_patient_9month*Est_Num_TCM_enrollees)
    writeLines(paste("TCM cost per patient per enrollment: ", format_money(mean(TCM_cost_per_patient_9month))))
    }else
      if (Intervention_Length==1 ){TCM_cost=data.frame(TCM_cost_per_patient_1month*Est_Num_TCM_enrollees)
      writeLines(paste("TCM cost per patient per enrollment: ", format_money(mean(TCM_cost_per_patient_1month))))
      }else
        if (Intervention_Length==2 ){TCM_cost=data.frame(TCM_cost_per_patient_2month*Est_Num_TCM_enrollees)
        writeLines(paste("TCM cost per patient per enrollment: ", format_money(mean(TCM_cost_per_patient_2month))))
        } else
          Print("Please specify the lenth of intervention: 1 month, 2 months, or 9 months")
    
    
    
    num_care_giver<-ceiling(min(1, Est_Num_TCM_enrollees/runif(1, 20, 40)))
    salary_admin<-runif(1, 80000, 120000)
    
    if (start_up_cost_ui=="Enter_num"){start_up_cost<-rep(1000,input$start_up_cost_num)
    writeLines(paste("TCM Initial Cost: ", format_money(input$start_up_cost_num)))}
    
    if (start_up_cost_ui=="Default"){start_up_cost<-Staff_SALARY*100*num_care_giver+3*4*5*100*runif(1, 5, 20)+salary_admin
    writeLines(paste("TCM Initial Cost: ", format_money(mean(start_up_cost))))}
    #number of nurses required: min(1, Est_Num_TCM_enrollees/runif(1, 20, 40))
    #2 week training cost plus equipment fee if any: Staff_SALARY*100
    # salary_admin: 2 full time administrative workers work for half a year
    # 3*4*5*100*runif(1, 5, 20) mean 5-20 decision makers spend 3 months, every week spend 5 hours on the program, hourly salary $100

    
    M=data.frame(start_up_cost)
    start_up_cost_yearly<-apply(M,1,function(x) PMT(rate=0.03,nper=number_yrs,pv=-x))
    
    
    
    if (fixed_annual_TCM_cost=="Default"){fixed_annual_TCM_cost<-ceiling(num_care_giver/10)*salary_admin}
    
    TCM_cost<-(TCM_cost+start_up_cost_yearly+fixed_annual_TCM_cost)*Hosp_perc
    
    
    colnames(TCM_cost)<-"TCM_cost"
    
    writeLines(paste("TCM Total Cost: ", format_money(mean(TCM_cost$TCM_cost))))
    
    
    
    
    
    
    
    
    
    
})

  

  output$cost_analysis_p<- renderPlot({
    search_hosp<-as.character(Hospital_General_Information[which(Hospital_General_Information$PROVIDER_NUMBER == search_i()),2])
    
    Operating_Margin<-input$Operating_Margin/100

    Multiple_Chronic_Conditions_Perc<-input$Multiple_Chronic_Conditions_Perc/100
    Enrollee_candidates_ratio<-input$Enrollee_candidates_ratio/100
    select_DRG<-input$select_DRG

    Staffing<-input$Staffing
    Intervention_Length<-input$Intervention_Length
    
    start_up_cost_ui<-input$start_up_cost_ui
    Penalty_Scale<-input$Penalty_Scale
    Penalty_Limit_value<-input$Penalty_Limit_value
    Payment_System<-input$Payment_System
    Hosp_perc<-input$Hosp_perc
    
    
    
    
    
    
    
    
    
    
    
    #Multiple_Chronic_Conditions_Perc=0.7  ### in literature review
    select_MCC=FALSE ### MCC defined in DRG
    #select_DRG=FALSE
    #Enrollee_candidates_ratio=0.8 ###What is the percentage of TCM candidates that are willing to enroll in TCM program
    
    
    ###############################################################################################
    ###### related to payment systems and hospital margin  #########################################
    #Payment_System<-"FFS" # choose from "Capitation", "FFS" or "Revenue_Approach"
    #Operating_Margin<-0.05  ###this is important, admission loss * margin = financial loss
    
    
    ##################################################################################
    ###### related to program Effectiveness  #########################################
    #Read_reduction<-sort(rpert(1000,0.0001,0.1,0.05,4)) #30 days readmission reduction, Note: rpert <- function( n, x.min, x.max, x.mode, lambda)
    Adjust_Read_reduction="TRUE"
    #Read_reduction_annual<-sort(rpert(1000,0.02,0.2,0.1,4)) #annual readmission reduction, Note: rpert <- function( n, x.min, x.max, x.mode, lambda)
    # JAGS RCT paper (2004) ALL HF		Min	Mode	Max	Distribution
    # 365 days	Control	0	1.34	8	
    # TCM group	0	0.88	5	
    ##(2.34-1.88)/2.34=0.1966
    
    
    #####################################################################################
    ###### related to program operating issues  #########################################
    
    #Staffing=4
    
    # Advanced Practice Nurse = 1 
    # Registered Nurse = 2
    # Community Health Worker = 3
    # Combined Workforce =4
    
    #Intervention_Length =9 
    
    # 1 Month 
    # 2 Month 
    # 9 Month 
    # Under 9 month intervention: in the first 2 months, using Advanced Practice Nurses for intensive care,
    # in the following 7 months, using community health workers for level 1 care, using Registered Nurse for level 2 care, and Advanced Practice Nurse for level 3 care.
    
    
    #start_up_cost="Default" # Enter a number or "Default", default start up cost includes training cost and administative cost
    fixed_annual_TCM_cost="Default" # Enter a number or "Default", default fixed cost includes administative cost, 1 admin per 5 care givers
    # start_up_cost=0 # Enter a number or "Default", default start up cost includes training cost and administative cost
    # fixed_annual_TCM_cost=0 # Enter a number or "Default", default fixed cost includes administative cost, 1 admin per 5 care givers
    
    
    
    ###########################################################################
    ###### related to policy changes  #########################################
    
    # Readmissions Adjustment Factor = the higher of the Ratio or 0.97 (3% reduction). 
    # (For FY 2013, the higher of the Ratio or 0.99% (1% reduction), and for FY 2014, the higher of the Ratio or 0.98% (2% reduction).)
    #Penalty_Scale=1
    Penalty_Limit=TRUE
    #Penalty_Limit_value=0.97
    
    
    #################################################################
    ###### future variables  ########################################
    
    #scenario one # baseline is the first year
    # Read_reduction_n_year<-cbind(sort(rpert(1000,0.01,0.06,0.02,4)),sort(rpert(1000,0.001,0.8,0.04,4)),sort(rpert(1000,0.01,0.8,0.05,4)),sort(rpert(1000,0.02,0.1,0.05,4))) 
    # Read_reduction_annual_n_year<-cbind(sort(rpert(1000,0.001,0.1,0.05,4)),sort(rpert(1000,0.01,0.15,0.08,4)),sort(rpert(1000,0.02,0.2,0.1,4)),sort(rpert(1000,0.02,0.25,0.1,4))) 
    # 
    # number_yrs=4
    
    # read_reduction_min=-0.02
    # read_reduction_max=0.05
    # read_reduction_mean=0.01
    
    read_reduction_min=input$read_reduction_min/100
    read_reduction_max=input$read_reduction_max/100
    read_reduction_mean=input$read_reduction_mode/100
    read_reduction_scale=input$read_reduction_scale
    Read_reduction_n_year<-data.frame(sort(rpert(1000,read_reduction_min,read_reduction_max,read_reduction_mean,read_reduction_scale)))
    colnames(Read_reduction_n_year)<-"value"
    
    read_reduction_annual_min=input$read_reduction_annual_min/100
    read_reduction_annual_max=input$read_reduction_annual_max/100
    read_reduction_annual_mean=input$read_reduction_annual_mode/100
    read_reduction_annual_scale=input$read_reduction_annual_scale
    Read_reduction_annual_n_year<-data.frame(sort(rpert(1000,read_reduction_annual_min,read_reduction_annual_max,read_reduction_annual_mean,read_reduction_annual_scale)))
    colnames(Read_reduction_annual_n_year)<-"value"
    number_yrs=1
    ############   Find hospital related information  ###########################
    
    Discharge_annual_change<-Discharge_yearly_Change$year_change[which(Discharge_yearly_Change$PROVIDER_NUMBER==search_i())]
    Discharge_annual_change<-0.8*Discharge_annual_change+0.2*0.97
    Ad_payment_annual_change<-1.04  
    bed_refill<-input$bed_refill
    
    
    
    #################################################################
    ###### other variables  #########################################
    
    Read_pay_adjust=1 # if readmitted, the cost payment could be less/greater than first admission payment
    Discharges_Case_Ratio<-1.3 ### averaged number of discharges per patient
    
    
    
    
    
    
    
    ##system variables
    NumPert<-1000
    Readmt_ApplyAll<-TRUE ##adjusted for inconsistency of the data
    reg_read_change_perc<-0.5 ##for the actual readmission rate change, the weighting for using the regression model versus using one year one hospital historical change
    
    
    
    
    
    
    
    Year=2014
    
    
    
    
    ############################################################################################
    ##########################       Patient Input TCM Exclusion        ########################
    TCM_Inpatients<- subset(DRGData, !(DRG_Number %in% TCM_Exclusion))
    
    paste("Total Number of Medciare FFS discharges with diagnoses in TCM exclusion list in", Year , "are: ", sum(TCM_Inpatients$Total_Discharges),
          ", exclude", round(1-sum(TCM_Inpatients$Total_Discharges)/sum(DRGData$Total_Discharges),4)*100,'%' )
    
    
    
    
    ############################################################################################
    #######################       Patient Input Selected Diagnosis        ######################
    
    if (select_DRG==TRUE)
    {TCM_Inpatients<- subset(DRGData, DRG_Number %in% Selected_DRG)
    
    paste("Total Number of Medciare FFS discharges with diagnoses in TCM exclusion list in", Year , "are: ", sum(TCM_Inpatients$Total_Discharges),
          ", exclude", round(1-sum(TCM_Inpatients$Total_Discharges)/sum(DRGData$Total_Discharges),4)*100,'%' )}
    
    
    
    
    ############################################################################################
    ##########################       Patient Input MCC        ########################
    if (select_MCC==TRUE){
      TCM_Inpatients<- subset(TCM_Inpatients, DRG_Severity== "W MCC")}
    
    
    ####### Select Patients with Multiple Chronic Conditions     ###########################################
    if (select_MCC==FALSE){
      TCM_Inpatients$Total_Discharges<-round(TCM_Inpatients$Total_Discharges*Multiple_Chronic_Conditions_Perc,0)
    }
    
    paste("Total Number of Medciare FFS discharges with diagnoses in TCM exclusion list in", Year , "are: ", sum(TCM_Inpatients$Total_Discharges),
          ", exclude", round(1-sum(TCM_Inpatients$Total_Discharges)/sum(DRGData$Total_Discharges),4)*100,'%' )
    
    
    
    
    ############################################################################################
    #######################       Patient willing to enroll in TCM       ######################
    
    TCM_Inpatients$Total_Discharges<-round(TCM_Inpatients$Total_Discharges*Enrollee_candidates_ratio,0)
    paste("Total Number of Medciare FFS discharges with diagnoses in TCM exclusion list and are willing to enroll in TCM in", Year , "are: ", sum(TCM_Inpatients$Total_Discharges),
          ", exclude", round(1-sum(TCM_Inpatients$Total_Discharges)/sum(DRGData$Total_Discharges),4)*100,'%' )
    
    
    
    
    ################################################################################################################################
    #####################################          Admission Loss Calculation         ##############################################
    
    ###############      Organize  TCM_Inpatients Data       ################
    DRGData_dt<-data.table(TCM_Inpatients)
    Hospitals_TCM<-DRGData_dt[,list(Average_Total_Payments=sum(Total_Discharges*Average_Total_Payments)/sum(Total_Discharges),
                                    Sum_Total_Discharges=sum(Total_Discharges),
                                    Sum_Total_Payment=sum(Total_Discharges*Average_Total_Payments)),
                              by=PROVIDER_NUMBER]
    
    
    Hospitals_TCM<-merge(Hospital_General_Information_simp, Hospitals_TCM, by= "PROVIDER_NUMBER",all.x=TRUE)
    Hospitals_TCM$Hospital_Name<-as.character(Hospitals_TCM$Hospital_Name)
    ##########################################################################
    if (is.na(Base_data0802$Bed_Utilization[which(Base_data0802$PROVIDER_NUMBER==search_i())])){
      Base_data0802$Bed_Utilization[which(Base_data0802$PROVIDER_NUMBER==search_i())]<-
        IME_GME2013$TOTAL_HOSPITAL_DAYS[which(IME_GME2013$PROVIDER_NUMBER==search_i())]/IME_GME2013$TOTAL_HOSPITAL_BED_DAYS_AVAILABLE[which(IME_GME2013$PROVIDER_NUMBER==search_i())]}
    
    if (bed_refill=="Default"){
      bed_refill<-bed_refill_fc(Base_data0802$Bed_Utilization[which(Base_data0802$PROVIDER_NUMBER == search_i())])}
    
    if (bed_refill=="Enter_num"){
      bed_refill<-input$bed_refill_num/100}
    
    search_i_row=which(Hospitals_TCM$PROVIDER_NUMBER == search_i())
    if (Payment_System %in% c("FFS","Revenue_Approach")){
      Hospitals_TCM_Ad_loss<-Hospitals_TCM$Sum_Total_Payment[search_i_row]*Read_reduction_annual_n_year*(1-bed_refill)*Read_pay_adjust ## already include the willingness to enroll
    }
    if (Payment_System=="Capitation"){
      Hospitals_TCM_Ad_loss<-Hospitals_TCM$Sum_Total_Payment[search_i_row]*Read_reduction_annual_n_year*Read_pay_adjust ## already include the willingness to enroll
    }
    Hospitals_TCM_Ad_loss<-as.data.frame(Hospitals_TCM_Ad_loss)
    
    
    AD_annual_change<-1
    for (i in 1:number_yrs){
      AD_annual_change<-cbind(AD_annual_change,AD_annual_change[i]*Discharge_annual_change*Ad_payment_annual_change)
      Hospitals_TCM_Ad_loss[,i]=Hospitals_TCM_Ad_loss[,i]*AD_annual_change[i]}
    
    # ggplot(Hospitals_TCM_Ad_loss, aes(x=Hospitals_TCM_Ad_loss, y=..density..)) +
    #   geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
    #   geom_density()+scale_x_continuous(paste(simpleCap(Hospitals_TCM$Hospital_Name[search_i_row]),"Admission Reduction Loss"),labels = scales::dollar)+
    #   theme(axis.title.y=element_blank(),
    #         axis.text.y=element_blank(),
    #         axis.ticks.y=element_blank())
    
    
    
    
    
    
    ##############################################################################################################################
    #####################################             Penalty Calculation           ##############################################
    
    #READMISSION_REDUCTION_RATE_2015reshape <- read.csv("READMISSION_REDUCTION_RATE_2015reshape.csv")
    
    
    Penalty_Inpatients<- subset(DRGData, DRG_Number %in% Penalty_DRG)
    
    Penalty_Inpatient_sub<-subset(Penalty_Inpatients,select=c('PROVIDER_NUMBER','Diagnosis','Total_Discharges','Average_Total_Payments'))
    #Penalty_Inpatient_sub$Total_Discharges<-round(Penalty_Inpatient_sub$Total_Discharges*Enrollee_candidates_ratio,0) #double count for the TCM_enrollees_Total_cases_ratio
    Penalty_Inpatient_sub_dt<-data.table(Penalty_Inpatient_sub)
    Penalty_Inpatient_sub_reshape<-Penalty_Inpatient_sub_dt[,list(
      Sum_Total_Discharges=sum(Total_Discharges),
      Sum_Total_Payments=sum(Total_Discharges*Average_Total_Payments)),
      by=list(PROVIDER_NUMBER, Diagnosis)]
    ## the sum is for the same diagnosis but different level of complications
    Penalty_Inpatient_sub_reshape<-as.data.frame(Penalty_Inpatient_sub_reshape)
    
    if (Year==2014){
      
      READMISSION_REDUCTION_RATE_penalty<-subset(READMISSION_REDUCTION_RATE,select=
                                                   c('Provider_Number','Measure_Name','Number_of_Discharges_2014','Expected_Readmission_Rate_2014','Actual_Readmission_Rate_2014'))
      Penalty_Inpatient_sub_reshape<-merge(Penalty_Inpatient_sub_reshape,READMISSION_REDUCTION_RATE_penalty,by.x =c('PROVIDER_NUMBER','Diagnosis'),by.y = c('Provider_Number','Measure_Name'),all=TRUE )                                           
    }
    
    colnames(Penalty_Inpatient_sub_reshape)<-c("PROVIDER_NUMBER" ,"Diagnosis","Sum_Total_Discharges","Sum_Total_Payments","Number_of_Discharges","Expected_Readmission_Rate","Actual_Readmission_Rate" )
    
    #convert discharges to cases
    #Penalty_Inpatient_sub_reshape$Sum_Total_Cases<-Penalty_Inpatient_sub_reshape$Sum_Total_Discharges/Discharges_Case_Ratio
    
    ## duplicated boring code
    ## Penalty_Inpatient_sub<-subset(Penalty_Inpatients,select=c('PROVIDER_NUMBER','Diagnosis','Total_Discharges'))
    ## Penalty_Inpatient_sub$Diagnosis<-as.factor(Penalty_Inpatient_sub$Diagnosis)
    ## Penalty_Inpatient_sub_reshape<-cast(Penalty_Inpatient_sub, PROVIDER_NUMBER~Diagnosis,sum)
    
    
    if (Year==2014){Actual_Factor<-PUF2014$X2014_Factor[which( PUF2014$PROV==search_i())]}
    #READMISSION_REDUCTION_RATE$expected_read_change[which(READMISSION_REDUCTION_RATE$Provider_Number==search_i&READMISSION_REDUCTION_RATE$Measure_Name=="AMI")]
    
    READMISSION_REDUCTION_RATE_selectedHosp<-subset(READMISSION_REDUCTION_RATE,select=c(Provider_Number,Measure_Name,expected_read_change,Y15_14_actual_read_change), Provider_Number==search_i())
    Penalty_Inpatient_selectedHosp<-Penalty_Inpatient_sub_reshape[which(Penalty_Inpatient_sub_reshape$PROVIDER_NUMBER==search_i()),]
    Penalty_Inpatient_selectedHosp<-merge(Penalty_Inpatient_selectedHosp,READMISSION_REDUCTION_RATE_selectedHosp,
                                          by.x =c('PROVIDER_NUMBER','Diagnosis'),by.y = c('Provider_Number','Measure_Name'))
    
    
    
    ###############adding regression to calcuate annual change forth e actual readmission changes begin ##################
    
    Penalty_Inpatient_selectedHosp$reg_actual_read_change<-Penalty_Inpatient_selectedHosp$Y15_14_actual_read_change
    Penalty_Inpatient_selectedHosp$final_actual_read_change<-Penalty_Inpatient_selectedHosp$Y15_14_actual_read_change
    for (j in 1:nrow(Penalty_Inpatient_selectedHosp)){
      match_read<-which(Readmission_Rate_Change$Readmission_Rate==round(Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[j],3))
      if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="AMI"){
        AMI_L<-Readmission_Rate_Change$AMI_L[match_read]
        AMI_H<-Readmission_Rate_Change$AMI_H[match_read]
        Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,AMI_L,AMI_H)}else 
          if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="COPD"){
            COPD_L<-Readmission_Rate_Change$COPD_L[match_read]
            COPD_H<-Readmission_Rate_Change$COPD_H[match_read]
            Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,COPD_L,COPD_H)}else 
              if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="HF"){
                HF_L<-Readmission_Rate_Change$HF_L[match_read]
                HF_H<-Readmission_Rate_Change$HF_H[match_read]
                Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,HF_L,HF_H)}else
                  if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="HIP"){
                    HIP_L<-Readmission_Rate_Change$HIP_L[match_read]
                    HIP_H<-Readmission_Rate_Change$HIP_H[match_read]
                    Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,HIP_L,HIP_H)}else 
                      if (Penalty_Inpatient_selectedHosp$Diagnosis[j]=="PNE"){
                        PNE_L<-Readmission_Rate_Change$PNE_L[match_read]
                        PNE_H<-Readmission_Rate_Change$PNE_H[match_read]
                        Penalty_Inpatient_selectedHosp$reg_actual_read_change[j]<-runif(1,PNE_L,PNE_H)}
      
    }
    Penalty_Inpatient_selectedHosp$final_actual_read_change<-reg_read_change_perc*Penalty_Inpatient_selectedHosp$reg_actual_read_change+
      (1-reg_read_change_perc)*Penalty_Inpatient_selectedHosp$Y15_14_actual_read_change
    Penalty_Inpatient_selectedHosp$year<-2014
    Penalty_Inpatient_selectedHosp_allyr<-Penalty_Inpatient_selectedHosp
    ###############adding regression to calcuate annual change forth e actual readmission changes end ##################
    
    #save all the readmission information for future years
    if (number_yrs>1){
      for (i in 2:number_yrs) {
        Penalty_Inpatient_selectedHosp$year<-2013+i
        Penalty_Inpatient_selectedHosp$Sum_Total_Discharges<-round(Penalty_Inpatient_selectedHosp$Sum_Total_Discharges*(Discharge_annual_change^(i-1)),0)
        Penalty_Inpatient_selectedHosp$Number_of_Discharges<-round(Penalty_Inpatient_selectedHosp$Number_of_Discharges*(Discharge_annual_change^(i-1)),0)
        Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate<-Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate*(Penalty_Inpatient_selectedHosp$expected_read_change^(i-1))
        Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate<-Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate*(Penalty_Inpatient_selectedHosp$final_actual_read_change^(i-1))
        Penalty_Inpatient_selectedHosp_allyr<-rbind(Penalty_Inpatient_selectedHosp_allyr,Penalty_Inpatient_selectedHosp)   
        
      }
    }
    # PROVIDER_NUMBER Diagnosis Sum_Total_Discharges Sum_Total_Payments Number_of_Discharges Expected_Readmission_Rate Actual_Readmission_Rate
    # 13044          390223       AMI                   75             904806                  594                      17.9              0.16835017
    # 13045          390223      COPD                   38             293450                  121                      21.5              0.24793388
    # 13046          390223        HF                  174            1771871                  606                      22.9              0.19636964
    # 13047          390223       HIP                   NA                 NA                  373                       5.6              0.08579088
    # 13048          390223       PNE                   44             449171                  118                      18.2              0.22881356
    #   # # 
    # TCM_HF_enrollees<-round(Penalty_Inpatient_sub_reshape$HF[which(Penalty_Inpatient_sub_reshape$PROVIDER_NUMBER==search_i)],0)
    # Total_HF_cases<-READMISSION_REDUCTION_RATE_reshape$Number_of_Discharges_2015_HF[which(READMISSION_REDUCTION_RATE_reshape$Provider_Number==search_i)]
    
    Penalty_Inpatient_selectedHosp$TCM_enrollees_Total_cases_ratio<-Enrollee_candidates_ratio*Penalty_Inpatient_selectedHosp$Sum_Total_Discharges/Penalty_Inpatient_selectedHosp$Number_of_Discharges 
    
    
    if(all(DRG_COPD %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="COPD")]=0}
    if(all(DRG_HF %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="HF")]=0}
    if(all(DRG_AMI %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="AMI")]=0}
    if(all(DRG_PNE %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="PNE")]=0}
    if(all(DRG_HIP %!in% TCM_Inpatients$DRG_Number=="TRUE")){ Penalty_Inpatient_selectedHosp_allyr$Sum_Total_Discharges[which( Penalty_Inpatient_selectedHosp_allyr$Diagnosis=="HIP")]=0}
    
    # because the sum of total discharges DRG under one diagnosis is different in Readmission Rate file and Inpatient cost file, thus we have to adjust the ratio
    if (Readmt_ApplyAll<-TRUE){TCM_enrollees_Total_cases_ratio=rep(Enrollee_candidates_ratio,nrow(Penalty_Inpatient_selectedHosp_allyr))}
    
    
    
    
    
    EST_penalty_reduction_n_years<-data.frame(matrix(NA, nrow = NumPert,ncol=number_yrs))
    
    for (k in 1:number_yrs){
      ###################                   Calculate Excess ratio for all diagnosis             #######################################################
      excess_ratio<-data.frame(matrix(NA, nrow = NumPert, ncol = nrow(Penalty_Inpatient_selectedHosp)))
      actual_read<-data.frame(matrix(NA, nrow = NumPert, ncol = nrow(Penalty_Inpatient_selectedHosp)))
      
      
      Penalty_Inpatient_selectedHosp<-subset(Penalty_Inpatient_selectedHosp_allyr,year==2013+k)
      
      for (i in 1: nrow(Penalty_Inpatient_selectedHosp)){
        
        actual_read[,i]<-as.data.frame((TCM_enrollees_Total_cases_ratio[i]*(Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i]-Read_reduction_n_year[,k])+
                                          (1-TCM_enrollees_Total_cases_ratio[i])*Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i]))
        
        excess_ratio[,i]<-as.data.frame((TCM_enrollees_Total_cases_ratio[i]*(Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i]-Read_reduction_n_year[,k])+
                                           (1-TCM_enrollees_Total_cases_ratio[i])*Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i])*100/
                                          Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[i])}
      
      colnames(actual_read)=Penalty_Inpatient_selectedHosp$Diagnosis
      colnames(excess_ratio)=Penalty_Inpatient_selectedHosp$Diagnosis
      actual_read_mean<-colMeans(actual_read, na.rm = TRUE)
      excess_ratio[excess_ratio<=1]<-0
      excess_ratio[excess_ratio>1]<-excess_ratio[excess_ratio>1]-1
      
      
      ########   using the same number to replicate the excess ratio, in order to make penalty compariable   ###########################################
      excess_ratio_actual<-data.frame(matrix(NA, nrow = 1, ncol = nrow(Penalty_Inpatient_selectedHosp)))
      for (i in 1: nrow(Penalty_Inpatient_selectedHosp)){
        excess_ratio_actual[,i]<-Penalty_Inpatient_selectedHosp$Actual_Readmission_Rate[i]*100/
          Penalty_Inpatient_selectedHosp$Expected_Readmission_Rate[i]}
      
      excess_ratio_actual[excess_ratio_actual<=1]<-0
      excess_ratio_actual[excess_ratio_actual>1]<-excess_ratio_actual[excess_ratio_actual>1]-1   
      
      
      
      
      ###################                   Calculate EST factor and penalty for selected hosp  #######################################################
      for (i in 1: nrow(Penalty_Inpatient_selectedHosp)){
        excess_ratio[,i]<- excess_ratio[,i]*Penalty_Inpatient_selectedHosp$Sum_Total_Payments[i]
        excess_ratio_actual[,i]<-excess_ratio_actual[,i]*Penalty_Inpatient_selectedHosp$Sum_Total_Payments[i]}
      
      excess_ratio$Aggregate_payments_excess<-rowSums(excess_ratio,na.rm=TRUE)##1000 samples
      excess_ratio_actual$Aggregate_payments_excess<-sum(excess_ratio_actual,na.rm = TRUE)##1 actual number
      
      
      
      ####assume effect of readmission reduction on Aggregate_payments_all_discharge is ignorable
      #if missing data####
      if (is.na(Base_data0802$TOTAL_HOSPITAL_MEDICARE_DISCHARGES[which(Base_data0802$PROVIDER_NUMBER==search_i())])){
        Base_data0802$TOTAL_HOSPITAL_MEDICARE_DISCHARGES[which(Base_data0802$PROVIDER_NUMBER==search_i())]<-IME_GME2013$TOTAL_HOSPITAL_MEDICARE_DISCHARGES[which(IME_GME2013$PROVIDER_NUMBER==search_i())]}
      
      Aggregate_payments_all_discharge<-Base_data0802$TOTAL_HOSPITAL_MEDICARE_DISCHARGES[which(Base_data0802$PROVIDER_NUMBER==search_i())]*
        Base_data0802$Average_100DRG_Total_Payments[which(Base_data0802$PROVIDER_NUMBER==search_i())]   
      
      excess_ratio$EST_Factor<-round(1-excess_ratio$Aggregate_payments_excess/Aggregate_payments_all_discharge,4)
      excess_ratio_actual$EST_Factor<-round(1-excess_ratio_actual$Aggregate_payments_excess/Aggregate_payments_all_discharge,4)
      
      if (Penalty_Limit==TRUE){
        excess_ratio$EST_Factor<-max(excess_ratio$EST_Factor,Penalty_Limit_value)
        excess_ratio_actual$EST_Factor<-max(excess_ratio_actual$EST_Factor,Penalty_Limit_value)
      }
      excess_ratio$EST_penalty<-excess_ratio$EST_Factor*Aggregate_payments_all_discharge
      excess_ratio$EST_penalty_reduction<-(excess_ratio$EST_Factor-excess_ratio_actual$EST_Factor)*Aggregate_payments_all_discharge*Penalty_Scale
      
      #update the new readmission rate for next year, do not need to update, because without TCM actual readmission going back to norm
      #Penalty_Inpatient_selectedHosp_allyr$Actual_Readmission_Rate[which( Penalty_Inpatient_selectedHosp_allyr$year==2014+k)]<-actual_read_mean
      
      EST_penalty_reduction_n_years[,k]<-excess_ratio$EST_penalty_reduction
    }
    
    
    
    ########################################################################################################################
    #####################################            TCM Cost Analysis           ##############################################
    
    
    
    NURSE_PRACTIONER_H_SALARY=rpert(1000,67,80,73,4)
    REGISTERED_NURSE_H_SALARY=rpert(1000,46,59,52,4)
    COMMUNITY_HEALTH_WORKER_H_SALARY=rpert(1000,19,34,23.5,4)
    Combined_H_SALARY=rpert(1000,55,75,67,4)
    
    #No adjustment
    one_month_hour=rpert(1000,10,22,14,4)
    two_month_intensive_hours=rpert(1000,13,27,18,4)
    densityrank_i=1
    
    seven_month_level_1_hours=rpert(1000,3,5,3.8,4)
    seven_month_level_2_hours=rpert(1000,10,24,11.4,4) 
    seven_month_level_3_hours=rpert(1000,18,35,20,4)
    seven_month_level_1_perc=0.4
    seven_month_level_2_perc=0.4
    seven_month_level_3_perc=0.2
    
    #With density adjustment:
    one_month_hour=rpert(1000,12,15,13,4)
    two_month_intensive_hours=rpert(1000,15,18,16,4)
    
    seven_month_level_1_hours=rpert(1000,3,4,3.25,4)
    seven_month_level_2_hours=rpert(1000,8.5,16,10.5,4) 
    seven_month_level_3_hours=rpert(1000,15.5,24,17.2,4)
    seven_month_level_1_perc=0.4
    seven_month_level_2_perc=0.4
    seven_month_level_3_perc=0.2
    
    #three_month_hours=rpert(1000,16,33,22,4)
    #six_month_hours=rpert(1000,22,40,26,4)
    #nine_month_hours=rpert(1000,22,40,26,4)
    
    # 1 month intervention hours = uniform (12,15)*[0.6*(1-population density)+0.9] (default) range(10-22)
    # 2 month intervention hours = uniform (15,18)*[0.6*(1-population density)+0.9] (default) range(13-27)
    # 3 month intervention hours = uniform (18,22)*[0.6*(1-population density)+0.9] (default) range(16-33)
    # 6 month intervention hours = uniform (22,27)*[0.6*(1-population density)+0.9] (default) range(20-40)
    # population density look for excel, "densityrank" column
    
    
    
    # Staffing=1
    # 
    # # Advanced Practice Nurse = 1 
    # # Registered Nurse = 2
    # # Community Health Worker = 3
    # # Combined Workforce =4
    # 
    # Intervention_Length =2 
    # 
    # # 1 Month 
    # # 2 Month 
    # # 9 Month 
    #   # Under 9 month intervention: in the first 2 months, using Advanced Practice Nurses for intensive care,
    #   # in the following 7 months, using community health workers for level 1 care, using Registered Nurse for level 2 care, and Advanced Practice Nurse for level 3 care.
    #   
    
    ###################TCM Cost###########################################
    
    temp_i<-which(Base_data0802$PROVIDER_NUMBER==search_i())
    Salary_Adjusted_Factor_i<-Base_data0802$Salary_Adjusted_Factor[temp_i]
    densityrank_i<-Base_data0802$densityrank[temp_i]
    
    
    if (Staffing==1){Staff_SALARY=NURSE_PRACTIONER_H_SALARY} else 
      if (Staffing==2){Staff_SALARY=REGISTERED_NURSE_H_SALARY}else
        if (Staffing==3){Staff_SALARY=COMMUNITY_HEALTH_WORKER_H_SALARY}else 
          if (Staffing==4){Staff_SALARY=Combined_H_SALARY}else {
            print("Wrong staffing!")}
    
    TCM_cost_per_patient_1month<-Salary_Adjusted_Factor_i*Staff_SALARY*one_month_hour*densityrank_i
    TCM_cost_per_patient_2month<-Salary_Adjusted_Factor_i*Staff_SALARY*two_month_intensive_hours*densityrank_i
    TCM_cost_per_patient_9month<-Salary_Adjusted_Factor_i*densityrank_i*(Staff_SALARY*(two_month_intensive_hours+
                                                                                         seven_month_level_1_perc*seven_month_level_1_hours+
                                                                                         seven_month_level_2_perc*seven_month_level_2_hours+
                                                                                         seven_month_level_3_perc*seven_month_level_3_hours))
    
    
    
    
    if (Staffing==4 & Intervention_Length ==9 ){
      TCM_cost_per_patient_9month<-Salary_Adjusted_Factor_i*densityrank_i*(NURSE_PRACTIONER_H_SALARY*two_month_intensive_hours+
                                                                             COMMUNITY_HEALTH_WORKER_H_SALARY*seven_month_level_1_perc*seven_month_level_1_hours+
                                                                             REGISTERED_NURSE_H_SALARY*seven_month_level_2_perc*seven_month_level_2_hours+
                                                                             NURSE_PRACTIONER_H_SALARY*seven_month_level_3_perc*seven_month_level_3_hours)
      
    }
    
    Est_Num_TCM_enrollees<-round(sum(TCM_Inpatients$Total_Discharges[which(TCM_Inpatients$PROVIDER_NUMBER==search_i())])/Discharges_Case_Ratio,0)
    #Do not need to *Enrollee_candidates_ratio because already included in TCM_Inpatients file
    
    
    
    if (Intervention_Length==9 ){TCM_cost=data.frame(TCM_cost_per_patient_9month*Est_Num_TCM_enrollees)}else
      if (Intervention_Length==1 ){TCM_cost=data.frame(TCM_cost_per_patient_1month*Est_Num_TCM_enrollees)}else
        if (Intervention_Length==2 ){TCM_cost=data.frame(TCM_cost_per_patient_2month*Est_Num_TCM_enrollees)} else
          Print("Please specify the lenth of intervention: 1 month, 2 months, or 9 months")
    
    
    
    num_care_giver<-ceiling(min(1, Est_Num_TCM_enrollees/runif(1, 20, 40)))
    salary_admin<-runif(1, 80000, 120000)
    
    if (start_up_cost_ui=="Enter_num"){start_up_cost<-rep(1000,input$start_up_cost_num)}
    
    if (start_up_cost_ui=="Default"){start_up_cost<-Staff_SALARY*100*num_care_giver+3*4*5*100*runif(1, 5, 20)+salary_admin}
    #number of nurses required: min(1, Est_Num_TCM_enrollees/runif(1, 20, 40))
    #2 week training cost plus equipment fee if any: Staff_SALARY*100
    # salary_admin: 2 full time administrative workers work for half a year
    # 3*4*5*100*runif(1, 5, 20) mean 5-20 decision makers spend 3 months, every week spend 5 hours on the program, hourly salary $100
  
    M=data.frame(start_up_cost)
    start_up_cost_yearly<-apply(M,1,function(x) PMT(rate=0.03,nper=number_yrs,pv=-x))
    
    
    if (fixed_annual_TCM_cost=="Default"){fixed_annual_TCM_cost<-ceiling(num_care_giver/10)*salary_admin}
    
    TCM_cost<-(TCM_cost+start_up_cost_yearly+fixed_annual_TCM_cost)*Hosp_perc
    
    colnames(TCM_cost)<-"TCM_cost"
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # ########################################################################################################################
    # #####################################             Cost Analysis           ##############################################
    
    library(gridExtra)
    
    if (k>1){yr_title_ad<-"Projected"}else{yr_title_ad<-""}
    if (select_DRG==TRUE){title_ad<-"with selected DRGs"}else{title_ad<-""}
    if (Staffing %in% c(1,4)){title_ad2="APNs led"} else
      if (Staffing %in% c(2,3)){title_ad2="non-APNs led"} else {title_ad2=""}
    
    plots <- list()  # new empty list
    
    for (k in 1:number_yrs) {
      
      output_table<-cbind(Hospitals_TCM_Ad_loss[,k],EST_penalty_reduction_n_years[,k],TCM_cost$TCM_cost)
      output_table<-data.frame(output_table)
      colnames(output_table)<-c("Ad_loss","Penalty_reduction","TCM_cost")
      if (Payment_System =="Revenue_Approach"){
        
        Savings<-output_table$Penalty_reduction-output_table$Ad_loss-output_table$TCM_cost
        Savings<-as.data.frame(Savings)
        Savings$Hospital_Balance="NA"
        if (length(which(Savings$Savings>0))>0){Savings$Hospital_Balance[which(Savings$Savings>0)]<-"Positive"}
        if (length(which(Savings$Savings<0))>0){Savings$Hospital_Balance[which(Savings$Savings<=0)]<-"Negative"}
      }
      
      if (Payment_System =="Capitation"){
        Savings<-output_table$Ad_loss*(1-Operating_Margin)-output_table$TCM_cost
        Savings<-as.data.frame(Savings)
        Savings$Hospital_Balance="NA"
        if (length(which(Savings$Savings>0))>0){Savings$Hospital_Balance[which(Savings$Savings>0)]<-"Positive"}
        if (length(which(Savings$Savings<0))>0){Savings$Hospital_Balance[which(Savings$Savings<=0)]<-"Negative"}  
      }
      
      if (Payment_System =="FFS"){
        Savings<-output_table$Penalty_reduction-output_table$Ad_loss*Operating_Margin-output_table$TCM_cost
        Savings<-as.data.frame(Savings)
        Savings$Hospital_Balance="NA"
        if (length(which(Savings$Savings>0))>0){Savings$Hospital_Balance[which(Savings$Savings>0)]<-"Positive"}
        if (length(which(Savings$Savings<0))>0){Savings$Hospital_Balance[which(Savings$Savings<=0)]<-"Negative"}  
      }
      
      
      
      #############################         PLot the final economic analysis  ######################################
      
      if(length(which(Savings$Savings>0))>0){
        fig<-ggplot(Savings, aes(x=Savings, y=..density..)) +
          geom_histogram(colour="grey60", size=.2,alpha=0.3) +
          geom_density(fill="#009E73",alpha=0.8)
        
      }
      
      
      if(length(which(Savings$Savings<0))>0){
        fig<-ggplot(Savings, aes(x=Savings, y=..density..)) +
          geom_histogram(colour="grey60", size=.2,alpha=0.2) +
          geom_density(fill="red",alpha=0.5)
        
      }
      
      
      if(length(which(Savings$Savings>0))>0 & length(which(Savings$Savings<0))>0){
        density_plot<-ggplot(Savings, aes(x=Savings, y=..density..)) +
          #geom_histogram(colour="grey60", size=.2,alpha=0.2) +
          geom_density(fill="red",alpha=0.3)+
          geom_vline(aes(xintercept=0),colour="Black", linetype="dashed", size=1)
        
        dpb <- ggplot_build(density_plot)
        
        x1 <- min(which(dpb$data[[1]]$x >=0))
        x2 <- max(which(dpb$data[[1]]$x <=max(Savings$Savings)))
        
        
        
        fig<-density_plot +
          geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:x2],
                                    y=dpb$data[[1]]$y[x1:x2]),
                    aes(x=x, y=y), fill="#009E73")
        
        #http://stackoverflow.com/questions/32722849/how-to-shade-specific-region-under-ggplot2-density-curve
      }
      
      #     plots[[k]] <- fig+scale_x_continuous(paste(simpleCap(search_hosp),"Cost Analysis\nYear",k,
      #                                                "under",Payment_System,title_ad,"\nwith Penalty Scale equals ",Penalty_Scale,
      #                                                "\nwith ",title_ad2,Intervention_Length, "month(s) intervention",
      #                                                "\n30 days readmission reduction range:",read_reduction_min, "to", read_reduction_max,"with mean of",read_reduction_mean),labels = scales::dollar)  # add each plot into plot list
      #     
      
      plots[[k]] <- fig+ylab("Likelyhood of Outcome")+
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())+
        scale_x_continuous(paste(simpleCap(search_hosp),yr_title_ad,"\nYear",k+2013,title_ad,
                                 "under",Payment_System),labels = scales::dollar)+ scale_y_continuous("Likelyhood of Outcome")  # add each plot into plot list
    }
    
    output_table$Savings<-output_table$Penalty_reduction-output_table$Ad_loss*Operating_Margin-output_table$TCM_cost
    
    library(reshape)
    mdata <- melt(output_table)
    # 
    # ggplot(mdata,aes(x=value)) + 
    #   geom_histogram(data=subset(mdata,variable == 'Penalty_reduction'),fill = "red", alpha = 0.2) +
    #   geom_histogram(data=subset(mdata,variable == 'Ad_loss'),fill = "blue", alpha = 0.2) +
    #   geom_histogram(data=subset(mdata,variable == 'TCM_cost'),fill = "green", alpha = 0.2)+
    #   geom_histogram(data=subset(mdata,variable == 'Savings'),fill = "black", alpha = 0.5)+ 
    #   scale_fill_discrete("",labels=c("Admission Loss","Penalty Reduction","TCM Cost", "Net Savings"))+
    #   scale_x_continuous(paste(simpleCap(search_hosp),"Cost Analysis"),labels = scales::dollar)
    
    cost_analysis_sep_p<-ggplot(mdata, aes(x=value, fill=variable)) + geom_histogram(alpha=0.3, position="identity",bins = 200)+ 
      scale_fill_discrete("",labels=c("Admission Loss","Penalty Reduction","TCM Cost", "Net Savings"))+
      scale_x_continuous(paste(simpleCap(search_hosp),"Cost Analysis"),labels = scales::dollar)

    
    
    cost_analysis_p<-plots[[1]]
    print(cost_analysis_p)
  })
  
  
  
}