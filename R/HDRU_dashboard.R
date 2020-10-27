#' @title
#' Generate a PDF report from the data.
#' 
#' @description
#' Generates an HTML report containing summaries of the data, including comparison of lengths of hospital stay by sex and age group, outcome by sex,
#' comorbidity, symptom and treatment distributions and distribution of vital signs on presentation at hospital. 
#' @param admissionData Data table with the current admission data from the MLW data portal
#' @param dailyData Data frame with the current daily data from the MLW data portal
#' @param curPeriod Character string vector in yyyy-mm-dd format; giving the Mondays of the weeks to include in the report (if unit=="week"); e.g. dmy("2020"/07/06,"2020/07/13","2020/07/20","2020/07/27") or the start of the months (if unit="month") to include; e.g. dmy("2020/04/01","2020/05/01","2020/06/01","2020/07/01")
#' @param unit Character string that is either 'week' or 'month' giving the grouping to use for the bar plots
#' @param minDay Character string in yyy-mm-dd format indicating the start of the period to summarise; defaults to NULL (in this case determined from curPeriod); if both minDay and maxDay are not NULL, then they determine the range of records to include in the tables and the plots that are not stratified by week or month
#' @param maxDay Character string in yyy-mm-dd format indicating the end of the period to summarise; defaults to NULL (in this case determined from curPeriod); if both minDay and maxDay are not NULL, then they determine the range of records to include in the tables and the plots that are not stratified by week or month
#' @param file.name Path to a html file to write the dashboard to
#' 
#' @return Html dashboard visualising the provided HDRU data
#' @import rmarkdown flexdashboard ggplot2 tibble dplyr lubridate wesanderson knitr
#' @importFrom filesstrings file.move
#' @importFrom kableExtra kable_styling pack_rows column_spec
#' @importFrom dplyr filter summarise group_by mutate 
#' @importFrom stats approxfun
#' 
#' @examples
#' # You will need to
#' #  * Load the admission data from the data portal (argument admissionData).
#' #  * Load the daily data from the data portal (argument dailyData).
#' #  * Set the weeks for which you want summaries (argument curWeek).
#' #  * Specify a file name (with .html extension) that will contain the dashboard report.
#' # The example below assumes you've got the 2 input data files sitting in your working directory.
#' # Once you have run the HDRUdashboard() function, open the specified output html file.
#' 
#' library(HDRU)
#' 
#' admissionData<-read.csv("hdru_admission_raw.csv")
#' dailyData<-read.csv("hdru_daily_raw.csv")
#' curWeek<-dmy("06/07/2020","13/07/2020","20/07/2020","27/07/2020")
#' 
#' HDRUdashboard(admissionData=admissionData,dailyData=dailyData,curWeek=curWeek,file.name="HDRUdashboard.html")
#' 
#' @export HDRUdashboard

HDRUdashboard<-function(admissionData,dailyData,curPeriod,unit="week",minDay=NULL,maxDay=NULL,file.name){
  #admissionData <- read_csv("hdru_admission_raw.csv")
  # not all PIDs are unique, e.g. KAA7V0 - can people be admitted, then discharged, then re-admitted from HDRU?
  # which variable captures date of discharge?
  #dailyData <- read_csv("hdru_daily_raw.csv")
  #dailyData$data_date<-mdy(dailyData$data_date) # only needed for manually provided data rather than downloaded from data portal
  # some PIDs in daily data that are not in the admission data
  # setdiff(dailyData$pid,admissionData$pid)
  
  # can have multiple admissions of same individual, so need to append admission date to pid
  admissionData<-admissionData %>% add_column(admission=1)
  dailyData<-dailyData %>% add_column(admission=1)
  
  admissionData$data_date<-ymd(admissionData$data_date)
  dailyData$data_date<-ymd(dailyData$data_date)
  
  for(pid in names(table(admissionData$pid)[table(admissionData$pid)>1])){
    admDates<-ymd(admissionData$data_date[admissionData$pid==pid])
    for(d in sort(admDates,decreasing=F)[-1]){
      admissionData$admission[admissionData$pid==pid & ymd(admissionData$data_date)>=d]<-admissionData$admission[admissionData$pid==pid & ymd(admissionData$data_date)>=d]+1
      dailyData$admission[dailyData$pid==pid & ymd(dailyData$data_date)>=d]<-dailyData$admission[dailyData$pid==pid & ymd(dailyData$data_date)>=d]+1
    }
  }
  
  admissionData$pid<-toupper(admissionData$pid)
  dailyData$pid<-toupper(dailyData$pid)
  admissionData<-admissionData %>% add_column(pidAdm=paste(sep="_",admissionData$pid,admissionData$admission))
  dailyData$pidAdm<-paste(sep="_",dailyData$pid,dailyData$admission)
  # note: no outcome for first admission of KAA7V0 recorded (last and only instance is 'alive, still in critical care area') 
  
  admissionDataMissing<-dailyData %>%
    filter(!is.element(el=pidAdm,set=admissionData$pidAdm)) %>%
    select(pidAdm,data_date) %>%
    group_by(pidAdm) %>%
    summarise(minDate=min(ymd(data_date)))
  
  admissionData<-as_tibble(plyr::rbind.fill(admissionData,admissionDataMissing))
  
  
  # merge data; needed to calculate duration of organ support; use individual files for tables figures that only focus on admission or daily data
  datAdmTmp<-admissionData 
  datDailyTmp<-dailyData
  colnames(datAdmTmp)<-paste(sep="_","adm",colnames(datAdmTmp)) 
  colnames(datDailyTmp)<-paste(sep="_","dly",colnames(datDailyTmp))
  datMerged<-full_join(datAdmTmp,datDailyTmp,by=c("adm_pidAdm" = "dly_pidAdm"))
  
  osaDur<-datMerged %>%
    select(adm_pidAdm,adm_adm1,adm_adm2,adm_adm3,adm_adm4,adm_adm5,adm_adm6,adm_adm7,adm_adm99,adm_data_date,dly_osa1,dly_osa2,dly_osa3,dly_osa4,dly_osa5,dly_osa6,dly_osa7,dly_osa8,dly_data_date) %>%
    group_by(adm_pidAdm) %>%
    summarise(adm_adm1=first(adm_adm1),
              adm_adm2=first(adm_adm2),
              adm_adm3=first(adm_adm3),
              adm_adm4=first(adm_adm4),
              adm_adm5=first(adm_adm5),
              adm_adm6=first(adm_adm6),
              adm_adm7=first(adm_adm7),
              adm_adm99=first(adm_adm99),
              adm_data_date=first(adm_data_date),
              osa1Dur=sum(dly_osa1[!is.na(dly_osa1)]),
              osa2Dur=sum(dly_osa2[!is.na(dly_osa2)]),
              osa3Dur=sum(dly_osa3[!is.na(dly_osa3)]),
              osa4Dur=sum(dly_osa4[!is.na(dly_osa4)]),
              osa5Dur=sum(dly_osa5[!is.na(dly_osa5)]),
              osa6Dur=sum(dly_osa6[!is.na(dly_osa6)]),
              osa7Dur=sum(dly_osa7[!is.na(dly_osa7)]),
              osa8Dur=sum(dly_osa8[!is.na(dly_osa8)]),
              dly_first_data=min(ymd(dly_data_date))) # counts days where organ support was recorded
  
  tmp1<-ifelse(!is.na(osaDur$adm_adm1) & osaDur$adm_adm1==1 & (is.na(osaDur$dly_first_data) | (!is.na(osaDur$adm_data_date) & ymd(osaDur$adm_data_date)<ymd(osaDur$dly_first_data)) | (!is.na(osaDur$osa1Dur) & osaDur$osa1Dur==0)),1,0)
  tmp2<-ifelse(!is.na(osaDur$adm_adm2) & osaDur$adm_adm2==1 & (is.na(osaDur$dly_first_data) | (!is.na(osaDur$adm_data_date) & ymd(osaDur$adm_data_date)<ymd(osaDur$dly_first_data)) | (!is.na(osaDur$osa2Dur) & osaDur$osa2Dur==0)),1,0)
  tmp3<-ifelse(!is.na(osaDur$adm_adm3) & osaDur$adm_adm3==1 & (is.na(osaDur$dly_first_data) | (!is.na(osaDur$adm_data_date) & ymd(osaDur$adm_data_date)<ymd(osaDur$dly_first_data)) | (!is.na(osaDur$osa3Dur) & osaDur$osa3Dur==0)),1,0)
  tmp4<-ifelse(!is.na(osaDur$adm_adm4) & osaDur$adm_adm4==1 & (is.na(osaDur$dly_first_data) | (!is.na(osaDur$adm_data_date) & ymd(osaDur$adm_data_date)<ymd(osaDur$dly_first_data)) | (!is.na(osaDur$osa4Dur) & osaDur$osa4Dur==0)),1,0)
  tmp5<-ifelse(!is.na(osaDur$adm_adm5) & osaDur$adm_adm5==1 & (is.na(osaDur$dly_first_data) | (!is.na(osaDur$adm_data_date) & ymd(osaDur$adm_data_date)<ymd(osaDur$dly_first_data)) | (!is.na(osaDur$osa5Dur) & osaDur$osa5Dur==0)),1,0)
  tmp6<-ifelse(!is.na(osaDur$adm_adm6) & osaDur$adm_adm6==1 & (is.na(osaDur$dly_first_data) | (!is.na(osaDur$adm_data_date) & ymd(osaDur$adm_data_date)<ymd(osaDur$dly_first_data)) | (!is.na(osaDur$osa6Dur) & osaDur$osa6Dur==0)),1,0)
  tmp7<-ifelse(!is.na(osaDur$adm_adm7) & osaDur$adm_adm7==1 & (is.na(osaDur$dly_first_data) | (!is.na(osaDur$adm_data_date) & ymd(osaDur$adm_data_date)<ymd(osaDur$dly_first_data)) | (!is.na(osaDur$osa7Dur) & osaDur$osa7Dur==0)),1,0)
  tmp8<-ifelse(!is.na(osaDur$adm_adm99) & osaDur$adm_adm99==1 & (is.na(osaDur$dly_first_data) | (!is.na(osaDur$adm_data_date) & ymd(osaDur$adm_data_date)<ymd(osaDur$dly_first_data)) | (!is.na(osaDur$osa8Dur) & osaDur$osa8Dur==0)),1,0)
  
  osaDur$osa1Dur<-osaDur$osa1Dur+tmp1
  osaDur$osa2Dur<-osaDur$osa2Dur+tmp2
  osaDur$osa3Dur<-osaDur$osa3Dur+tmp3
  osaDur$osa4Dur<-osaDur$osa4Dur+tmp4
  osaDur$osa5Dur<-osaDur$osa5Dur+tmp5
  osaDur$osa6Dur<-osaDur$osa6Dur+tmp6
  osaDur$osa7Dur<-osaDur$osa7Dur+tmp7
  osaDur$osa8Dur<-osaDur$osa8Dur+tmp8
  
  rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8)
  
  osaDur<-osaDur %>% 
    add_column(osa1=ifelse(osaDur$osa1Dur>0,1,0),
               osa2=ifelse(osaDur$osa2Dur>0,1,0),
               osa3=ifelse(osaDur$osa3Dur>0,1,0),
               osa4=ifelse(osaDur$osa4Dur>0,1,0),
               osa5=ifelse(osaDur$osa5Dur>0,1,0),
               osa6=ifelse(osaDur$osa6Dur>0,1,0),
               osa7=ifelse(osaDur$osa7Dur>0,1,0),
               osa8=ifelse(osaDur$osa8Dur>0,1,0)
    )
  
  admissionData<-left_join(admissionData,osaDur,by=c("pidAdm" = "adm_pidAdm"))

  admissionData<-admissionData %>% add_column(sexFactor=factor(ifelse(admissionData$sex==0,"Female","Male"),levels=c("Female","Male")))
  admissionData<-admissionData %>% add_column(pregFactor=factor(ifelse(admissionData$pregnancy==3,"Not known to be pregnant",ifelse(admissionData$pregnancy==2,"Recently pregnant",ifelse(admissionData$pregnancy==1,"Currently pregnant.",NA))),levels=c("Not known to be pregnant","Recently pregnant","Currently pregnant")))
  admissionData<-admissionData %>% add_column(weight=4*admissionData$muac-50)
  # Cattermole et al (2016)
  # some super light people!
  
  # height from ulna length estimation was based on a formula for children - hence why so many tall people!
  # admissionData$height<-case_when(
  #   admissionData$sexFactor=="Male" ~ (4.605*admissionData$ulnar_l+1.308*admissionData$age+28.003)/100,
  #   admissionData$sexFactor=="Female" ~ (4.459*admissionData$ulnar_l+1.315*admissionData$age+31.485)/100
  # )
  #   # Gauld et al (2004); formula gives result in cm, hence dividing by 100 for meters
  #   # very very tall people!!!
  
  ulna2HeightConvFun<-function(ulna,sex,age,convTab=ulna2HeightConvTable){
    col<-case_when(
      sex=="Male" & age<65 ~ "menBelow65yrs_m",
      sex=="Male" & age>=5 ~ "menAbove65yrs_m",
      sex=="Female" & age<65 ~ "womenBelow65yrs_m",
      sex=="Female" & age>=65 ~ "womenAbove65yrs_m",
    )
    
    f<-list()
    f[["menBelow65yrs_m"]]<-approxfun(x=convTab$ulna_cm,y=convTab$menBelow65yrs_m,method="linear",rule=1)
    f[["menAbove65yrs_m"]]<-approxfun(x=convTab$ulna_cm,y=convTab$menAbove65yrs_m,method="linear",rule=1)
    f[["womenBelow65yrs_m"]]<-approxfun(x=convTab$ulna_cm,y=convTab$womenBelow65yrs_m,method="linear",rule=1)
    f[["womenAbove65yrs_m"]]<-approxfun(x=convTab$ulna_cm,y=convTab$womenAbove65yrs_m,method="linear",rule=1)
    
    res<-ifelse(col=="menBelow65yrs_m",1,0)*f[["menBelow65yrs_m"]](ulna) +
      ifelse(col=="menAbove65yrs_m",1,0)*f[["menAbove65yrs_m"]](ulna) +
      ifelse(col=="womenBelow65yrs_m",1,0)*f[["womenBelow65yrs_m"]](ulna) +
      ifelse(col=="womenAbove65yrs_m",1,0)*f[["womenAbove65yrs_m"]](ulna)
    
    return(res)
  }
  
  admissionData<-admissionData %>% add_column(height=ulna2HeightConvFun(ulna=admissionData$ulnar_l,sex=admissionData$sexFactor,age=admissionData$age))
  # still fairly tall people: median height is 1.80m -- ie. half are 1.80m or taller...
  # but better than with previous formula from Gauld et al (2004)
  
  admissionData<-admissionData %>% add_column(bmi=admissionData$weight/(admissionData$height^2))
  # these strike me as exceptionally low; lowest ever recorded BMI is 7.5, yet her we have several below that...
  admissionData<-admissionData %>% add_column(bmiFactor=factor(case_when(
    admissionData$bmi<18.5 ~ "<18.5",
    admissionData$bmi>=18.5 & admissionData$bmi<25 ~ "18.5-<25",
    admissionData$bmi>=25 & admissionData$bmi<30 ~ "25-<30",
    admissionData$bmi>=30 & admissionData$bmi<40~ "30-<40",
    admissionData$bmi>=40 ~ "40+"),
    levels=c("<18.5","18.5-<25","25-<30","30-<40","40+")
  ))
  
  admissionData$depFactor<-factor(case_when(
    admissionData$dependecy==1 ~ "Able to live without assistance in daily activities",
    admissionData$dependecy==2 ~ "Minor assistance with some daily activities",
    admissionData$dependecy==3 ~ "Major assistance with majority of or all daily activities",
    admissionData$dependecy==4 ~ "Total assistance with all daily activities"),
    levels=c("Able to live without assistance in daily activities",
             "Minor assistance with some daily activities",
             "Major assistance with majority of or all daily activities",
             "Total assistance with all daily activities")
  )
  
  admissionData$hivFactor<-factor(case_when(
    admissionData$serop==0 ~ "No HIV",
    admissionData$serop==1 & admissionData$art==1 ~ "ART compliant",
    admissionData$serop==1 & admissionData$art==2 ~ "ART non-compliant",
    admissionData$serop==1 & admissionData$art==3 ~ "Not on ART"),
    levels=c("No HIV","ART compliant","ART non-compliant","Not on ART")
  )
  
  admissionData$tbFactor<-factor(case_when(
    admissionData$lpp==0 | admissionData$tb==0 ~ "No TB",
    admissionData$tb==1 ~ "TB"),
    levels=c("No TB","TB")
  )
  
  admissionData$hypFactor<-factor(case_when(
    admissionData$cpp==0 | admissionData$sh==0 ~ "No severe hypertension",
    admissionData$sh==1 ~ "Severe hypertension"),
    levels=c("No severe hypertension","Hypertension")
  )
  
  admissionData$ccfFactor<-factor(case_when(
    admissionData$cpp==0 | admissionData$ccf==0 ~ "No congestive cardiac failure",
    admissionData$ccf==1 ~ "Congestive cardiac failure"),
    levels=c("No congestive cardiac failure","Congestive cardiac failure")
  )
  
  admissionData$strokeFactor<-factor(case_when(
    admissionData$npp==0 | admissionData$stroke==0 ~ "No stroke",
    admissionData$stroke==1 ~ "Stroke"),
    levels=c("No stroke","Stroke")
  )
  
  admissionData$diabetesFactor<-factor(case_when(
    admissionData$hpp==0 | (admissionData$d_insulin==0 & admissionData$d_tablets==0 & admissionData$d_no_tret)==0 ~ "No diabetes",
    admissionData$d_insulin==1 ~ "On insulin",
    admissionData$d_tablets==1 ~ "On tablets",
    admissionData$d_no_tret==1 ~ "Not on treatment"),
    levels=c("No diabetes","On insulin","On tablets","Not on treatment")
  )
  
  admissionData$news2<- # https://en.wikipedia.org/wiki/Early_warning_score#/media/File:National_Early_Warning_Score_chart,_Royal_College_of_Physicians,_version_2.png
    case_when(admissionData$nvrrate<=8 ~ 3,
              admissionData$nvrrate>8 & admissionData$nvrrate<=11 ~ 1,
              admissionData$nvrrate>11 & admissionData$nvrrate<=20 ~ 0,
              admissionData$nvrrate>20 & admissionData$nvrrate<=24 ~ 2,
              admissionData$nvrrate>24 ~ 3) +
    case_when(admissionData$ao2sat<=91 ~ 3,
              admissionData$ao2sat>91 & admissionData$ao2sat<=93 ~ 2,
              admissionData$ao2sat>93 & admissionData$ao2sat<96 ~ 1,
              admissionData$ao2sat>=96 ~ 0) +
    case_when(admissionData$adm1==1 ~ 2,
              admissionData$adm1==0 ~ 0) +
    case_when(admissionData$adtemp<=35 ~ 3,
              admissionData$adtemp>35 & admissionData$adtemp<=36 ~ 1,
              admissionData$adtemp>36 & admissionData$adtemp<=38 ~ 0,
              admissionData$adtemp>38 & admissionData$adtemp<=39 ~ 1,
              admissionData$adtemp>39 ~ 2) +
    case_when(admissionData$bp_sys_low<=90 ~ 3,
              admissionData$bp_sys_low>90 & admissionData$bp_sys_low<=100 ~ 2,
              admissionData$bp_sys_low>100 & admissionData$bp_sys_low<=110 ~ 1,
              admissionData$bp_sys_low>110 & admissionData$bp_sys_low<220 ~ 0,
              admissionData$bp_sys_low>=220 ~ 3) +
    case_when(admissionData$hrate_low<=40 ~ 3,
              admissionData$hrate_low>40 & admissionData$hrate_low<=50 ~ 1,
              admissionData$hrate_low>50 & admissionData$hrate_low<=90 ~ 0,
              admissionData$hrate_low>90 & admissionData$hrate_low<=110 ~ 1,
              admissionData$hrate_low>110 & admissionData$hrate_low<=130 ~ 2,
              admissionData$hrate_low>130 ~ 3) +
    case_when(admissionData$avpu==1 ~ 0,
              admissionData$avpu>1 ~ 3) # also gives 3 points for avpu==7 (other)
  
  admissionData$uva<- # doi 10.1136/bmjgh-2017-000344; no GCS or BCS recorded at admission
    case_when(admissionData$adtemp<36 ~ 2,
              admissionData$adtemp>=36 ~ 0) +
    case_when(admissionData$hrate_low<120 ~ 0,
              admissionData$hrate_low>=120 ~ 1) +
    case_when(admissionData$nvrrate<30 ~ 0,
              admissionData$nvrrate>=30 ~ 1) +
    case_when(admissionData$bp_sys_low<90 ~ 1,
              admissionData$bp_sys_low>=90 ~ 0) +
    case_when(admissionData$ao2sat<92 ~ 2,
              admissionData$ao2sat>=92 ~ 0) +
    case_when(is.na(admissionData$serop) | admissionData$serop==0 ~ 0,
              admissionData$serop==1 ~ 2)
  
  admissionData$pfRatio<-
    case_when(admissionData$abga==2 ~ admissionData$abga2/admissionData$abga6, # need to check with DMSU what units these would be measured in
              admissionData$abga==1 ~ (10^(0.48+0.78*log10(admissionData$ao2sat/(admissionData$ao2req/100))))/7.50062) # doi 10.1097/CCM.0b013e31819cefa9; note log10 in the equation; confirm that FiO2 needs to be a fraction, but SpO2 a percentage -- seems too strange...
  
  admissionData<-admissionData %>% add_column(
    agegrp=factor(case_when(admissionData$age<30 ~ "0-29",
                            admissionData$age>=30 & admissionData$age<40 ~ "30-39",
                            admissionData$age>=40 & admissionData$age<50 ~ "40-49",
                            admissionData$age>=50 & admissionData$age<60 ~ "50-59",
                            admissionData$age>=60 ~ "60+"),
                  levels=c("0-29","30-39","40-49","50-59","60+"))
  )
  
  dailyData$outcomeFactor<-factor(case_when(
    dailyData$outcome==0 ~ "Dead",
    dailyData$outcome==1 ~ "Alive, in critical care",
    dailyData$outcome==2 ~ "Alive, discharged",
    dailyData$outcome==3 ~ "Absconded"),
    levels=c("Dead","Alive, in critical care","Alive, discharged","Absconded")
  )
  
  dailyData$admissionDate<-ymd(admissionData$data_date[match(dailyData$pidAdm,admissionData$pidAdm)])
  
  datOutcome<-dailyData %>%
    arrange(pidAdm,ymd(data_date)) %>%
    mutate(pidAdm=pidAdm,outcome=ifelse(!is.na(outcome) & outcome!=1,1,0)) %>%
    group_by(pidAdm) %>%
    summarise(outcomeKnown=max(outcome),outcome=last(outcomeFactor),LOS=max(ymd(data_date)-ymd(admissionDate)))
  levels(datOutcome$outcome)[levels(datOutcome$outcome)=="Alive, in critical care"]<-"Unknown"
  datOutcome$outcome[is.na(datOutcome$outcome)]<-"Unknown"
  
  admissionData<-left_join(admissionData,datOutcome,by="pidAdm")
  admissionData$outcome[is.na(admissionData$outcome)]<-"Unknown"
  
  admissionData$outcomeAliveOrDead<-factor(case_when(
    admissionData$hospital_outcome=="ALIVE" ~ "Discharged (hospital)",
    admissionData$hospital_outcome=="DEAD" & admissionData$outcome=="Alive, discharged" ~ "Discharged (critical care), died in hospital",
    admissionData$outcome=="Dead" ~ "Died in critical care",
    (is.na(admissionData$outcome) | admissionData$outcome=="Unknown") & is.na(admissionData$hospital_outcome) ~ "Outcome unknown",
    admissionData$hospital_outcome=="DEAD" & (is.na(admissionData$outcome) | admissionData$outcome=="Unknown") ~ "Unknown critical care outcome, died in hospital",
    is.na(admissionData$hospital_outcome) & admissionData$outcome=="Alive, discharged" ~ "Discharged (critical care), unknown hospital outcome"),
    levels=c("Discharged (hospital)","Discharged (critical care), died in hospital","Died in critical care","Discharged (critical care), unknown hospital outcome","Outcome unknown")
  )
  
  #curWeek<-dmy("27/04/2020","04/05/2020","11/05/2020","18/05/2020","25/05/2020")
  #curWeek<-dmy("27/04/2020","04/05/2020","11/05/2020","18/05/2020","25/05/2020","01/06/2020","08/06/2020","15/06/2020", "22/06/2020","29/06/2020","06/07/2020","13/07/2020","20/07/2020","27/07/2020")
  #curWeek<-dmy("06/07/2020","13/07/2020","20/07/2020","27/07/2020")
  # weeks to start on Mondays
  # this variable to become a shiny input / selection field
  if(is.null(minDay) | is.null(maxDay)){
    if(unit=="week"){
      minDay<-min(ymd(curPeriod))
      maxDay<-max(ymd(curPeriod)+6)
    }else if(unit=="month"){
      minDay<-floor_date(min(ymd(curPeriod)),unit="month")
      maxDay<-ceiling_date(max(ymd(curPeriod)),unit="month")-1
    }else{
      stop("Parameter 'unit' needs to be one of 'week' or 'month'.")
    }
  }
  
  admissionDataCurPeriod<-admissionData %>%
    filter(ymd(data_date)>=ymd(minDay) & ymd(data_date)<=ymd(maxDay))
  
  report.rmd.file <- system.file("rmd", "HDRUdashboard.Rmd", package = "HDRU")
  render(report.rmd.file, output_file=file.name)
  file.move(system.file("rmd", file.name, package = "HDRU"), getwd(), overwrite = TRUE)
}
