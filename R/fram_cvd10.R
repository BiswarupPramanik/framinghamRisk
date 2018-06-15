#' Framingham 10-year Cardiovascular Disease Risk (Point Model)
#'
#' This function calculates the 10-year Cardiovascular Disease Risk using the Point Model according to Framingham Heart Study.
#'
#' @param sex A character,Sex,Possible values - "Female","Male"
#' @param age A number,Age in years,Warning:If age<20years
#' @param sbp A number,Systolic Blodd Pressure in mm of Hg
#' @param htn_rx A character,Hypertension Treatment Status,Possible values - "Yes","No"
#' @param hdl A number,HDL-Cholesterol in mg/dL
#' @param tchl A number,Total-Cholesterol in mg/dL
#' @param smk A character,Smoking Status,Possible values - "Yes","No"
#' @param dm A character,Diabetes Status,Possible values - "Yes","No"
#' 
#' @return Framingham 10-year Cardiovascular Disease Risk & Heart/Vascular Age.
#' @author Biswarup Pramanik
#' @details
#' This function calculates the 10-year Cardiovascular Disease Risk using the Point Model according to Framingham Heart Study
#' based on eight predictors provided to it(sex,age,sbp,htn_rx,hdl,tchl,smk,dm).
#' @references
#' D'Agostino RB Sr, Vasan RS, Pencina MJ, Wolf PA, Cobain M, Massaro JM, Kannel WB. 
#' General cardiovascular risk profile for use in primary care: the Framingham Heart Study. 
#' Circulation. 2008 Feb 12;117(6):743-53. \url{https://doi.org/10.1161/CIRCULATIONAHA.107.699579}.
#' @export
fram_cvd10<-function(sex,age,sbp,htn_rx,hdl,tchl,smk,dm)
{

#Load data
df<-data.frame(sex,age,sbp,htn_rx,hdl,tchl,smk,dm)

#Warning Messages
if(any(df$age < 30)) warning("Check Age:: Minimum age is 30 years for risk calculation")

#Sex
for(sex in df$sex){
#FEMALE
if(sex=="Female"){
scale<-read.csv(file=system.file("data","Framingham_CVDrisk_10years_Female.csv",package = "framinghamRisk"),header=TRUE)

#Age
df$age<-ifelse(df$age>=30 & df$age<=34,32,df$age)
df$age<-ifelse(df$age>=35 & df$age<=39,37,df$age)
df$age<-ifelse(df$age>=40 & df$age<=44,42,df$age)
df$age<-ifelse(df$age>=45 & df$age<=49,47,df$age)
df$age<-ifelse(df$age>=50 & df$age<=54,52,df$age)
df$age<-ifelse(df$age>=55 & df$age<=59,57,df$age)
df$age<-ifelse(df$age>=60 & df$age<=64,62,df$age)
df$age<-ifelse(df$age>=65 & df$age<=69,67,df$age)
df$age<-ifelse(df$age>=70 & df$age<=74,72,df$age)
df$age<-ifelse(df$age>=75,77,df$age)

df$age_points<-scale$points[match(df$age,scale$age)]

#Systolic Blood Pressure
df$sbp<-ifelse(df$sbp<120,110,df$sbp)
df$sbp<-ifelse(df$sbp>=120 & df$sbp<=129,125,df$sbp)
df$sbp<-ifelse(df$sbp>=130 & df$sbp<=139,135,df$sbp)
df$sbp<-ifelse(df$sbp>=140 & df$sbp<=149,145,df$sbp)
df$sbp<-ifelse(df$sbp>=150 & df$sbp<=159,155,df$sbp)
df$sbp<-ifelse(df$sbp>=160,170,df$sbp)

#HDL
df$hdl<-ifelse(df$hdl>=60,70,df$hdl)
df$hdl<-ifelse(df$hdl<=59 & df$hdl>=50,55,df$hdl)
df$hdl<-ifelse(df$hdl<=49 & df$hdl>=45,47,df$hdl)
df$hdl<-ifelse(df$hdl<=44 & df$hdl>=35,40,df$hdl)
df$hdl<-ifelse(df$hdl<35,30,df$hdl)

df$hdl_points<-scale$points[match(df$hdl,scale$hdl)]

#Total Cholesterol
df$tchl<-ifelse(df$tchl<160,150,df$tchl)
df$tchl<-ifelse(df$tchl>=160 & df$tchl<=199,180,df$tchl)
df$tchl<-ifelse(df$tchl>=200 & df$tchl<=239,220,df$tchl)
df$tchl<-ifelse(df$tchl>=240 & df$tchl<=279,260,df$tchl)
df$tchl<-ifelse(df$tchl>=280,300,df$tchl)

df$tchl_points<-scale$points[match(df$tchl,scale$tchl)]

#Smoking Status
df$smk_points<-scale$points[match(df$smk,scale$smk)]

#Diabetes Status
df$dm_points<-scale$points[match(df$dm,scale$dm)]

#Hypertension Treatment
for(val in df$htn_rx){
if(val=="Yes"){
df$sbp_points<-scale$points[match(df$sbp,scale$sbp_rx)]

#Total Points
df$total_points<-(df$age_points+df$sbp_points+df$hdl_points+df$tchl_points+df$smk_points+df$dm_points)

#10 year CVD Risk
df$risk<-scale$risk[match(df$total_points,scale$risk_points)]

#Heart/Vascular Age
df$heart_age<-scale$heart_age[match(df$total_points,scale$heart_points)]

df$risk<-as.character(df$risk)
df$heart_age<-as.character(df$heart_age)

return(cbind(df$risk,df$heart_age));
}

if(val=="No"){
df$sbp_points<-scale$points[match(df$sbp,scale$sbp_no_rx)]

#Total Points
df$total_points<-(df$age_points+df$sbp_points+df$hdl_points+df$tchl_points+df$smk_points+df$dm_points)

#10 year CVD Risk
df$risk<-scale$risk[match(df$total_points,scale$risk_points)]

#Heart/Vascular Age
df$heart_age<-scale$heart_age[match(df$total_points,scale$heart_points)]

df$risk<-as.character(df$risk)
df$heart_age<-as.character(df$heart_age)

return(cbind(df$risk,df$heart_age));
}
}




}

#MALE
if(sex=="Male"){
scale<-read.csv(file=system.file("data","Framingham_CVDrisk_10years_Male.csv",package = "framinghamRisk"),header=TRUE)

#Age
df$age<-ifelse(df$age>=30 & df$age<=34,32,df$age)
df$age<-ifelse(df$age>=35 & df$age<=39,37,df$age)
df$age<-ifelse(df$age>=40 & df$age<=44,42,df$age)
df$age<-ifelse(df$age>=45 & df$age<=49,47,df$age)
df$age<-ifelse(df$age>=50 & df$age<=54,52,df$age)
df$age<-ifelse(df$age>=55 & df$age<=59,57,df$age)
df$age<-ifelse(df$age>=60 & df$age<=64,62,df$age)
df$age<-ifelse(df$age>=65 & df$age<=69,67,df$age)
df$age<-ifelse(df$age>=70 & df$age<=74,72,df$age)
df$age<-ifelse(df$age>=75,77,df$age)

df$age_points<-scale$points[match(df$age,scale$age)]

#Systolic Blood Pressure
df$sbp<-ifelse(df$sbp<120,110,df$sbp)
df$sbp<-ifelse(df$sbp>=120 & df$sbp<=129,125,df$sbp)
df$sbp<-ifelse(df$sbp>=130 & df$sbp<=139,135,df$sbp)
df$sbp<-ifelse(df$sbp>=140 & df$sbp<=149,145,df$sbp)
df$sbp<-ifelse(df$sbp>=150 & df$sbp<=159,155,df$sbp)
df$sbp<-ifelse(df$sbp>=160,170,df$sbp)

#HDL
df$hdl<-ifelse(df$hdl>=60,70,df$hdl)
df$hdl<-ifelse(df$hdl<=59 & df$hdl>=50,55,df$hdl)
df$hdl<-ifelse(df$hdl<=49 & df$hdl>=45,47,df$hdl)
df$hdl<-ifelse(df$hdl<=44 & df$hdl>=35,40,df$hdl)
df$hdl<-ifelse(df$hdl<35,30,df$hdl)

df$hdl_points<-scale$points[match(df$hdl,scale$hdl)]

#Total Cholesterol
df$tchl<-ifelse(df$tchl<160,150,df$tchl)
df$tchl<-ifelse(df$tchl>=160 & df$tchl<=199,180,df$tchl)
df$tchl<-ifelse(df$tchl>=200 & df$tchl<=239,220,df$tchl)
df$tchl<-ifelse(df$tchl>=240 & df$tchl<=279,260,df$tchl)
df$tchl<-ifelse(df$tchl>=280,300,df$tchl)

df$tchl_points<-scale$points[match(df$tchl,scale$tchl)]

#Smoking Status
df$smk_points<-scale$points[match(df$smk,scale$smk)]

#Diabetes Status
df$dm_points<-scale$points[match(df$dm,scale$dm)]
#Hypertension Treatment
for(val in df$htn_rx){
if(val=="Yes"){
df$sbp_points<-scale$points[match(df$sbp,scale$sbp_rx)]

#Total Points
df$total_points<-(df$age_points+df$sbp_points+df$hdl_points+df$tchl_points+df$smk_points+df$dm_points)

#10 year CVD Risk
df$risk<-scale$risk[match(df$total_points,scale$risk_points)]

#Heart/Vascular Age
df$heart_age<-scale$heart_age[match(df$total_points,scale$heart_points)]

df$risk<-as.character(df$risk)
df$heart_age<-as.character(df$heart_age)

return(cbind(df$risk,df$heart_age));
}

if(val=="No"){
df$sbp_points<-scale$points[match(df$sbp,scale$sbp_no_rx)]

#Total Points
df$total_points<-(df$age_points+df$sbp_points+df$hdl_points+df$tchl_points+df$smk_points+df$dm_points)

#10 year CVD Risk
df$risk<-scale$risk[match(df$total_points,scale$risk_points)]

#Heart/Vascular Age
df$heart_age<-scale$heart_age[match(df$total_points,scale$heart_points)]

df$risk<-as.character(df$risk)
df$heart_age<-as.character(df$heart_age)

return(cbind(df$risk,df$heart_age));
}
}
}

}


}
