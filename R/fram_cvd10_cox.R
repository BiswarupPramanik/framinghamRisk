#' Framingham 10-year Cardiovascular Disease Risk (Cox Model)
#'
#' This function calculates the 10-year Cardiovascular Disease Risk using Cox Model according to Framingham Heart Study.
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
#' This function calculates the 10-year Cardiovascular Disease Risk using Cox Model according to Framingham Heart Study
#' based on eight predictors provided to it(sex,age,sbp,htn_rx,hdl,tchl,smk,dm).
#' @references
#' D'Agostino RB Sr, Vasan RS, Pencina MJ, Wolf PA, Cobain M, Massaro JM, Kannel WB. 
#' General cardiovascular risk profile for use in primary care: the Framingham Heart Study. 
#' Circulation. 2008 Feb 12;117(6):743-53. \url{https://doi.org/10.1161/CIRCULATIONAHA.107.699579}.
#' @export
fram_cvd10_cox<-function(sex,age,sbp,htn_rx,hdl,tchl,smk,dm)
{

#Load data
df<-data.frame(sex,age,sbp,htn_rx,hdl,tchl,smk,dm)

#Smoking Status
df$smk<-ifelse(df$smk=="No",0,df$smk)
df$smk<-ifelse(df$smk=="Yes",1,df$smk)

#Diabetes Status
df$dm<-ifelse(df$dm=="No",0,df$dm)
df$dm<-ifelse(df$dm=="Yes",1,df$dm)

#Sex
for(sex in df$sex){
if(sex=="Female"){
for(val in df$htn_rx){
if(val=="No"){
df$risk<-(1-(0.95012^(exp((2.32888*log(df$age)+1.20904*log(df$tchl)-0.70833*log(df$hdl)+2.76157*log(df$sbp)+0.52873*(df$smk)+0.69154*(df$dm))-(2.32888*3.8686+1.20904*5.3504-0.70833*4.0176+2.76157*4.2400+2.82263*0.5826+0.52873*0.3423+0.69154*0.0376)))))
df$risk<-as.numeric(df$risk)

df$heart_age<-exp(((log((log((1-df$risk),base=0.95012)),base=exp(1)))/2.32888)+3.8686)
df$heart_age<-as.character(df$heart_age)

df$risk<-df$risk*100
return(cbind(df$risk,df$heart_age));
}
if(val=="Yes"){
df$risk<-(1-(0.95012^(exp((2.32888*log(df$age)+1.20904*log(df$tchl)-0.70833*log(df$hdl)+2.82263*(df$sbp)+0.52873*(df$smk)+0.69154*(df$dm))-(2.32888*3.8686+1.20904*5.3504-0.70833*4.0176+2.76157*4.2400+2.82263*0.5826+0.52873*0.3423+0.69154*0.0376)))))
df$risk<-as.numeric(df$risk)

df$heart_age<-exp(((log((log((1-df$risk),base=0.95012)),base=exp(1)))/2.32888)+3.8686)
df$heart_age<-as.character(df$heart_age)
df$risk<-df$risk*100
return(cbind(df$risk,df$heart_age));
}
}
}

if(sex=="Male"){
for(val in df$htn_rx){
if(val=="No"){
df$risk<-(1-(0.88936^(exp((3.06117*log(df$age)+1.12370*log(df$tchl)-0.93263*log(df$hdl)+1.93303*log(df$sbp)+0.65451*(df$smk)+0.57367*(df$dm))-(3.06117*3.8560+1.12370*5.3420-0.93263*3.7686+1.93303*4.3544+1.99881*0.5019+0.65451*0.3522+0.57367*0.0650)))))
df$risk<-as.numeric(df$risk)

df$heart_age<-exp(((log((log((1-df$risk),base=0.88936)),base=exp(1)))/3.06117)+3.8560)
df$heart_age<-as.character(df$heart_age)
df$risk<-df$risk*100
return(cbind(df$risk,df$heart_age));
}
if(val=="Yes"){
df$risk<-(1-(0.88936^(exp((3.06117*log(df$age)+1.12370*log(df$tchl)-0.93263*log(df$hdl)+1.99881*(df$sbp)+0.65451*(df$smk)+0.57367*(df$dm))-(3.06117*3.8560+1.12370*5.3420-0.93263*3.7686+1.93303*4.3544+1.99881*0.5019+0.65451*0.3522+0.57367*0.0650)))))
df$risk<-as.numeric(df$risk)

df$heart_age<-exp(((log((log((1-df$risk),base=0.88936)),base=exp(1)))/3.06117)+3.8560)
df$heart_age<-as.character(df$heart_age)
df$risk<-df$risk*100
return(cbind(df$risk,df$heart_age));
}
}
}

}


}
