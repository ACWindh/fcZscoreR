

load("./inst/extdata/References_Schneider_etal_2005_25-02-20.rda")
load("./inst/extdata/References_Krishnan_etal_2016_25-02-20.rda")

#' Calculate z-scores for fetal cardiac parameters in Doppler Sono
#'
#' @param actual the observed value in mm
#' @param mod_value the value of the moderator (see Details)
#' @param card_param which cardiac parameters is used (see Details)
#' @param moderator one of EGA, BPD, FL, FW (see Details)
#' @param method see details
#' @param out one of "z" for the acutal z-score or "centile" for the percentile
#'
#' @return z-score or percentile
#' @export
#'
#' @details
#' - *mod_value*   for GA: must be in weeks as numeric, continuous value. e.g. 23+4 is given as 23,57
#' - *card_param*
#'     in mm, cardiac parametes can be
#'     - "AoVA" for Aortic valve annulus,
#'     - "PDA" for patent ductus arteriosus,
#'     - "Asc Ao", "Dsc Ao" for ascending or descending aorta,
#'     - "LPA", "RPA", "MPA" for Left \ right \ main pulmonary artery,
#'     - "LVL", "RVL" for left or right inlet or length
#'     - "MVA" for Mitral valve annulus,
#'     - "PVA" for Pulmonary valve annulus ,
#'     - "TVA" for Tricuspid valve annulus
#'     - only Krishnan:
#'        - "AoI" for Aortic isthmus
#'     - only Schneider:
#'        - "LVArea", "RVArea", for the circumference of the left or right ventricle,
#'        - "LVEDD", "RVEDD" for end diastolic diameter for left or right ventricle,
#'        - "IVC" for Inferior vena cava,
#' - *moderator*
#'     - EGA - estimated gestational age in weeks,
#'     - BPD - biparietal diameter in mm,
#'     - FL - femur length in mm
#'     - FW - fetal weight (based on Haddock formula, only available in reference values by Krishan et al. 2016)
#' - *method* one of "Schneider", "Krishnan"
#
#' @references
#' 1. Krishnan, A. et al. Predictive Models for Normal Fetal Cardiac Structures. Journal of the American Society of Echocardiography 29, 1197–1206 (2016).
#' 2. Schneider, C. et al. Development of Z-scores for fetal cardiac dimensions from echocardiography. Ultrasound in Obstetrics & Gynecology 26, 599–605 (2005).

#' @examples
#' fetal_zscore(actual= 4.8, mod_value=27+3/7,
#' card_param="AoVA", moderator="EGA",
#' method="Schneider", out="z")
#'
#' fetal_zscore(actual= 4.8, mod_value=metricGA("27+3"),
#' card_param="AoVA", moderator="EGA",
#' method="Krishnan", out="z")



fetal_zscore <- function(actual, mod_value, card_param, moderator, method, out){
if (method == "Schneider")  {
  ref= References_Schneider_etal_2005
  if(!moderator %in% ref$mod)
    stop(paste("Error: Moderator *moderator* must be one of the following:\r\n",
               paste(names(table(ref$mod)), collapse="\r\n")))

  if(!card_param %in% ref$parameter)
    stop(paste("Error: Cardiac parameters *card_param* must be one of the following:",
               paste(names(table(ref$parameter)), collapse="\r\n")))


  inter = ref[ref$parameter == card_param &
                ref$mod == moderator, "interc"]
  mult = ref[ref$parameter == card_param &
               ref$mod == moderator, "multiplier"]
  rmse = ref[ref$parameter == card_param &
               ref$mod == moderator, "root_MSE"]

  log.pred = mult*log(mod_value) + inter #; exp(log.pred)
  zscore = (log(actual/10) - log.pred)/rmse
  centile =  pnorm(zscore)*100

  if(out== "z")  value = (zscore)
  if(out=="centile") value = (centile)
  return(value)
}

  if(method == "Krishnan"){
    actual=actual/10 # in cm statt in mm
    ref= References_Krishnan_etal_2016
    if(!moderator %in% ref$Variable)
      stop(paste("Error: Moderator *moderator* must be one of the following:\r\n",
                 paste(names(table(ref$Variable)), collapse="\r\n")))

    if(!card_param %in% ref$Parameter)
      stop(paste("Error: Cardiac parameters *card_param* must be one of the following:",
                 paste(names(table(ref$Parameter)), collapse="\r\n")))

    vec <- ref[ref$Parameter == card_param &
                 ref$Variable == moderator, ]
    expected = vec$Intercept + vec$m1*mod_value + vec$m2*(mod_value^2 )+ vec$m3*mod_value^3

    if(startsWith(vec[,"Outcome"], "ln")){
      zscore = (log(actual) - expected )/vec$Root.MSE }

    if(startsWith(vec[,"Outcome"], "sqrt")){
      zscore = (sqrt(actual) - expected)/vec$Root.MSE }

    if(vec[,"Outcome"]==card_param){
     zscore = (actual - expected)/vec$Root.MSE}

    return(zscore)
  }
  centile =  pnorm(zscore)*100

  if(out== "z")  value = (zscore)
  if(out=="centile") value = (centile)
  return(value)
}




