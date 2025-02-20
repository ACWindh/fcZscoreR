#' @title Calculation of z-scores for umbilical Doppler Sonography
#'
#' @param index one of PI (pulsatility index), RI (resitance index), or ratio for ystolic/diastolic ratio
#' @param value the observed value for either PI, RI or ratio
#' @param return_value which parameter should be calculated can be "z" or "centile"
#' @param GA estimated gestational age as exact weeks 20+1 = 20+1/7 = 20.1428571
#'
#' @return z-score or percentile of the z-score
#' @importFrom stats pnorm
#' @export
#'
#' @references  Drukker et al. 2020 (INTERGROWTH-21st project), <https://pmc.ncbi.nlm.nih.gov/articles/PMC7287403/#sec2>
#' @examples
#' z_doppler(index="PI", value=1, GA=metricGA("36+4"), return_value = "z")
#' z_doppler(index="PI", value=1, GA=36+4/7, return_value = "centile")



z_doppler <- function(index="PI", value, GA, return_value = "z"){
  requireNamespace("stats")
  if(!index %in% c("PI", "RI", "ratio"))
    stop(paste("Index must be one of the following:\r\n",
               "PI", "RI", "ratio", collapse = "\r\n"))
  if(!is.numeric(GA))
    stop(paste("GA must be in exact weeks, \r\n
    if e.g. in the form of 35+5, consider using metricGA to convert"))

if (index == "PI"){

I_skew  = -0.0768617 # Pulsatility index
I_mean  = 1.02944 + 77.7456*GA^(-2) - 0.000004455*GA^3 # mean
I_cv = -0.00645693 + 254.885*log(GA)*GA^-2 - 715.949*GA^-2 # coefficient of variation
}

if (index == "RI"){
I_skew =  0.0172944 # Resistance index
I_mean = 0.674914 + 25.3909*GA^(-2) - 0.0000022523*GA^3 # mean
I_cv = 0.0375921 + 60.7614*log(GA)*GA^(-2) - 183.336*GA^(-2) # coefficient of variation
}


if (index == "ratio"){
I_skew =  -0.2752483 #Systolic/diastolic ratio skewness
I_mean = 2.60358 + 445.991*GA^(-2) - 0.0000108754*GA^3 # mean
I_cv = -0.503202 + 1268.37*log(GA)*GA^(-2) - 3417.37*GA^(-2) # coefficient of variation
}

#z = λ–1∗{exp[(y-µ)∗ λ∗ σ–1]–1}
zval = (I_skew^(-1))*(exp((value-I_mean)*I_skew*I_cv^(-1))-1)
#Centile 	c = normal(z) * 100
centile = pnorm(zval)*100

if(return_value == "z") return(zval)
if(return_value == "centile") return(centile)
}



