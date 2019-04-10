





#' Present Value
#'
#' Calculates the present value of a given future value
#'
#' @param fv Future Value
#' @param r Discount Rate
#' @param n Number of Compounding Periods
#'
#' @return Returns the Present Value
#' @export
#'
#' @examples pv(5000, 0.08/12, 5*12)
pv <- function(fv, r, n) { fv/(1+r)^(n)}


#' Future Value
#'
#' Calculates the Future Value given a Present Value
#'
#' @param pv Present Value
#' @param r Discount Rate
#' @param n Number of Compounding Periods
#'
#' @return Returns the Future Value
#' @export
#'
#' @examples fv(5000, 0.08/12, 5*12)
fv <- function(pv, r, n) {pv*(1 + r)^(n)}



#' Geometric Mean Return
#'
#' @param c Periodic returns in decimal form
#'
#' @return Returns the Geometric Mean Return
#' @export
#'
#' @examples geometric(c(0.05, 0.02, -0.03, 0.09, -0.02))
geometric <- function(c) {

  c2 <- c + 1
  (prod(c2))^(1/length(c)) - 1

}



#' Return Calculation
#'
#' @param vector Vector from which to calculate the periodic return
#'
#' @return Returns the Periodic Percent Return
#' @export
#'
#' @examples r.calc(c(100, 75, 50, 80, 125))
r.calc <- function(vector) {
  r1 <- c(diff(vector)/vector)
  r1[is.na(r1)] <- 0
  print(r1)
}



#' Annuity Loan Calculation
#'
#' Calculates the payment, present value, future value, rate, or the number of periods
#'
#' @param type Loan parameter to return. ("pv", "fv", "pmt", "nper", "rate")
#' @param pv Present Value
#' @param fv Future Value
#' @param pmt Periodic Payment
#' @param n Number of Periods
#' @param r Rate
#' @param end Logical, set to TRUE. If FALSE, payments are made at the beginning the period.
#'
#' @return Returns the selected Annuity Loan Parameter
#' @export
#'
#' @examples annuity(type = "pmt", pv = -2000, fv = 0, n  = 4 * 12, r = 0.06/12, end = TRUE)
annuity <- function(type = c("pv", "fv", "pmt", "nper", "rate"), pv, fv = 0, pmt, n, r, end = TRUE) {

  if(end == TRUE){
    t <- 0
  } else {
    t <- 1
  }


  if(type == "pmt"){


    (-pv*(1+r)^n - fv) / ((((1+r)^n-1)/r)*(1+r*t))


  } else if(type == "pv"){


    (-pmt*((((1+r)^n)-1)/r)*(1+r*t) - fv) / (1+r)^n


  } else if(type == "fv"){


    -pv*(1+r)^n - pmt*(((1+r)^n-1)/r)*(1+r*t)


  } else if(type == "nper"){

    annuity <- data.frame(N = c(0:2000))
    annuity$value <- abs(pv*(1+r)^annuity$N + pmt*((((1+r)^annuity$N)-1)/r)*(1+r*t)+fv)
    annuity$N[match(min(annuity$value), annuity$value)]


  }else if(type == "rate"){


    annuity <- data.frame(R = seq(0.0001, 2, 0.0001))
    annuity$value <- abs(pv*(1+annuity$R)^n + pmt*((((1+annuity$R)^n)-1)/annuity$R)*(1+annuity$R*t)+fv)
    annuity$R[match(min(annuity$value), annuity$value)]


  }else{

    stop("check your inputs")

  }
}




#' Interest Payment
#'
#' Calculates the interest portion of the payment in period "x"
#'
#' @param pv Present Value
#' @param fv Future Value
#' @param n Number of Periods
#' @param r Rate
#' @param x Period in which to calculate the interest portion of the payment
#' @param end If FALSE, payments are made at the beginning of the period
#'
#' @return Returns the Interest Portion of the Payment in Period "x"
#' @export
#'
#' @examples ipmt(pv = 20000, fv = 0, n = 5 * 12, r = 0.05/12, x = 12, end = TRUE)
ipmt <- function(pv, fv = 0, n, r, x, end = TRUE){


  if(end == TRUE){
    t <- 0
  } else {
    t <- 1
  }

  pmt <- (-pv*(1+r)^n - fv) / ((((1+r)^n-1)/r)*(1+r*t))


  if(end == TRUE) {

    -r*pv*(1+r)^(x-1) - pmt*((1+r)^(x-1)-1)

  }else{

    -r*(pv+pmt)*(1+r)^(x-2) - pmt*((1+r)^(x-2)-1)

  }
}



#' Principal Payment
#'
#' Calculates the principal of the payment in period "x"
#'
#' @param pv Present Value
#' @param fv Future Value
#' @param n Number of Periods
#' @param r Rate
#' @param x Period in which to calculate the principal portion of the payment
#' @param end If FALSE, payments are made at the beginning of the period
#'
#' @return Returns the Principal Portion of the Payment in Period "x"
#' @export
#'
#' @examples ppmt(pv  = 5000, fv = 0, n = 4 * 12, r = 0.06/12, x = 12, end = TRUE)
ppmt <- function(pv, fv = 0, n, r, x, end = TRUE){


  if(end == TRUE){
    t <- 0
  } else {
    t <- 1
  }

  pmt <- (-pv*(1+r)^n - fv) / ((((1+r)^n-1)/r)*(1+r*t))


  if(end == TRUE) {

    ipmt <- (-r*pv*(1+r)^(x-1) - pmt*((1+r)^(x-1)-1))

  }else{

    ipmt <- (-r*(pv+pmt)*(1+r)^(x-2) - pmt*((1+r)^(x-2)-1))

  }

  pmt - ipmt
}




#' Amortization Schedule
#'
#' Creates an amortization schedule of a loan
#'
#' @param r Rate
#' @param n Number of Periods
#' @param pv Present Value
#' @param fv Future Value, set = 0
#' @param end If FALSE, payments are made at the beginning of the period
#'
#' @return Returns the Amortization Schedule in a dataframe
#' @export
#'
#' @examples schedule(r = 0.06/12, n = 10 * 12, pv = -5000, fv = 0, end = TRUE)
schedule <- function(r, n, pv, fv = 0, end = TRUE) {
  Payment_Schedule <- data.frame(Period = c(1:n))
  Payment_Schedule$Payment <- round(-1 * annuity(type = "pmt", n = n, r = r, pv = pv, end = end, fv = fv),2)
  Payment_Schedule$Cum.Pmt <- round(cumsum(Payment_Schedule$Payment),2)
  Payment_Schedule$Interest <- round(-1 * ipmt(pv = pv, n = n, r = r, x = Payment_Schedule$Period, fv = fv, end = end),2)
  Payment_Schedule$Cum.Int <- round(cumsum(Payment_Schedule$Interest),2)
  Payment_Schedule$Principal <- round(-1 * ppmt(pv = pv, n = n, r = r, x = Payment_Schedule$Period, fv = fv, end = end),2)
  Payment_Schedule$Cum.Prin <- round(cumsum(Payment_Schedule$Principal),2)
  Payment_Schedule$Rem.Bal <- round(pv - Payment_Schedule$Cum.Prin,2)

  overlay <- c(0, 0, 0, 0, 0, 0, 0, pv)
  Payment_Schedule <- rbind(overlay, Payment_Schedule)

  Payment_Schedule
}




#' Amortization Schedule With Irregular Payments
#'
#' Creates an amortization schedule of a loan with irregular payments and withdrawals
#'
#' @param payments Vector of payments, the first payment must be 0
#' @param dates Vector of dates, the first date is the date of origination
#' @param apr Annual rate
#' @param pv Present Value
#'
#' @return Returns the irregular Amortization Schedule in a Dataframe
#' @export
#'
#' @examples irregular(payments = c(0, 200, -100, 500),
#' dates = c("2019-01-01", "2019-02-08", "2019-03-20", "2019-05-05"), apr = 0.05, pv = 2000)
irregular <- function(payments, dates, apr, pv){

  if(!payments[1]==0){
    stop("Initial payment must be zero")
  }
  dates <- as.Date(dates, origin = "1970-01-01")
  Schedule <- data.frame(Dates = dates, Payments = 0, Cum.Pmts = 0, Interest = 0, Cum.Int = 0,
                         Principal = 0, Cum.Prin = 0, Balance = pv)

  for(i in 2:length(payments)){
    Schedule$Dates[i] <- dates[i]
    Schedule$Payments[i] <- payments[i]
    Schedule$Cum.Pmts <- cumsum(Schedule$Payments)
    Schedule$Interest[i] <- ((as.numeric(dates[i])-as.numeric(dates[i-1]))/365 *apr*Schedule$Balance[i-1])
    Schedule$Cum.Int <- cumsum(Schedule$Interest)
    Schedule$Principal[i] <- Schedule$Payments[i]-Schedule$Interest[i]
    Schedule$Cum.Prin <- cumsum(Schedule$Principal)
    Schedule$Balance[i] <- pv - Schedule$Cum.Prin[i]
  }


Schedule

}





