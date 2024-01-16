#2 Net Present Value (NPV)======
rep(10,4) #create a vector by repeating 10 4 times
# output of the function above is: 10 10 10 10
seq( 1, 11, 2) #create a sequence of data start from 1 and end at 11. 2 is the increment of the sequence.
#outout will be: 1 3 5 7 9 11

calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CAPEX, OPEX=0){
  costs_op <- rep(OPEX, lifetime_yrs) #operating cost
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}
#Exercise: if annual revenue is 14000000, and Capital expenditure is 150000000, then please calculate Net present value. Should we invest this project?
CAPEX=1.16e6*56e3+590*306.6*0.558*180

npv=calc_NPV(annual_revenue = 103*481e3,lifetime_yrs=25, CAPEX = CAPEX)
ifelse(npv>0, "Support","obeject" )

#3 Levelized cost of electricity (LCOE)=====
#Life_span_generation_kWH is one of the required inputs to estimate the Levelized
#cost of electricity (following function)
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

#NPV of cost. if you don't consider the operational cost, you can just use CAPEX as proxy for NPV of cost
LCOE <- function(NPV_cost,Life_span_generation){
  lcoe <- NPV_cost/Life_span_generation
  return(round(lcoe,2))
}
LCOE(CAPEX,Life_span_generation_kWH(481e6) )


