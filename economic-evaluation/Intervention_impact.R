#' Calculate logical vectors where particular intervention value is changed from
#' T to F, this is used later to filter counterfactuals.
#' @param x Named logical vector, names are interventions.
#' @returns A list, where each element is a logical vector for an intervention, 
#' that had value T in x, but here the particular intervention value is F
#' @examples
counterfactual_lgc<-function(x){
  res <- lapply(which(x), function(i) {
    y <- x
    y[i] <- FALSE
    names(y) <- names(x)
    y
  })
  return(res)
}

#' Filter counterfactuals using a list of logical vectors
#' @param df data table containing interv as column names
#' @param target logical vector which is used to filter interventions
#' @param interv character vector with names of all logical intervention 
#' columns included in df
#' @returns data.table filter from df, where the columns interv correspond to
#' logical given in target  
#' @examples
filter_lgc<-function(df,
                     target,
                     interv=c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                              "STD_Nets","PBO_Nets","IG2_Nets",
                              "SMC","PMC")){
  subdf <- df[, .SD, .SDcols = interv]
  out<- df[rowSums(subdf == matrix(target, nrow(df), length(target), byrow = TRUE)) == length(target)]
  return(out)
}



#' Calculate per intervention impact for all interventions contained in the 
#' scenario
#' 
#' @param df data.table containing all intervention combinations
#' @param strata named vector, first three names have to be admin unit, age, 
#' scenario_name (in that order, scenario_name as fixed string), 
#' may contain additional entries
#' @param indicator character vector of impact indicators to be used 
#' @param interv character vector with names of all logical intervention 
#' columns included in df
#' @param keep_col character vector of columns from df or names from strata to 
#' be kept in output

#' @returns data.table with admin unit, age, scenario name in long format with
#' metric column for possible averted indicator (with prefix ca.) and value 
#' column for difference between value without intervention and with intervention,
#' may contain additional columns as detailed in keep_col 
#' @examples

per_interv_impact<-function(df,
                            strata=c(DHIS2_Dist="Ulanga District Council",
                                     age="0-100",
                                     scenario_name="ICCM_IPTSc_IRS",
                                     plan_mock_alias="nsp1"),##simulation for which per intervention impact should be calculated, order of first three names is important
                            indicator=c("tot.mean_tSevere"),
                            interv=c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                                     "STD_Nets","PBO_Nets","IG2_Nets","SMC","PMC"),
                            keep_col=NULL){
  #strata names for joining, bust be first three in that order
  strat_join<-names(strata)[1:3]
  ##get intervention logicals for strata scenario
  strata<-as.data.table(t(strata))
  setkeyv(df, strat_join)
  setkeyv(strata, strat_join)
  target<-as.logical(df[strata][,..interv])
  names(target)<-interv
  
  ##prepare counterfactual data needed for calculation
  counterfactual_cols<-c(strat_join,indicator,interv)
  if(!is.null(keep_col)){counterfactual_cols<-c(counterfactual_cols,keep_col)}
  lapply(counterfactual_lgc(target),
         function(x) {
           setkeyv(df, strat_join[1:2])
           setkeyv(strata, strat_join[1:2])
           filter_lgc(df[strata][,..counterfactual_cols],x)}
  )->counterfactual_df
  counterfactual_df <- counterfactual_df[lapply(counterfactual_df, nrow) > 0]
  
  ##calculate difference between indicators in strata and counterfactuals
  setkeyv(df, strat_join)
  setkeyv(strata, strat_join)
  strata_df<-df[strata][,..counterfactual_cols]
  
  lapply(counterfactual_df,
         function(x) {
           setkeyv(x, strat_join[1:2])#join on district an age
           setkeyv(strata, strat_join[1:2])#join on district an age
           foo<-x[strata_df]#join on district an age
           for (k in indicator){
             foo[,paste0("ca.",k):=get(k)-get(paste0("i.",k))]
           }
           foo_cols <- c(strat_join[strat_join!="scenario_name"],"i.scenario_name",paste0("ca.",indicator))
           if(!is.null(keep_col)){foo_cols<-c(foo_cols,keep_col)}
           melt(
             foo[, ..foo_cols],
             id.vars =  setdiff(foo_cols,indicator),
             measure.vars = paste0("ca.",indicator),
             variable.name = "metric",
             value.name = "value"
           )->foo
           return(foo)
         }
  )->counterfactual_df
  
  lapply(names(counterfactual_df),
         function(x) {
           foo <- counterfactual_df[[x]]
           foo <- foo[,intervention:=x]
           setnames(foo, 
                    old = c("i.scenario_name"), 
                    new = c("scenario_name"))
           return(foo)
         }
  )->result
  
  result<-rbindlist(result)
  result<-result[,(paste0("ca.",indicator)):=NULL]
  
  return(result)
}


