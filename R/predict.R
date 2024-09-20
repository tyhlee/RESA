country_intercept <- c(0.206614071785856,0.396778585810532,-0.178651089036148,
                       0.140143798052199,-0.0486966807921188,-0.577918436418405,
                       -0.626417793505833,0.184877150531234,-0.351575709891019,
                       -0.42543686187852,-0.224596747478995,-0.273500483451308,
                       -0.191490368668896,-0.546521509778813,-0.486370198935542,
                       -0.0640986751415901,-0.0158264766340979,0.0550198650433058,
                       -0.251361024127084,-0.734096688481075,0.200233727694716,
                       -0.487058826693942)

list_countries <- sort(c("Argentina","Australia","Brazil","Bulgaria","Canada",
                         "Colombia","Denmark","France","Germany","Greece",
                         "Italy","Japan","Korea","Kuwait","Mexico",
                         "Netherlands","Saudi Arabia","Spain",
                         "Taiwan","UAE","UK","USA"))

# RE_model_rate <- read_rds("data/RE_model_rate.rds")
# tmp_re_model_frame <- RE_model_rate$model
# colnames(tmp_re_model_frame) <- c("RE","log_rate","wt")
# RE_model <- lm(RE~log_rate,data=tmp_re_model_frame,weights=wt)
RE_model <- read_rds("data/RE_model.rds")
risk_to_background_intercept <- function(chosen_risk){
  chosen_risk <- max(min(chosen_risk,0.999),0.001)
  # risk = 1 - exp(-rate)
  # 1 - risk = exp(-rate)
  # rate = -log(1-risk)
  tmp_rate <- -log(1-chosen_risk)

  predict(RE_model,newdata=data.frame(log_rate=log(tmp_rate))) %>%
    as.numeric()
}

list_model_coef <- read_rds("data/list_model_coef.rds")

all_covars <- c("asthma_control",
                "percpredprefev1",
                "fev1_fvc_preratio",
                "longterm_ocs",
                "eosin_bsl",
                "noxide_result",
                "crs_np",
                "macrolide")

model_params <- c("asthma_control",
                  "percpredprefev1",
                  "fev1_fvc_preratio",
                  "longterm_ocs",
                  "eosin_bsl",
                  "noxide_result",
                  "crs_np", "macrolide")

model_param_index <- read_rds("data/model_param_index.rds")

model_index_finder <- function(input_key,param_index=model_param_index){
  as.numeric(which(param_index$key == input_key))
}

compute_cond_mu <- function(chosen_input_values,
                            chosen_params,
                            chosen_intercept){
  length_input <- length(chosen_input_values)
  length_params <- length(chosen_params$estimate)

  if(length_input>length_params){
    chosen_input_values <- chosen_input_values[1:length_params]
  }
  # print(chosen_input_values)
  cond_link <- chosen_intercept + sum(chosen_input_values * chosen_params$estimate)
  return(exp(cond_link))

}

compute_zero_prob <- function(zero_params){
  tmp_link <- zero_params$estimate[1]
  return(1/(1+exp(-tmp_link)))
}

country_level_finder <- function(intercepts,tmp_country){
  if(str_detect(tmp_country,".*[0-9].*")){
    tmp_val <- as.numeric(tmp_country)/100
    return(risk_to_background_intercept(tmp_val))
  } else{
    intercepts %>%
      filter(country == tmp_country[1]) %>%
      select(intercept) %>%
      unlist() %>%
      as.numeric()
  }
}


process_asthma_control <- function(tmp_asthma_control){
  if(is.na(tmp_asthma_control)){
    return(c(NA,NA))
  } else{
    if(tmp_asthma_control=="Uncontrolled"){
      return(c(0,0))
    } else if(tmp_asthma_control=="Partially controlled"){
      return(c(1,0))
    } else{
      return(c(0,1))
    }
  }
}


process_crs <- function(tmp_crs){
  if(is.na(tmp_crs)){
    return(c(NA,NA))
  } else{
    if(tmp_crs=="No"){
      return(c(0,0))
    } else if(tmp_crs=="Yes"){
      return(c(0,1))
    } else{
      return(c(1,0))
    }
  }
}


process_input_values <- function(chosen_input_values){

  c(0,
    0,
    chosen_input_values[3] %>% as.numeric(),
    as.numeric(chosen_input_values[1])/100,
    as.numeric(chosen_input_values[2] %in% c("Male",'male')),
    process_asthma_control(chosen_input_values[4]),
    ifelse(is.na(chosen_input_values[5]),NA,as.numeric(chosen_input_values[5])/100),
    ifelse(is.na(chosen_input_values[6]),NA,as.numeric(chosen_input_values[6])/100),
    as.numeric(chosen_input_values[7] %in% c('Yes','yes')),
    as.numeric(chosen_input_values[8])/1000,
    as.numeric(chosen_input_values[9])/1000,
    process_crs(chosen_input_values[10]),
    as.numeric(chosen_input_values[11] %in% c('Yes','yes')),
    0,
    0) %>%
    na.omit() %>%
    as.vector()

}

compute_risk <- function(model_index,
                         input_country,
                         input_values){

  chosen_model_params <- list_model_coef[[model_index]]

  sigma_model <- chosen_model_params$sigma

  zero_prob <- compute_zero_prob(chosen_model_params$zero_param)

  chosen_intercept <- country_level_finder(chosen_model_params$intercept,
                                           input_country)
  cond_mu <- compute_cond_mu(process_input_values(input_values),
                             chosen_model_params$cond_param,
                             chosen_intercept)

  risk_0 <- zero_prob+(1-zero_prob) *pnbinom(0,size=sigma_model, mu=cond_mu)
  risk_1 <- 1-(zero_prob+(1-zero_prob) *pnbinom(0,size=sigma_model, mu=cond_mu))
  risk_2 <- 1-(zero_prob +(1-zero_prob) * pnbinom(1,size=sigma_model, mu=cond_mu))

  return(c(risk_0,risk_1,risk_2))
}

# samplePatients <- data.frame(ID = c(100,101),
#            setting = c(30,"Canada"),
#            age = c(50,30),
#            sex = c("Male","Female"),
#            sev_exac_history = c(4,1),
#            asthma_control = c("Well-controlled","Uncontrolled"),
#            perpredprefev1 = c(40,70),
#            fev1_fvc_prepratio = c(30,24),
#            ocs = c(0,1),
#            FeNO = c(300,400),
#            blood_eosin = c(300,150),
#            crs_np = c("No","Yes"),
#            macrolide = c("Yes","No"))


#' RESA: The risk calculator for severe asthma exacerbations in patients with severe asthma
#' @param data patient data matrix. Can have one or many patients in it. See \link{samplePatients}.
#' @return patient data with predicted risk
#'
#' @import dplyr
#' @import readr
#' @examples
#' results <- resa(data = samplePatients)
#' @export

resa <- function(data){

  samplePatients_colNames <- c("ID","setting","age","sex", "sev_exac_history",
                               "asthma_control",
                               "perpredprefev1",
                               "fev1_fvc_prepratio",
                               "ocs",
                               "FeNO",
                               "blood_eosin",
                               "crs_np", "macrolide")

  check_colnames <- match(samplePatients_colNames,colnames(data))

  if(any(is.na(check_colnames))){
    stop("Please check the column names of the patient data.The column names must match those of samplePatients.")
  }

  if(any(c(is.na(data$ID),is.na(data$setting),is.na(data$age),is.na(data$sex),is.na(data$sev_exac_history)))){
    stop("You must provide the mandatory predictors: ID, setting, age, sex, and sev_exac_history.")
  }

  tmp_setting <- data$setting
  tmp_setting_numeric <- suppressWarnings(parse_number(tmp_setting))

  if(any(tmp_setting_numeric>=100 | tmp_setting_numeric <=0, na.rm = T)){
    stop("Setting must be greater than 0% and less than 100%.")
  }

  if(!all(tmp_setting[is.na(tmp_setting_numeric)] %in% list_countries)){
    stop("Your setting (region) is not supported by RESA. Please check the region or provide the risk of having a severe exacerbation in your setting.")
  }

  if(any(data$age<18)){
    stop("This tool is not intended for patients under 18 years of age.")
  } else if(any(data$age>100)){
    data <- data %>%
      mutate(age = ifelse(age>100,100,age))
    warning("Age is truncated to 100 years.")
  }

  if(!all(data$sex %in% c("Male","male",'Female',"Female"))){
    stop("Sex must be either Male or Female.")
  }

  if(!all(data$asthma_control %in% c("Well-controlled","Uncontrolled","Partially-controlled"))){
    stop("Asthma control level must be one of Well-controlled, Partially-controlled, or Uncontrolled.")
  }

  if(any(data$perpredprefev1>100)){
    data <- data %>%
      mutate(perpredprefev1 = ifelse(perpredprefev1>100,100,perpredprefev1))
    warning("perpredprefev1 has been truncated to 100%.")
  }

  if(any(data$fev1_fvc_prepratio>100 | data$fev1_fvc_prepratio<0)){
    stop("fev1_fvc_prepratio must be between 0 and 100% (or NA if unknown).")
  }

  if(!all(data$ocs %in% c("Yes",'yes',"no","No"))){
    stop("ocs must be either Yes or No.")
  }

  if(!all(data$macrolide %in% c("Yes",'yes',"no","No"))){
    stop("macrolide must be either Yes or No.")
  }

  if(!all(data$crs_np %in% c("Yes",'yes',"no","No","Yes with nasal polyps"))){
    stop("crs_np must be either Yes, Yes with nasal polyps, or No.")
  }

  if(any(data$FeNO <0)){
    stop("FeNO cannot be negative.")
  }

  if(any(data$FeNO >1000)){
    data <- data %>%
      mutate(FeNO = ifelse(FeNO>1000,1000,FeNO))
    warning("FeNO has been truncated to 1,000 ppb.")
  }

  if(any(data$blood_eosin <0)){
    stop("blood_eosin cannot be negative.")
  }

  if(any(data$blood_eosin >1000)){
    data <- data %>%
      mutate(blood_eosin = ifelse(blood_eosin>1000,1000,blood_eosin))
    warning("blood_eosin has been truncated to 1,000 per microliter.")
  }


  data_temp <- data[samplePatients_colNames]

  tmp_key <- apply(data_temp %>%
                     select(asthma_control,
                            perpredprefev1,
                            fev1_fvc_prepratio,
                            ocs,
                            blood_eosin,
                            FeNO,
                            crs_np,
                            macrolide),
                   2,
                   function(x){
                     (!is.na(x)) %>%
                       as.numeric()
                   }) %>%
    as.data.frame()

  data_temp$key <- do.call(paste0, as.data.frame(tmp_key, stringsAsFactors=FALSE))

  data_temp <- data_temp %>%
    left_join(model_param_index %>%
                select(key,model_index),
              by='key') %>%
    rename(index = model_index)

  res <- c()

  for(i in 1:nrow(data_temp)){
    tmp_data <- data_temp[i,]
    res[[i]] <- compute_risk(model_index = tmp_data$index,
                 input_country = tmp_data$setting,
                 input_values = process_input_values(c(tmp_data$age,
                                                     ifelse(is.null(tmp_data$sex),NA,tmp_data$sex),
                                                     tmp_data$sev_exac_history,
                                                     ifelse(is.null(tmp_data$asthma_control),NA,tmp_data$asthma_control),
                                                     tmp_data$perpredprefev1,
                                                     tmp_data$fev1_fvc_prepratio,
                                                     ifelse(is.null(tmp_data$ocs),NA,tmp_data$ocs),
                                                     tmp_data$blood_eosin,
                                                     tmp_data$FeNO,
                                                     ifelse(is.null(tmp_data$crs_np),NA,tmp_data$crs_np),
                                                     ifelse(is.null(tmp_data$macrolide),NA,tmp_data$macrolide))))
  }

  res <- res %>%
    do.call(rbind,.)
  colnames(res) <- c("predicted_no_severe_exacerbation",
                     "predicted_at_least_one_severe_exacerbation",
                     "predicted_at_least_two_severe_exacerbations")

  return(cbind(data,res))

}

