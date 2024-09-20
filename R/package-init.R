#' @import dplyr
#' @import readr
NULL

#' Sample Patient Characteristics Inputs
#'
#' A dataset containing sample patient characteristics to run the prediction model.
#' Predictors are as follows:
#'
#' \itemize{
#'   \item ID (optinal): a unique patient identifier
#'   \item setting (mandatory): the region that is supported by RESA (Argentina, Australia, Brazil, Bulgaria, Canada, Colombia, Denmark, France, Germany, Greece, Italy, Japan, Korea, Kuwait, Mexico, Netherlands, Saudi Arabia, Spain, Taiwan, UAE, UK, USA) OR the risk (\%) of having a severe asthma exacerbation in your setting.
#'   \item age (mandatory): the age (in years) of the patient between 18 years and 100 years.
#'   \item sex (mandatory): the sex of the patient (Male or Female).
#'   \item sev_exac_history (mandatory): the number of severe asthma exacerbations in the past 12 months.
#'   \item asthma_control: the asthma control level (Well-controlled, Partially-controlled, or Uncontrolled) assessed either by the Global Initiative Network of Asthma criteria, Asthma Control Questionnaire (ACQ) score, or Asthma Control Test (ACT) score.
#'   \item perpredprefev1: the most recent measurement of percentage predicted forced expiratory volume in 1 second (FEV1) before bronchodilator use. It is truncated at 100\%.
#'   \item fev1_fvc_prepratio: the most recent measurement of forced expiratory volume in 1 second (FEV1) to forced vital capacity (FVC) ratio (\%) before bronchodilator use. If it is more than 100\%, put 100.
#'   \item ocs: whether the patient has used oral corticosteroids (OCS) continuously more than 3 months in the past 12 months.
#'   \item FeNO: the most recent measurement of fractional exhaled nitric oxide (FeNO) in ppb. If it is more than 1,000, put 1,000.
#'   \item blood_eosin: the most recent measurement of blood eosinophil count (per microliter). If it is more 1,000, put 1,000.
#'   \item crs_np: whether the patient has chronic rhinosinusitis (No, Yes, or Yes with nasal polyps).
#'   \item macrolide: whether the patient has used macrolide (azithromycin, clarithromycin, and erythromycin) in the past 12 months.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name samplePatients
#' @format A data frame with 2 rows and 13 variables
NULL
