data_dic_to_sdaps("smoking/SmokingNicotineAndMoodInIBD_DataDictionary_2016-10-11.csv",
                  "smoking/smoking-sdaps-project.tex",
                  "smoking/smoking-sdaps-dictionary.csv",
                  "Smoking and Nicotine in IBD",
                  c("main_survey", "patient_health_questionnaire_9",
                    "generalized_anxiety_disorder_7_item_gad7_scale_sco",
                    "list_of_threatening_experiences", "smartphone_studies"))
data_dic_to_sdaps(
  "homefc/HomeFCServiceEvaluation_DataDictionary_2017-08-14.csv",
  "homefc/homefc-sdaps-project.tex",
  "homefc/homefc-sdaps-dictionary.csv",
  "Home Faecal Calprotectin Evaluation"
)
# data_dic_to_sdaps("IBDPatientSatisfactionSurvey_DataDictionary_2016-10-07.csv",
#                   "c:/ubuntu/patient-satisfaction-sdaps-project.tex",
#                   "patient-satisfaction-dictionary.csv",
#                   "IBD patient satisfaction survey")

data_dic_to_sdaps(
  "ccuk-dep/CCUKCD1BaselineVersion1_DataDictionary_2017-09-15 (3).csv",
  "ccuk-dep/ccuk-dep-sdaps-project.tex",
  "ccuk-dep/ccuk-dep-sdaps-dictionary.csv",
  "CCUK Depression in Ulcerative Colitis Project"
)
