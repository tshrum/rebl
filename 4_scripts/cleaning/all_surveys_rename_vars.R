# all_surveys_rename_vars.R
# 2024-08-21 update

#' Takes raw data from all surveys (1, 2a, 2b, 3) and renames variables. Note
#' that all REBL items will end in either '_' for a standard question or '_r'
#' for a reverse-coded question. For slight variations on questions between
#' survey 1 and 2, the survey 1 question will end in '_V1_' or '_V1_r'.

#' Also note that the test_set (sometimes called validation set...) from Chris's
#' thesis is not totally cleaned. I only renamed the REBL items that
#' we are actually using, and ditched the rest.

#' Input: raw csv files from Qualtrics
#' Output: 
#'    1. 'all_surveys_renamed.rds' (list of all four named surveys)
#'    2. A dataframe that contains all renamed vars, the question text, the 
#'      question code, and whether it was reverse coded. Use this to check for
#'      shenanigans. Saved as .rds and as .csv to manually explore.
#'    3. Also creating a vector of all the total possible rebl items names. This
#'      isn't really necessary any more since we added the underscores to the
#'      end of REBL names, but some of the older (defunct) scripts still call 
#'      for it, so I'll leave it in for now.



# Load Data ---------------------------------------------------------------


# Load packages
pacman::p_load(
  dplyr,
  readr,
  purrr,
  stringr,
  tibble
)

# Pull in all raw csvs. Doing some extra rigamarole to make sure we only grab 
# the right ones and that they come out in the right order

# Pull out key patterns from each file name, then grab file names in that order
patterns <- c( 'Beta_', 'Part 1_', 'Part 2_', 'Test')

# Get full file paths in order of patterns, to make sure they load in order
file_paths <-
  map_chr(patterns, ~ list.files('1_raw', pattern = .x, full.names = TRUE))

# Upload all three survey csvs
surveys <- map(file_paths, ~ read_csv(.x, comment = '#', trim_ws = TRUE, show_col_types = FALSE)) %>% 
  setNames(c('pilot', 'survey_2a', 'survey_2b', 'survey_3'))

# Check them out
names(surveys)
map(surveys, dim)
map(surveys, names)
map(surveys, get_str)

# Initalize list for results. Will save each result below in this list
all_surveys_renamed <- list()



# Rename Survey 1 ---------------------------------------------------------


#' Note that there are some slight differences in wording of questions between
#' survey 1 and other sruveys.

all_surveys_renamed[['survey_1']] <- surveys[[1]] |> 
  rename(
    'duration' = 'Duration (in seconds)',
    'consent' = Q1.1,
    'prolificID' = Q2.1,
    "waterShortShower_"= Q4.1_1,
    "waterDailyShower_r"= Q4.1_2,
    "waterFewerShowers_"= Q4.1_3,
    "waterLawn_r"= Q4.1_4,
    "waterGarden_r"= Q4.1_5,
    "waterDishNotFull_r"= Q4.1_6,
    "waterWashingNotFull_r"= Q4.1_7,
    "waterTeethStop_V1_"= Q4.1_8,
    "waterShowerStop_"= Q4.1_9,
    'season' = Q5.1,            # Ditching seasonal questions, so no '_' needed
    "homeLowAtNight"= Q5.2_1,
    "homeLowAway"= Q5.2_2,
    "homeNightLowAC"= Q5.3_1,
    "homeAwayAC"= Q5.3_2,
    "homeFanLeftOn"= Q5.3_3,
    "homeFan"= Q5.3_4,
    "homeEnergySaverSetting_"= Q5.4_1,
    "waterSaverSetting_"= Q5.4_2,
    "homeLightsLeftOn_V1_r"= Q5.4_3,
    "attentionCheck1"= Q5.4_4,
    "homeElectricOff_"= Q5.4_5,
    "homeUnplug_"= Q5.4_6,
    "homeTvLeftOn_r"= Q5.4_7,
    "socialReduceImpact_"= Q6.1_1,
    "socialCriticalScience_r"= Q6.1_2,
    "socialSupportive_V1_"= Q6.1_3,
    "socialCritical_V1_r"= Q6.1_4,
    "socialSupportive_"= Q6.1_5,
    "foodMeat_r"= Q7.1,
    "foodMeatlessDinner_"= Q7.2_1,
    "foodMeatlessLunch_"= Q7.2_2,
    "foodMeatConventional_r"= Q7.2_3,
    "foodMeatLocal_"= Q7.2_4,
    "foodFishConventional_r"= Q7.2_5,
    "attentionCheck2"= Q7.2_6,
    "foodBeef_r"= Q7.2_7,
    "foodPoultry_r"= Q7.2_8,
    "foodSeafood_r"= Q7.2_9,
    "foodMeatEveryDay_V1_r"= Q7.2_10,
    "foodCowMilk_r"= Q7.3_1,
    "foodNonDairyMilk_"= Q7.3_2,
    "foodAlmondMilk_"= Q7.3_3,
    "foodOatMilk_"= Q7.3_4,
    "foodEggs_r"= Q7.4_1,
    "foodSustainableAnimal_"= Q7.4_2,
    "foodForaged_V1_"= Q7.4_3,
    "foodProduceConventional_r"= Q7.4_4,
    "foodProduceNoPest_"= Q7.4_5,
    "foodLocal_V1_"= Q7.4_6,
    "foodDistanceRefrain_"= Q7.4_7,
    "foodGarden_"= Q7.4_8,
    "foodRestaurant_r"= Q7.4_9,
    "foodTakeOut_r"= Q7.4_10,
    "foodOwnLunch_"= Q7.4_11,
    "foodTofu_V1_"= Q7.4_12,
    "packDisposableWaterDrink_r" = Q8.1_1,
    "packDisposableWaterBuy_r" = Q8.1_2,
    "packDisposableNonWater_r" = Q8.1_3,
    "packDeclineBag_V1_" = Q8.1_4,
    "packReducedPlastic_V1_" = Q8.1_5,
    "packGlassAlumOverPlastic_" = Q8.1_6,
    "packAlumOverGlass_" = Q8.1_7,
    "packPlasticStraw_r" = Q8.1_8,
    "purchReceipt_r" = Q8.1_9,
    "packReusedSingleUse_" = Q8.2_1,
    "packReusedPaperPlasticBags_" = Q8.2_2,
    "packReusedZiploc_" = Q8.2_3,
    "packReusableBags_V1_" = Q8.3_1,
    "packReusableBottle_V1_" = Q8.3_2,
    "packReusableBottleNonWater_" = Q8.3_3,
    "packContainerToRestaurant_V1_" = Q8.3_4,
    "packCarriedUtensils_V1_"= Q8.3_5,
    "packReusableNapkin_" = Q8.3_6,
    "packClothNapkin_r" = Q8.3_7,
    "packPaperTowel_r" = Q8.3_8,
    "packRecycledContainer_V1_"= Q8.4_1,
    "packThrownAwayRecycling_r" = Q8.4_2,
    "packPullRecycleFromTrash_" = Q8.4_3,
    "packCompost_V1_" = Q8.4_4,
    "packCarriedTrash_V1_" = Q8.4_5,
    "foodThrownBadFood_r" = Q8.4_6,
    "foodThrownGoodFood_r" = Q8.4_7,
    "brokenQuestion"= Q8.4_8,
    "packCutOpen_" = Q8.5_1,
    "packDilutedSoap_" = Q8.5_2,
    "purchThrownAwayEarly_r" = Q8.5_3,
    "purchGaveToFriend_" = Q8.5_4,
    "purchRepurposedNotThrown_" = Q8.6_1,
    "purchGaveNotThrown_" = Q8.6_2,
    "purchSoldNotThrown_" = Q8.6_3,
    "purchTriedSecondHand_" = Q8.7_1,
    "purchBoughtSecondHand_" = Q8.7_2,
    "purchReplaceNotRepair_r" = Q8.7_3,
    "purchBuyNothing_" = Q8.7_4,
    "purchPrintedPaper_r" = Q8.7_5,
    "purchLibrary_" = Q8.7_6,
    "purchEnviroCleaning_" = Q8.7_7,
    "transDriveLess_" = Q9.1_1,
    "transCarpool_" = Q9.1_2,
    "transPublic_" = Q9.1_3,
    "transWalk_" = Q9.1_4,
    "transBike_V1_" = Q9.1_5,
    "transDriveDaily_r" = Q9.2_1,
    "purchSlowShipping_" = Q9.2_2,
    "transLimitFuel_" = Q9.2_3,
    "transTirePressure_" = Q9.2_4,
    "fashMend_" = Q10.1_1,
    "purchOnlyNecessities_V1_" = Q10.1_2,
    "fashReturn_r" = Q10.1_3,
    "fashPutAside_" = Q10.1_4,
    "fashShoppedSecondHand_" = Q10.1_5,
    "fashBoughtNew_r" = Q10.1_6,
    "fashFast_r" = Q10.1_7,
    "fashOnlineReturn_r" = Q10.1_8,
    "fashClothingSwap_" = Q10.1_9,
    "fashSustainable_" = Q10.1_10,
    "fashRepairable_" = Q10.1_11,
    "fashNatural" = Q10.2,            # Not REBL (not a "in last week" question)
    "fashSynthetic" = Q10.3,          # Not REBL (not a "in last week" question)
    "fashRecyclable"= Q10.4,          # Not REBL (not a "in last week" question)
    "fashShoesResolable"= Q10.5,      # Not REBL (not a "in last week" question)
    'enviroID1' = Q12.1,             # Van der Werff 2013 environmental identity
    'enviroID2' = Q12.2,             
    'enviroID3' = Q12.3,
    "greenGlow1" = Q13.2,            # Jia and Linden 2020
    "greenGlow2" = Q13.3,
    "greenGlow3" = Q13.4,
    "greenGlow4personalNorm1" = Q13.5, # Green glow and norms overlap
    "personalNorm2" = Q14.1,           # van der Werff et al 2013 personal norms
    "personalNorm3" = Q14.2,        
    'valuesPreventingPollution' = Q15.1_1,   # ESVS (Steg et al 2014) 
    'valuesProtectingEnvironment' = Q15.1_2, # (described in Bouman et al 2018)
    'valuesRespectingEarth' = Q15.1_3,
    'valuesUnifyWithNature' = Q15.1_4,
    'valuesEquality' = Q15.1_5,
    'valuesSocialJustice' = Q15.1_6,
    'valuesWorldPeace' = Q15.1_7,
    'valuesHelpful' = Q15.1_8,
    'valuesPleasure' = Q15.1_9,
    'valuesEnjoyingLife' = Q15.1_10,
    'valuesSelfIndulgent' = Q15.1_11,
    'valuesSocialPower' = Q15.1_12,
    'valuesAuthority' = Q15.1_13,
    'valuesInfluential' = Q15.1_14,
    'valuesWealth' = Q15.1_15,
    'valuesAmbitious' = Q15.1_16,
    "personalResponsibility" = Q16.1, # Bouman et al 2020
    'ccStatements' = Q16.2,           # Which of these three statements about cc
    'ccWorry' = Q16.3,                # Bouman et al 2020
    'extremeWeatherFiveYears' = Q16.4,
    "age"= Q17.2,
    "gender"= Q17.3,
    "genderOther"= Q17.3_3_TEXT,
    "raceWhite"= Q17.4_1,
    "raceBlack"= Q17.4_2,
    "raceHispanicLatino"= Q17.4_3,
    "raceAsian"= Q17.4_4,
    "raceNative"= Q17.4_5,
    "raceIslander"= Q17.4_6,
    "raceOther"= Q17.4_7,
    "raceOtherText"= Q17.4_7_TEXT,
    "education"= Q17.5,
    "zip"= Q17.6,
    "rurality"= Q17.7,
    "income"= Q17.8,
    "children"= Q17.9,
    "election"= Q17.10,
    "politics"= Q17.11
  )


names(all_surveys_renamed[[1]])



# Rename Survey 2a --------------------------------------------------------


all_surveys_renamed[['survey_2a']] <- surveys[[2]] |>
  rename(
    "duration" = "Duration (in seconds)",
    "consent" = Q1.1,
    "prolificID" = Q2.1,
    "dictatorGame" = Q3.1,
    "waterWashingNotFull_r" = Q5.1_1,
    "waterDishNotFull_r" = Q5.1_2,
    "waterShowerStop_" = Q5.1_3,
    "waterTeethStop_" = Q5.1_4,
    "waterLawn_r" = Q5.1_5,
    "homeHeatOrCool" = Q6.1,   # Not using seasonal questions, so no '_'
    "homeBelow68" = Q6.2_1,
    "homeLowAtNight" = Q6.2_2,
    "homeLowAway" = Q6.2_3,
    "homeSpaceHeater" = Q6.2_4,
    "homeAbove72" = Q6.3_1,
    "homeIncreaseNight" = Q6.3_2,
    "homeIncreaseAway" = Q6.3_3,
    "homeFan" = Q6.3_4,
    "homeClothesCold_" = Q6.4_1,
    "homeClothesHang_" = Q6.4_2,
    "homeClothesDryer_r" = Q6.4_3,
    "homeLightsOff_" = Q6.4_4,    # Similar question in s1, but leaving them on
    "socialCritical_r" = Q7.1_1,
    "socialSupportive_" = Q7.1_2,
    "socialReduceImpact_" = Q7.1_3,
    "socialOutside_" = Q7.1_4,
    "socialDocumentary_" = Q7.1_5,
    "socialRead_" = Q7.1_6,
    "socialGroup_" = Q7.1_7,
    "socialConversation_" = Q7.1_8,
    "foodOrganicVeg_" = Q8.1_1,
    "foodLocal_" = Q8.1_2,
    "foodGarden_" = Q8.1_3,
    "foodOwnLunch_" = Q8.1_4,
    "foodForage_" = Q8.1_5,
    "foodRefrainedDistance_" = Q8.1_6,
    "foodMeat_r" = Q8.2_1,
    "foodBeef_r" = Q8.2_2,
    "foodLunchNoMeat_" = Q8.2_3,
    "foodDinnerNoMeat_" = Q8.2_4,
    "foodMeatEveryDay_r" = Q8.2_5,
    "foodCowMilk_r" = Q8.3_1,
    "foodTofu_" = Q8.3_2,
    "foodNonDairyMilk_" = Q8.3_3,
    "foodOatMilk_" = Q8.3_4,
    "foodVegan_" = Q8.3_5,
    "packDisposableWaterAway_r" = Q9.1_1,
    "packDisposableWaterHome_r" = Q9.1_2,
    "packTapWaterHome_" = Q9.1_3,
    "packNonWaterDisposable_r" = Q9.1_4,
    "packReusableBottle_" = Q9.1_5,
    "packReusableMug_" = Q9.1_6,
    "packDisposableCoffee_r" = Q9.1_7,
    "packDeclineBag_" = Q9.2_1,
    "packReusableBag_" = Q9.2_2,
    "packDisposableBag_r" = Q9.2_3,
    "packContainerToRestaurant_" = Q9.2_4,
    "packCarriedUtensils_" = Q9.2_5,
    "packDisposablePlate_r" = Q9.2_6,
    "packRags_" = Q9.2_7,
    "packReusableNapkin_" = Q9.2_8,
    "packZiploc_r" = Q9.2_9,
    "attentionCheck1" = Q9.2_10,
    "packPaperTowel_r" = Q9.2_11,
    "packReducedPlastic_" = Q9.2_12,
    "packGlassAlumOverPlastic_" = Q9.2_13,
    "packAlumOverGlass_" = Q9.2_14,
    "packRecycledContainer_" = Q9.3_1,
    "packRecycledPaper_" = Q9.3_2,
    "packPickUpRecycle_" = Q9.3_3,
    "packCompost_" = Q9.3_4,
    "packCarriedTrash_" = Q9.3_5,
    "packPickedUpLitter_" = Q9.3_6,
    "foodForageBSCheck" = Q9.3_7,         # Not a REBL item - BS check
    "packThrownAwayRecycling_r" = Q9.3_8,
    "packPullRecycleFromTrash_" = Q9.3_9,
    "packDilutedSoap_" = Q9.4_1,
    "packGaveNotThrown_" = Q9.4_2,
    "packRepaired_" = Q9.4_3,
    "packReusedPaperPlasticBags_" = Q9.4_4,
    "purchThrownAwayEarly_r" = Q9.4_5,
    "foodThrownBadFood_r" = Q9.4_6,
    "foodThrownGoodFood_r" = Q9.4_7,
    "packReusedSingleUse_" = Q9.4_8,
    "purchBoughtSecondHand_" = Q9.4_9,
    "packGaveToFriend_" = Q9.4_10,
    "purchBuyNothing_" = Q9.4_11,
    "purchSlowShipping_" = Q9.4_12,
    "transWalk_" = Q10.1_1,
    "transBike_" = Q10.1_2,
    "transPublic_" = Q10.1_3,
    "transCarpool_" = Q10.1_4,
    "attentionCheck2" = Q10.1_5,
    "transDrivenAlone_r" = Q10.1_6,
    "transTaxiAlone_r" = Q10.1_7,
    "transTirePressure_" = Q10.1_8,
    "fashFast_r" = Q11.1_1,
    "purchOnlyNecessities_" = Q11.1_2,
    "fashPutAside_" = Q11.1_3,
    "fashShoppedSecondHand_" = Q11.1_4,
    "purchTriedSecondHand_" = Q11.1_5,       
    # No Q12 in survey
    "enviroID1" = Q13.2,                   
    "enviroID2" = Q13.3,                   
    "enviroID3" = Q13.4,
    "enviroID4" = Q13.5,                
    "greenGlow1" = Q14.2,
    "greenGlow2" = Q14.3,
    "greenGlow3" = Q14.4,
    "greenGlow4" = Q14.5,
    "efficacy1" = Q15.2,                   # First 4 efficacy are Roberts 1996
    "efficacy2" = Q15.3,      
    "efficacy3" = Q15.4,
    "efficacy4" = Q15.5,
    "efficacy5" = Q15.6,                   # Last 2 added for ewe sem
    "attentionCheck3" = Q15.7,
    "efficacy6" = Q15.8,
    "ccBelief1" = Q16.2,
    "ccBelief2" = Q16.3,
    "attentionCheck4" = Q16.4,
    "ccBelief3" = Q16.5,
    "ccBelief4" = Q16.6,
    "ccBelief5" = Q16.7,
    "age" = Q17.2,
    "gender" = Q17.3,
    "genderOther" = Q17.3_3_TEXT,
    "race" = Q17.4,
    "raceOther" = Q17.4_7_TEXT,
    "education" = Q17.5,
    "rurality" = Q17.6,
    "income" = Q17.7,
    "children" = Q17.8,
    "election" = Q17.9,
    "politics" = Q17.10
  )

names(all_surveys_renamed[[2]])



# Rename Survey 2b --------------------------------------------------------


all_surveys_renamed[['survey_2b']] <- surveys[[3]] |> 
  rename(
    "duration" = "Duration (in seconds)",
    "consent" = Q1.1,
    "prolificID"= Q2.1,
    "waterWashingNotFull_r" = Q4.1_1,
    "waterDishNotFull_r" = Q4.1_2,
    "waterShowerStop_" = Q4.1_3,
    "waterTeethStop_" = Q4.1_4,
    "waterLawn_r" = Q4.1_5,
    "homeHeatOrCool" = Q5.1,  # Not using seasonal questions
    "homeBelow68" = Q5.2_1,
    "homeLowAtNight" = Q5.2_2,
    "homeLowAway" = Q5.2_3,
    "homeSpaceHeater" = Q5.2_4,
    "homeAbove72" = Q5.3_1,
    "homeIncreaseNight" = Q5.3_2,
    "homeIncreaseAway" = Q5.3_3,
    "homeFan" = Q5.3_4,
    "homeClothesCold_" = Q5.4_1,
    "homeClothesHang_" = Q5.4_2,
    "homeClothesDryer_r" = Q5.4_3,
    "homeLightsOff_" = Q5.4_4,
    "socialCritical_r" = Q6.1_1,
    "socialSupportive_" = Q6.1_2,
    "socialReduceImpact_" = Q6.1_3,
    "socialOutside_" = Q6.1_4,
    "socialDocumentary_" = Q6.1_5,
    "socialRead_" = Q6.1_6,
    "socialGroup_" = Q6.1_7,
    "socialConversation_" = Q6.1_8,
    "foodOrganicVeg_" = Q7.1_1,#
    "foodLocal_" = Q7.1_2,
    "foodGarden_" = Q7.1_3,
    "foodOwnLunch_" = Q7.1_4,
    "foodForage_" = Q7.1_5,
    "foodRefrainedDistance_" = Q7.1_6,
    "foodMeat_r" = Q7.2_1,
    "foodBeef_r" = Q7.2_2,
    "foodLunchNoMeat_" = Q7.2_3,
    "foodDinnerNoMeat_" = Q7.2_4,
    "foodMeatEveryDay_r" = Q7.2_5,
    "foodCowMilk_r" = Q7.3_1,
    "foodTofu_" = Q7.3_2,
    "foodNonDairyMilk_" = Q7.3_3,
    "foodOatMilk_" = Q7.3_4,
    "foodVegan_" = Q7.3_5,
    "packDisposableWaterAway_r" = Q8.1_1,
    "packDisposableWaterHome_r" = Q8.1_2,
    "packTapWaterHome_" = Q8.1_3,
    "packNonWaterDisposable_r" = Q8.1_4,
    "packReusableBottle_" = Q8.1_5,
    "packReusableMug_" = Q8.1_6,
    "packDisposableCoffee_r" = Q8.1_7,
    "packDeclineBag_" = Q8.2_1,
    "packReusableBag_" = Q8.2_2,
    "packDisposableBag_r" = Q8.2_3,
    "packContainerToRestaurant_" = Q8.2_4,
    "packCarriedUtensils_" = Q8.2_5,
    "packDisposablePlate_r" = Q8.2_6,
    "packRags_" = Q8.2_7,
    "packReusableNapkin_" = Q8.2_8,
    "packZiploc_r" = Q8.2_9,
    "attentionCheck1" = Q8.2_10,
    "packPaperTowel_r" = Q8.2_11,
    "packReducedPlastic_" = Q8.2_12,
    "packGlassAlumOverPlastic_" = Q8.2_13,
    "packAlumOverGlass_" = Q8.2_14,
    "packRecycledContainer_" = Q8.3_1,
    "packRecycledPaper_" = Q8.3_2,
    "packPickUpRecycle_" = Q8.3_3,
    "packCompost_" = Q8.3_4,
    "packCarriedTrash_" = Q8.3_5,
    "packPickedUpLitter_" = Q8.3_6,
    "foodForageBSCheck" = Q8.3_7,      # Not REBL - BS Check
    "packThrownAwayRecycling_r" = Q8.3_8,
    "packPullRecycleFromTrash_" = Q8.3_9,
    "packDilutedSoap_" = Q8.4_1,
    "packGaveNotThrown_" = Q8.4_2,
    "packRepaired_" = Q8.4_3,
    "packReusedPaperPlasticBags_" = Q8.4_4,
    "purchThrownAwayEarly_r" = Q8.4_5,
    "foodThrownBadFood_r" = Q8.4_6,
    "foodThrownGoodFood_r" = Q8.4_7,
    "packReusedSingleUse_" = Q8.4_8,
    "purchBoughtSecondHand_" = Q8.4_9,
    "packGaveToFriend_" = Q8.4_10,
    "purchBuyNothing_" = Q8.4_11,
    "purchSlowShipping_" = Q8.4_12,
    "transWalk_" = Q9.1_1,
    "transBike_" = Q9.1_2,
    "transPublic_" = Q9.1_3,
    "transCarpool_" = Q9.1_4,
    "attentionCheck2" = Q9.1_5,
    "transDrivenAlone_r" = Q9.1_6,
    "transTaxiAlone_r" = Q9.1_7,
    "transTirePressure_" = Q9.1_8,
    "fashFast_r" = Q10.1_1,
    "purchOnlyNecessities_" = Q10.1_2,
    "fashPutAside_" = Q10.1_3,
    "fashShoppedSecondHand_" = Q10.1_4,
    "purchTriedSecondHand_" = Q10.1_5,
    "hempPurchased_" = Q11.1,
    "hempUsed_" = Q11.2,
    "eweExpGen1" = Q13.2,
    "eweExpGen2" = Q13.3,
    "eweExpGen3" = Q13.4,
    "eweExpGen4" = Q13.5,
    "eweExpGen5" = Q13.6,
    "eweExpSpec" = Q14.1,
    "eweWhatTypes"= Q14.2,
    "eweWhatTypesText" = Q14.2_8_TEXT,
    "eweAffectedHow" = Q14.3,
    "eweAffectedHowText" = Q14.3_7_TEXT,
    "eweHowMany" = Q14.4,
    "eweSigType" = Q14.5,
    "eweSigTypeText" = Q14.5_8_TEXT,
    "eweSigYear" = Q14.6,
    "eweSigMonth" = Q14.7,
    "eweSigSeverity" = Q14.8_1,
    "eweSigCostPreIns" = Q14.9,
    "eweSigCostPostIns" = Q14.10,
    "attributionGen1" = Q15.1,
    "attributionGen2" = Q15.2,
    "attributionGen3" = Q15.4,
    "attributionGen4" = Q15.5,
    "attributionGen5" = Q15.6,
    "attributionSpec1" = Q16.2,
    "attributionSpec2" = Q16.3,
    "attributionSpec3" = Q16.4,
    "attributionSpec4" = Q16.5,
    "attributionSpec5" = Q16.6,
    "psychDist1" = Q18.2,
    "psychDist2" = Q18.3,
    "psychDist3" = Q18.4,
    "attentionCheck3" = Q18.5,
    "psychDist4" = Q18.6,
    "psychDist5" = Q18.7,
    "worry1" = Q19.2,
    "worry2" = Q19.3,
    "worry3" = Q19.4,
    "worry4" = Q19.5,
    "worry5" = Q19.6,
    "socialNorms1" = Q20.2,
    "socialNorms2" = Q20.3,
    "socialNorms3" = Q20.4,
    "attentionCheck4" = Q20.5,
    "socialNorms4" = Q20.6,
    "socialNorms5" = Q20.7
  )

names(all_surveys_renamed[[3]])



# Rename Survey 3 ---------------------------------------------------------


all_surveys_renamed[['survey_3']] <- surveys$survey_3 |> 
  rename(
    "duration" = "Duration (in seconds)",
    "consent" = Q1.1,
    "prolificID" = Q2.1,
    
    # REBL items
    "waterWashingNotFull_r" = Q4.1_1,
    "waterDishNotFull_r" = Q4.1_2,
    "waterShowerStop_" = Q4.1_3,
    "waterTeethStop_" = Q4.1_4,
    
    # Note: 5.1 through 5.3 are seasonal heating/cooling questions. Not naming.
    'homeClothesCold_' = Q5.4_1,
    'homeClothesHang_' = Q5.4_2,
    'homeLightsOff_' = Q5.4_3,
    'socialSupportive_' = Q6.1_2,
    'socialReduceImpact_' = Q6.1_3,
    'socialOutside_' = Q6.1_4,
    'socialDocumentary_' = Q6.1_5,
    'socialRead_' = Q6.1_6,
    'socialGroup_' = Q6.1_7,
    'socialConversation_' = Q6.1_8,
    "foodOrganicVeg_" = Q7.1_1,
    'foodLocal_' = Q7.1_2,
    "foodLunchNoMeat_" = Q7.2_3,
    "foodDinnerNoMeat_" = Q7.2_4,  
    "foodMeatEveryDay_" = Q7.2_5,       
    "foodTofu_" = Q7.3_1,    
    "foodOatMilk_" = Q7.3_3,            
    "packReusableBottle_" = Q8.1_4,   
    "packReusableMug_" = Q8.1_5,   
    "packReusableBag_" = Q8.2_2,  
    "packContainerToRestaurant_" = Q8.2_4,     
    "packCarriedUtensils_" = Q8.2_5,
    "packRags_" = Q8.2_6,
    "packReusableNapkin_" = Q8.2_7,       
    "packAlumOverGlass_" = Q8.2_11,
    "packRecycledContainer_" = Q8.3_1,
    "packPickUpRecycle_" = Q8.3_3,
    "packCompost_" = Q8.3_4,
    "packPickedUpLitter_" = Q8.3_6,
    "packPullRecycleFromTrash_" = Q8.3_9,
    "packDilutedSoap_" = Q8.4_1, 
    "packReusedPaperPlasticBags_" = Q8.4_4,
    "purchBuyNothing_" = Q8.4_7,
    "purchSlowShipping_" = Q8.4_8,
    "transWalk_" = Q9.1_1,
    "transPublic_" = Q9.1_3,
    "transCarpool_" = Q9.1_4,
    
    # Attention checks
    'attentionCheck1' = Q8.2_9,
    'attentionCheck2' = Q9.1_5,
    'attentionCheck3' = Q13.5,
    'attentionCheck4' = Q14.4,
    'attentionCheck5' = Q15.5,
    
    # BS checks
    'foodForage_' = Q7.1_4,
    'foodForageBSCheck' = Q8.3_7,
    'foodVegan_' = Q7.3_4,
    'foodBeef_r' = Q7.2_2,
    'foodMeat_r' = Q7.2_1,
    'foodMeatEveryDay_r' = Q7.2_5,
    
    # Demographics
    'age' = Q18.2,
    'gender' = Q18.3,
    'genderOtherText' = Q18.3_3_TEXT,
    'race' = Q18.4,
    'raceOtherText' = Q18.4_7_TEXT,
    'education' = Q18.5,
    'rurality' = Q18.6,
    'income' = Q18.7,
    'children' = Q18.8,
    'election' = Q18.9,
    'politics' = Q18.10,
    
    # SEM indicators
    'pd1' = Q14.2,
    'pd2' = Q14.3,
    'pd5' = Q14.5,
    'att1' = Q16.5,
    'att2' = Q16.6,
    'att3' = Q16.7,
    'att4' = Q16.8,
    'eewe1' = Q17.2,
    'eewe3' = Q17.4,
    'eewe5' = Q17.6
  ) %>% 

  # Let's also just get rid of all the test set items we didn't name.
  select(-matches('^Q[0-9]'))



# Get All REBL Names ------------------------------------------------------


# Getting a total list of all REBL item names Also a DF that shows PEB coding
# and question text to manually check coding.
all_rebl_items <- map2_df(all_surveys_renamed, names(all_surveys_renamed), ~ {
  df <- .x %>% 
    # Grab items that end in _ or _r
    select(matches('_$|_r$')) %>% 
    # Just keep first row with question text and column names (rebl items)
    slice(1) %>% 
    # Transpose to turn rows into columns
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('rebl_item', 'question_text')) %>% 
    # Make a column specifying which survey it came from
    mutate(survey = str_split_i(.y, '_', 2),
           peb_coding = case_when(
             str_detect(rebl_item, '_r$') ~ 'reverse',
             TRUE ~ 'standard'
           ))
}) %>% 
  # Group identical items from different surveys
  # So we can see if they have the same text
  group_by(rebl_item, peb_coding, question_text) %>%
  summarise(surveys = paste(survey, collapse = ', ')) %>% 
  ungroup() %>% 
  # Then just rearrange things conveniently
  select(rebl_item, surveys, peb_coding, question_text) %>% 
  arrange(rebl_item, surveys)

get_str(all_rebl_items[[1]])
get_str(all_rebl_items)
head(all_rebl_items)
# Noice.



# All Names and Text ------------------------------------------------------


# Now everything, not just REBL items, and keep it in order
map(all_surveys_renamed, get_str)

# Just want column names and the first row
all_names <- imap(all_surveys_renamed, ~ {
  .x %>% 
    slice(1) %>% 
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('name', 'text')) %>% 
    mutate(survey = .y)
}) %>% list_rbind()



# Check, Save, clear ------------------------------------------------------



# Check out renamed surveys
names(all_surveys_renamed)
map(all_surveys_renamed, dim)

# Save renamed surveys
saveRDS(all_surveys_renamed, '5_objects/cleaning/all_surveys_renamed.rds')

# Save DF of REBL items, PEB coding, and text for manually checking
write_csv(all_rebl_items, '6_outputs/cleaning/rebl_item_coding.csv')
saveRDS(all_rebl_items, '5_objects/cleaning/rebl_item_coding.rds')

# Save full list of REBL names only
saveRDS(all_rebl_items$rebl_item, '5_objects/all_rebl_item_names.rds')

# Save all names and text for all surveys
write_csv(all_names, '6_outputs/cleaning/all_names_and_text.csv')


clear_data()
