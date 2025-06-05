#' ---
#' title: "00_FUN_Combine_raw_error_and_iterative_improvement_heatmaps"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Combine heatmaps of raw error of iterative monthly case predictions and reduction in error of iterative monthly case predictions. 
#' 
#' Timeline:
#' =========
#' 05-06-2025: Prepared script. 

Combine_raw_error_and_iterative_improvement_heatmaps <- function(split_rmse_cases_heatmap, split_rmse_incid_heatmap, 
                                                                 split_mae_cases_heatmap, split_mae_incid_heatmap,
                                                                 Split_iterative_improvement_case_heatmap_rmse, Split_iterative_improvement_incid_heatmap_rmse, 
                                                                 Split_iterative_improvement_case_heatmap_mae, Split_iterative_improvement_incid_heatmap_mae){
  
  #--------------- Order lists 
  
  #----- Raw error heatmaps 
  split_rmse_cases_heatmap_ordered <- split_rmse_cases_heatmap[order(names(split_rmse_cases_heatmap))]
  split_rmse_incid_heatmap_ordered <- split_rmse_incid_heatmap[order(names(split_rmse_incid_heatmap))]
  split_mae_cases_heatmap_ordered <- split_mae_cases_heatmap[order(names(split_mae_cases_heatmap))]
  split_mae_incid_heatmap_ordered <- split_mae_incid_heatmap[order(names(split_mae_incid_heatmap))]
  
  #----- Change in error heatmaps 
  Split_iterative_improvement_cases_heatmap_rmse_ordered <- Split_iterative_improvement_case_heatmap_rmse[order(names(Split_iterative_improvement_case_heatmap_rmse))]
  Split_iterative_improvement_incid_heatmap_rmse_ordered <- Split_iterative_improvement_incid_heatmap_rmse[order(names(Split_iterative_improvement_incid_heatmap_rmse))]
  Split_iterative_improvement_case_heatmap_mae_ordered <- Split_iterative_improvement_case_heatmap_mae[order(names(Split_iterative_improvement_case_heatmap_mae))]
  Split_iterative_improvement_incid_heatmap_mae_ordered <- Split_iterative_improvement_incid_heatmap_mae[order(names(Split_iterative_improvement_incid_heatmap_mae))]
  
  #--------------- Arrange plots 
  
  #- RMSE
  RMSE_cases_combined_heatmaps <- map2(split_rmse_cases_heatmap_ordered, 
                                       Split_iterative_improvement_cases_heatmap_rmse_ordered, ~ .x + .y + plot_layout(ncol = 2))
  RMSE_incid_combined_heatmaps <- map2(split_rmse_incid_heatmap_ordered, 
                                       Split_iterative_improvement_incid_heatmap_rmse_ordered, ~ .x + .y + plot_layout(ncol = 2))
  
  #- MAE
  MAE_cases_combined_heatmaps <- map2(split_mae_cases_heatmap_ordered, 
                                      Split_iterative_improvement_case_heatmap_mae_ordered, ~ .x + .y + plot_layout(ncol = 2))
  MAE_incid_combined_heatmaps <- map2(split_mae_incid_heatmap_ordered, 
                                      Split_iterative_improvement_incid_heatmap_mae_ordered, ~ .x + .y + plot_layout(ncol = 2))
  
  #--------------- Prep results 
  results <- list(RMSE_cases_combined_heatmaps = RMSE_cases_combined_heatmaps,
                  RMSE_incid_combined_heatmaps = RMSE_incid_combined_heatmaps,
                  MAE_cases_combined_heatmaps = MAE_cases_combined_heatmaps, 
                  MAE_incid_combined_heatmaps = MAE_incid_combined_heatmaps)
  
  return(results)

}