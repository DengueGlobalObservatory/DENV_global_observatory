#' ---
#' title: "00_FUN_comparing_LOOCV_vs_rolling_CV"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Compare rolling CV and LOOCV
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.

comparing_LOOCV_vs_rolling_CV <- function(LOOCV_res, rolling_cv_res){
  
  #---------- Filter LOOCV_res for countries with enough data for rolling cv 
  
  LOOCV_res_filtered <- LOOCV_res %>% 
    filter(iso3 %in% rolling_cv_res$iso3)
  
  #---------- Calculate RMSE and correlation for each 
  
  LOOCV_res_filtered_RMSE <- LOOCV_res_filtered %>% 
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    summarise(Cases_rmse = rmse(Cases_clean, Predicted_cases),
              Cases_cor = cor(Cases_clean, Predicted_cases)) %>%
    arrange(iso3) %>% 
    mutate(Method = "LOOCV")
  
  rolling_cv_res_RMSE <- rolling_cv_res %>% 
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    summarise(Cases_rmse = rmse(Cases_clean, Predicted_cases),
              Cases_cor = cor(Cases_clean, Predicted_cases)) %>% 
    arrange(iso3) %>% 
    mutate(Method = "Rolling CV")
  
  #---------- Compare RMSE and correlation 
  # 
  # comparing_LOOCV_and_rolling_CV_res <- data.frame(Country = LOOCV_res_filtered_RMSE$Country, 
  #                                                   iso3 = LOOCV_res_filtered_RMSE$iso3,
  #                                                   LOOCV_RMSE = LOOCV_res_filtered_RMSE$Cases_rmse,
  #                                                   LOOCV_cor = LOOCV_res_filtered_RMSE$Cases_cor,
  #                                                   rolling_cv_RMSE = rolling_cv_res_RMSE$Cases_rmse,
  #                                                   rolling_cv_cor = rolling_cv_res_RMSE$Cases_cor)

  comparing_LOOCV_and_rolling_CV_res <- rbind(LOOCV_res_filtered_RMSE, rolling_cv_res_RMSE)
  # comparing_LOOCV_and_rolling_CV_res2 <- cbind(Country = LOOCV_res_filtered_RMSE$Country, 
  #                                              iso3 = LOOCV_res_filtered_RMSE$iso3,
  #                                              LOOCV_RMSE = LOOCV_res_filtered_RMSE$Cases_rmse, 
  #                                              Rolling_CV_RMSE = rolling_cv_res_RMSE$Cases_rmse, 
  #                                              LOOCV_cor = LOOCV_res_filtered_RMSE$Cases_cor,
  #                                              Rolling_CV_cor = rolling_cv_res_RMSE$Cases_cor) %>% 
  #   as.data.frame() %>% 
  #   mutate(RMSE_ratio = LOOCV_RMSE / Rolling_CV_RMSE,
  #          COR_ratio = LOOCV_cor / Rolling_CV_cor)
  
  #---------- Plotting
  
  comparing_LOOCV_and_rolling_CV_RMSE_plot <- ggplot(comparing_LOOCV_and_rolling_CV_res,
                                                     mapping = aes(x = Country, y = Cases_rmse, fill = Method)) + 
    geom_col(position = "dodge") + 
    labs(x = "Country", y = "Cases RMSE") +
    ggtitle("Comparing performance of LOOCV and rolling CV (RMSE)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  comparing_LOOCV_and_rolling_CV_cor_plot <- ggplot(comparing_LOOCV_and_rolling_CV_res,
                                                     mapping = aes(x = Country, y = Cases_cor, fill = Method)) + 
    geom_col(position = "dodge") + 
    labs(x = "Country", y = "Actual vs predicted cases correlation") +
    ggtitle("Comparing performance of LOOCV and rolling CV (Cor)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # 
  # comparing_LOOCV_and_rolling_CV_cor_plot <- ggplot(comparing_LOOCV_and_rolling_CV_res2, mapping = aes(x = Country, y = COR_ratio)) + 
  #   geom_col() + 
  #   labs(x = "Correlation", y = "Correlation of LOOCV and Rolling CV actual vs predicted cor") +
  #   ggtitle("Comparing performance of LOOCV and rolling CV (Cor)") +
  #   theme_minimal() +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # 
  # comparing_LOOCV_and_rolling_CV_rmse_plot <- ggplot(comparing_LOOCV_and_rolling_CV_res2, mapping = aes(x = Country, y = RMSE_ratio)) + 
  #   geom_col() + 
  #   labs(x = "RMSE", y = "Correlation of LOOCV and Rolling CV RMSE") +
  #   ggtitle("Comparing performance of LOOCV and rolling CV (RMSE)") +
  #   theme_minimal() +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # 
  
  
  #---------- Preparing results 
  results <- list(RMSE_plot = comparing_LOOCV_and_rolling_CV_RMSE_plot,
                  Cor_plot = comparing_LOOCV_and_rolling_CV_cor_plot)

  return(results)
}