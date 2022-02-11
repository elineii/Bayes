estimations_metrics <- function (df, posterior, k_1, k_2) {
  MSE_beta_1 = c()
  MSE_beta_2 = c()
  MAPE_beta_1 = c()
  MAPE_beta_2 = c()

  beta = c(colMeans(posterior$beta_1), colMeans(posterior$beta_2))
  rho = mean(posterior$rho)
  
  for (i in 1:k_1) {
    MSE_beta_1 = c(MSE_beta_1, rmse(beta_1[i], posterior$beta_1[, i]))
    MAPE_beta_1 = c(MAPE_beta_1, mape(beta_1[i], posterior$beta_1[, i]))
  }
  
  for (i in 1:k_2) {
    MSE_beta_2 = c(MSE_beta_2, rmse(beta_2[i], posterior$beta_2[, i]))
    MAPE_beta_2 = c(MAPE_beta_2, mape(beta_2[i], posterior$beta_2[, i]))
  }
  
  MSE_rho = rmse(sigma[1,2] / sqrt((sigma[1,1] * sigma[2,2])), rho)
  MAPE_rho = mape(sigma[1,2] / sqrt((sigma[1,1] * sigma[2,2])), rho)
  
  df <- rbind(df, c(beta, rho, 
                    MSE_beta_1, MSE_beta_2, MSE_rho, 
                    MAPE_beta_1, MAPE_beta_2, MAPE_rho))
  return(df)
}
