
mae_title <-
  function(mae){
    str_c(ifelse(mae@metadata$data_source == "simulated", "Simulated", "Actual Study"), " Data as of ", mae@metadata$date)
  }