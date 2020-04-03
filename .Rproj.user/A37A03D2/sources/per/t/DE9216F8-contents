distortWithColoredNoise = function(x, sd = 0.1, movingAverageOrder = 7L, direction = "symmetric"){

  rnorm2 <- function(x, mu, sd) {mu + sd * (x - mean(x)) / sd(x)}

  stopifnot(direction %in% c("upward", "downward", "symmetric"))
  stopifnot(movingAverageOrder > 0)
  stopifnot(sd > 0)

  whiteNoise = rnorm(n = length(x) + 2 * movingAverageOrder, mean = 1, sd = sd)
  coloredNoise = forecast::ma(whiteNoise, order = movingAverageOrder) %>% as.numeric()
  coloredNoise = coloredNoise[(movingAverageOrder + 1):(length(x) + movingAverageOrder)]
  coloredNoiseNormalized = coloredNoise %>% rnorm2(mu = 1, sd = sd)

  if (direction == "upward") coloredNoiseOneSided = 1 + abs(coloredNoiseNormalized - 1)
  else if (direction == "downward") coloredNoiseOneSided = 1 - abs(coloredNoiseNormalized - 1)
  else coloredNoiseOneSided = coloredNoiseNormalized

  xBlurried = x * coloredNoiseOneSided
  return(xBlurried)
}

createDummyDataBloodAnalysis = function(patientID, dates, tableTemplateValues){

  #currentRow = tableTemplateValues[1,]
  extendToManyDates = function(currentRow, dates, patientID){
    stopifnot(nrow(currentRow) == 1)
    tibbleReturn = tibble(patientID = patientID,
                          datetime = dates,
                          parameter = currentRow$parameter,
                          value = rep(runif(n = 1, min = currentRow$normal_min, max = currentRow$normal_max), length.out = length(dates)) %>%
                            distortWithColoredNoise(sd = 0.05, movingAverageOrder = sample(30:1000, size = 1)) %>% abs(),
                          absolute_min = 0,
                          normal_min = currentRow$normal_min,
                          normal_max = currentRow$normal_max,
                          absolute_max = Inf)
  }

  # rangeY = range(tibbleReturn$normal_min, tibbleReturn$value, tibbleReturn$normal_max)
  # ggplot(tibbleReturn, aes(x = datetime, y = value))+
  #   geom_ribbon(aes(ymin = absolute_min, ymax = normal_min), fill = "salmon", alpha = 0.4)+
  #   geom_ribbon(aes(ymin = normal_max, ymax = absolute_max), fill= "salmon", alpha = 0.5)+
  #   geom_point()+
  #   geom_line(alpha = 0.5)+
  #   theme_bw()+
  #   coord_cartesian(ylim = c(0.9 * rangeY[1], 1.1 * rangeY[2]))

  table2 = tableTemplateValues %>%
    distinct(parameter, .keep_all = TRUE) %>%
    group_by(parameter) %>%
    do(extendToManyDates(., patientID = patientID, dates = dates))
  return(table2)
}


createTablePatientsAndDates = function(numberOfPatients, maxNumberOfDays, tableTemplateValues){
  tablePatientsAndDates = lapply(seq(numberOfPatients), function(ii){
    message(ii)
    dates = seq(from = Sys.time() - days(sample(4:maxNumberOfDays, 1)),
                to = Sys.time(),
                by = "1 day") %>% with_tz("UTC")

    patientID = ii %>% digest(algo = "xxhash32")

    tableReturn = createDummyDataBloodAnalysis(patientID = patientID,
                                               dates = dates,
                                               tableTemplateValues = tableTemplateValues)
  }) %>%
    data.table::rbindlist()
}
