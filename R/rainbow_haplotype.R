#' @export
prepForRainbow <-function(inputData, sampleCol = s_Sample, targetCol= p_name, popUIDCol = h_popUID, relAbundCol = c_AveragedFrac, minPopSize = 3, colorOuput = 11, barHeight = 0.80){
  inputData_filt = inputData %>%
    group_by({{sampleCol}})  %>%
    mutate(targetNumber = length(unique({{targetCol}}))) %>%
    group_by() %>%
    mutate("{{sampleCol}}" := as.character({{sampleCol}})) %>%
    group_by({{sampleCol}}) %>%
    group_by() %>%
    mutate("{{sampleCol}}" := factor({{sampleCol}})) %>%
    group_by({{sampleCol}}) %>%
    arrange({{popUIDCol}}) %>%
    group_by({{sampleCol}}, {{targetCol}}, {{popUIDCol}}) %>%
    summarise("{{relAbundCol}}" := sum({{relAbundCol}})) %>%
    group_by({{sampleCol}}, {{targetCol}})
  inputData_filt = inputData_filt%>%
    group_by({{sampleCol}}, {{targetCol}}) %>%
    mutate(totalAbund = sum({{relAbundCol}})) %>%
    mutate("{{relAbundCol}}" :={{relAbundCol}}/totalAbund)

  inputData_filt = inputData_filt %>%
    group_by({{sampleCol}}, {{targetCol}}, {{popUIDCol}}) %>%
    mutate(s_COI = length(unique({{popUIDCol}})))


  inputData_filt = inputData_filt %>%
    group_by() %>%
    group_by({{sampleCol}}, {{targetCol}}) %>%
    mutate(relAbundCol_mod = {{relAbundCol}} * barHeight) %>%
    mutate(fracCumSum = cumsum({{relAbundCol}}) - {{relAbundCol}}) %>%
    mutate(fracModCumSum = cumsum(relAbundCol_mod) - relAbundCol_mod) %>%
    mutate(fakeFrac = 1/unique(s_COI))  %>%
    mutate(fakeFracMod = fakeFrac * barHeight)  %>%
    mutate(fakeFracCumSum = cumsum(fakeFrac) - fakeFrac) %>%
    mutate(fakeFracModCumSum = cumsum(fakeFracMod) - fakeFracMod)

  inputData_filt_popName = inputData_filt %>%
    select({{sampleCol}}, {{targetCol}}, {{popUIDCol}}) %>%
    unique() %>%
    group_by({{targetCol}}, {{popUIDCol}}) %>%
    summarise(samp_n = n()) %>%
    arrange({{targetCol}}, desc(samp_n)) %>%
    group_by({{targetCol}}) %>%
    mutate(popid = row_number())  %>%
    group_by({{targetCol}}) %>%
    mutate(maxPopid = max(popid))



  inputData_filt = inputData_filt %>%
    left_join(inputData_filt_popName)


  inputData_filt_tarFilt = inputData_filt %>%
    filter(maxPopid >= minPopSize) %>%
    group_by()

  inputData_filt_tarFilt = inputData_filt_tarFilt%>%
    group_by() %>%
    mutate("{{targetCol}}" := factor({{targetCol}}))


  colorsOutput = colorOuput;
  #colorsOutput =  length(unique(inputData_filt_tarFilt${{targetCol}}))
  targetNumber = 0
  targetToHue = tibble()
  tempTarCol = inputData_filt_tarFilt %>% select({{targetCol}})

  for(tarname in levels(tempTarCol[[1]] ) ) {
    targetToHueForTarget = tibble("{{targetCol}}" := tarname, hueMod = (targetNumber%% colorsOutput) + 1)
    targetToHue = targetToHue %>%
      bind_rows(targetToHueForTarget)
    targetNumber = targetNumber + 1;
  }
  inputData_filt_tarFilt = inputData_filt_tarFilt %>%
    group_by({{targetCol}}) %>%
    mutate(popidFrac = (popid-1)/(maxPopid))
  tempTarCol = inputData_filt_tarFilt %>% select({{targetCol}})

  targetToHue = targetToHue %>%
    mutate("{{targetCol}}" := factor({{targetCol}}, levels = levels(tempTarCol[[1]])))
  inputData_filt_tarFilt = inputData_filt_tarFilt %>%
    left_join(targetToHue) %>%
    mutate(popidPerc = 100 * popidFrac) %>%
    mutate(popidFracRegColor = round(abs((popidPerc + (hueMod/colorsOutput) *100) %% 200 -0.0001 ) %% 100) ) %>%
    mutate(popidPercLog = log((popidFrac * 99) + 1 , base = 100) * 100 ) %>%
    mutate(popidFracLogColor = round(abs((popidPercLog + (hueMod/colorsOutput) *100) %% 200 -0.0001 ) %% 100) )
  return(inputData_filt_tarFilt)
}

genRainbowHapPlotObjActualFracLogColor <-function(prepData, sampleCol = s_Sample, targetCol= p_name, popUIDCol = h_popUID, relAbundCol = c_AveragedFrac, colors = RColorBrewer::brewer.pal(11, "Spectral")){
  sofonias_theme = theme_bw() +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank() )+
    theme(axis.line.x = element_line(color="black", size = 0.3),axis.line.y =
            element_line(color="black", size = 0.3))+
    theme(text=element_text(size=12, family="Helvetica"))+
    theme(axis.text.y = element_text(size=12))+
    theme(axis.text.x = element_text(size=12)) +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5))
  sampleNamesDf = prepData %>%
    group_by() %>%
    select({{sampleCol}}) %>%
    unique() %>%
    arrange({{sampleCol}})
  return (ggplot(prepData) +
            geom_rect(aes(xmin = as.numeric({{targetCol}}) -0.5,
                          xmax = as.numeric({{targetCol}}) +0.5,
                          ymin = as.numeric({{sampleCol}}) + fracModCumSum - 0.5,
                          ymax = as.numeric({{sampleCol}}) + fracModCumSum + relAbundCol_mod - 0.5,
                          fill = popidFracLogColor,
                          "{{sampleCol}}" = {{sampleCol}},
                          "{{popUIDCol}}" = {{popUIDCol}},
                          "{{targetCol}}"= {{targetCol}},
                          "{{relAbundCol}}" = {{relAbundCol}}
            ),
            color = "black") +
            scale_fill_gradientn(colours = colors) +
            scale_y_continuous(breaks = 1:length(sampleNamesDf[[1]]), labels = sampleNamesDf[[1]] ) +
            sofonias_theme +
            theme(axis.text.x = element_blank()) +
            guides(fill = F))
}

#' @export
genRainbowHapPlotObj <-function(prepData, sampleCol = s_Sample, targetCol= p_name, popUIDCol = h_popUID, relAbundCol = c_AveragedFrac, colors = RColorBrewer::brewer.pal(11, "Spectral")){
  genRainbowHapPlotObjActualFracLogColor(prepData, {{sampleCol}}, {{targetCol}}, {{popUIDCol}}, {{relAbundCol}}, colors)
}
