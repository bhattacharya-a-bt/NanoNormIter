#### INPUT: rcc - input from rcc (use readRcc from NanoStringQCPro)
####         numSD - number of standard deviations to calibrate the LOD
#### OUTPUT: flag for limit of detection

limitOfDetectionQC <- function(rcc,numSD = 0){



    counts = rcc$Code_Summary
    posE = as.numeric(counts$Count[counts$Name == 'POS_E'])
    negControls = as.numeric(counts$Count[grepl('NEG',counts$Name)])
    if(!(posE > mean(negControls) + numSD*sd(negControls))) {return('Flag')}
    if (posE > mean(negControls) + numSD*sd(negControls)) {return('No flag')}

}
