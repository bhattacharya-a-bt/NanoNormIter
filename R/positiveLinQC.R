#### INPUT: rcc - input from rcc (use readRcc from NanoStringQCPro)
#### OUTPUT: flag for linearity for positive controls

positiveLinQC <- function(rcc){

    counts = rcc$Code_Summary
    posControls = as.numeric(counts$Count[grepl('POS_',counts$Name)])
    known = c(128,128/4,128/16,128/64,128/256,128/(256*4))
    r2 = summary(lm(sort(posControls)~sort(known)))$r.squared
    if(!(r2 > .95) | is.na(r2)) {return('Flag')}
    if(r2 > .95) {return('No flag')}

}
