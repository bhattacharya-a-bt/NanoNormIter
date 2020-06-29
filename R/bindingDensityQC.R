#### INPUT: rcc - input from rcc (use readRcc from NanoStringQCPro)
####         low, high - the lower and upper limits for binding density
#### OUTPUT: flag for binding density

bindingDensityQC <- function(rcc,low,high){

    bd = as.numeric(rcc$Lane_Attributes[6])
    if(!(bd < high & bd > low)) {return('Flag')}
    if (bd < high & bd > low) {return('No flag')}


}
