#### INPUT: rcc - input from rcc (use readRcc from NanoStringQCPro)
#### OUTPUT: flag for imaging quality
imagingQC <- function(rcc){

    fovRatio = as.numeric(rcc$Lane_Attributes[3]) / as.numeric(rcc$Lane_Attributes[2])
    if (!(fovRatio > .75)) {return('Flag')}
    if (fovRatio > .75) {return('No flag')}

}
