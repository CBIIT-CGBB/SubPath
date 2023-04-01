
do.scale <- function(x, newMin, newMax){
  (x - min(x))/(max(x) - min(x)) * (newMax - newMin) + newMin
}