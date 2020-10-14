##### Sample Code for Module 1#####


your_function <- function(y_t,t,maxcap) {
  # Some computation here #
#  check_up = toupper(y_t)
#  check_low = tolower(y_t)
#  clean_index = which(check_up == check_low)
#  y_t = as.numeric(y_t[clean_index])
#  t = as.numeric(t[clean_index])
  y_t = as.numeric(y_t)
  t = as.numeric(t)
  clean_index = which(!y_t%in% NA)
  y_t = as.numeric(y_t[clean_index])
  t = as.numeric(t[clean_index])
  
  n = length(y_t)
  cal = rep(0,n)
  for (i in 2:n){
    cal[i] = abs(y_t[i]-y_t[i-1])
  }
  index=c(which.max(cal):n)
  y_t = y_t[index]
  t = t[index]
  
  y_bar = mean(y_t);t_bar = mean(t)
  y = y_t-y_bar;t = t-t_bar
  slope = sum(y*t)/sum(t^2)
  intercept = y_bar - slope*t_bar
  predicted_time = (0.9*maxcap-intercept)/slope 
  return(predicted_time)
}




### Robustness/Error Tolerance ###
maxcap = 10; y_t = c(1:3,rep(NA,7)); t = 1:10; 
tryCatch(your_function(y_t,t,maxcap),error=function(e) "error")
maxcap = 10; y_t = c(1:3,sample(c(rep("a",5),rep(NA,5)),7)); t = 1:10; 
tryCatch(your_function(y_t,t,maxcap),error=function(e) "error")
# and other examples.

### Accuracy ###
maxcap = 15; y_t = 1:10; t = 1:10; trueTime = 14
# some loss function l(your_function(y_t,t,maxcap),trueTime)
# square-error loss:
(your_function(y_t,t,maxcap) - trueTime)^2
# non-symmetric square-error loss:
if(trueTime >= your_function(y_t,t,maxcap)) {
  (trueTime - your_function(y_t,t,maxcap))^2
} else {
  Inf
}  

### Speed ###
maxcap=2*10^5
y_t = 1:(10^5);  y_t[sample(1:10^5,10)] = NA; y_t[sample(1:10^5,10)] = 0; 
t = 1:(10^5);

codetime = rep(0,10)
for(i in 1:10) {
  start=Sys.time()
  output = your_function(y_t,t,maxcap) 
  end=Sys.time()
  codetime[i] = as.numeric(end-start)
}
mean(codetime)

### Scalability  ###
codetime = 1:6; maxcap = 10^7
for(i in 1:6) {
  start=Sys.time()
  y_t = 1:(10^i); t = 1:(10^i);
  output = your_function(y_t,t,maxcap) 
  end=Sys.time()
  codetime[i] = as.numeric(end-start)
}
plot(1:6,log(codetime,10))

