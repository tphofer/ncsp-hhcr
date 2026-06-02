di "Welcome to the Stata environment for Chapter 1 - Bistatistics 1 _n  You are now ready to run all the examples and problems in this chapter"

cap prog drop ex_sample
prog define ex_sample
clear
qui set obs 5600000
set seed 39392
qui drawnorm sbp ,mean(122) sd(19)
di _n "This data is from a census of Massachusetts where every adult had a blood pressure measured"
sum
end

exit
cap prog drop 
prog define 


end