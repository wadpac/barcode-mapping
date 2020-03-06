Code base written by Xinhui Wang, which we are revising.

To use this do:

library(acc)
library(pawacc)
library(foreign)

path_input=getwd() # root of this repository

dirR = "/home/vincent/projects/annelinde/Sequence_Mapping_Rcode/scripts"
for (i in dir(dirR, full.names = T)) source(i)

file_list<- list.files("./input/", pattern ="*.csv", all.files = FALSE)

barcoding_main_last(file_list)
