# wide- long-format data

#http://seananderson.ca/2013/10/19/reshape.html

require(reshape2)

# wide-format
#   ozone   wind  temp
# 1 23.62 11.623 65.55
# 2 29.44 10.267 79.10
# 3 59.12  8.942 83.90
# 4 59.96  8.794 83.97

# long-format
#    variable  value
# 1     ozone 23.615
# 2     ozone 29.444
# 3     ozone 59.115
# 4     ozone 59.962
# 5      wind 11.623
# 6      wind 10.267
# 7      wind  8.942
# 8      wind  8.794
# 9      temp 65.548
# 10     temp 79.100
# 11     temp 83.903
# 12     temp 83.968

# melt: wide -> long
# cast: long -> wide

names(airquality) <- tolower(names(airquality))
head(airquality)

aql <- melt(airquality, id.vars = c("month", "day")) # [a]ir [q]uality [l]ong format
head(aql)

aqw <- dcast(aql, month + day ~ variable) # can be added value.var = "foreground_median" if set value should be other than variable
head(aqw)
