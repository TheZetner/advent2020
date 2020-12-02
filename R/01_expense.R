# Day 1
# 20201201

# Libraries

library(purrr)
library(dplyr)


# Get Data ####

# Apparently HTTPS is stymieing my normal readLines() from "https://adventofcode.com/2020/day/1/input"
# Do this horseshit instead
# datapasta::df_paste()
i <- c(1322L,1211L,1427L,1428L,1953L,1220L,1629L,1186L,1354L,1776L,1906L,1849L,1327L,1423L,401L,1806L,1239L,1934L,1256L,1223L,1504L,1365L,1653L,1706L,1465L,1810L,1089L,1447L,1983L,1505L,1763L,1590L,1843L,1534L,1886L,1842L,1878L,1785L,1121L,1857L,1496L,1696L,1863L,1944L,1692L,1255L,1572L,1767L,1509L,1845L,1479L,1935L,1507L,1852L,1193L,1797L,1573L,1317L,1266L,1707L,1819L,925L,1976L,1908L,1571L,1646L,1625L,1719L,1980L,1970L,1566L,1679L,1484L,1818L,1985L,1794L,1699L,1530L,1645L,370L,1658L,1345L,1730L,1340L,1281L,1722L,1623L,1148L,1545L,1728L,1325L,1164L,1462L,1893L,1736L,160L,1543L,1371L,1930L,1162L,2010L,1302L,1967L,1889L,1547L,1335L,1416L,1359L,1622L,1682L,1701L,1939L,1697L,1436L,1367L,1119L,1741L,1466L,1997L,1856L,1824L,1323L,1478L,1963L,1832L,1748L,1260L,1244L,1834L,1990L,1567L,1147L,1588L,1694L,1487L,1151L,1347L,1315L,1502L,546L,730L,1742L,1869L,1277L,1224L,1169L,1708L,1661L,174L,1207L,1801L,1880L,1390L,1747L,1215L,1684L,1498L,1965L,1933L,1693L,1129L,1578L,1189L,1251L,1727L,1440L,1178L,746L,1564L,944L,1822L,1225L,1523L,1575L,1185L,37L,1866L,1766L,1737L,1800L,1633L,1796L,1161L,1932L,1583L,1395L,1288L,1991L,229L,1875L,1540L,1876L,1191L,1858L,1713L,1725L,1955L,1250L,1987L,1724L)


# Part A ####

purrr::walk(i, function(x){
  z <- x + i
  if(2020 %in% z){
    m <- i[match(2020, z)]
    print(paste("Found", x, "and", m, "which multiply to", x*m))
  }
  })


# Part B ####

# This could probably be refactored into one step. Meh.

# First create a list of vectors summing up each element of i with each other
l <- purrr::map(i, ~ .x + i)

# Subtract those sums from 2020: the difference is the remaining element. Find that in i, discard the rest. Get Product
purrr::map(l, function(x){
  z <- 2020 - x
  if(any(c(z) %in% i)){
    m <- x[match(z, i)] - i[match(z, i)]
    m[!is.na(m)]
  }
}) %>%
  purrr::discard(is.null) %>%
  unlist() %>%
  unique() %>%
  cumprod()







