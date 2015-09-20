# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# load the required data table library
library (data.table)
# unzip the data file and read the data
unzipData <- unz('activity.zip', 'activity.csv')
activityData <- read.table(unzipData, header = TRUE, sep = ',')
# convert the data into data table
activityData <- data.table(activityData)
# process the data to create date frame with the total steps group by date
dataSummary <- as.data.frame(activityData[, j=list(total = sum(steps, na.rm = TRUE)),by = date])
```


## What is mean total number of steps taken per day?

```r
#calculate the mean and median of the total steps per day
meanSteps <- as.integer(mean(dataSummary$total))
medianSteps <- median(dataSummary$total)
# histogram of total steps per day
hist(dataSummary$total, xlab='Total Steps', ylab='Frequency', main='Histogram of total steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

- **Mean total number of steps taken per day (rounded to integer) = 9354**
- **Median total number of steps taken per day = 10395**

## What is the average daily activity pattern?

```r
# process the original data to create date frame with the total steps group by interval and the mean
dataSummaryDailyPattern <- as.data.frame(activityData[, j=list(total = sum(steps, na.rm = TRUE), mean = as.integer(mean(steps, na.rm = TRUE))),by = interval])

# Calculate the interval which has the maximum number of steps
maxMean <-(max(dataSummaryDailyPattern$mean, na.rm=TRUE))
maxInterval <-with(dataSummaryDailyPattern, interval[mean == maxMean])
maxInterval <- sprintf("%04d", maxInterval)
maxInterval <- format(strptime(maxInterval, format="%H%M"), format = "%H:%M")

# plot the time series for the interval and mean
plot(dataSummaryDailyPattern$interval, dataSummaryDailyPattern$mean, type='l', xlab = 'Interval', ylab = 'Average steps', main = 'Daily Activity Pattern')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

- **Interval which has the maximum number of steps = 08:35**

## Imputing missing values

```r
# Calculate the total number of NA values in the original data table
totalNA <- sum(is.na(activityData$steps))
# Copy the original dataset
activityDataWithoutNA <- activityData
# Iterate through all the intervals and replace NA values
for (i in 1:nrow(dataSummaryDailyPattern)) {
    activityDataWithoutNA$steps[is.na(activityDataWithoutNA$steps) & activityDataWithoutNA$interval==dataSummaryDailyPattern[i,]$interval] <- dataSummaryDailyPattern[i,]$mean
}
# Create a data frame for the new data set
dataSummaryWithoutNA <- as.data.frame(activityDataWithoutNA[, j=list(total = sum(steps, na.rm = TRUE)),by = date])
#calculate the mean and median of the total steps per day
meanStepsWithoutNA <- as.integer(mean(dataSummaryWithoutNA$total))
medianStepsWithoutNA <- median(dataSummaryWithoutNA$total)
```

- **Strategy for filling the missing values**
    - Replace the missing values with the mean value for that interval across all days
    - Iterate through each interval and replace all the NA values for that interval in the main data set
    - Calculate the mean and median again
- **Total number of missing values in the data set = 2304**
- **Mean total number of steps taken per day, after replacing NA values (rounded to integer) = 10749**
- **Median total number of steps taken per day, after replacing NA values = 10641**
- **Impact of imputting missing data:**
    - Mean increased from 9354 to 10749
    - Median increased from 10395 to 10641


```r
# histogram of total steps per day
hist(dataSummaryWithoutNA$total, xlab='Total Steps', ylab='Frequency', main='Histogram of total steps per day without NA values')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

```r
print(dataSummaryDailyPattern)
```

```
##     interval total mean
## 1          0    91    1
## 2          5    18    0
## 3         10     7    0
## 4         15     8    0
## 5         20     4    0
## 6         25   111    2
## 7         30    28    0
## 8         35    46    0
## 9         40     0    0
## 10        45    78    1
## 11        50    16    0
## 12        55     7    0
## 13       100    17    0
## 14       105    36    0
## 15       110     8    0
## 16       115    18    0
## 17       120     0    0
## 18       125    59    1
## 19       130    97    1
## 20       135     9    0
## 21       140     9    0
## 22       145    20    0
## 23       150    14    0
## 24       155     0    0
## 25       200     0    0
## 26       205     0    0
## 27       210    60    1
## 28       215     0    0
## 29       220     0    0
## 30       225     7    0
## 31       230     0    0
## 32       235    12    0
## 33       240     0    0
## 34       245     0    0
## 35       250    82    1
## 36       255    50    0
## 37       300     0    0
## 38       305     0    0
## 39       310     0    0
## 40       315     0    0
## 41       320    11    0
## 42       325    33    0
## 43       330    86    1
## 44       335    31    0
## 45       340    26    0
## 46       345     4    0
## 47       350     0    0
## 48       355     0    0
## 49       400    63    1
## 50       405    50    0
## 51       410   136    2
## 52       415     0    0
## 53       420    18    0
## 54       425    19    0
## 55       430   218    4
## 56       435    35    0
## 57       440   185    3
## 58       445    44    0
## 59       450   165    3
## 60       455    59    1
## 61       500     0    0
## 62       505    83    1
## 63       510   159    3
## 64       515   119    2
## 65       520   176    3
## 66       525   157    2
## 67       530   111    2
## 68       535   321    6
## 69       540   849   16
## 70       545   972   18
## 71       550  2091   39
## 72       555  2358   44
## 73       600  1669   31
## 74       605  2611   49
## 75       610  2850   53
## 76       615  3363   63
## 77       620  2648   49
## 78       625  2495   47
## 79       630  2764   52
## 80       635  2085   39
## 81       640  2333   44
## 82       645  2341   44
## 83       650  1980   37
## 84       655  2599   49
## 85       700  2322   43
## 86       705  2352   44
## 87       710  2677   50
## 88       715  2889   54
## 89       720  2646   49
## 90       725  2702   50
## 91       730  2951   55
## 92       735  2349   44
## 93       740  2770   52
## 94       745  3686   69
## 95       750  3066   57
## 96       755  2976   56
## 97       800  3889   73
## 98       805  3615   68
## 99       810  6860  129
## 100      815  8349  157
## 101      820  9071  171
## 102      825  8236  155
## 103      830  9397  177
## 104      835 10927  206
## 105      840 10384  195
## 106      845  9517  179
## 107      850  9720  183
## 108      855  8852  167
## 109      900  7603  143
## 110      905  6574  124
## 111      910  5783  109
## 112      915  5730  108
## 113      920  5497  103
## 114      925  5086   95
## 115      930  3509   66
## 116      935  2397   45
## 117      940  1314   24
## 118      945  2054   38
## 119      950  1854   34
## 120      955  1116   21
## 121     1000  2150   40
## 122     1005  1430   26
## 123     1010  2248   42
## 124     1015  2791   52
## 125     1020  2063   38
## 126     1025  2692   50
## 127     1030  2347   44
## 128     1035  1983   37
## 129     1040  1839   34
## 130     1045  1502   28
## 131     1050  1330   25
## 132     1055  1693   31
## 133     1100  1662   31
## 134     1105  1573   29
## 135     1110  1130   21
## 136     1115  1354   25
## 137     1120  1504   28
## 138     1125  1403   26
## 139     1130  1772   33
## 140     1135  2649   49
## 141     1140  2228   42
## 142     1145  2364   44
## 143     1150  2440   46
## 144     1155  3137   59
## 145     1200  3385   63
## 146     1205  4648   87
## 147     1210  5027   94
## 148     1215  4917   92
## 149     1220  3360   63
## 150     1225  2659   50
## 151     1230  2887   54
## 152     1235  1718   32
## 153     1240  1406   26
## 154     1245  2000   37
## 155     1250  2388   45
## 156     1255  3566   67
## 157     1300  2244   42
## 158     1305  2114   39
## 159     1310  2293   43
## 160     1315  2172   40
## 161     1320  2451   46
## 162     1325  2991   56
## 163     1330  2266   42
## 164     1335  1332   25
## 165     1340  2118   39
## 166     1345  2838   53
## 167     1350  2508   47
## 168     1355  3223   60
## 169     1400  2955   55
## 170     1405  2754   51
## 171     1410  2310   43
## 172     1415  2581   48
## 173     1420  1880   35
## 174     1425  1990   37
## 175     1430  2218   41
## 176     1435  1458   27
## 177     1440   907   17
## 178     1445  1382   26
## 179     1450  2312   43
## 180     1455  2320   43
## 181     1500  1591   30
## 182     1505  1912   36
## 183     1510  1881   35
## 184     1515  2059   38
## 185     1520  2436   45
## 186     1525  2531   47
## 187     1530  2551   48
## 188     1535  3462   65
## 189     1540  4394   82
## 190     1545  5229   98
## 191     1550  5412  102
## 192     1555  4450   83
## 193     1600  3293   62
## 194     1605  3399   64
## 195     1610  3951   74
## 196     1615  3348   63
## 197     1620  3016   56
## 198     1625  3168   59
## 199     1630  2325   43
## 200     1635  2044   38
## 201     1640  2367   44
## 202     1645  2409   45
## 203     1650  2449   46
## 204     1655  2315   43
## 205     1700  2471   46
## 206     1705  2984   56
## 207     1710  2688   50
## 208     1715  3245   61
## 209     1720  3854   72
## 210     1725  4184   78
## 211     1730  3654   68
## 212     1735  3162   59
## 213     1740  3980   75
## 214     1745  2995   56
## 215     1750  1843   34
## 216     1755  1985   37
## 217     1800  2156   40
## 218     1805  3075   58
## 219     1810  3959   74
## 220     1815  4522   85
## 221     1820  3141   59
## 222     1825  3592   67
## 223     1830  4118   77
## 224     1835  3935   74
## 225     1840  4523   85
## 226     1845  5271   99
## 227     1850  4589   86
## 228     1855  4537   85
## 229     1900  4498   84
## 230     1905  4125   77
## 231     1910  3076   58
## 232     1915  2828   53
## 233     1920  1925   36
## 234     1925  1098   20
## 235     1930  1452   27
## 236     1935  2121   40
## 237     1940  1601   30
## 238     1945  1354   25
## 239     1950  2420   45
## 240     1955  1777   33
## 241     2000  1040   19
## 242     2005  1008   19
## 243     2010  1025   19
## 244     2015  1767   33
## 245     2020  1421   26
## 246     2025  1122   21
## 247     2030  1447   27
## 248     2035  1131   21
## 249     2040  1036   19
## 250     2045  1130   21
## 251     2050  1712   32
## 252     2055  1068   20
## 253     2100   845   15
## 254     2105   913   17
## 255     2110  1243   23
## 256     2115  1020   19
## 257     2120   660   12
## 258     2125   425    8
## 259     2130   777   14
## 260     2135   864   16
## 261     2140   460    8
## 262     2145   413    7
## 263     2150   431    8
## 264     2155   139    2
## 265     2200    77    1
## 266     2205   195    3
## 267     2210   255    4
## 268     2215   451    8
## 269     2220   375    7
## 270     2225   461    8
## 271     2230   517    9
## 272     2235   117    2
## 273     2240    17    0
## 274     2245     6    0
## 275     2250    85    1
## 276     2255   244    4
## 277     2300   175    3
## 278     2305   151    2
## 279     2310     0    0
## 280     2315    44    0
## 281     2320    51    0
## 282     2325    84    1
## 283     2330   138    2
## 284     2335   249    4
## 285     2340   175    3
## 286     2345    34    0
## 287     2350    12    0
## 288     2355    57    1
```

```r
print(dataSummary)
```

```
##          date total
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01     0
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04     0
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09     0
## 41 2012-11-10     0
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14     0
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30     0
```

```r
print(dataSummaryWithoutNA)
```

```
##          date total
## 1  2012-10-01 10641
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08 10641
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01 10641
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04 10641
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09 10641
## 41 2012-11-10 10641
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14 10641
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30 10641
```

```r
print(sum(activityDataWithoutNA$steps[activityDataWithoutNA$date=='2012-10-02']))
```

```
## [1] 126
```
