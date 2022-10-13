## Lesson 7 - Thursday 10/13/22

* Our first step for today is to read in last week's dataset:

```r
df <- read.csv(file="minn.txt",sep=",",header=T)
head(df,n=10)
tail(df,n=10)
```

* Here is the output:

```rout
> df <- read.csv(file="minn.txt",sep=",",header=T)
> head(df,n=10)
   id ta td aggcirc y
1   1  1  1       1 1
2   2  1  1       1 1
3   3  1  1       1 1
4   4  1  1       1 1
5   5  1  1       1 1
6   6  1  1       1 1
7   7  1  1       1 1
8   8  1  1       1 0
9   9  1  1       1 0
10 10  1  1       1 0
> tail(df,n=10)
     id ta td aggcirc y
304 304  3  3       0 0
305 305  3  3       0 0
306 306  3  3       0 0
307 307  3  3       0 0
308 308  3  3       0 0
309 309  3  3       0 0
310 310  3  3       0 0
311 311  3  3       0 0
312 312  3  3       0 0
313 313  3  3       0 0
> 
```
