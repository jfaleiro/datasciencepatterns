# Markov Chains: Predictive Model for Next Typed Word
J Faleiro  
July 15, 2016  

## Understanding the Problem and Getting the Data

# Required libraries


```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ngram, ggplot2, gridExtra, caret, feather)
```

Downloading the data


```r
url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
file <- 'training.zip'
download.file(url, destfile=file, method='curl')
unzip(file)
```

Size in Mb of blogs?


```r
system('ls -lh final/en_US/en_US.blogs.txt', intern=TRUE)
```

```
## [1] "-rw-r--r--  1 jfaleiro  staff   200M Jul 15 18:57 final/en_US/en_US.blogs.txt"
```

How many lines in each of the en_US dataset?


```r
files <- list.files(recursive=TRUE, pattern='en_US.*.txt')
descriptions <- sapply(files, function(f) {
    print(f)
    sizeMb <- file.info(f)[1]/1024/1024
    lines <- readLines(f)
    maxLineLength <- which.max(lapply(lines, nchar))
    wordCount <- sum(sapply(strsplit(lines, '\\s+'), length))
    c(name=f, sizeMB=sizeMb, lines=length(lines), maxLineLength=maxLineLength, words=wordCount)
})
```

```
## [1] "final/en_US/en_US.blogs.txt"
## [1] "final/en_US/en_US.news.txt"
## [1] "final/en_US/en_US.twitter.txt"
```

```
## Warning in readLines(f): line 167155 appears to contain an embedded nul
```

```
## Warning in readLines(f): line 268547 appears to contain an embedded nul
```

```
## Warning in readLines(f): line 1274086 appears to contain an embedded nul
```

```
## Warning in readLines(f): line 1759032 appears to contain an embedded nul
```


```r
descriptions
```

```
##               final/en_US/en_US.blogs.txt   final/en_US/en_US.news.txt  
## name          "final/en_US/en_US.blogs.txt" "final/en_US/en_US.news.txt"
## sizeMB.size   200.4242                      196.2775                    
## lines         899288                        1010242                     
## maxLineLength 483415                        123628                      
## words         37334149                      34372814                    
##               final/en_US/en_US.twitter.txt  
## name          "final/en_US/en_US.twitter.txt"
## sizeMB.size   159.3641                       
## lines         2360148                        
## maxLineLength 26                             
## words         30373565
```


```r
df <- data.frame(descriptions)
df
```

```
##               final.en_US.en_US.blogs.txt final.en_US.en_US.news.txt
## name          final/en_US/en_US.blogs.txt final/en_US/en_US.news.txt
## sizeMB.size                      200.4242                   196.2775
## lines                              899288                    1010242
## maxLineLength                      483415                     123628
## words                            37334149                   34372814
##               final.en_US.en_US.twitter.txt
## name          final/en_US/en_US.twitter.txt
## sizeMB.size                        159.3641
## lines                               2360148
## maxLineLength                            26
## words                              30373565
```

Reading twitter US:


```r
twitter <- readLines('final/en_US/en_US.twitter.txt')
```

```
## Warning in readLines("final/en_US/en_US.twitter.txt"): line 167155 appears
## to contain an embedded nul
```

```
## Warning in readLines("final/en_US/en_US.twitter.txt"): line 268547 appears
## to contain an embedded nul
```

```
## Warning in readLines("final/en_US/en_US.twitter.txt"): line 1274086 appears
## to contain an embedded nul
```

```
## Warning in readLines("final/en_US/en_US.twitter.txt"): line 1759032 appears
## to contain an embedded nul
```

What is rate of love to hate mentions in twitter?


```r
loveCount <- length(grep('love', twitter))
hateCount <- length(grep('hate', twitter))
loveCount / hateCount
```

```
## [1] 4.108592
```

Where is the word 'biostat' mentioned in twitter?


```r
twitter[grep('biostat', twitter)]
```

```
## [1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"
```

How many times a specific sentence is mentioned in twitter?


```r
length(grep('A computer once beat me at chess, but it was no match for me at kickboxing', twitter))
```

```
## [1] 3
```

Read all blogs in US english:


```r
blogs <- readLines('final/en_US/en_US.blogs.txt')
```

Read all news in US english:


```r
news <- readLines('final/en_US/en_US.news.txt')
```

What are the longest lines in chars in each of the feeds?


```r
which.max(lapply(twitter, nchar))
```

```
## [1] 26
```

```r
which.max(lapply(blogs, nchar))
```

```
## [1] 483415
```

```r
which.max(lapply(news, nchar))
```

```
## [1] 123628
```

## Using the Data

Random sample news data by 10%:


```r
length(news)
```

```
## [1] 1010242
```

```r
set.seed(123)
smallNews <- sample(news, size=length(news)*0.10)
length(smallNews)
```

```
## [1] 101024
```


```r
length(blogs)
```

```
## [1] 899288
```

```r
set.seed(123)
smallBlogs <- sample(news, size=length(blogs)*0.10)
length(smallBlogs)
```

```
## [1] 89928
```


```r
length(twitter)
```

```
## [1] 2360148
```

```r
set.seed(123)
smallTwitter <- sample(news, size=length(twitter)*0.10)
length(smallTwitter)
```

```
## [1] 236014
```

Creating n-grams - good explanation at https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf


```r
nw <- paste0(paste(smallNews, collapse='/n'),
             paste(smallBlogs, collapse='/n'),
             paste(smallTwitter, collapse='/n')
)
nw1 <- ngram(nw, n=1)
nw2 <- ngram(nw, n=2)
nw3 <- ngram(nw, n=3)
nw4 <- ngram(nw, n=4)
nw.pt.1 <- get.phrasetable(nw1)
nw.pt.2 <- get.phrasetable(nw2)
nw.pt.3 <- get.phrasetable(nw3)
nw.pt.4 <- get.phrasetable(nw4)
```


```r
write_feather(nw.pt.1, 'ngrams-1.feather')
write_feather(nw.pt.2, 'ngrams-2.feather')
write_feather(nw.pt.3, 'ngrams-3.feather')
write_feather(nw.pt.4, 'ngrams-4.feather')
```


```r
saveRDS(nw.pt.1, 'ngrams-1.rds')
saveRDS(nw.pt.2, 'ngrams-2.rds')
saveRDS(nw.pt.3, 'ngrams-3.rds')
saveRDS(nw.pt.4, 'ngrams-4.rds')
```


```r
nrow(nw.pt.1)
```

```
## [1] 538023
```

```r
nrow(nw.pt.2)
```

```
## [1] 3144787
```

```r
nrow(nw.pt.3)
```

```
## [1] 6025737
```

```r
nrow(nw.pt.4)
```

```
## [1] 7321265
```

```r
grid.arrange(tableGrob(head(nw.pt.1)), 
             tableGrob(head(nw.pt.2)), 
             tableGrob(head(nw.pt.3)),
             tableGrob(head(nw.pt.4)),
             ncol=2)
```

![](index_files/figure-html/shows.ngrams.news.us-1.png)<!-- -->

## Predicting Words




```r
p <- predictNextWord('I','th')
```

```
## [1] "re= ^I th"
## [1] "nrow= 5"
```

```r
ggplot(p, aes(x=ngrams, y=prop)) +
    theme_light() +
    geom_bar(stat='identity')
```

![](index_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# hack to order X by value of Y
p$ngrams <- factor(p$ngrams, levels=p$ngrams[order(-p$prop)])
ggplot(p, aes(x=ngrams, y=prop)) +
    theme_light() +
    geom_bar(stat='identity') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
```

![](index_files/figure-html/unnamed-chunk-6-2.png)<!-- -->


```r
sapply(c(1,23), function(x) {x+1})
```

```
## [1]  2 24
```

```r
predictNextWord('','')
```

```
## [1] "re= ^"
## [1] "nrow= 5"
```

```
##   ngrams   freq       prop
## 1   the  724664 0.05140058
## 2    to  375286 0.02661912
## 3   and  357295 0.02534301
## 4     a  348106 0.02469124
## 5    of  322759 0.02289337
```

```r
predictNextWord('','the')
```

```
## [1] "re= ^the"
## [1] "nrow= 5"
```

```
##    ngrams   freq         prop
## 1    the  724664 0.0514005775
## 34  they   36847 0.0026135658
## 36 their   34908 0.0024760321
## 86 there   13168 0.0009340091
## 92  them   12590 0.0008930115
```

```r
predictNextWord('I','')
```

```
## [1] "re= ^I "
## [1] "nrow= 5"
```

```
##       ngrams freq         prop
## 103   I was  3242 2.299558e-04
## 140 I think  2547 1.806593e-04
## 194  I have  2105 1.493081e-04
## 225 I don't  1911 1.355477e-04
## 375   I can  1351 9.582674e-05
```

```r
predictNextWord('I','th')
```

```
## [1] "re= ^I th"
## [1] "nrow= 5"
```

```
##             ngrams freq         prop
## 140       I think  2547 1.806593e-04
## 1162    I thought   583 4.135232e-05
## 12390  I thought,    91 6.454651e-06
## 13610    I think,    84 5.958139e-06
## 41391 I thought,"    31 2.198837e-06
```

```r
predictNextWord('I will', '')
```

```
## [1] "re= ^I will "
## [1] "nrow= 5"
```

```
##               ngrams freq         prop
## 4944      I will be    63 4.468605e-06
## 8511     I will not    45 3.191860e-06
## 12815  I will never    34 2.411628e-06
## 28966 I will always    19 1.347674e-06
## 33389   I will have    18 1.276744e-06
```

```r
predictNextWord('I   \twill', '')
```

```
## [1] "re= ^I will "
## [1] "nrow= 5"
```

```
##               ngrams freq         prop
## 4944      I will be    63 4.468605e-06
## 8511     I will not    45 3.191860e-06
## 12815  I will never    34 2.411628e-06
## 28966 I will always    19 1.347674e-06
## 33389   I will have    18 1.276744e-06
```

```r
predictNextWord('I   will', 'pr')
```

```
## [1] "re= ^I will pr"
## [1] "nrow= 1"
## [1] "re= ^will pr"
## [1] "nrow= 5"
```

```
##                   ngrams freq         prop
## 3527064 I will probably     1 7.093023e-08
## 4864       will provide   198 1.404418e-05
## 5104      will probably   190 1.347674e-05
## 8685       will present   123 8.724418e-06
## 29641      will produce    42 2.979069e-06
```


```r
predictNextWord('The guy in front of me just bought a pound of bacon, a bouquet, and a case of', '')
```

```
## [1] "re= ^a case of "
## [1] "nrow= 5"
```

```
##                      ngrams freq         prop
## 500362       a case of too     3 2.127907e-07
## 508548   a case of letting     3 2.127907e-07
## 970408     a case of teary     3 2.127907e-07
## 1032298 a case of thallium     3 2.127907e-07
## 1123201     a case of acid     3 2.127907e-07
```

```r
predictNextWord("You're the reason why I smile everyday. Can you follow me please? It would mean the", '')
```

```
## [1] "re= ^would mean the "
## [1] "nrow= 5"
```

```
##                               ngrams freq         prop
## 1374854 would mean the displacement     3 2.127907e-07
## 2383572       would mean the school     3 2.127907e-07
## 3739146        would mean the board     1 7.093024e-08
## 5083643      would mean the poorest     1 7.093024e-08
## 5445197          would mean the new     1 7.093024e-08
```


```r
sentences <- list(
    c('The guy in front of me just bought a pound of bacon, a bouquet, and a case of', ''),
    c("You're the reason why I smile everyday. Can you follow me please? It would mean the", '')
)
results <- sapply(sentences, function(x) {predictNextWord(x[1], x[2])[[1]]})
```

```
## [1] "re= ^a case of "
## [1] "nrow= 5"
## [1] "re= ^would mean the "
## [1] "nrow= 5"
```

```r
results
```

```
##      [,1]                  [,2]                          
## [1,] "a case of too "      "would mean the displacement "
## [2,] "a case of letting "  "would mean the school "      
## [3,] "a case of teary "    "would mean the board "       
## [4,] "a case of thallium " "would mean the poorest "     
## [5,] "a case of acid "     "would mean the new "
```
