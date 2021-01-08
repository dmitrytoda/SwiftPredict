# SwiftPredict

This is a ngram model to predict the next word in English text based on some history words. To understand fully how it works, read the report in **main.html**. To see a working POC web app, go to https://dmitrytoda.shinyapps.io/SwiftPredict/

All the code comes as separate helper functions, one per file, that are described below.

## Building the model

These functions are needed to build the model from data set:

* `createSample()` randomly chooses some lines from an input text file to create a smaller size sample for exploratory analysis
* `file2sentences()` reads text from file(s) and returns a `quanteda::corpus` object split into one-sentece documents
* `str2tokens()` splits each (one-sentece) text into tokens (words) with the following preprocessing:

        - remove puncuation
        - remove special symbols
        - remove numbers
        - remove URLs
        - remove tokens that contain no letters
        - remove tokens that contain non-English characters

*  `nFreq()` builds a ngram frequency table for a given `quanteda::tokens` object (that is, calculates how many times every given ngram is observed)
* `removeOOD()` receives a ngram frequency table, replaces out-of-dictionary words with a special "<UNK>" token and recalculates frequencies by collapsing equivalent ngrams
* `keep3()` keeps only top-3 predictions for each possible history and replaces integer ngram frequencies with factor prediction ranks (1, 2, 3)

## Using the model

Once you have a prepared model, only two functions are needed to use it:

* `combined_predict()` makes predictions based on some history, a ngram model and a dictonary
* `my_cond()` is a helper function to create `data.table`-compatible conditions for fast ngram binary search

## Web app

Shiny_app folder contains all the code for a POC Shiny web app that allows you to input any text and get a prediction along with ngrams that contributed to it.

* server.R and UI.R contain server and UI code respectively
* model20_with_dict contains the 20,000 word dictionary and 1- to 6-gram model itself
* other files are just copied from the root folder for deployment to Shiny servers

## Legacy files

* `_shrink model.R` and `collapse_ngrams.R` were used to collapse the initial big 50k word dictionary model to the current 20k one