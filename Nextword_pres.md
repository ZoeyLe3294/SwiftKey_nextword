SwiftKey Next Word Prediction
========================================================
author: Zoey Le
date: September 28, 2018
autosize: true
transition: concave
font-family: 'Arial'
Coursera Capstone Project
Introduction
========================================================
Large databases comprising of text in a target language are commonly used when generating language models for various purposes. In this project, we will explore the major features of the text data given for the Coursera Data Science Capstone through Johns Hopkins University. The project is sponsored by SwiftKey. 

The ultimate purpose of this project is to built a Shiny app that suggest possible words when users type some random sentences.


Project process
========================================================

- Data Exploratory: including data overview and cleaning are presented at [Milestone report](http://rpubs.com/zoeyle/419123)
- Prepare unigram, bigram, trigram and quadgram from the data
- Using back-off model to suggest user top words that likely to appear next
- Build Shiny app 


Back-off model
========================================================

The algorithm will use last few words (up to 3 words) given from the user to match top most combinations frequently  appearing on Prepared data grams (unigram, bigram, trigram and quadgram). In specific, it follows these steps below:

- Step 1: Load and clean the input
- Step 2: Trim the last words (up to 3 words)
- Step 3: Match trimmed words with Quadgram, Trigram, Bigram and Unigram. If 3 words selected then Quadgram data is used. If 2 words selected then Trigram data is used. If 1 words selected then Bigram data is used. If none words selected then Unigram data is used
- Step 4: If Step 2 gives result then send out last words of top 4 matches as the suggestion for the next words
- Step 5: If Step 2 fails, back to step 2 with 1 word reduced from the previous trimming process


Result and limitation
========================================================
Shiny will look like:

![result](Shiny_img.png)

Details can be found in these links:

- [Shiny app]()
- [Github]()

<strong>However:</strong> Since there is a limitation on RAM of the computers, the prediction model is biased toward the train data size. Hence, the result is not as accurate as expected