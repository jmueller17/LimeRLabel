---
output:
  pdf_document: default
  html_document: default
---
# LimeRLabel

LimeRLabel faciliates importing and labelling LimeSurvey result data into R. 

Getting data into R from LimeSurvey is still a rather cumbersume process, especially when questionnairs have multiple language versions. Currently two options are available. First, users can export manually two files (an R syntax file and the CSV data file) from within LimeSurvey as long as the pre-installed "ExportR" plugin is enabled. Both files need then to be combined manually from within R in order to produce a data frame with variable lables. In case of a survey conducted in several languages, this methods breaks down as the ExportR plugin has a [bug](https://bugs.limesurvey.org/view.php?id=16626). The exported labels always use the default language of the questionnaire no matter which other language option has been selected during the export settings dialog. Generating reports in any other language than the default one does not work. 

Second, the Remote API for LimeSurvey offers the possibility to directly download the result dataset into R via the [limer](https://github.com/cloudyr/limer) or [LimeRick](https://github.com/k127/LimeRick) package. Although the remote api call can specify the language of the dataset, the downloaded CSV only contains the variable lables of actual responses and not all possible answer choices. The Remote API does not offer the possibility to download the R Syntax file.

The LimeRLabel package addesses these issues. It produces a complete labelled R data frame in any of the available languages of the questionnaire. It does so by combining the LimeSurvey Survey Structure file (*.lss) archive with the CSV data downloaded either manually or via the Remove API calls. A correctly labelled data frame is key to produce efficiently the corresponding graphics and frequency table in any of the available languages. 


## Installation

The LimeRLabel package can be downloaded and installed from Github with: 

```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jmueller17/LimeRLabel")
```
  

## Example usage

### Export LimeSurvey Survey Structure file 
Export the Survey Structure (Display/export > Survey Structure \*.lss file) and download it to your computer. The *.lss file contains a complete archive of your questionnaire without any response data. It provides a convenient form to export and import (or copy) questionnaires in LimeSurvey. It contains the complete question labels and answer lables in all languages. 


### Export Survey Result Data 
Manually export the result data from LimeSurvey. Make sure the ExportR plugin is activated. Responses as well as questions should be exported as **code only** (not full answer or question texts).

OR

Download result data set with the LimeRick package directly into R (see [instructions](https://github.com/k127/LimeRick)). Be sure the Remote API is activated in your LimeSurvey installation. Again, variable names and responses should contain the internal LimeSurvey **code only** and not the full text. For example: 

```
# ... 
# LimeRick session key, etc. 

# Your Limesurvey Survey ID
lsSurveyID <- 477469

# download data
df.lsdata <- lsExportResponses(lsSurveyID, 
                                completionStatus = "all", 
                                headingType = "code", 
                                responseType="short", 
                                lang="en")

```


### Combine data with labels

First, read and extract all question and sub-question lables with the ``extract_question_labels()` function.  

```
question_labels <- extract_question_labels("path/to/limesurvey_survey_xxxxx.lss")
```
... and assign the question lables to variables in the result data frame with `set_question_labels()`

```
df.lsdata <- set_question_labels(df.lsdata, question_labels, "en")
```

This attaches to each column (variable) a "label" attribute that contains the question text in the specified language. In case a variable represents a sub-question (as used in matrix questions for example where respondents rate specific answer items), the "label" attribute returns the sub-question text. This makese sense for producing correctly labelled likert polts or tables  for example. 

Second, read and extract answer labels with `extract_response_labels()`

```
response_labels <- extract_response_labels("path/to/limesurvey_survey_xxxxx.lss")
```

... and assign it to the data frame with `set_response_labels()`

```
df.lsdata <- set_response_labels(df.lsdata, response_labels, "en")
```

This will produce factor variables for most question types and convert response codes into correctly labelled factor levels.  Numeric input, date and different free text input variables will be ignored. 

The data is now correctly labelled and producing plots or tables should work smoothly and produce the correct labels. Question texts can be easily retrieved with `get_label()` and `get_parent_label()` functions. 

The `levels()` function will display all answer lables for a given variable. 


### Switching between language version

In case questions and answer labels have been created in different languages, it is easy to switch the language of the result data frame. Simply repeat the calls to `set_response_labels()` and `set_question_labels()` with the alternate language code

```
df.lsdata <- set_question_labels(df.lsdata, question_labels, "de")
df.lsdata <- set_response_labels(df.lsdata, response_labels, "de")
```


### Save data frame to disk

The LimeRLabel package contains utility functions to write and read the labelled data frame as an *encrypted* file to disk. This requires of course that a valid GPG key is available. If available and data has been downloaded via the Remote API, no unencrypted copies of the result data are necessary on disk.  








