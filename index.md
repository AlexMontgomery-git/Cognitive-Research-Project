---
title: "Codebook"
author: "Alex Montgomery"
date: "22/08/2021"
output:
  html_document:
    keep_md: yes
---

## Project Description
This project examined the relationship between Overwatch proficiency and executive functions

## Collection of the raw data
Executive function data was downloaded from tatool.web and the Overwatch proficiency data and demographic data was downloaded from a Qualtrics survey.

## Executive function data

### RT Interference
- This continuous variable was calculated by by subtracting RTs in the congruent trials from RTs in the incongruent trials and dividing the result by the congruent RTs. Higher RT interference scores indicate worse performance on the task and consequently a worse inhibitory ability.
- Variable can take the value of any number i.e. -infinity to infinity (4 dp). 

##### Congurent
- This continuous variable is the average reaction time to respond to congruent trials.
- Variable can take any value from 0 to infinity (4 dp).

##### Incongurent
- This continuous variable is the average reaction time to respond to incongruent trials.
- Variable can take any value from 0 to infinity (4 dp).

### Switch costs
- This continuous variable was calculated by subtracting repetition RTs from switch RTs and dividing the result by average RTs. Higher switch cost scores indicate worse performance on the task and consequently a worse shifting ability.
- Variable can take the value of any number i.e. -infinity to infinity (5 dp).

##### Repetition RT
- This continuous variable is the average reaction time to respond to stimuli with the same classification as the last stimuli i.e. either shape or colour.
- Variable can take any value from 0 to infinity (4 dp).

##### Switch RT
- This continuous variable is the average reaction time to respond to stimuli with a different classificationfrom the last stimuli i.e. either shape or colour.
- Variable can take any value from 0 to infinity (4 dp).

### Updating accuracy
- This continuous variable was calculated by dividing the number of correctly solved items by the total number of items. Higher updating accuracy scores indicate better performance on the task and consequently a better updating ability.
- Variable can take any value from 0 to 1 (2 dp).

## Overwatch proficiency data
 
### Current SR
- This continuous variable represents the current skill rating of the participants at the time of completing the experiment
- Variable can be any integer from 500 to 5000

### Peak SR
- This continuous variable represents the peak skill rating a participant has reached during their time playing Overwatch.
- Variable can be any integer from 500 to 5000

### Total Hours
- This continuous variable represents the total number of hours a participant has spent playing Overwatch.
- Variable can be any integer from 0 to 100000.

### Weekly Hours
- This continuous variable represents the number of hours a participant estimates they spend playing Overwatch each week.
- Variable can be any value from 0 to 100000 (1 dp).

### Level
- This categorical variable represents whether a participants regards themselves as a casual or proffesional Overwatch player.
- Variable can either be 'casual', 'professional' or 'prefer not to say'.

### League
- This categorical variable represents whether a participants has played in an Overwatch league or not.
- Variable can either be 'yes', 'no' or 'prefer not to say'.

### Class
- This categorical variable represents what Overwatch class a participants mostly plays.
- Variable can either be 'damage', 'support', 'tank' or 'prefer not to say'.

## Demographic data

### Age
- This continuous variable represents the age of the participant at the time of completing the experiment.
- Variable can be any integer from 18 to 100.

### SES
- This nominal variable represents the self-reported social economic status of the participant at the time of completing the experiment.
- Variable can be any integer from 1 to 10.

### Education
- This categorical variable represents the level of education of the participant at the time of completing the experiment.
- Variable can be 'no schooling completed', 'less than a high school diploma (GCSEs)', 'high school degree or equivalent (A levels)', 'bachelor's degree', 'master's degree', 'doctorate degree', or 'prefer not to say'

### Education code
- This nominal data has been transformed from the education variable to transform qualitative data into quantitative data.
- Variable can be any integer from 1 to 6.

### Gender
- This categorical variable represents the gender of the participant.
- Variable can be 'male', 'female', 'other', or 'prefer not to say'.

### Country
- This qualitative variable represents the country of residence of the participant.
- Variable is a text box so it can contain any combination of letters.

### Race
- This qualitative variable represents the race of the participant.
- Variable is a text box so it can contain any combination of letters.

## Z transformed variables

### zCSR
- This variable is the Z transformation of current SR

### zAGE
- This variable is the Z transformation of age

### zSES
- This variable is the Z transformation of SES

### zEDU
- This variable is the Z transformation of education code

### zTH
- This variable is the Z transformation of total hours

### zWH
- This variable is the Z transformation of weekly hours 
