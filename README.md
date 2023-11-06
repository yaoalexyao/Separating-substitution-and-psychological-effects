# Separating-substitution-and-psychological-effects
Data and code sharing for paper Separating Substitution and Psychological Effects with A Two-step  Conjoint Approach: Application to Luxury Goods

This conjoint data includes 1020 respondents. Each respondent takes one general DCE taks and three driver DCE tasks. Attribute levels and conumer demographics information is porovided in the following documents after being rescaled: brand.csv, color.csv, shape.csv, price.csv, budget.csv, and Demo.csv. Gendral DCE task choices are recorded in "choice.csv", while three driver DCE task choices are recorded in identity.csv, self reward.csv, and quality.csv. 

Step 1 coefficients.csv records the estimated coefficients in Step 1 in the R code. It is also used in the following step to calculate the size and memberships of Segment 1 (wiht positive psychological effect) and Segment 2 (with negative psychological effect).

Step 2 coefficients.csv records the estimated coefficients in Step 2 (for Segment 1) in the R code. It is also used in the following step to calculate the size and memberships of Segment 1a (price prestige seeker) and Segment 1b (self rewarder).

Code sunglasses.R provides estimation code for all three steps.

Sunglasses conjoint design illustration (translatead).doc illustrates a sample conjoint design.
