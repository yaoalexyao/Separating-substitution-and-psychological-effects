# Separating-substitution-and-psychological-effects
Data and code sharing for paper "Separating Substitution and Psychological Effects with A Two-step Conjoint Approach: Application to Luxury Goods"

Data:

This conjoint data includes 1020 respondents. Each respondent takes one general DCE taks and three driver DCE tasks. Product (attribute levels), budget and conumer demographics information is porovided in the following documents after being rescaled: 

	brand.csv (two levels)
	color.csv (two levels)
	shape.csv (three levels)
	price.csv (four levels)
	budget.csv (four levels)
	Demo.csv (marital status, gender, education, income, and age). 

Gendral DCE task choices are recorded in: 

	choice.csv, 
 
Model estimation took three steps: In step 1, we model the total psychological effect separately from the substitution effect. To achieve this, we use the product, budget, consumer, as well as and general DCE task choice information in Part A of our conjoint design, following the model described in Equation (5) in section 3.1 in the paper. The estimated coefficients in Step 1 is recorded in 

	Step 1 coefficients.csv 
 
After establishing the psychological effect, we next model how the three potential drivers (identity match, self-reward, and perceived quality) mediate this effect in Step 2. We used the estimated coefficients in Step 1 to calculate the segment size and memberships of Segment 1 (wiht positive psychological effect) and Segment 2 (with negative psychological effect). Specifically, for each segment, we assessed whether each driver indicator influences the final product choice, using additional data from three driver DCE task choices. 
 
 	identity.csv
	self reward.csv
	quality.csv

"Step 2 coefficients.csv" records the estimated coefficients in Step 2 (for Segment 1). These estimates were used in the following step to calculate the segment size and memberships of sub-segment Segment 1a (price prestige seeker) and Segment 1b (self rewarder). In step 3, we explored the relationship between price and each driver indicator for each sub-segment. 


Code:

"Code sunglasses with notes.R" provides estimation code for all three steps.


Sample conjoint design

"Sunglasses conjoint design illustration (translated).doc" provides a sample conjoint design.
