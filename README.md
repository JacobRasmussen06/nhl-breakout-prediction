# nhl-breakout-prediction
**Predicting NHL Breakout Forwards Using Machine Learning Techniques and Advanced Hockey Analytics. **

Each year in the National Hockey League (NHL), there are players denoted as "breakout players", players whose stat line increases dramatically from one season to the next. Some breakouts are anticipated, while others are more difficult to identify. This project develops machine learning models that assigns probability of breakout to NHL forwards, attempting to predict breakout forwards using player performance and analytics from NHL seasons. 

Two XGBoost models were trained and balance different objectives:
- Precision Model: highly precise at the top level of players, accurately assigning high probability to a majority of its top 10 and 20.
- Coverage Model: casts a far wider net, more accurate at the top 50 level while sacrificing some precision at the top 10 level.

**Structure**

code/          Data cleaning, feature engineering, model training

data/          Raw and processed datasets

figures/       Figures used in the report

report/        Quarto report and rendered outputs

