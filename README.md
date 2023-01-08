PoQIT: Big Data Bowl Competition 2023 Submission 

https://www.kaggle.com/code/sethlanza/poqit-big-data-bowl-2023-submission

-Developed a new metric (PoQIT: Pocket Quality Iteration) to analyze the pocket established by the offensive line during a play using tracking data provided by the NFL.

![LV test](https://user-images.githubusercontent.com/81538390/211197022-9502d1b6-b844-46c7-8d67-1518747fd305.gif)

![NE test](https://user-images.githubusercontent.com/81538390/211197038-50733a3e-ed4a-4a23-9ca6-5a133def28b6.gif)

![Area during the Play Cap](https://user-images.githubusercontent.com/81538390/211197054-8166b157-5428-42e1-b6c4-e1be62499746.png)

![PoQIT Density Plot cap](https://user-images.githubusercontent.com/81538390/211197046-cafd4a43-2ebc-4e7c-9d04-7a2fd9042eae.png)


-Combined multiple sources of data including highly-granular, complex tracking data during the ETL process.

-Completed Feature Engineering to create a series of new features that capture some more complexity of football.

-Trained both XGBOOST (eXtreme Gradient Boosting) and BART (Bayesian Additive Regression Trees) models on the PoQIT metric.

![FIXED XGBOOST Feature Importance Plot ALL DATA cap](https://user-images.githubusercontent.com/81538390/211197007-b9b0180d-97ba-44f3-9671-4e070676d982.png)

![FIXED BART Model Feature Importance ALL DATA Cap](https://user-images.githubusercontent.com/81538390/211197014-293ca3d5-1e05-4076-afbb-bdd5a9a3dc4b.png)


-Conducted a Network Shortest-Path Analysis to see which positions had the shortest Time to Close Distance between two nodes.

-Compared the Feature Importance from both models to develop a list of prescriptive steps for an NFL team to enact.

![Full PoQIT Tables](https://user-images.githubusercontent.com/81538390/211197058-65a96633-6ee5-4702-9f9b-46c9daa3f7bb.png)
