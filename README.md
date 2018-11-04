# Quality Control in Manufacturing Process

Executive Summary

Objective

Methods

Finding (result)




1.	Introduction:

With a highly competitive market due to the growth of global demand for goods and services, quality control becomes an essential and crucial part in any production process. To ensure that the products can be produced comply with strict requirements, manufacturers or regulatory authorities increasingly rely on several techniques during manufacturing process. One among of those is quality control products during manufacturing by classifying production outcome based upon input information in order to determine the quality of the product and allows any necessary changes to be addressed early on. To understand more deeply about manufacturing process and how quality control with classification can be applied in, let take a quick look at their definition and explanation.

What is manufacturing process? As a Wiki definition, manufacturing process is defined as “the steps through which raw materials are transformed into a final product. The manufacturing process begins with the product design, and materials specification from which the product is made. These materials are then modified through manufacturing processes to become the required part.”

So, how does quality control in manufacturing process with classification work? Specifically, quality control in manufacturing process with classification is a simple approach to detect a good or bad quality of a batch production with historical data from similar previous batch production. These archived information included input raw material, ratio of raw material in use, or material brand names in last production process. The information then will be used  as a training data set to establish a model classifier. For each algorithms or techniques used in creating a classifier, the prediction output may be different. Particularly, in this project, we applied 4 different algorithms for classification. Details of these algorithms will be explained deeper in the next section Algorithms and Techniques. From now, the model classifier will be used in order to predict the quality of current batch production and provide or make appropriate decision for next stage in production process.

2.	Algorithm and Techniques:

As mentioned previously, we applied 4 common various algorithms to establish a model classifier for our project. They are Naive Bayes, Decision Tree C5.0, Support Vector Machine SVM, and Deep Learning Neural Network H2O. 

2.1	Naive Bayes

Naive Bayes is a simple technique for constructing classifiers: models that assign class labels to problem instances, represented as vectors of feature values, where the class labels are drawn from some finite set. There is not a single algorithm for training such classifiers, but a family of algorithms based on a common principle: all naive Bayes classifiers assume that the value of a particular feature is independent of the value of any other feature, given the class variable.

2.2	Decision Tree

Decision tree learners are powerful classifiers, which utilize a tree structure to model the relationships among the features and the potential outcomes. A decision tree classifier uses a structure of branching decisions, which channel examples into a final predicted class value.

Decision trees are built using a heuristic called recursive partitioning. This approach is also commonly known as divide and conquer because it splits the data into subsets, which are then split repeatedly into even smaller subsets, and so on and so forth until the process stops when the algorithm determines the data within the subsets are sufficiently homogenous, or another stopping criterion has been met.

There are numerous implementations of decision trees, but one of the most well-known implementations is the C5.0 algorithm. Therefore, in this project, we use C5.0 algorithms for our classification problem.

2.3	Support Vector Machine

A Support Vector Machine (SVM) can be imagined as a surface that creates a boundary between points of data plotted in multidimensional that represent examples and their feature values. The goal of a SVM is to create a flat boundary called a hyperplane, which divides the space to create fairly homogeneous partitions on either side. SVMs use a boundary called a hyperplane to partition data into groups of similar class values.

2.4	Deep Learning Neural Network H2O

H2O’s Deep Learning is based on a multi-layer feedforward artificial neural network that is trained with stochastic gradient descent using back-propagation. The network can contain a large number of hidden layers consisting of neurons with tanh, rectifier, and maxout activation functions.

3.	Methodology:
3.1	Data description  (step 1)

This is product quality data obtained during the manufacturing process. There are two CSV data files in it: 1) param_data_train, 2) param_data_test. There is 12 features in training data set and similarly, 11 features in testing data set which lack of label feature. While training set contains num observations, testing set has num observations. Since data had been collected from company, all it information has been confidentially encoded for security purpose. 

Features include product_no (number of batch product), label (good (1) or bad (0)), material A, material B, Brand Name, Material Size, Mix Proportion and some other numerical parameters. The purpose of this project is to create a model classifier and implement it on the test datasets to predict the product quality (predict quality label).


3.2	Exploring and Preprocessing data (step 2)

3.2.1	Data Visualization

3.2.2	Data Preprocessing

3.3	Implementation (step 3)

4.	Result and Improvement:

4.1	Model results and evaluation (step 4)

4.2	Model improvement (step 5)

5.	Conclusion:



Appendix

Reference


