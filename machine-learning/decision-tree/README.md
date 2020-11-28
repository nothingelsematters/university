# Decision tree

## Goals

1. To implement the algorithm of building the decision tree.
2. Search for the optimal height of the tree.
3. To implement the algorithm of building a forest of deciduous trees.
4. Results analysis.

## Data Sets

Use these data sets to test your classifier. Each dataset is pre-divided into a training and test sample. The class label is the last number in each line. For convenience, they are also available in `.txt` format.

## Task

For each dataset, determine the optimal height of the decision tree for the accuracy of the classification on the test set.

Select two datasets: the minimum and maximum optimal height datasets. For these two datasets, plot the height dependency of the accuracy classification on the training set and the test set.

In this lab work, you are allowed to use sklearn.tree.DecisionTreeClassifier. If you use this implementation, in addition to the tree height, you need to configure hyperparameters and splitter (see documentation).

For each dataset, build a forest of deciduous trees without height limitation (i.e. without pruning) and determine the accuracy of classification on the training and check set.
