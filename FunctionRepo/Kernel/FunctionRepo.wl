ClearAll["FunctionRepo`*", "FunctionRepo`*`*"];

BeginPackage["FunctionRepo`"]

crossValidateModel::usage = "crossValidateModel[data, fitFunction] repeatedly splits the data into training/validation subsets; then fits a model using fitFunction on the training set and validates the result with the validation set.";
conditionedMultinormalDistribution::usage = "conditionedMultinormalDistribution[dist, {i1 -> val1, ...}, {j1, j2, ...}] gives the {j1, j2, ...} marginal of dist when the indices {i1, ...} are conditioned to values {val1, ...}";
kullbackLeiblerDivergence::usage = "kullbackLeiblerDivergence[P, Q] computes the Kullback-Leibler divergence from distribution Q to P";
multiNonlinearModelFit;
sparseAssociation;
firstMatchingValue::usage = "firstMatchingValue[{expr_1, expr_2, ...}, pattern] evalutates held expr_i in turn, returning the value of the first expression that evaluates to a result matching the pattern.";
deleteContainedStrings::usage = "deleteContainedStrings[{str1, str2, ...}] deletes every string that is a substring of at least one other string in the list. Preserves ordering.";
convertDataFormat::usage = "convertDataFormat[data, type] attempts to convert machine learning data to a different format to make it easier to switch out fitting methods.";
maximumSpacingEstimation::usage = "maximumSpacingEstimation[data, dist] fits dist to data using the maximum spacing estimation method.";

tukeyMedianPolish;
Get["FunctionRepo`tukeyMedianPolish`"];

EndPackage[]