# dawg

`dawg` is a program that build supervised learning models.  Currently,
`dawg` supports building decision tree ensembles using Friedman's
Stochastic Gradient Boosted Trees algorithm, `dawg` consists of a
single executable, which is used to preprocess training data, learn,
evaluate models, and generate source code to evaluate models.  `dawg`
offers several subcommands, each with its own command-line options and
documentation, (available with the `--help` flag).

## csv to `dog` translation

The `dawg csv` subcommand is used to preprocess a csv file containing
a header of feature names, and data samples, one per line.

Example:

```
dawg csv -i file.csv -o file.dog
```

As we will see later, `dawg` learns over this `dog` file, not the
original csv file.  The `dog` file contains a compressed binary
representation of the original csv data.  `dawg` supports both
categorical and ordinal features.

The contents of the `dog` file can be inspected with the `dawg
dog-meta` and `dawg dog-inspect` subcommands.  The `dog-meta-short`
provides a listing of all the features contained in a dog file, along
with their feature id, name, type (`ord` for ordinal and `cat` for
categorical), and cardinality.

```
dawg dog-meta-short file.doc
```

The `dawg dog-meta` subcommand provides a more comprehensive view of
the feature metadata:

```
dawg dog-meta file.doc
```

We can limit the output of `dawg dog-meta` to a single feature with
the `-k` and `-v` flags:

```
dawg dog-meta file.dog -k name -v salary
```

This displays all metadata record associated with the feature whose
name is `salary` .  Similarly,

```
dawg dog-meta file.dog -k id -v 11
```

displays the metadata record associated with the feature whose id is
11.

Finally, the `dog-inspect` subcommand enumerates the values of a
single feature, along with the (zero-indexed) observation number:


## learning models

`dawg` currently supports learning of decision tree ensemble models
with the stochastic gradient-boosting tree algorithm.  Two prediction
targets are supported: continuous, with the square-loss objective, and
binary, with the logistic-loss objective.  The learning rate, tree
depth, and several other parameters are controlled on the
command-line.

Example:

```
dawg learn -i file.dog -o file.mod -r 0.01 -d 3 -l logistic -y gender
```

This learns a binary classifier over training set `file.dog` for
binary target `gender`, with a learning rate of 0.01 and a maximum tree
depth of 3.  Once `dawg` has converged, it creates the output model
file `file.mod` .

## evaluating models

With a model in hand, we can apply it to data, whether seen during
training or not.  The `eval` subcommand applies the model to a csv
file containing the data for the features included in the model.

Example:

```
dawg eval -i test.csv -m file.mod -p predictions.txt --importance report.txt
```

In this example, the file `predictions.txt` contains the output of the
model, one value per line, corresponding to each sample in file
`test.csv` .  For binary classifier, these values represent
probabilities.  The output file 'report.txt' contains various
importance metrics for each of the features included in the model, in
descending order of importance.

For binary classifier, the `bi-metrics` subcommand is available to
compute various model quality metrics, including accuracy, AUC, the
confusion matrix, and and deviance (logistic loss).  `bi-metrics`
expects as input a file consisting of the label (either `0` or `1`)
and the prediction, one such pair separated by whitespace per line.

Example:

```
paste label.txt prediction.txt | dawg bi-metrics
```

Here, we imagine that we have the `0` or `1` labels in a file
`labels.txt`, and that we use the Unix `paste` command to merge that
file and the prediction file `prediction.txt` created by the `eval`
subcommand.

## generating code to evaluate models

`dawg` can generate code (currently, only in Python) to evaluate a
model.  This is useful in embedding the model in some larger codebase,
and do so with minimal dependencies on external libraries.

Example:

```
dawg gen -m -i file.mod -o model.py
```

This creates a Python module `model` in file `model.py`, with a single
function `eval_by_name` .  That function takes as its only argument a
dictionary of feature name and feature value pairs.  The values can be
numbers (either `float`'s or `int`'s, corresponding to ordinal
features) or strings (corresponding to categorical features).
