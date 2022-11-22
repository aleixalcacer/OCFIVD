# Ordinal classification for interval-valued data

* The `datasets/` folder contains all the datasets used in this paper.

* The `methods/` contains a collection of `.R` scripts that implement new methods for ordinal classification of interval-valued data. Each method must implement:
  - `[method_name](train_Data, train_labels, ...)` function that should return an object of the `[method_class]` class.
  - `predict.[method_class](method_class_object, test_data)` function that should return the predictions for `test_data`.

* `script.R` is used for evaluate the methods in a remote machine:
  ```shell
  ssh user@machine
  git clone https://github.com/aleixalcacer/OCFIVD.git
  cd OCFIVD
  R CMD BATCH script.R &
  ```
  
  
