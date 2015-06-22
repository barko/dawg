* specify features by name or id using the follwing syntax `i:12345`
  (or id:12345) where 12345 is the feature id, or with `n:salary` (or
  `name:salary`) where `salary` is the feature name.  Do this for all
  cmdline arguments taking a feature id or name.

* allow for the sparse specification of the csv header; all
  unspecified columns can then only be referred to by id.

* fix `OMakefile` so that we can just make with `omake` rather than the
  more cumbersome `OCAMLPATH=$PATH omake`

* apply allowed training duration (implied by deadline) to each fold,
  rather for to the entire training.  Alternatively, divide the
  training duration by number of folds.

* create better cli scheme for execluding/including features