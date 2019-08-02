(eval-when (compile) (optimize-level 3))

(library (chez patricia-fx)
  (export empty?
	  empty-tree
	  patricia-tree?
	  tree-equal?
	  empty?
	  lookup
	  insert-with
	  insert
	  delete
	  merge-with
	  union-with
	  intersect-with
	  intersection-with
	  difference
	  symmetric-difference
	  singleton
	  tree->alist
	  tree->keys
	  tree->items
	  tree-ifilter
	  tree-filter
	  tree-fold-right
	  tree-fold-left
	  tree-ifold-left
	  tree-ifold-right
	  tree-for-each
	  tree-map
	  tree-imap
	  successor
	  predecessor
	  minimum
	  maximum
	  tree-size
	  view-tree)
  (import (chezscheme))

  (include "bit-fx.scm")
  (include "patricia.scm"))
