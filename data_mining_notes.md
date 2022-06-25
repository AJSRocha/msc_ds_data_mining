R Notebook
================

-   Frequent Pattern Mining
-   Sequence Mining
-   Web Mining
-   Information Retrieval
-   Text Mining
-   Outlier Mining

# 1 - Association rules

-   **Support** of a set: Proportion of transactions that contain the
    items of the set

-   **Confidence** of a rule: Proportion of transactions that contain
    the rule (given {A, B} -\> {C} / {A, B}})

## Mining Association rules

The task typically becomes, given a dataset **D**, to establish a
minimum support for rules (**minsupp**) and minimum confidence
(**minconf**) in order to find all existing rules that fullfill those
criteria.

A strategy is necessary to approach this problem in a sensible manner.
In a dataset with *k* items, there are
![2^k-1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;2%5Ek-1 "2^k-1")
possible combinations. One possible solution is the *Apriori Algorithn*

### Apriori Algorithm

-   **Downward Closure Property**: Every subset of a frequent subset is
    also, by definition, frequent.

-   **Apriori Pruning Principle**: If an itemset is below *minsupp*, it
    is safe to discard all of its supersets.

-   Possible Implementation:

    -   Prune all itemsets of size 1 below minimum support;
    -   Repeat process with itemsets of size 2,3… *k* (realistically,
        *k* - \[all_pruned_items\])
    -   **Rule Generation:**

## Compact Representation of itemsets

-   *s* is a **maximal frequent itemset** if it is a frequent itemset
    for which none of its supersets is frequent.

    -   Example: If, from the range {A,B,C,D}, {B,C} is frequent but
        {A,B,C}, {B,C,D} and {A,B,C,D} are not, then {B,C} qualifies as
        a maximal frequent itemset.

-   *s* is a **closed frequent itemset** if it is a frequent itemset
    that has no frequent supersets with the same support.

## Rule optimization

-   We can define a minimum improvement in confidence for a superset to
    be considered.

    -   Example: If conf{B,C} = 0.50 and conf{B,C,D} = 0.501, we can
        judge wether the gain of 0.01 matches a threshold of usefulness.

-   A rule can be interesting if we determine that the items are not
    **statistically independent**:

-   Some rules can be uninteresting or flat out misleading despite
    having high support and confidence;

-   Useful measure: **lift**:

    lift{A -\> B} = conf{A -\> B}/support{B} = support{A,B} /
    support{A}\*support{B}

    -   measures the influence of A in the presence of B;

    -   lift = 1: A and B are independent;

    -   lift \< 1: A and B are negatively correlated;

    -   lift \> 1: A and B are positively correlated;

    -   lift{A -\> B} = lift{B -\> A}

-   Useful measure: **conviction**:

    -   conviction{A -\> B} = 1-support{B}/ 1 - conf{A-\>B}

    -   Assymetrical, unlike *lift*

    -   

## FP-growth Algorythm

\[\_\] Completar!

## Class Association Rules

# 2 - Sequence Mining

-   Attributes: *size*, *length* (length k is a *k-sequence*)

-   A is a **Subsequence** of B: every element of A needs to be a subset
    of a unique element of B, and the order should be maintained.

-   **Support** of a sequence is the fraction of total data sequences
    that contain that sequence as a subsequence

-   The task is, typically, that given a dataset *S*, to find all the
    sequences with minimal support.

## GSP - Generalized Sequencial Pattern

-   Use similar approach to *Apriori* to prune candidates

    -   If a sequence X falls below the minimal support, so will its
        supersequences

-   GSP Bottlenecks:

    -   generates a huge set of candidate sequences (especially 2-item
        candidate sequences)
    -   scans the database multiple times
    -   the length of each candidate grows by one at each database scan
    -   a long pattern grows up from short patterns
    -   an exponential number of short candidates
    -   inefficient for mining long sequential patterns

## PrefixSpan Algorithms

-   More efficient and less memory hungry than GSP

-   It does not generate candidates

-   GSP performs breath-first search

-   PrefixSpan performs depth-first search

-   It uses prefix-based projection: less projections and quickly
    shrinking sequences

-   No candidate sequence needs to be generated \*• Projected databases
    keep shrinking

-   Major cost of PrefixSpan: constructing projected databases with
    recursively similar suffixes.

-   if it fits on memory, keeping pointers to the suffix offset of the
    sequence, avoids physical copy.

# Exercices:

\[\_\] FP-Growth \[\_\] GSP \[\_\] PrefixSpan
