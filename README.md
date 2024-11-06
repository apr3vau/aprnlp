# Perceptron NLP Processors in Common Lisp

Here's a set of NLP processors written in Common Lisp by April & May.
This includes a part-of-speech tagger and a transition-based
dependency parser. A trivial searching-based lemmatizer is also
included. 

It's still in primary stage, and has been tested under LispWorks and
SBCL.

Performance:

| \          | Speed         | Model Size | Correctness |
| ---------- | ------------- | ---------- | ----------- |
| pos-tagger |  143k words/s |      3.8MB |         94% |
| dep-parser |   85k words/s |       11MB |         86% |

Load the system to evaluate the result / using the model. The
`test-training` function provides a sample training code for each kind
of processor.

By the way, there's also an underdeveloping graph-based dependency
parser in [dep-graph.lisp](./dep-graph.lisp). Its correctness is only
~65% currently.

**Notice that this codebase used another utility
[FOR-LOOP](https://github.com/apr3vau/for-loop) developed by us.
Please also install that repo while evaluating this system.**

## Acknowledgement

Thanks to SBCL for its super efficient compilation.

The code is developed under LispWorks.

Supporting Neurodiversity & Transgender & Plurality!
