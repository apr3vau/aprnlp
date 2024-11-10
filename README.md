# Perceptron NLP Processors in Common Lisp

Here's a set of NLP processors written in Common Lisp by April & May.
This includes a part-of-speech tagger and a transition-based
dependency parser (currently only unlabelled). A trivial
searching-based lemmatizer is also included.

It's still in primary stage, and has been tested under LispWorks and
SBCL.

Performance (on SBCL):

| \                | Speed         | Model Size | Correctness |
| ---------------- | ------------- | ---------- | ----------- |
| pos-tagger       |  143k words/s |      3.8MB |         94% |
| dep-parser (UAS) |   46k words/s |     14.6MB |         94% |
| dep-parser (LAS) |   46k words/s |     16.2MB |         92% |

Load the system to evaluate the result / using the model. The
`test-training` function provides a sample training code for each kind
of processor.

**For SBCL, the default heap size is not sufficient for training the
dependency parser. Please increase the heap size, for example,
starting SBCL with following argument:**

	sbcl --dynamic-space-size 4096

## Notes

For some reason, the loaded model may not produce same result with
training time under SBCL. Open an issue if anyone has faced this
problem and I'll fix it.

There's also an underdeveloping graph-based dependency parser in
[dep-graph.lisp](./dep-graph.lisp). Its correctness is only ~65%
currently. It's included in the repository but not in the ASDF system.

## Acknowledgement

Thanks to SBCL for its super efficient compilation.

The code is developed under LispWorks.

Supporting Neurodiversity & Transgender & Plurality!
