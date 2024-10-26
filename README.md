# A Part-of-Speech Tagger in Common Lisp

This is a POS tagger written in Common Lisp. It's still in primary
stage, and has been tested under LispWorks and SBCL.

Features:

- Extremely Fast (~143k words per second)
- Extremely Small (4.8MB model size)

The current correctness is ~93.8% on UD English Treebank(GUM). The
algorithm is not good at differing nouns and proper nouns, the
correctness can be ~96.0 if we merge these two tags.

The speed of this model can reach to \~143k words per second (24006
words in 0.171s), and the model size is only \~4.8MB (both under SBCL).

Load the system to evaluate the result / using the model. Sample
training code is (temporarily) at the end of [core.lisp](./core.lisp)
(the `test-training` function).

Notice that this codebase used another utility
[FOR-LOOP](https://github.com/apr3vau/for-loop) developed by us.
Please also install that repo while evaluating this system.

## Acknowledgement

Thanks to SBCL for its super efficient compilation.

The code is developed under LispWorks.

Supporting Neurodivergent & Transgender & Plurality!
