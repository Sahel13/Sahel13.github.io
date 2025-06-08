---
title: Expected Information as Expected Utility
description: I discuss the article of the same name by José M. Bernardo from 1979, which shows that the expected information gain, a popular metric used in Bayesian experimental design, is itself a solution to maximum expected utility problem under some assumptions on the utility function.
date: 2025-06-08
---

Imagine a scientist planning a clinical trial to determine the optimal dosage of a new diabetes
drug. Due to constraints on budget and patient safety, only a limited number of tests can be
conducted, so the scientist must decide which dosage levels to test to most
effectively learn the relationship between dose and patient response. To make
these decisions systematically, the scientist can turn to Bayesian experimental
design (BED), which is a principled framework for designing optimal
experiments under uncertainty.

Suppose the scientist models the dose--response relationship using a
parametric form governed by unknown parameters $\theta \in \Theta$---such as the
maximum effect of the drug, the dose achieving half that effect, and the
baseline response. To learn about these parameters, she must choose a _design_ $\xi$, which could
specify, for example, a set of dosage levels and how patients are allocated to them.
A widely used objective in BED is to choose the design that maximizes the _expected
information gain_ (EIG) [@lindley1956measure]:

$$
\mathrm{EIG}(\xi) \coloneqq \mathbb{E}_{p(y \mid \xi)} \Bigl[ \mathbb{H} \bigl[ p_{\theta}(\theta)
\bigr] - \mathbb{H} \bigl[ p_{\theta \mid y, \xi}(\theta) \bigr] \Bigr].
$$

It quantifies how much we expect to reduce our uncertainty about $\theta$, starting from some
prior belief $p_{\theta}$, on applying the design $\xi$. Here, $\mathbb{H}$ denotes Shannon entropy
(or differential entropy for continuous $\theta$), and the expectation is over possible outcomes $y$
with distribution $p(y \mid \xi)$.

The EIG is intuitive---it is the expected reduction in entropy from prior to posterior. But
intuition aside, it raises a natural question. The reduction in Shannon entropy is only one of many
ways to measure uncertainty reduction, or even more generally, the utility of an experiment $(\xi,
y)$ [@huan2024optimal]. Is there, then, any fundamental reason to prefer the EIG over alternative
utility measures?

It turns out the answer is yes. In @bernardo1979expected, José M. Bernardo, a student of Dennis
Lindley, gave a justification for the EIG grounded in decision theory. He showed
that the EIG arises naturally from a decision problem under reasonable
assumptions on the utility function. I find this result extremely remarkable,
and this is an appreciation post about half-a-century later :)

## The Decision-Theoretic Setup

Consider a decision problem where a scientist has to report a distribution $q \in
\mathcal{P}(\Theta)$ for $\theta$, where the decision space $\mathcal{P}(\Theta)$ is the set of
probability measures on $\Theta$.[^generalized_bayes] The utility obtained if she reports $q$ when
the true parameter is $\theta$ is quantified by means of a utility function $u:\mathcal{P}(\Theta)
\times \Theta \to \mathbb{R}$.[^scoring_rule] Suppose the scientist applies a design $\xi$ and
observes an outcome $y$, after which she updates her belief to $p_{\theta \mid y, \xi}(\theta)$
using Bayes' rule. Then, her expected utility when reporting $q$ is

$$
\int u(q, \theta) \, p_{\theta \mid y, \xi}(d\theta).
$$

Under the principle of maximum expected utility, the scientist should report the distribution
$q^{*}$ that maximizes the above integral, which may not be her actual belief $p_{\theta \mid y,
\xi}$. Hence, to discourage lying, we require that $u$ is a _strictly proper_ utility function,
meaning that

$$
\sup_{q} \int u(q, \theta) \, p_{\theta \mid y, \xi}(d\theta) = \int u(p_{\theta \mid y, \xi}, \theta) \, p_{\theta \mid y, \xi}(d\theta),
$$

with the supremum only attained at $q^{*} = p_{\theta \mid y, \xi}$. In other words, truth-telling
should be the optimal strategy.

The second restriction we need on the utility function is that it is _local_, i.e., $u(q, \theta) =
u\bigl(q(\theta), \theta\bigr)$ for all $\theta \in \Theta$. Locality means the utility function
only depends on the density of the reported distribution at the true parameter $\theta$, which feels
reasonable (though admittedly not as easy to motivate as properness). The striking result of
@bernardo1979expected is that if the utility function $u$ is strictly proper, local, and
sufficiently smooth (as a function of $q$), then it is of the form

$$
u(q, \theta) = A \log q(\theta) + B(\theta),
$$

where the constant $A$ and function $B$ can be arbitrary. Plugging this back into the requirement
that $u$ is proper, we see that the maximum expected utility is

$$
\begin{align*}
\sup_{q} \int u(q, \theta) \, p_{\theta \mid y, \xi}(d\theta)
& = \int \left( A \log p_{\theta \mid y, \xi}(\theta) + B(\theta) \right) p_{\theta \mid y, \xi}(d\theta) \\
& = -A \mathbb{H}\left[ p(\theta \mid x, \xi) \right] + \mathbb{E}_{\theta \mid y, \xi}[B(\theta)].
\end{align*}
$$

Lo and behold, the Shannon entropy appears!

## Back to Experimental Design

Now, the gain in expected utility for the scientist from the experiment $(\xi, y)$ is

$$
\begin{align*}
\mathrm{Gain}
& = \textrm{Max utility after observing } y - \textrm{Max utility from the prior} \\
& = \sup_{q} \int u(q, \theta) \, p_{\theta \mid y, \xi}(d\theta) - \sup_{p} \int u(p, \theta) \, p_{\theta}(d\theta),
\end{align*}
$$

which simplifies to

$$
A \Bigl[ \mathbb{H}\bigl[p_{\theta}(\theta)\bigr] - \mathbb{H}\bigl[ p_{\theta \mid y, \xi}(\theta) \bigr] \Bigr] + \mathbb{E}_{\theta \mid y, \xi}[B(\theta)]
  - \mathbb{E}_{\theta}[B(\theta)].
$$

To assess the quality of a design $\xi$, we take the expectation over the observation distribution
$p(y \mid \xi)$, leaving us with

$$
\begin{align*}
& A \cdot \mathbb{E}_{y \mid \xi}\Bigl[ \mathbb{H}\bigl[p_{\theta}(\theta)\bigr] - \mathbb{H}\bigl[ p_{\theta \mid y, \xi}(\theta) \bigr] \Bigr] + \underbrace{\mathbb{E}_{y \mid \xi} \Bigl[ \mathbb{E}_{\theta \mid y, \xi}[B(\theta)] - \mathbb{E}_{\theta}[B(\theta)] \Bigr]}_{=0} \\
& = A \cdot \mathrm{EIG}(\xi).
\end{align*}
$$

This shows that, for the purpose of choosing the best design, the choice of $A$ and $B$ is
irrelevant, and the utility function can simply be the log density, $u(q, \theta) = \log q(\theta)$.

## Concluding Remarks

To motivate his paper, Bernardo writes in the abstract:

> ... a scientist typically does not have, nor can be normally expected to have, a clear idea of the
> utility of his results.

By considering a decision problem of choosing the best distribution to report, and showing that the
EIG arises naturally under reasonable assumptions on the utility function, Bernardo makes a
compelling argument for using the EIG in precisely those settings where the scientist cannot
quantify the utility of her results _a priori_.

[^scoring_rule]:
    Such a utility function is also known as a _scoring rule_ [@gneiting2007strictly],
    which measures how well a distribution explains an observed value of a random variable.

[^generalized_bayes]:
    This decision problem is also adopted by @bissiri2016general to devise a more
    general framework for Bayesian inference.
