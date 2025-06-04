---
title: Expected Information as Expected Utility
description: I discuss the article of the same name by José M. Bernardo from 1979, which shows that the expected information gain, a popular metric used in Bayesian experimental design, is itself a solution to maximum expected utility problem under some assumptions on the utility function.
date: 2025-06-04
---

In Bayesian experimental design, the _expected information gain_ [@lindley1956measure]

$$
\mathrm{EIG}(\xi) \coloneqq \mathbb{E}_{p(y \mid \xi)} \Bigl[ \mathbb{H} \bigl[ p_{\theta}(\theta) \bigr] - \mathbb{H} \bigl[ p_{\theta \mid y, \xi}(\theta) \bigr] \Bigr],
$$

has become the predominant metric for evaluating the quality of an experimental design $\xi$.
It measures how much information we expect to gain about the unknown parameters $\theta \in \Theta$ after observing data $y$ generated under that design.
Here, $\mathbb{H}$ denotes Shannon entropy (or differential entropy, for continuous $\theta$), and the expectation is taken over all possible observations under the design.

The EIG is intuitive---it is the expected reduction in entropy from prior to posterior. But intuition aside, it raises a natural question. The reduction in Shannon entropy is only one of many ways to measure uncertainty reduction, or even more generally, the utility of an experiment $(\xi, y)$ [@huan2024optimal]. Is there, then, a fundamental reason to prefer the EIG over alternative utility measures?

It turns out the answer is yes. In @bernardo1979expected, José M. Bernardo, a student of Dennis Lindley, gave a justification grounded in decision theory.
He showed that the EIG arises naturally from a decision problem under reasonable assumptions on the utility function.
I find this result extremely remarkable, and this is an appreciation post about half-a-century later :)

## The Decision-Theoretic Setup

Consider a decision problem where a scientist has to report a distribution $q \in \mathcal{P}(\Theta)$ for $\theta$, where the decision space $\mathcal{P}(\Theta)$ is the set of probability measures on $\Theta$.[^generalized_bayes]
The utility obtained if she reports $q$ when the true parameter is $\theta$ is quantified by means of a utility function $u:\mathcal{P}(\Theta) \times \Theta \to \mathbb{R}$.[^scoring_rule]
Suppose the scientist applies a design $\xi$ and observes an outcome $y$, after which she updates her belief to $p_{\theta \mid y, \xi}(\theta)$ using Bayes' rule.
Then, her _expected utility_ when reporting a distribution $q$ is

$$
\int u(q, \theta) \, p_{\theta \mid y, \xi}(d\theta).
$$

Under the principle of maximum expected utility, the scientist should report the distribution $q^{*}$ that maximizes the above integral, which may not be her actual belief $p_{\theta \mid y, \xi}$. Hence, to discourage lying, we require that $u$ is a _strictly proper_ utility function, meaning that

$$
\sup_{q} \int u(q, \theta) \, p_{\theta \mid y, \xi}(d\theta) = \int u(p_{\theta \mid y, \xi}, \theta) \, p_{\theta \mid y, \xi}(d\theta)
$$

with the supremum only attained at $q^{*} = p_{\theta \mid y, \xi}$. In other words, truth-telling should be the optimal strategy.

The second restriction we need on the utility function is that it is _local_, i.e., $u(q, \theta) = u\bigl(q(\theta), \theta\bigr)$ for all $\theta \in \Theta$.
Locality means the utility function only depends on the density of the reported distribution at the true parameter $\theta$, which feels reasonable (though admittedly not as easy to motivate as properness).
The striking result of @bernardo1979expected is that if the utility function $u$ is proper, local, and sufficiently smooth (as a function of $q$), then it must be of the form

$$
u(q, \theta) = A \log q(\theta) + B(\theta),
$$

where the constant $A$ and function $B$ can be arbitrary.
Plugging this back into the requirement that $u$ is proper, we see that the maximum expected utility is

$$
\begin{align*}
\sup_{q} \int u(q, \theta) \, p_{\theta \mid y, \xi}(d\theta)
& = \int \left( A \log p_{\theta \mid y, \xi}(\theta) + B(\theta) \right) p_{\theta \mid y, \xi}(d\theta) \\
& = -A \mathbb{H}\left[ p(\theta \mid x, \xi) \right] + \mathbb{E}_{\theta \mid y, \xi}[B(\theta)].
\end{align*}
$$

Lo and behold, the Shannon entropy appears! How cool is that?

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

Finally, to assess the quality of a design $\xi$, we take the expectation over the observation distribution $p(y \mid \xi)$, leaving us with

$$
\begin{align*}
& A \cdot \mathbb{E}_{y \mid \xi}\Bigl[ \mathbb{H}\bigl[p_{\theta}(\theta)\bigr] - \mathbb{H}\bigl[ p_{\theta \mid y, \xi}(\theta) \bigr] \Bigr] + \underbrace{\mathbb{E}_{y \mid \xi} \Bigl[ \mathbb{E}_{\theta \mid y, \xi}[B(\theta)] - \mathbb{E}_{\theta}[B(\theta)] \Bigr]}_{=0} \\
& = A \cdot \mathrm{EIG}(\xi).
\end{align*}
$$

This shows that, for the purpose of choosing the best design, the choice of $A$ and $B$ is irrelevant, and the utility function can simply be the log density, $u(q, \theta) = \log q(\theta)$.

## Concluding Remarks

To motivate his paper, Bernardo writes in the abstract:

> ... a scientist typically does not have, nor can be normally expected to have, a clear idea of the utility of his results.

By considering a decision problem of choosing the best distribution to report, and showing that the EIG arises naturally under reasonable assumptions on the utility function, Bernardo makes a compelling argument for using the EIG in precisely those settings where the scientist cannot quantify the utility of her results apriori.

Note that the whole argument breaks down if the prior $p_{\theta}$ or the likelihood $p(y \mid \theta, \xi)$ is misspecified, because then the posterior $p_{\theta \mid y, \xi}$ given by Bayes' rule is not valid.

[^scoring_rule]: Such a utility function is also known as a _scoring rule_ [@gneiting2007strictly], which explains how well a distribution explains an observed value of a random variable.
[^generalized_bayes]: This is the same viewpoint that is shared by @bissiri2016general to devise a more general framework for Bayesian inference based on loss functions.
