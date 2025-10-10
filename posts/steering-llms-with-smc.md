---
title: Steering Language Models with Sequential Monte Carlo
description: How to give your language model the blues with SMC.
date: 2025-10-09
image: steering-llms-with-smc.png
---

A good chunk of my work as part of my PhD involves using sequential Monte Carlo
(SMC) methods to solve decision-making problems. SMC algorithms are
used to efficiently sample from sequences of distributions, and while they are
mostly used in physics, signal processing and Bayesian statistics, recently
they have also found uses in inference-time alignment of generative models. In
this post, I'll show how I used SMC to "steer" a tiny pre-trained language
model into writing sad stories.

## Background: How LLMs Generate Text

A language model works over a vocabulary $\mathsf{S}$ of tokens. At each step $t \in \mathbb{N}$, it predicts the probability distribution of the next token given the history so far:

$$
s_{1:t-1} \mapsto p(\cdot \mid s_{1:t-1}) \in \mathcal{P}(\mathsf{S}),
$$

where $\mathcal{P}(\mathsf{S})$ is the set of distributions over the vocabulary
$\textsf{S}$ and $s_{1:t-1} \coloneqq \{ s_{1}, s_{2}, \dots, s_{t-1} \}$.
Generating text with the language model is the task of sampling a sequence
$s_{1:T}$ from the joint distribution

$$
\mathbb{P}_{T}(s_{1:T}) \coloneqq \prod_{t=1}^{T} p(s_{t} \mid s_{1:t-1}), 
\quad \text{where } s_{1:0} \coloneqq \emptyset.
$$

## Inference-Time Alignment of LLMs

Inference-time alignment refers to modifying a pre-trained model’s sampling
behavior without changing its parameters. Instead of fine-tuning the model
weights, we intervene at sampling time and adjust how likely the model is to
pick certain continuations. Sequential Monte Carlo provides a natural
framework for this: by iteratively reweighting and resampling partial
generations based on a reward signal, we can nudge the model toward desired
behaviors while retaining some of its inherent randomness and diversity.

Formally, we specify our preferences through a sequence of reward functions

$$
r_{t}: \mathsf{S}^{t} \to \mathbb{R}, \quad t \geq 1,
$$

which score partial sequences. This setup is quite general: $r_t$ could encode
stylistic preferences, safety constraints, or factuality. In my case, $r_{t}$
is just a neural network that has been trained to output a “sadness score” in
$[0, 1]$ for a given phrase. We then define a new distribution $\mathbb{Q}_{T}$
over sequences, which is _tilted_ towards the cumulative reward:

$$
\mathbb{Q}_{T}(s_{1:T}) \propto \mathbb{P}_{T}(s_{1:T}) \cdot \exp\left\{\sum_{t=1}^{T} \eta \cdot r_{t}(s_{1:t})\right\}, \quad \eta > 0.
$$

Intuitively, $\mathbb{Q}_T$ reweights the model’s likelihoods so that sequences
with higher cumulative reward become exponentially more probable.

Sampling from $\mathbb{Q}_{T}$ is straightforward with SMC. The recipe is as
follows, with steps 1 and 2 repeated for all $n \in \{1, \dots, N\}, N \in
\mathbb{N}$:

1. **Propose:** Sample a token $s_{t}^{n} \sim p(\cdot \mid s_{1:t-1}^{n})$ and append to sequence, $s_{1:t}^{n} = (s_{1:t-1}^{n}, s_{t}^{n})$.
2. **Weight:** Compute unnormalized weight $w_{t}^{n} = \exp \{ \eta \cdot r_{t}(s_{1:t}^{n}) \}$, then normalize: $W_{t}^{n} = w_{t}^{n} / \sum_{m=1}^{N} w_{t}^{m}$.
3. **Resample:** Draw $N$ new particles from $\{s_{1:t}^n\}_{n=1}^N$ with replacement, proportionally to $W_{t}^{n}$. (This duplicates the ‘sad’ phrases and prunes away overly cheerful ones.)
4. **Repeat:** Until $t = T$.

Over time, the population of particles gradually concentrates on high-reward
trajectories, in this case the sadder continuations.

## Sob Story Time

To illustrate the method, I'm using
[TinyStories-33M](https://huggingface.co/roneneldan/TinyStories-33M), a
language model trained on short children’s stories [@eldan2023tinystories].
Importantly, this model is **not** fine-tuned for sadness (or anything else),
and is just a pure text predictor. The code accompanying this post is available
on [GitHub](https://github.com/Sahel13/llmxsmc/tree/main).

I gave TinyStories the prompt:

> *“When the prince came home, he saw”*

Here’s what the base model produced (one sample from $\mathbb{P}_{T}$):

> *“When the prince came home, he saw the heavy bag of jewelry. He wanted to buy it and wear it. He asked the king to sell it to him. The kind king said ‘Yes!’, and …”*

Hmm, way too cheerful for our tastes. Now here’s what happens after steering with SMC (one sample from $\mathbb{Q}_{T}$):

> *“When the prince came home, he saw the sad family sitting by the stove. He felt very sad too. He had lost his rare treasure box and now it was gone forever.”*

That's more like it!

## Bonus: Steering as Optimization

A simple but neat result is that the steered distribution $\mathbb{Q}_{T}$ is the minimizer of

$$
\mathcal{L}(Q) = \mathbb{E}_{Q} \left[ -\sum_{t=1}^{T} r_{t}(s_{1:t}) \right] 
+ \frac{1}{\eta} \, \mathcal{D}_{\mathrm{KL}} \left[ Q \Vert \mathbb{P}_{T} \right], \quad Q \in \mathcal{P}(\mathsf{S}),
$$

where $\mathcal{D}_{\mathrm{KL}}$ is the Kullback-Leibler (KL) divergence [see,
e.g., @bissiri2016general]. The first term on the RHS is responsible for
maximizing the cumulative reward (sadness), while the second term is a
regularizer forcing $Q$ to stay close to the base model $\mathbb{P}_{T}$. This
regularization term prevents collapse into a small number of “super sad”
trajectories, thus preserving diversity of model outputs.

This optimization perspective also makes clear the connection to reinforcement
learning from human feedback [RLHF, @ziegler2020fine], where the same objective
$\mathcal{L}(Q)$ is minimized by fine-tuning the model weights. Here, we're
skipping the optimization and directly sampling from the minimizer with SMC.

## Parting Notes

The algorithm presented here is the simplest version of SMC (known as the
"bootstrap" particle filter), and in practice more sophisticated techniques are
required to actually deliver on the promise of preserving output diversity.
These techniques include adaptive resampling schedules and twisting, see, e.g.,
@naesseth2019elements. For a state-of-the-art application of SMC to language
models, I recommend @zhao2024probabilistic.
