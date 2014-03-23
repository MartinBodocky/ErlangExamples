ErlangExamples
==============

My Erlang learning process

The purpose of this repository is my learnig, you can find here aprroaches which I have chosen, please if you have a question, do not hesitate to ask.

<h2>Part 1</h2>

I created factory shopping cart with separate credit card provider, this example are not implemeneted with fault-tolerant in mind, it shows mainly usage for ETS/DETS tables as persistent storage for your data. I don't use OTP behaviours here. Examples how to use is in module tests.erl, where I created EUnit tests for this solution. I added diagram shows processes used in solution, to picture my architecture.

<br/>
<img src="/Docs/Part%201.png" height="250" width="420">



<h2>Part 2</h2>

I took the factory solution form Part 1 and make it fault tolerant, I created two purely supervisor modules which restarts factory and credit card provider processes if it fails. Also I have restarted sessions processes if it fails but with slightly different strategy. When it faisl three times in one row i will restart the factory subtree. You need to see tests.erl for usage as in Part 1. Please see my following diagram which shows supervision trees.

<br/>
<img src="/Docs/Part%202.png" height="250" width="620">


<h2>Part 3</h2>

Also I upgraded Part 2 for distribution, where I created four nodes, two for factory process and two for credit card provider process. The distribution strategy is if primary fails then secondary will suply requests, I apply request to nodes by broadcasting them. Please keep on mind this solutions suffers from inconsistence when second node is unavailable in receiving requests. I will do my best to add this ability to solution asap. Also this solution is not ready for netsplit. I created diagram which pictures my architecture in this solution.

<br/>
<img src="/Docs/Part%203.1.png" height="250" width="620">



