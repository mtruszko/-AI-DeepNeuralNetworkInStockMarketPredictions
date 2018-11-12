<h3 align="center">
  <img src="Resources/ICON3.png" width="300">
</h3>

# Application of Deep Neural Network in Stock Market Return Prediction

<h3 align="center">
  <img src="Resources/CompetitionImage.png" width="600">
</h3>

Project created as part of Msc. 
[Contest data.](https://knowledgepit.fedcsis.org/mod/page/view.php?id=1012)
The 40.77% result I obtained, it would siuit me in the first 14.

## Abstract

One of the always-attracting topics is the financial market and its prediction for investment purposes. The author of the work proposes exploration of data using deep neural networks. Data provided thanks to the global ISMIS 2017 competition includes expert predictions on various stocks. The aim of the work is to examine the effectiveness of deep and recurrent neural networks on this type of data. Data due to its structure are very complicated and competition threshold hasn’t been achieved. Research is divided into experiments between which variables are the parameters and architecture of the network, from small  several layers networks to more and more complex ones. The research shows that not every data can be extracted to the right amount of knowledge, there may be too few of them, or the current technology is still too weak to achieve 100% efficiency of classification. On the other hand, the current progress is certainly worth paying attention to.

## Conclusions and Remarks

Summarizing, one can come to the conclusion that not every data  can be analyzed and learned hidden knowledge, in the same effective way. Sometimes there  are not enough samples for their level of complexity. On the other hand the technology we have is still not adequate.
Data used in the work belongs to described type of data, which confirms the fact that in the global competition no team has reached the base line, which is what the company ordering the task itself has achieved. The result itself can be interpreted in two ways, it is possible that such a level below 50% of effectiveness is acceptable in comparison to purely random results, which for 3 classes can be taken as 33%. But on the other side this results have a long way to expected 100% effectiveness.
The 40.77% result I obtained, also did not exceed the base line 43.95%, but in the results of the competition alone, it would place me in the first 14.
In my experiments, the best attempt was based on RNN, more precisely on GRU, created in 2014. And this is what shows the power of recurrent neural network solutions, which due to continuous development, can be said to be created in front of our eyes.
We can not forget about other solutions, whose results were only slightly weaker. The dropout mechanism is of great importance here, thanks to which working with very large neural networks makes sense. In my case, without adding a mechanism to randomly reset the weights, some networks would be unlearnable.
There are many methods and software available to experiment with neural networks that I came across at the beginning of my research. However, Keras proved to be the best in my case, which is characterised by the efficiency and speed of creating new models and network architectures in a programmatic way.
The k parameter, which mean the number of events in the sequence, had a large impact on the effectiveness of the models, but the biggest differences were at its initial values. The optimal value turned out to be 20. The difference between 20 and 35 brought only a slight loss. This could be due to incomplete data and irregular number of predictions for transactions, which created a lot of zero places in the data prepared for network entry.
The final result could also have been affected by the lack of information about the market on which they are a transaction, or the lack of data about the experts or companies that  predict transactions. As a result, one can be caught up with the attention that in the further development it might be helpful to add the some kind of expert ranking, in which each expert would have his weight statistically fixed on the test set.
Another solution for further research could be to add further dimensions to data with information about experts or market, in the one hot system.
Another improvement for further experimenting would be a new graphics card because bigger networks and bigger data require more and more computation time, which began to be very noticeable during RNN experiments.
