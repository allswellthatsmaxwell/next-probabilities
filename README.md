# next-probabilities
Look into the properties of P(t<sub>i</sub> | ¬t<sub>j</sub> &forall; j < i). This is the probability that an event will happen on the next timestep, given that it hasn't happened on any timesteps yet. 

For example, if:  
• you are modeling bus wait times with a Poisson  
• they come an average of every ten minutes  
• you ran up on the previous bus pulling away  
• you've been waiting for t<sub>i−1</sub> minutes after that  
then we can ask things like:  
◦ what's the probability of the bus coming during minute t<sub>i</sub>?   
◦ Is P(arrival = 11 | arrival > 10) greater than P(arrival = 10 | arrival > 9)?   
◦ What minutes-so-far value maximizes the chance of the bus coming in the next minute, and how does that relate to the average time?  
