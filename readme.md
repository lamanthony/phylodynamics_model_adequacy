# Phylodynamics model adequacy and classification


### Notes for simulations

- For simulations:
	- 1,000 trees for each setting
		- BD
		- SIR sampling
		- Exp coalescent
		- Structured 
			- simulate from a constant coal and fit structure to get parameters
			- simulate coalescent trees with mu/2, mu/4
		- Constant coalescent
		- Use flu-like parameters
	- Rescale all trees, such that the total tree length is 1.0
	- Test statistics
		- Use all those from Saulnier et al. 2017, plus those we have already.