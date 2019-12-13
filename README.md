Project for the maximum flow problem :

1-Compile with "make"

2-Usage : infile source sink outfile 
example of usage: ./ftest.native graphs/graph1 0 5 graphs/graph1_result

In the terminal, you can see :  
    the settings 
    the differents paths and the flow to add to those paths
    th solution of the maximum flow problem

3-Convert .dot to .svg : dot -Tsvg your-dot-file > some-output-file.svg
example of usage : dot -Tsvg graphs/graph1_result.dot > graphs/graph1_result.svg



