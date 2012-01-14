ggson: declarative way for producing repetitive plots
======================================================

Introduction
------------
System administrators or Performance Engineers need plots from SAR outputs. Producing the same set of plots for different outputs becomes boring. ggson allows user to specify the input search pattern, variables to plot, and type of plot (boxplot, jitter, scatter) in a json file. Now, this JSON can now be used for anytime you have CSV output of sar.

Advantage
---------
You can have different JSON input for different version of sar and get the same set of ouuput.


Example
------
For example, you have sar CPU data for two tests and extracted in CSV format.

    alltest/
        test1/
            |
            sar.cpu
        test2/
            |
            sar.cpu


Create a JSON

    cat > cpu.json
    {
        "CPU_compare" {
            "pattern" : "*.cpu$",
            "category" : "CPU",
            "graph_set1" : {
                "group" : [ "%idle", "%user", "%system"],
                "graph" : ["boxplt", "timeseries" ]
            }
    }
    ^D

Run `Rscript main.r -s alltest -j cpu.json -d path/to/store/plots`

