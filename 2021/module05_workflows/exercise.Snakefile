

## --------------------------------------------------------------------------------
## config

COUNTRIES=["dk", "swe", "uk"]


## --------------------------------------------------------------------------------
## targets

rule all:
    input:
        expand("{country}.plot.pdf", country=COUNTRIES)

        
## --------------------------------------------------------------------------------
## rules

rule filter_table:
    input:
        "data/{country}.csv"
    output:
        "data/{country}.filtered.csv"
    shell:
        """
        cat {input} | head -n 200 > {output}
        """

        
rule rename_table:
    input:
        "data/{country}.filtered.csv"
    output:
        "data/{country}.filtered.renamed.csv"
    shell:
        """
        mv {input} {output}
        """

rule plot_table:
    input:
        "data/{country}.filtered.renamed.csv"
    output:
        "{country}.plot.pdf"
    shell:
        """
        Rscript plot.R {input} {output}
        """

