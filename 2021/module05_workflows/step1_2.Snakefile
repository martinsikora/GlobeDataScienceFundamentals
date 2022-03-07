

rule filter_table:
    input:
        "data/{country}.csv"
    output:
        "data/{country}.filtered.csv"
    shell:
        """
        cat {input} | tail -n 10 > {output}
        """
        
rule plot_table:
    input:
        "data/{country}.filtered.csv"
    output:
        "{country}.plot.pdf"
    shell:
        """
        Rscript plot.R {input} {output}
        """
