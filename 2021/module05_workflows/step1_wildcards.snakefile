

rule filter_table:
    input:
        "data/{country}.csv"
    output:
        "data/{country}.filtered.csv"
    shell:
        """
        cat {input} | tail -n 10 > {output}
        """
