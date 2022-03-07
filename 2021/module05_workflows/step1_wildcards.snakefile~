

rule filter_table:
    input:
        "data/dk.csv"
    output:
        "data/dk.filtered.csv"
    shell:
        """
        cat {input} | tail -n 10 > {output}
        """
