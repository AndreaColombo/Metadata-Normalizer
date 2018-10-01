# Metadata-Enricher

Metadata-Enricher is a tool for ontology-driven metadata enrichment. 
It is built within the [GeCo](http://www.bioinformatics.deib.polimi.it/geco/?home) (Data Driven Genomic Computing) project to annotate metadata which describe genomic datasets with ontological terms, their preferred labels, synonyms, hypernyms and hyponyms.

The Enricher interacts with the Genomic Conceptual Model (GCM) relational database created within the project [Metadata-Manager](https://github.com/DEIB-GECO/Metadata-Manager).
Extracting the values contained in the GCM tables, the Enricher starts a workflow which annotates them by using both previously imported terms and external ontology search services.
If the automatic curation fails, we provide an interface for expert users to:
- manually select a suggested annotation
- manually propose a new annotation
- correct previously mistaken annotations

## Repository Structure
There are four main folders in the project folder /scr/main/scala:
- `recommender`: contains the classes useful for 
    1) choosing the most suitable ontology search services for our project
    2) choosing the ontologies which are most appropriate to annotate values from a specific attribute
- `enricher`: contains the engine which performs the annotation of values and fills the Local Knowledge Base
- `user_interface`: contains the classes needed for the user feedback routines
- `config`:  contains configuration files useful for the process
