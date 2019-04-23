### Web Scraping Works
#### 1. OAE
- [Input data](http://sparql.hegroup.org/sparql?default-graph-uri=&query=%23To+find+all+OAE+terms+with+mapped+MEDDRA+ID%0D%0A%0D%0APREFIX+obo-term%3A+%3Chttp%3A%2F%2Fpurl.obolibrary.org%2Fobo%2F%3E%0D%0ASELECT+DISTINCT+%3Fx+%3Flabel+%3Fmeddra%0D%0Afrom+%3Chttp%3A%2F%2Fpurl.obolibrary.org%2Fobo%2Fmerged%2FOAE%3E%0D%0AWHERE%0D%0A%7B%0D%0A%3Fx+rdfs%3Alabel++%3Flabel.%0D%0A%3Fx+obo-term%3AOAE_0004334+%3Fmeddra.%0D%0A%7D%0D%0A&format=text%2Fhtml&timeout=0&debug=on)
- R code: Webscraping_OAE.R
- Output data: Webpage_OAE.csv, OAE_duplicates.csv

#### 2. Meddra
In VAERS data, there is no coulumn for the MEDDRA ID and some AE names have old names. We did a web scraping to clean this data. The output data 'meddra_ptlist.csv' has the pairs of (old names in VAERS data, with corresponding MEDDRA ID) and (Formal=new name, with corresponding MEDDRA ID).
- Input data: VAERSSYMPOM data (2000~2018), [BioPortal HTML source](http://bioportal.bioontology.org/ontologies/MEDDRA/?p=classes&conceptid=root), [RESTful API](http://data.bioontology.org/documentation#nav_home)
- R code: CleaningMeddraTerms.R
- Output data: meddra_ptlist.csv (AE_NAME: AE names in VAERS; ID: MeDDRA ID for AE_NAME; MEDDRA_AE_NAME: new AE names with MEDDRA_ID)

