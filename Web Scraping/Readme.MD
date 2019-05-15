### Web Scraping Works
#### 1. OAE
- [Input data](http://sparql.hegroup.org/sparql?default-graph-uri=&query=%23To+find+all+OAE+terms+with+mapped+MEDDRA+ID%0D%0A%0D%0APREFIX+obo-term%3A+%3Chttp%3A%2F%2Fpurl.obolibrary.org%2Fobo%2F%3E%0D%0ASELECT+DISTINCT+%3Fx+%3Flabel+%3Fmeddra%0D%0Afrom+%3Chttp%3A%2F%2Fpurl.obolibrary.org%2Fobo%2Fmerged%2FOAE%3E%0D%0AWHERE%0D%0A%7B%0D%0A%3Fx+rdfs%3Alabel++%3Flabel.%0D%0A%3Fx+obo-term%3AOAE_0004334+%3Fmeddra.%0D%0A%7D%0D%0A&format=text%2Fhtml&timeout=0&debug=on)
- R code: Webscraping_OAE.R
- Output data: Webpage_OAE.csv, OAE_duplicates.csv

#### 2. Meddra
In VAERS data, we have the names of the adverse effect(AE), but not their corresponding ID information. In addition, some of AE names in VAERS data are the old version or the informal version in terms of MedDRA standard. Thus, we implemented a web scraping process for cleaning these names into the current and formal version and for gathering their MedDRA ID. For the implementation, we gathered the data directly from BioPortal HTML source or using RESTful API which is provided for free from BioPortal.

First, we filtered out the AE names which are matched in our reference data (ddall.csv; the ontology structure data). Since we already have the MedDRA ID of the matched AE names in the reference data, we just saved the names and their ID into an empty list (meddra_ptlist.csv). In case of the unmatched AEs, we searched their name on BioPortal using RESTful API and gathered the corresponding MedDRA ID, and saved them into 'meddra_ptlist.csv'.

Now we have the "meddra_ptlist.csv" which has the information of the AE names from VAERS data, and their MedDRA ID. However, one should note that VAERS data has been recorded over from 2000 to 2018. In this long period, the ontology structure and formal name of MedDRA terminologies may have changed; thus, we have to update each AE terms to the current and formal one. In BioPortal, one can find the field names "Classified as" or "classifies" which describe the relationship between the old names and the new name of a specific MedDRA terminology. For example,  if there is the "Classified as" field in the page of a specific term B and the data of this field is A, A is the current formal MedDRA name of B. Furthermore, one can find the "classifies" field from the page of the current formal name such as A which has the list of its old names. Using such structure from BioPortal HTML source, we added the columns for the updated current AE names and their MedDRA ID.

- Input data: VAERSSYMPOM data (2000~2018), [BioPortal HTML source](http://bioportal.bioontology.org/ontologies/MEDDRA/?p=classes&conceptid=root), [RESTful API](http://data.bioontology.org/documentation#nav_home)
- R code: CleaningMeddraTerms.R
- Output data: meddra_ptlist.csv (AE_NAME: AE names in VAERS; ID: MeDDRA ID for AE_NAME; MEDDRA_AE_NAME: new AE names with MEDDRA_ID)

