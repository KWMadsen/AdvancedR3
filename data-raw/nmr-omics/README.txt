This README.txt file was generated on 2022-05-31 by Martina Palomino-Schätzlein

-------------------
GENERAL INFORMATION
-------------------

Title of Dataset: Normalized NMR integration values from the metabolomic and lipidomic analysis of red blood cell extract from type 1 diabetes patients and matched healthy subjects

Author Information 
Principal author Jose Raúl Herance
Medical Molecular Imaging Research Group, Vall d’Hebron Research Institute, CIBBIM-Nanomedicine, and Autonomous University of Barcelona, Barcelona, Spain; and CIBER-bbn (ISCIII), Madrid, Spain .
Author Andreea Ciudin
Diabetes and Metabolism Research Unit, Vall d’Hebron Research Institute, Autonomous University of Barcelona, Barcelona, Spain; and CIBERDEM (ISCIII), Madrid, Spain.
Author Rubén Lamas-Domingo
NMR Facility, Centro de Investigación Príncipe Felipe, Valencia, Spain.
Author Carolina Aparicio-Gómez
Medical Molecular Imaging Research Group, Vall d’Hebron Research Institute, CIBBIM-Nanomedicine, and Autonomous University of Barcelona, Barcelona, Spain; and CIBER-bbn (ISCIII), Madrid, Spain .
Author Cristina Hernández
Diabetes and Metabolism Research Unit, Vall d’Hebron Research Institute, Autonomous University of Barcelona, Barcelona, Spain; and CIBERDEM (ISCIII), Madrid, Spain.
Author Rafael Simó
Diabetes and Metabolism Research Unit, Vall d’Hebron Research Institute, Autonomous University of Barcelona, Barcelona, Spain; and CIBERDEM (ISCIII), Madrid, Spain.
Lead contact and Principal author Martina Palomino-Schätzlein
NMR Facility, Centro de Investigación Príncipe Felipe, Valencia, Spain; and ProtoQSAR SL, CEEI (Centro Europeo de Empresas Innovadoras), Parque Tecnológico de Valencia, Valencia, Spain.

Date of data collection: 2018

Geographic location of data collection: Valencia, Spain. 39.4527, -0.3443

Information about funding sources or sponsorship that supported the collection of the data: 
All consumables, and part of laboratory equipment and human resources of this work was supported by the Carlos III Health Institute and the European Regional Development Fund (PI16/02064) and the Agency for Management of University and Research Grants (AGAUR) of Catalonia (2017SGR1303). Nuclear Magnetic Resonance Spectrometers employed for metabolite and lipid quantification have been funded by Generalitat Valenciana and co-financed with ERDF funds (OP ERDF of Comunitat Valenciana 2014–2020).

General description: 
The aim of the study was the search of new biomarkers of the diabetes type 1 (T1D) in red blood cells (RBCs). For this, the metabolomic and the lipidomic profile of RBCs from T1D patients and matched healthy controls was determined by NMR spectroscopy, and different multivariate discrimination models were built to select the metabolites and lipids that change most significantly. Relevant metabolites were further confirmed by univariate statistical analysis. Robust separation in the metabolomic and lipidomic profiles of RBCs from patients and controls was confirmed by orthogonal projection on latent structure discriminant analysis (OPLS-DA), random forest analysis and significance analysis of metabolites (SAM). The main changes were detected in the levels of amino acids, organic acids, creatine and phosphocreatine, lipid change length and choline derivatives, demonstrating changes ins glycolysis, BCAA metabolism, and phospholipid metabolism. 

Keywords: type 1 diabetes, NMR spectroscopy, metabolomics, lipidomics, red blood cells



--------------------------
SHARING/ACCESS INFORMATION
-------------------------- 

Open Access to data: access to the data is fully open

Citation for and links to publications that cite or use the data: 
manuscript submitted for publication

Links/relationships to previous or related data sets: N/A

Links to other publicly accessible locations of the data: N/A




--------------------
DATA & FILE OVERVIEW
--------------------

File list: 
-NMR_Integration_Data_Aqueous_Metabolomics
-NMR_Integration_Data_Lipidomics

Type of version of the dataset: the tables contain the data of the integration of the spectral peaks normalized to total intensity. 

Size: 196 KB + 19 KB



--------------------------
METHODOLOGICAL INFORMATION
--------------------------

Peripheral blood samples. Blood samples were collected followed a similar protocol as in previous studies.(Domingo-Ortí et al. 2021; Palomino-Schätzlein et al. 2020) Briefly, peripheral blood was collected under fasting conditions, stored at 4° C and processed within the first hour. The pellet of RBCs was obtained after pouring 5 mL of freshly extracted peripheral blood into a quartz tube with 10 mL of Ficoll and left standing around 30 minutes until 2 phases were separated by gravity. Then, the supernatant was poured off and the pellet was washed twice with 10 mL of cool Hank's balanced salt solution in a centrifuge at 200 g and 4 ºC for 10 minutes without brakes. Cell counting was performed using a dilution 1:1000 of cells on a Neubauer chamber and the purity was tested with flow cytometry. For storage, 200 µL of ice-cold methanol for quenching was added per 10 million cells and the samples frozen directly at -80 ºC. 
Extraction of polar metabolites. Metabolites were extracted from RBCs as described previously.(Palomino-Schätzlein et al. 2020) Briefly, frozen samples were placed on ice and allowed to thaw for 5 min. 800 µL of chloroform at 4°C was added. After 10 min, the samples were homogenized with a vortex, resuspended with a pipette and transferred to a plastic tube. For uniform cell breakage, the samples were placed in liquid nitrogen for 1 min and then allowed to thaw on ice for 2 min. This step was repeated twice. Afterwards, 1250 µL of distilled water and 1250 µL of chloroform were added and the sample vortexed. Then, samples were centrifuged at 13000 g for 20 min at 4°C to separate the phases. The upper phase that contains polar metabolites in a mixture of water/methanol was separated and lyophilized overnight, and the lower chloroform phase was evaporated under nitrogen flux. Extracts were stored at -80 ºC until preparing samples for NMR analysis. Frozen samples for NMR analysis were placed on ice and allowed to thaw for 5 min. 550 µl of NMR buffer (100 mM Na2HPO4 in D2O at pH 7.4) containing 0.1 mM deuterated trimethylsilylpropanoic acid as internal standard was added to the polar samples and transferred to a 5 mm NMR tube. 600 µl of deuterated chloroform containing 0.003% v/v tetramethylsilane was added to the non-polar samples and transferred to a 5 mm NMR tube. Samples were analysed the same day and stored at 4 ºC until the analysis.
NMR experiments. NMR spectra were recorded as described previously(Palomino-Schätzlein et al. 2020) at 27 ºC on a Bruker AVII-600 spectrometer using a 5 mm triple resonance cryoprobe and processed using Topspin3.2 software (Bruker Biospin). 1H 1D noesy NMR spectra were acquired with 256 free induction decays (FIDs). 64k data points were digitalized over a spectral width of 30 ppm for an optimal baseline correction. A 4s relaxation delay was incorporated between FIDs and water presaturation was applied for aqueous samples. The FID values were multiplied by an exponential function with a 0.5 Hz line broadening factor. 
Data analysis. 1H-NMR signals from the spectra were assigned with the help of previous data(Palomino-Schätzlein et al. 2018, 2020) and the spectral databases Human Metabolome Database (HMBD) and Biological Magnetic Resonance Bank (BMRB). (Ulrich et al. 2008; Wishart et al. 2022) In ambiguous cases, the assignment was confirmed by spiking the spectra with reference compounds. Spectra were normalized to total intensity, excluding glucose and solvent signals, to minimize the differences in concentration and experimental error during the extraction process. Optimal integration regions were defined for each metabolite, an attempt being made to select the signals without overlapping (Table S1). Integration was performed with global spectrum deconvolution (GSD) in MestreNova 12 (Mestrelab Research S. L.).
Domingo-Ortí, Inés et al. 2021. “Metabolic Footprint of Aging and Obesity in Red Blood Cells.” Aging 13(4): 4850–80.
Palomino-Schätzlein, Martina et al. 2020. “A Translational in Vivo and in Vitro Metabolomic Study Reveals Altered Metabolic Pathways in Red Blood Cells of Type 2 Diabetes.” Journal of Clinical Medicine 9(6): 1–18.
Ulrich, Eldon L. et al. 2008. “BioMagResBank.” Nucleic Acids Research 36(SUPPL. 1).
Wishart, D.S., Jewison, T., Guo, A.C., Wilson, M., Knox, C., Liu, Y., Djoumbou, Y., Mandal, R., Aziat, F., Dong, E., et al. (2013). HMDB 3.0--The Human Metabolome Database in 2013. Nucleic Acids Res 41, D801-807.


--------------------------
DATA-SPECIFIC INFORMATION 
--------------------------

Number of variables:
Two variables: Healthy control, patients with type 1 diabetes (T1D)
Both groups were matched for age, gender and BMI.

Number of cases/rows: 
20 samples were analysed for each group and are represented in columns. Each sample has for codes: code = individual samples coder, gender = W for woman, M = for man, age in years, class = diabetes type 1 (T1D) and healthy control (CT). 

Variable list, defining any abbreviations, units of measure, codes or symbols used:
-The peak limits are indicated in parts per million
-NMR values are the area integration of the normalised spectra.

Missing data codes:
For those metabolites with a clear assignment, the name is indicated in the first row. If the cell is named be U (for unknown) + left integration limit, the peak cannot be unambiguously assigned to a chemical species.

Specialized formats or other abbreviations used:
2,3-BPG: 2,3-Bisphosphoglyceric acid
ADP: adenosine diphosphate
AMP: adenosine monophosphate
ATP: adenosine triphosphate
cAMP: cyclic adenosine monophosphate
CDCl3: deuterated chloroform
CH2: lipid chain.
CH3: methyl group
DMA: dimethylamine
DMA: dimethylamine
FA: fatty acids
GDP: guanosine 5'-diphosphate
GMP: guanosine 5'-monophosphate
GPC: glycerophosphocholine
GSH: reduced glutathione
GSSG: oxidized glutathione
GTP: guanosine 5'-triphosphate
IMP: inosinic monophosphate
MUFA: monounsaturated fatty acids
NAD+: nicotinamide adenine dinucleotide
NADH: reduced nicotinamide adenine dinucleotide
NADP+: nicotinamide adenine dinucleotide phosphate
NADPH: reduced nicotinamide adenine dinucleotide phosphate
PC: phosphocholine
PEP: phosphoenopyruvate
PUFA: polyunsaturated fatty acids
TAG: tryacylglycerols
TMA: trimethylamine
TSP: Trimethylsilylpropanoic acid
UDP: uridine diphosphate
