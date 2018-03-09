import BioPortal.Requests._
import play.api.libs.json.{JsValue, Json}

import Ontologies.Ontology
import Preprocessing.parse
import com.github.tototoshi.csv._

import  java.io._

object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"

  val input2 = "B cell lymphoma,Burkitt's lymphoma,Cornelia De Lange Syndrome 1; CDLS1; Nipped-B-Like; NIPBL,De Lange phenotype; developmental delay; profound retardation; seizures; 3 cousins are also affected; 46,XY,-22,+der (22)t(3;22)(q25.3;p12),Down syndrome,Dukes' type C, grade IV, colorectal adenocarcinoma,Ewing's sarcoma,IgAk myeloma,Metastatic neuroblastoma from bone marrow,T-acute lymphoblastic leukemia (T-ALL; type III cortical),T-cell acute lymphoblastic leukemia cell line ATCC CRL-2629,acute T cell leukemia,acute promyelocytic leukemia,apparently healthy,breast cancer (adenocarcinoma),carcinoma (prostate),cervical adenocarcinoma,chromosomal abnormalities; ICF syndrome,chronic myelogenous leukemia (CML),clinically affected; microcephaly; low frontal hairline; synophris; penciled arched eyebrows; short nose; crescent shaped mouth; hirsutism; micromelia; short thumbs; mental retardation; clinically normal monozygotic twin sister is GM13976,clinically normal; 4 paternal cousins have Cornelia de Lange syndrome; 46,XY, t(3;22)(q25.3;p12)n,clinically normal; monozygotic twin sister with Cornelia De Lange syndrome is GM13977,colorectal adenocarcinoma,colorectal carcinoma,control,endometrial adenocarcinoma,grade IV, adenocarcinoma,healthy,healthy with non-obstructive CAD (coronary artery disease),hepatocellular carcinoma,human B cell non-Hodgkin's lymphoma,immunoglobulin A lambda myeloma,large cell lymphoma; diffuse mixed histiocytic and lymphocytic lymphoma; follicular B cell lymphoma,malignant pluripotent embryonal carcinoma,malignant primitive neuroectodermal tumor,mammary ductal carcinoma,medulloblastoma,negative for Bac/Fung/Myc, negative for HIV-1/HBV/HCV,neuroblastoma,neuroglioma,normal,osteosarcoma,pancreatic carcinoma,plasmacytoma; myeloma,progeria,prostate adenocarcinoma,refractory immunoblastic B cell lymphoma progressed from follicular centroblastic/centrocytic lymphoma,renal cell adenocarcinoma,retinoblastoma,rhabdoid tumor (Wilm's tumor),squamous cell carcinoma; mesothelioma,submaxillar tumor,unknown"
  val input = "leukocyte, phagocyte, motile cell, metabolising cell, dendritic cell, pipette"
  val recommender = Ontology.apply("recommender")
  recommender.say()
  //println(recommender.get_results(input))
  //nicola bergamasco
  val f = new File(path+"results2.csv")
  val writer = CSVWriter.open(f)
  println(recommender.get_results(parse(input)))
  writer.writeAll(recommender.get_results(parse(input)))
}