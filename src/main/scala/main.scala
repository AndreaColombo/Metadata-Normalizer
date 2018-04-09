import java.util.Calendar

import DBcon._
import Ontologies.Ontology
import Utils.Preprocessing.{lookup, parse}
import score_calculator.get_recommender_score


object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"

  var input = "B cell lymphoma,Burkitt's lymphoma,Cornelia De Lange Syndrome 1; CDLS1; Nipped-B-Like;NIPBL,De Lange phenotype; developmental delay; profound retardation; seizures;3 cousins are also affected; 46,XY,-22,+der (22)t(3;22)(q25.3;p12),Down syndrome,Dukes' type C, grade IV, colorectal adenocarcinoma,Ewing's sarcoma,IgAk myeloma,Metastatic neuroblastoma from bone marrow, T-acute lymphoblastic leukemia(T-ALL; type III cortical),T-cell acute lymphoblastic leukemia cell line ATCC CRL-2629,acute T cell leukemia,acute promyelocytic leukemia,apparently healthy,breast cancer (adenocarcinoma),carcinoma (prostate),cervical adenocarcinoma,chromosomal abnormalities; ICF syndrome,chronic myelogenous leukemia (CML),clinically affected; microcephaly; low frontal hairline; synophris; penciled arched eyebrows; short nose; crescent shaped mouth; hirsutism; micromelia; short thumbs; mental retardation; clinically normal monozygotic twin sister is GM13976,clinically normal; 4 paternal cousins have Cornelia de Lange syndrome; 46,XY, t(3;22)(q25.3;p12),clinically normal; monozygotic twin sister with Cornelia De Lange syndrome is GM13977,colorectal adenocarcinoma,colorectal carcinoma,control,endometrial adenocarcinoma,grade IV, adenocarcinoma,healthy,healthy with non-obstructive CAD (coronary artery disease),hepatocellular carcinoma,human B cell non-Hodgkin's lymphoma,immunoglobulin A lambda myeloma,large cell lymphoma; diffuse mixed histiocytic and lymphocytic lymphoma; follicular B cell lymphoma,malignant pluripotent embryonal carcinoma,malignant primitive neuroectodermal tumor,mammary ductal carcinoma,medulloblastoma,negative for Bac/Fung/Myc, negative for HIV-1/HBV/HCV,neuroblastoma,neuroglioma,normal,osteosarcoma,pancreatic carcinoma,plasmacytoma; myeloma,progeria,prostate adenocarcinoma,refractory immunoblastic B cell lymphoma progressed from follicular centroblastic/centrocytic lymphoma,renal cell adenocarcinoma,retinoblastoma,rhabdoid tumor (Wilm's tumor),squamous cell carcinoma; mesothelioma,submaxillar tumor,unknown"

  //ZOOMA
  val zooma = Ontology.apply("zooma")

  //RECOMMENDER
  val recommender = Ontology.apply("recommender")

  //BIOPORTAL
  val bioportal = Ontology.apply("bioportal")

  //OLS
  val ols = Ontology.apply("ols")

  score_calculator.calculate_ontology_score()


//  for (a <- term_type){
//
//    val b = parse(query_handler.run_q1(a))
//
//    println(a)
//    get_timestamp()
//
//    val tmp = b.split(",")
//    val tmp1 = tmp.splitAt(tmp.length/2)._1.toList
//    val tmp2 = tmp.splitAt(tmp.length/2)._2.toList
//    val recsys1 = tmp1.splitAt(tmp1.length/2)._1.mkString(",")
//    val recsys2 = tmp1.splitAt(tmp1.length/2)._2.mkString(",")
//    val recsys3 = tmp2.splitAt(tmp2.length/2)._1.mkString(",")
//    val recsys4 = tmp2.splitAt(tmp2.length/2)._2.mkString(",")

//    println("bioportal inizio")
//    db_handler.insert(bioportal.input(b))
//    println("bioportal fine")
//    get_timestamp()
//
//  recommender completo per tissue, disease e cell line

//    println("recsys 1 inizio")
//    recommender.input(recsys1)
//    println("recsys 1 fine")
//    get_timestamp()

//    println("recsys 2 inizio")
//    db_handler.insert(recommender.input(recsys2))
//    println("recsys 2 fine")
//    get_timestamp()
//
//    println("recsys 3 inizio")
//    db_handler.insert(recommender.input(recsys3))
//    println("recsys 3 fine")
//    get_timestamp()
//
//    println("recsys 4 inizio")
//    db_handler.insert(recommender.input(recsys4))
//    println("recsys 4 fine")
//    get_timestamp()

//    println("zooma inizio")
//    db_handler.insert(zooma.input(b))
//    println("zooma fine")
//    get_timestamp()

//    println("ols inizio")
//    db_handler.insert(ols.input(b))
//    println("ols fine")
//    get_timestamp()
//  }

  def get_timestamp() = {
    val now = Calendar.getInstance()
    println(now.get(Calendar.HOUR_OF_DAY)+":"+now.get(Calendar.MINUTE)+":"+now.get(Calendar.SECOND))
  }
}

