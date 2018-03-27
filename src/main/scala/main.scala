import java.io.File

import DBcon.db_handler
import Ontologies.Ontology
import Preprocessing.parse
import com.github.tototoshi.csv._

object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"

  var input = "B cell lymphoma,Burkitt's lymphoma,Cornelia De Lange Syndrome 1; CDLS1; Nipped-B-Like; NIPBL,De Lange phenotype; developmental delay; profound retardation; seizures; 3 cousins are also affected; 46,XY,-22,+der (22)t(3;22)(q25.3;p12),Down syndrome,Dukes' type C, grade IV, colorectal adenocarcinoma,Ewing's sarcoma,IgAk myeloma,Metastatic neuroblastoma from bone marrow, T-acute lymphoblastic leukemia(T-ALL; type III cortical),T-cell acute lymphoblastic leukemia cell line ATCC CRL-2629,acute T cell leukemia,acute promyelocytic leukemia,apparently healthy,breast cancer (adenocarcinoma),carcinoma (prostate),cervical adenocarcinoma,chromosomal abnormalities; ICF syndrome,chronic myelogenous leukemia (CML),clinically affected; microcephaly; low frontal hairline; synophris; penciled arched eyebrows; short nose; crescent shaped mouth; hirsutism; micromelia; short thumbs; mental retardation; clinically normal monozygotic twin sister is GM13976,clinically normal; 4 paternal cousins have Cornelia de Lange syndrome; 46,XY, t(3;22)(q25.3;p12),clinically normal; monozygotic twin sister with Cornelia De Lange syndrome is GM13977,colorectal adenocarcinoma,colorectal carcinoma,control,endometrial adenocarcinoma,grade IV, adenocarcinoma,healthy,healthy with non-obstructive CAD (coronary artery disease),hepatocellular carcinoma,human B cell non-Hodgkin's lymphoma,immunoglobulin A lambda myeloma,large cell lymphoma; diffuse mixed histiocytic and lymphocytic lymphoma; follicular B cell lymphoma,malignant pluripotent embryonal carcinoma,malignant primitive neuroectodermal tumor,mammary ductal carcinoma,medulloblastoma,negative for Bac/Fung/Myc, negative for HIV-1/HBV/HCV,neuroblastoma,neuroglioma,normal,osteosarcoma,pancreatic carcinoma,plasmacytoma; myeloma,progeria,prostate adenocarcinoma,refractory immunoblastic B cell lymphoma progressed from follicular centroblastic/centrocytic lymphoma,renal cell adenocarcinoma,retinoblastoma,rhabdoid tumor (Wilm's tumor),squamous cell carcinoma; mesothelioma,submaxillar tumor,unknown"
  val zooma = Ontology.apply("zooma")
  val parsed_input = parse(input)
//  var zooma_results: Seq[List[String]] = Seq()
//  for (a <- parsed_input) {
//    for(b <- a._2){
//      val tmp_result = zooma.get_results(b,a._1)
//      tmp_result.foreach(zooma_results:+=_)
//    }
//  }

  var recommender_input = ""
  parsed_input.foreach(s => recommender_input+= s._2.mkString(",")+",")
  recommender_input = recommender_input.dropRight(1)

  val recommender = Ontology.apply("recommender")
  var result = recommender.get_results(recommender_input,parsed_input)
  println("recsys ok")
//  val default = (-1,"")
//  val value = "nipped b like"
//  println(parsed_input.find(_._2.contains(value)).getOrElse(default)._1)

  val f = new File(path+"result-recsys-parsed-666.csv")
  val writer = CSVWriter.open(f)
  writer.writeAll(result)
  writer.close()


//  println(parsed_input)
//  val zooma_input = parsed_input.replace(' ','+').replace(',','+')
//  println(zooma_input)
//  var input_l = parsed_input.split(",").toList
//  println(input_l.length, input.replace(""""""", "").replace("\n", ",").replace("""\""", "").replace("/", ",").replace(";", ",").split(",").length)
//  zooma.get_results("NIPBL")

//  for (input <- input_l){
//    val tmp_result = zooma.get_results(input)
//    tmp_result.foreach(zooma_results:+=_)
//  }

//  println(result)
//  var terms_found = List[String]()
//  for (s <- result){
//    terms_found++=s(1).split("; ")
//  }
//  terms_found = terms_found.distinct
//  val unwanted = terms_found.toSet
//  var terms_not_found = input_l.filterNot(unwanted)
//  var terms_found_n = terms_found.length
//
//  var flag = true
//  var i = 0
//  while (flag){
//    input = ""
//    terms_not_found.foreach(input += _ +",")
//    result = recommender.get_results(parse(input))
//    for (s <- result){
//      terms_found++=s(1).split("; ")
//    }
//    terms_found = terms_found.distinct
//    input_l = parse(input).split(",").toList
//    terms_not_found = input_l.filterNot(terms_found.toSet)
//    i+=1
//    if (terms_found.length == terms_found_n) flag = false
//    else terms_found_n = terms_found.length
//    println("ciclo "+i)
//    println(terms_found_n+" trovati")
//    println(terms_not_found)
//  }
//  println()
//  println(terms_found)


}
