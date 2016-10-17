package edu.richmond.nlp;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import edu.stanford.nlp.pipeline.StanfordCoreNLP;

public final class RunClient {

  public static void main(final String[] args) throws FileNotFoundException, UnsupportedEncodingException, IOException {

    // set properties
    Properties prop = new Properties();
    prop.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, mention, dcoref, natlog, openie, sentiment");
    // prop.setProperty("annotators", "tokenize, ssplit, pos, lemma, parse");

    // Initalize the CoreNLP pipeline
    StanfordCoreNLP scnlp = new StanfordCoreNLP(prop);

    // Construct a list of files
    List<String> file_list = new ArrayList<String>();
    // file_list.add("/Users/taylor/files/sotu/sample.txt");
    file_list.add("/Users/taylor/files/sotu/raw_text/008.txt");
    file_list.add("/Users/taylor/files/sotu/raw_text/007.txt");
    file_list.add("/Users/taylor/files/sotu/raw_text/006.txt");
    file_list.add("/Users/taylor/files/sotu/raw_text/005.txt");
    file_list.add("/Users/taylor/files/sotu/raw_text/004.txt");
    file_list.add("/Users/taylor/files/sotu/raw_text/003.txt");
    file_list.add("/Users/taylor/files/sotu/raw_text/002.txt");
    file_list.add("/Users/taylor/files/sotu/raw_text/001.txt");

    // Call the AnnotationProcessor
    AnnotationProcessor ap = new AnnotationProcessor("/Users/taylor/Desktop/output/", "en", 0);
    ap.processFiles(file_list, scnlp);

    // String[] files = {"/Users/taylor/files/sotu/raw_text/240.txt"};
    // ap.processFiles(files, scnlp);
  }

}
