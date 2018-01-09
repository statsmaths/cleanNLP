package edu.richmond.nlp;

import java.util.List;
import java.util.Collection;
import java.util.Map;

import org.ejml.simple.SimpleMatrix;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreePrint;
import edu.stanford.nlp.trees.TreeCoreAnnotations;
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations;
import edu.stanford.nlp.neural.rnn.RNNCoreAnnotations;

public class CSVSentenceDocumentWriter {

  public String docID = "";
  public String header = "";

  public CSVSentenceDocumentWriter(String docID) {
    this.docID = docID;
    this.header = String.format("id,sid,sentiment%n");
  }

  public String print(CoreMap sentence) {

    StringBuilder sb = new StringBuilder();
    List<CoreLabel> tokens = sentence.get(CoreAnnotations.TokensAnnotation.class);
    Tree sentimentTree = sentence.get(SentimentCoreAnnotations.SentimentAnnotatedTree.class);

    if (sentimentTree != null) {
      Integer predClass = RNNCoreAnnotations.getPredictedClass(sentimentTree);
      String predClassString = (predClass == null) ? "NA" : predClass.toString();
      SimpleMatrix sm = RNNCoreAnnotations.getPredictions(sentimentTree);

      sb.append(String.format("doc%s,%d,%s%n", docID, tokens.get(0).sentIndex() + 1, predClassString));
    }

    return sb.toString();
  }

  public void setDocID (String docID) {
    this.docID = docID;
  }
}
