package edu.richmond.nlp;

import java.util.List;
import java.util.Collection;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.semgraph.*;
import edu.stanford.nlp.trees.GrammaticalRelation;
import edu.stanford.nlp.ie.util.RelationTriple;
import edu.stanford.nlp.naturalli.*;

public class CSVTripleDocumentWriter {

  public String docID = "";
  public String header = "";

  public CSVTripleDocumentWriter(String docID) {
    this.docID = docID;
    this.header = String.format("id,subject,object,relation,confidence,be_prefix,be_suffix," +
                                "of_suffix,tmod,sid,tid_subject,tid_subject_end,tid_object," +
                                "tid_object_end,tid_relation,tid_relation_end%n");
  }

  public String print(CoreMap sentence) {

    StringBuilder sb = new StringBuilder();
    Collection<RelationTriple> openieTriples = sentence.get(NaturalLogicAnnotations.RelationTriplesAnnotation.class);

    if (openieTriples != null) {
      for (RelationTriple triple : openieTriples ) {

        sb.append(String.format("%s,\"%s\",\"%s\",\"%s\",%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d%n", docID,
                  triple.subjectGloss(), triple.objectGloss(), triple.relationGloss(), triple.confidenceGloss(),
                  (triple.isPrefixBe()) ? 1 : 0, (triple.isSuffixBe()) ? 1 : 0,
                  (triple.isSuffixOf()) ? 1 : 0, (triple.istmod()) ? 1 : 0,
                  triple.subject.get(0).sentIndex(),
                  triple.subjectTokenSpan().first(), triple.subjectTokenSpan().second(),
                  triple.objectTokenSpan().first(), triple.objectTokenSpan().second(),
                  triple.relationTokenSpan().first(), triple.relationTokenSpan().second()));

      }
    }

    return sb.toString();
  }

  public void setDocID (String docID) {
    this.docID = docID;
  }

}