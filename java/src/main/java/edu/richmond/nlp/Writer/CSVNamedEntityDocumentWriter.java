package edu.richmond.nlp;

import java.util.List;
import java.util.Collection;
import java.util.Map;

import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.semgraph.*;
import edu.stanford.nlp.trees.GrammaticalRelation;

public class CSVNamedEntityDocumentWriter {

  public String docID = "";
  public String header = "";

  public CSVNamedEntityDocumentWriter(String docID) {
    this.docID = docID;
    this.header = String.format("id,sid,tid,tid_end,entity_type,entity,entity_normalized%n");
  }

  public String print(CoreMap sentence) {

    StringBuilder sb = new StringBuilder();
    List<CoreLabel> tokens = sentence.get(CoreAnnotations.TokensAnnotation.class);

    String entity = "";
    String entityNorm = "";
    String entityType = "";
    int tid = 0;
    int tidEnd = 0;
    int sid = 0;
    for (CoreLabel token : tokens) {
      String ner = token.getString(CoreAnnotations.NamedEntityTagAnnotation.class, "");

      if (ner.equals("O")) {
        if (!entity.equals("")) {
          sb.append(String.format("doc%s,%d,%d,%d,%s,\"%s\",\"%s\"%n",
                                  docID, sid, tid, tidEnd, entityType, entity, entityNorm));
          entity = "";
          entityType = "";
        }
      } else if (!ner.equals("")) {
        if (entity.equals("")) {
          tid = token.index();
          sid = token.sentIndex();
          entityNorm = token.getString(CoreAnnotations.NormalizedNamedEntityTagAnnotation.class, "");
          entity = token.word();
        } else {
          entity += " " + token.word();
        }
        tidEnd = token.index();
        entityType = ner;
      }

    }

    // flush last entity in case it ends the sentence
    if (!entity.equals("")) {
      sb.append(String.format("doc%s,%d,%d,%d,%s,\"%s\",\"%s\"%n", docID, sid + 1, tid, tidEnd, entityType, entity, entityNorm));
    }


    return sb.toString();
  }

  public void setDocID (String docID) {
    this.docID = docID;
  }
}
