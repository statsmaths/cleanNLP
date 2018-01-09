package edu.richmond.nlp;

import java.util.List;
import java.util.Collection;
import java.util.Map;

import edu.stanford.nlp.coref.CorefCoreAnnotations;
import edu.stanford.nlp.coref.data.CorefChain;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.semgraph.*;
import edu.stanford.nlp.trees.GrammaticalRelation;
import edu.stanford.nlp.pipeline.Annotation;

public class CSVCoreferenceDocumentWriter {

  public String docID = "";
  public String header = "";

  public CSVCoreferenceDocumentWriter(String docID) {
    this.docID = docID;
    this.header = String.format("id,rid,mid,mention,mention_type,number,gender,animacy,sid,tid,tid_end,tid_head%n");
  }

  public String print(Annotation doc) {

    StringBuilder sb = new StringBuilder();
    Map<Integer, CorefChain> corefChains = doc.get(CorefCoreAnnotations.CorefChainAnnotation.class);

    if (corefChains != null) {
      for (CorefChain chain : corefChains.values()) {
        CorefChain.CorefMention source = chain.getRepresentativeMention();
        for (CorefChain.CorefMention mention : chain.getMentionsInTextualOrder()) {
          int coref_representative = (mention == source) ? 1 : 0;

          sb.append(String.format("doc%s,%d,%d,\"%s\",%s,%s,%s,%s,%d,%d,%d,%d%n", docID,
                    chain.getChainID(), mention.mentionID, mention.mentionSpan,
                    mention.mentionType.toString(), mention.number.toString(), mention.gender.toString(),
                    mention.animacy.toString(), mention.sentNum,
                    mention.startIndex, mention.endIndex - 1, mention.headIndex));
        }
      }
    }

    return sb.toString();
  }

  public void setDocID(String docID) {
    this.docID = docID;
  }

}