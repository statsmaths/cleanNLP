package edu.richmond.nlp;

import java.util.List;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.semgraph.*;
import edu.stanford.nlp.trees.GrammaticalRelation;

public class CSVDependencyDocumentWriter {

  public String docID = "";
  public String header = "";

  public CSVDependencyDocumentWriter(String docID) {
    this.docID = docID;
    this.header = String.format("id,sid,tid,tid_target,relation,relation_full%n");
  }

  public String print(CoreMap sentence) {

    StringBuilder sb = new StringBuilder();
    SemanticGraph enhancedDependencies = sentence.get(SemanticGraphCoreAnnotations.EnhancedPlusPlusDependenciesAnnotation.class);
    //SemanticGraph enhancedDependencies = sentence.get(SemanticGraphCoreAnnotations.BasicDependenciesAnnotation.class);

    if (enhancedDependencies != null) {
      // Need to add the root ourselves
      for (IndexedWord root : enhancedDependencies.getRoots()) {
        String rel = GrammaticalRelation.ROOT.getLongName();
        rel = rel.replaceAll("\\s+", ""); // future proofing

        sb.append(String.format("doc%s,%d,%d,%d,%s,%s%n", docID, root.sentIndex() + 1,
                                0, root.index(), rel, rel));
      }

      for (SemanticGraphEdge edge : enhancedDependencies.edgeListSorted()) {
        GrammaticalRelation reln = edge.getRelation();
        String relnName = reln == null ? "" : reln.toString();
        String relnNameBasic = relnName.split(":",-1)[0];

        sb.append(String.format("doc%s,%d,%d,%d,%s,%s%n", docID, edge.getSource().sentIndex() + 1,
                                edge.getSource().index(), edge.getTarget().index(),
                                relnNameBasic, relnName));
      }
    }

    return sb.toString();
  }

  public void setDocID(String docID) {
    this.docID = docID;
  }

}