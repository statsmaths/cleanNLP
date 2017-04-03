package edu.richmond.nlp;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.*;

public class CSVTokenDocumentWriter {

  public String docID = "";
  public String header = "";

  private static final Map<String, String> posMap;
  static
  {
      posMap = new HashMap<String, String>();
      posMap.put("!", ".");
      posMap.put("#", ".");
      posMap.put("$", ".");
      posMap.put("''", ".");
      posMap.put("(", ".");
      posMap.put(")", ".");
      posMap.put(",", ".");
      posMap.put("-LRB-", ".");
      posMap.put("-RRB-", ".");
      posMap.put(".", ".");
      posMap.put(":", ".");
      posMap.put("?", ".");
      posMap.put("CC", "CONJ");
      posMap.put("CD", "NUM");
      posMap.put("CD|RB", "X");
      posMap.put("DT", "DET");
      posMap.put("EX", "DET");
      posMap.put("FW", "X");
      posMap.put("IN", "ADP");
      posMap.put("IN|RP", "ADP");
      posMap.put("JJ", "ADJ");
      posMap.put("JJR", "ADJ");
      posMap.put("JJRJR", "ADJ");
      posMap.put("JJS", "ADJ");
      posMap.put("JJ|RB", "ADJ");
      posMap.put("JJ|VBG", "ADJ");
      posMap.put("LS", "X");
      posMap.put("MD", "VERB");
      posMap.put("NN", "NOUN");
      posMap.put("NNP", "NOUN");
      posMap.put("NNPS", "NOUN");
      posMap.put("NNS", "NOUN");
      posMap.put("NN|NNS", "NOUN");
      posMap.put("NN|SYM", "NOUN");
      posMap.put("NN|VBG", "NOUN");
      posMap.put("NP", "NOUN");
      posMap.put("PDT", "DET");
      posMap.put("POS", "PRT");
      posMap.put("PRP", "PRON");
      posMap.put("PRP$", "PRON");
      posMap.put("PRP|VBP", "PRON");
      posMap.put("PRT", "PRT");
      posMap.put("RB", "ADV");
      posMap.put("RBR", "ADV");
      posMap.put("RBS", "ADV");
      posMap.put("RB|RP", "ADV");
      posMap.put("RB|VBG", "ADV");
      posMap.put("RN", "X");
      posMap.put("RP", "PRT");
      posMap.put("SYM", "X");
      posMap.put("TO", "PRT");
      posMap.put("UH", "X");
      posMap.put("VB", "VERB");
      posMap.put("VBD", "VERB");
      posMap.put("VBD|VBN", "VERB");
      posMap.put("VBG", "VERB");
      posMap.put("VBG|NN", "VERB");
      posMap.put("VBN", "VERB");
      posMap.put("VBP", "VERB");
      posMap.put("VBP|TO", "VERB");
      posMap.put("VBZ", "VERB");
      posMap.put("VP", "VERB");
      posMap.put("WDT", "DET");
      posMap.put("WH", "X");
      posMap.put("WP", "PRON");
      posMap.put("WP$", "PRON");
      posMap.put("WRB", "ADV");
      posMap.put("``", ".");
  }

  public CSVTokenDocumentWriter(String docID) {
    this.docID = docID;
    this.header = String.format("id,sid,tid,word,lemma,upos,pos,cid%n");
  }

  public String print(CoreMap sentence) {

    StringBuilder sb = new StringBuilder();

    List<CoreLabel> tokens = sentence.get(CoreAnnotations.TokensAnnotation.class);

    // add sentence root as a token
    if (tokens.size() > 0) {
      sb.append(String.format("%s,%d,0,\"ROOT\",\"ROOT\",\"\",\"\",%n", docID, tokens.get(0).sentIndex()));
    }


    for (CoreLabel token : tokens) {

      String word = token.word();
      String lemma = token.getString(CoreAnnotations.LemmaAnnotation.class, "");
      String pos = token.getString(CoreAnnotations.PartOfSpeechAnnotation.class, "");
      String upos = posMap.get(pos);
      if (upos == null) upos = "";

      String charOffsetStart = "";
      if (token.containsKey(CoreAnnotations.CharacterOffsetBeginAnnotation.class) && token.containsKey(CoreAnnotations.CharacterOffsetEndAnnotation.class)) {
        charOffsetStart = Integer.toString(token.get(CoreAnnotations.CharacterOffsetBeginAnnotation.class));
      }

      sb.append(String.format("%s,%d,%d,\"%s\",\"%s\",\"%s\",\"%s\",%s%n", docID,
                              token.sentIndex(), token.index(), word, lemma, upos, pos,
                              charOffsetStart));
    }

    return sb.toString();
  }

  public void setDocID (String docID) {
    this.docID = docID;
  }
}