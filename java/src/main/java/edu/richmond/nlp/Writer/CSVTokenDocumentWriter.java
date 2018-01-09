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

      // French mappings
      posMap.put("ADJ", "ADJ");
      posMap.put("ADJWH", "ADJ");
      posMap.put("ADV", "ADV");
      posMap.put("ADVWH", "ADV");
      posMap.put("CC", "CONJ"); // DUPLICATED FROM ENGLISH
      posMap.put("CLO", "PRON");
      posMap.put("CLR", "PRON");
      posMap.put("CLS", "PRON");
      posMap.put("CS", "CONJ");
      posMap.put("DET", "DET");
      posMap.put("DETWH", "DET");
      posMap.put("ET", "X");
      posMap.put("I", "X");
      posMap.put("N", "NOUN");
      posMap.put("NC", "NOUN");
      posMap.put("NPP", "NOUN");
      posMap.put("P", "ADP");
      posMap.put("P+D", "ADP");
      posMap.put("P+PRON", "ADP");
      posMap.put("PONCT", ".");
      posMap.put("PUNC", ".");
      posMap.put("PREF", "PRT");
      posMap.put("PRO", "PRON");
      posMap.put("PROREL", "PRON");
      posMap.put("PROWH", "PRON");
      posMap.put("V", "VERB");
      posMap.put("VIMP", "VERB");
      posMap.put("VINF", "VERB");
      posMap.put("VPP", "VERB");
      posMap.put("VPR", "VERB");
      posMap.put("VS", "VERB");

      // German mappings
      posMap.put("$*LRB*", ".");
      posMap.put("$,", ".");
      posMap.put("$.", ".");
      posMap.put("--", ".");
      posMap.put("ADJA", "ADJ");
      posMap.put("ADJD", "ADJ");
      posMap.put("ADV", "ADV");
      posMap.put("APPO", "ADP");
      posMap.put("APPR", "ADP");
      posMap.put("APPRART", "ADP");
      posMap.put("APZR", "ADP");
      posMap.put("ART", "DET");
      posMap.put("CARD", "NUM");
      posMap.put("FM", "X");
      posMap.put("ITJ", "X");
      posMap.put("KOKOM", "CONJ");
      posMap.put("KON", "CONJ");
      posMap.put("KOUI", "CONJ");
      posMap.put("KOUS", "CONJ");
      posMap.put("NE", "NOUN");
      posMap.put("NN", "NOUN");
      posMap.put("PDAT", "PRON");
      posMap.put("PDS", "PRON");
      posMap.put("PIAT", "PRON");
      posMap.put("PIDAT", "PRON");
      posMap.put("PIS", "PRON");
      posMap.put("PPER", "PRON");
      posMap.put("PPOSAT", "PRON");
      posMap.put("PPOSS", "PRON");
      posMap.put("PRELAT", "PRON");
      posMap.put("PRELS", "PRON");
      posMap.put("PRF", "PRON");
      posMap.put("PROAV", "PRON");
      posMap.put("PTKA", "PRT");
      posMap.put("PTKANT", "PRT");
      posMap.put("PTKNEG", "PRT");
      posMap.put("PTKVZ", "PRT");
      posMap.put("PTKZU", "PRT");
      posMap.put("PWAT", "PRON");
      posMap.put("PWAV", "PRON");
      posMap.put("PWS", "PRON");
      posMap.put("TRUNC", "X");
      posMap.put("VAFIN", "VERB");
      posMap.put("VAIMP", "VERB");
      posMap.put("VAINF", "VERB");
      posMap.put("VAPP", "VERB");
      posMap.put("VMFIN", "VERB");
      posMap.put("VMINF", "VERB");
      posMap.put("VMPP", "VERB");
      posMap.put("VVFIN", "VERB");
      posMap.put("VVIMP", "VERB");
      posMap.put("VVINF", "VERB");
      posMap.put("VVIZU", "VERB");
      posMap.put("VVPP", "VERB");
      posMap.put("XY", "X");

      // Spanish Mapping
      posMap.put("ADJ", "ADJ");
      posMap.put("ORD", "ADJ");
      posMap.put("QU", "ADJ");
      posMap.put("PAL", "ADP");
      posMap.put("PDEL", "ADP");
      posMap.put("PREP", "ADP");
      posMap.put("PREP/DEL", "ADP");
      posMap.put("ADV", "ADV");
      posMap.put("NEG", "ADV");
      posMap.put("CC", "CONJ");
      posMap.put("CCAD", "CONJ");
      posMap.put("CCNEG", "CONJ");
      posMap.put("CQUE", "CONJ");
      posMap.put("CSUBF", "CONJ");
      posMap.put("CSUBI", "CONJ");
      posMap.put("CSUBX", "CONJ");
      posMap.put("REL", "CONJ");
      posMap.put("ART", "DET");
      posMap.put("NC", "NOUN");
      posMap.put("NMEA", "NOUN");
      posMap.put("NMON", "NOUN");
      posMap.put("NP", "NOUN");
      posMap.put("CARD", "NUM");
      posMap.put("DM", "PRON");
      posMap.put("INT", "PRON");
      posMap.put("PPC", "PRON");
      posMap.put("PPO", "PRON");
      posMap.put("PPX", "PRON");
      posMap.put("SE", "PRON");
      posMap.put("VCLICger", "VERB");
      posMap.put("VCLICinf", "VERB");
      posMap.put("VCLICfin", "VERB");
      posMap.put("VEadj", "VERB");
      posMap.put("VEfin", "VERB");
      posMap.put("VEinf", "VERB");
      posMap.put("VEger", "VERB");
      posMap.put("VHadj", "VERB");
      posMap.put("VHfin", "VERB");
      posMap.put("VHger", "VERB");
      posMap.put("VHinf", "VERB");
      posMap.put("VLadj", "VERB");
      posMap.put("VLfin", "VERB");
      posMap.put("VLger", "VERB");
      posMap.put("VLinf", "VERB");
      posMap.put("VMadj", "VERB");
      posMap.put("VMfin", "VERB");
      posMap.put("VMger", "VERB");
      posMap.put("VMinf", "VERB");
      posMap.put("VSadj", "VERB");
      posMap.put("VSfin", "VERB");
      posMap.put("VSger", "VERB");
      posMap.put("VSinf", "VERB");
      posMap.put("VCLIinf", "VERB");
      posMap.put("VCLIger", "VERB");
      posMap.put("VCLIfin", "VERB");
      posMap.put("ITJN", "X");
      posMap.put("ACRNM", "X");
      posMap.put("ALFP", "X");
      posMap.put("ALFS", "X");
      posMap.put("CODE", "X");
      posMap.put("FO", "X");
      posMap.put("PE", "X");
      posMap.put("PNC", "X");
      posMap.put("SYM", "X");
      posMap.put("UMMX", "X");
      posMap.put("BACKSLASH", ".");
      posMap.put("CM", ".");
      posMap.put("COLON", ".");
      posMap.put("DASH", ".");
      posMap.put("DOTS", ".");
      posMap.put("FS", ".");
      posMap.put("LP", ".");
      posMap.put("PERCT", ".");
      posMap.put("QT", ".");
      posMap.put("RP", ".");
      posMap.put("SLASH", ".");
      posMap.put("SEMICOLON", ".");

      // Arabic Mapping
      posMap.put("--", "X");
      posMap.put("A-", "ADJ");
      posMap.put("C-", "CONJ");
      posMap.put("D-", "ADV");
      posMap.put("F-", "PRT");
      posMap.put("FI", "PRT");
      posMap.put("FN", "PRT");
      posMap.put("G-", ".");
      posMap.put("I-", "X");
      posMap.put("N-", "NOUN");
      posMap.put("P-", "ADP");
      posMap.put("Q-", "NUM");
      posMap.put("S-", "PRON");
      posMap.put("SD", "PRON");
      posMap.put("SR", "PRON");
      posMap.put("VC", "VERB");
      posMap.put("VI", "VERB");
      posMap.put("VP", "VERB");
      posMap.put("Y-", "X");
      posMap.put("Z-", "NOUN");
      posMap.put("_", "X");

      // These are the English Mappings:
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
      sb.append(String.format("doc%s,%d,0,\"ROOT\",\"ROOT\",\"\",\"\",%n", docID, tokens.get(0).sentIndex() + 1));
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

      sb.append(String.format("doc%s,%d,%d,\"%s\",\"%s\",\"%s\",\"%s\",%s%n", docID,
                              token.sentIndex() + 1, token.index(), word, lemma, upos, pos,
                              charOffsetStart));
    }

    return sb.toString();
  }

  public void setDocID (String docID) {
    this.docID = docID;
  }
}