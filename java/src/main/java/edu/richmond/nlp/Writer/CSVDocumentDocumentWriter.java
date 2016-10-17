package edu.richmond.nlp;

import java.util.List;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.pipeline.Annotation;

public class CSVDocumentDocumentWriter {

  public String docID = "";
  public String header = "";
  public String language = "";
  public String starttime = "";
  public String uri = "";
  public String version = "";

  public CSVDocumentDocumentWriter(String docID) {
    this.docID = docID;
    this.header = String.format("id,time,version,language,uri%n");
  }

  public String print(Annotation doc) {

    StringBuilder sb = new StringBuilder();
    sb.append(String.format("%s,%s,%s,%s,\"%s\"%n", docID, this.starttime,
                            this.version, this.language, this.uri));

    return sb.toString();
  }

  public void setDocID (String docID) {
    this.docID = docID;
  }

  public void setLanguage(String language) {
    this.language = language;
  }

  public void setStartTime(String starttime) {
    this.starttime = starttime;
  }

  public void setURI(String uri) {
    this.uri = uri;
  }

  public void setVersion(String version) {
    this.version = version;
  }

}